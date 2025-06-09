--- The verdict module for the verdict Lua static analysis tool.
-- @module verdict
local Parser = require("verdict.stuff.parser")
local Lexer = require("verdict.stuff.lexer")

local analyzer = require("verdict.analyzer")

local verdict = {}

verdict.Lexer = Lexer
verdict.Parser = Parser

verdict.analyzer = analyzer

--- Analyzes Lua source code and returns type checking results
---@param source string The Lua source code to analyze
---@return table Analysis results containing errors and type information
function verdict.analyze(source)
    local results = {
        errors = {},
        warnings = {},
        ast = nil,
        tokens = nil
    }

    -- Step 1: Lexical analysis
    local lexer = Lexer.new(source)
    local ok, tokens = pcall(function()
        return lexer:tokenize()
    end)

    if not ok then
        table.insert(results.errors, {
            type = "lexer_error",
            message = tokens, -- The error message
            phase = "lexical_analysis"
        })
        return results
    end

    results.tokens = tokens

    -- Step 2: Parsing
    local parser = Parser.new(tokens)
    local ast
    ok, ast = pcall(function()
        return parser:parse()
    end)

    if not ok then
        table.insert(results.errors, {
            type = "parser_error",
            message = ast, -- The error message
            phase = "parsing"
        })
        return results
    end

    results.ast = ast

    -- Step 3: Type analysis

    -- Uncomment the following lines to debug the AST structure
    -- for i, stmt in ipairs(ast or {}) do
    --     print(i .. ". " .. stmt.type .. " - " .. (stmt.data and stmt.data.name or "no name"))
    -- end
    -- print("===========================")

    local type_inference = analyzer.TypeInference.new()
    local analysis_result = type_inference:analyze(ast)

    -- Convert analyzer errors and warnings to our result format
    for _, error in ipairs(analysis_result.errors or {}) do
        table.insert(results.errors, {
            type = "type_error",
            message = error.message,
            line = error.line,
            column = error.column,
            phase = "type_analysis"
        })
    end

    -- Add warnings to results
    results.warnings = {}
    for _, warning in ipairs(analysis_result.warnings or {}) do
        table.insert(results.warnings, {
            type = "type_warning",
            message = warning.message,
            line = warning.line,
            column = warning.column,
            phase = "type_analysis"
        })
    end


    -- Add symbol table for inspection
    results.global_scope = type_inference.global_scope

    return results
end

--- Analyzes a file and returns results
---@param filename string Path to the Lua file to analyze
---@return table Analysis results
function verdict.analyze_file(filename)
    local file = io.open(filename, "r")
    if not file then
        return {
            errors = { {
                type = "file_error",
                message = "Could not open file: " .. filename,
                phase = "file_reading"
            } }
        }
    end

    local source = file:read("*all")
    file:close()

    return verdict.analyze(source)
end

--- Pretty prints analysis results
---@param results table Results from analyze() or analyze_file()
function verdict.print_results(results)
    print("=== Verdict Analysis Results ===")

    if #results.errors == 0 then
        print("✓ No errors found!")
    else
        print("✗ Found " .. #results.errors .. " error(s):")
        for i, error in ipairs(results.errors) do
            local location = ""
            if error.line then
                location = " (line " .. error.line
                if error.column then
                    location = location .. ", column " .. error.column
                end
                location = location .. ")"
            end
            print("  " .. i .. ". [" .. (error.phase or "unknown") .. "] " .. error.message .. location)
        end
    end

    if #results.warnings > 0 then
        print("⚠ Found " .. #results.warnings .. " warning(s):")
        for i, warning in ipairs(results.warnings) do
            print("  " .. i .. ". " .. warning.message)
        end
    end

    print()
end

return verdict
