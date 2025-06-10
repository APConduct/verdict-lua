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
        tokens = nil,
        parse_errors = {}
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
    local parse_result
    ok, parse_result = pcall(function()
        return parser:parse()
    end)

    if not ok then
        table.insert(results.errors, {
            type = "parser_error",
            message = parse_result, -- The error message
            phase = "parsing"
        })
        return results
    end

    -- Handle the new parser structure
    if parse_result.ast then
        results.ast = parse_result.ast
        results.parse_errors = parse_result.errors or {}
    else
        -- Fallback for old parser format
        results.ast = parse_result
    end

    -- Add parse errors to results
    for _, parse_error in ipairs(results.parse_errors) do
        table.insert(results.errors, {
            type = "parser_error",
            message = parse_error.message,
            line = parse_error.line,
            column = parse_error.column,
            phase = "parsing"
        })
    end

    -- Step 3: Type analysis
    if results.ast then
        local type_inference = analyzer.TypeInference.new()
        local analysis_result = type_inference:analyze(results.ast)

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
    end

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
            } },
            warnings = {}
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
            local location = ""
            if warning.line then
                location = " (line " .. warning.line
                if warning.column then
                    location = location .. ", column " .. warning.column
                end
                location = location .. ")"
            end
            print("  " .. i .. ". [" .. (warning.phase or "type_analysis") .. "] " .. warning.message .. location)
        end
    end

    print()
end

--- Enhanced analysis with additional options
---@param source string Source code to analyze
---@param options table Analysis options
---@return table Analysis results
function verdict.analyze_with_options(source, options)
    options = options or {}

    local results = verdict.analyze(source)

    -- Additional analysis based on options
    if options.check_complexity then
        -- TODO: Add complexity analysis here
    end

    if options.check_performance then
        -- TODO: Add performance analysis here
    end

    if options.strict_mode then
        -- Convert some warnings to errors
        local strict_errors = {}
        local remaining_warnings = {}

        for _, warning in ipairs(results.warnings) do
            if warning.type == "type_warning" then
                warning.type = "type_error"
                table.insert(strict_errors, warning)
            else
                table.insert(remaining_warnings, warning)
            end
        end

        for _, error in ipairs(strict_errors) do
            table.insert(results.errors, error)
        end

        results.warnings = remaining_warnings
    end

    return results
end

return verdict
