describe("Type Inference", function()
    local analyzer = require("verdict.analyzer")
    local TypeInference = analyzer.TypeInference
    local Type = analyzer.Type
    local Parser = require("verdict.stuff.parser")
    local Lexer = require("verdict.stuff.lexer")
    local helper = require("tests.helpers.test_helper")

    local function analyze(source)
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()
        local parser = Parser.new(tokens)
        local parse_result = parser:parse()

        local type_inference = TypeInference.new()
        return type_inference:analyze(parse_result.ast)
    end

    it("should infer number type for literals", function()
        local result = analyze("local x = 42")

        assert.is_table(result)
        assert.equal(0, #result.errors)

        local symbol = result.global_scope.symbols["x"]
        assert.is_not_nil(symbol)
        assert.equal("number", symbol.type.data.name)
    end)

    it("should infer string type for literals", function()
        local result = analyze("local x = 'hello'")

        assert.is_table(result)
        assert.equal(0, #result.errors)

        local symbol = result.global_scope.symbols["x"]
        assert.is_not_nil(symbol)
        assert.equal("string", symbol.type.data.name)
    end)

    it("should infer function types", function()
        local result = analyze("local function add(a, b) return a + b end")

        assert.is_table(result)
        assert.equal(0, #result.errors)

        local symbol = result.global_scope.symbols["add"]
        assert.is_not_nil(symbol)
        assert.equal("function", symbol.type.kind)
        assert.equal(2, #symbol.type.data.params)
    end)

    it("should detect type errors in operations", function()
        local result = analyze("local x = 'hello' + 42")

        assert.is_table(result)
        assert.is_true(#result.errors > 0)
        -- The error should be about attempting to perform arithmetic on a string
        assert.is_true(result.errors[1].message:match("arithmetic") ~= nil)
    end)

    it("should track variable usage", function()
        local result = analyze("local x = 42\nlocal y = x + 10")

        assert.is_table(result)
        assert.equal(0, #result.errors)

        local symbol_x = result.global_scope.symbols["x"]
        assert.is_not_nil(symbol_x)
        assert.is_true(symbol_x.used)

        local symbol_y = result.global_scope.symbols["y"]
        assert.is_not_nil(symbol_y)
        assert.is_false(symbol_y.used)
    end)

    it("should handle table types", function()
        local result = analyze("local t = { a = 1, b = 'hello' }")

        assert.is_table(result)
        assert.equal(0, #result.errors)

        local symbol = result.global_scope.symbols["t"]
        assert.is_not_nil(symbol)
        assert.equal("table", symbol.type.kind)

        -- Check the table fields
        assert.is_not_nil(symbol.type.data.fields.a)
        assert.equal("primitive", symbol.type.data.fields.a.kind)
        assert.equal("number", symbol.type.data.fields.a.data.name)

        assert.is_not_nil(symbol.type.data.fields.b)
        assert.equal("primitive", symbol.type.data.fields.b.kind)
        assert.equal("string", symbol.type.data.fields.b.data.name)
    end)

    it("should detect unused variables", function()
        local result = analyze("local unused = 42")

        assert.is_table(result)
        assert.equal(0, #result.errors)
        assert.is_true(#result.warnings > 0)

        -- The warning should be about the unused variable
        local found_warning = false
        for _, warning in ipairs(result.warnings) do
            if warning.message:match("Unused variable") then
                found_warning = true
                break
            end
        end

        assert.is_true(found_warning)
    end)

    -- TODO: add more tests for complex scenarios
end)
