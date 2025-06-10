describe("Full Analysis Pipeline", function()
    local verdict = require("verdict")
    local helper = require("tests.helpers.test_helper")

    it("should analyze a simple valid file", function()
        local source = helper.load_fixture("valid/simple.lua")
        local results = verdict.analyze(source)

        assert.is_table(results)
        assert.equal(0, #results.errors)
        assert.is_table(results.ast)
        assert.is_table(results.tokens)
    end)

    it("should detect lexical errors", function()
        local source = "local x = 'unclosed string"
        local results = verdict.analyze(source)

        assert.is_table(results)
        assert.is_true(#results.errors > 0)
        assert.equal("lexer_error", results.errors[1].type)
    end)

    it("should detect syntax errors", function()
        local source = "function malformed(\n  return 'oops'\nend"
        local results = verdict.analyze(source)

        assert.is_table(results)
        assert.is_true(#results.errors > 0)
        assert.equal("parser_error", results.errors[1].type)
    end)

    it("should detect type errors", function()
        local source = "local x = 'hello' + 42"
        local results = verdict.analyze(source)

        assert.is_table(results)
        assert.is_true(#results.errors > 0)
        assert.equal("type_error", results.errors[1].type)
    end)

    it("should handle complex valid code", function()
        local source = helper.load_fixture("valid/complex.lua")
        local results = verdict.analyze(source)

        assert.is_table(results)
        assert.equal(0, #results.errors)
        -- May have warnings, but shouldn't have errors
    end)

    -- TODO: Add more tests for specific features like type inference, control flow, etc.
end)
