describe("Analyzer", function()
    local verdict = require("verdict")

    it("should analyze a simple variable declaration", function()
        local source = "local x = 42"
        local results = verdict.analyze(source)

        assert.is_table(results)
        assert.is_table(results.errors)
        assert.equal(0, #results.errors, "Should have no errors")
    end)

    it("should detect type errors", function()
        local source = "local x = 'hello' + 42"
        local results = verdict.analyze(source)

        assert.is_table(results)
        assert.is_table(results.errors)
        assert.is_true(#results.errors > 0, "Should detect the type error")
    end)
end)
