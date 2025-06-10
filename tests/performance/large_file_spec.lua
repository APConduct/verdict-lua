describe("Performance Tests", function()
    local verdict = require("verdict")
    local helper = require("tests.helpers.test_helper")

    it("should analyze large files efficiently", function()
        local source = helper.load_fixture("performance/large_file.lua")

        local start_time = os.clock()
        local results = verdict.analyze(source)
        local end_time = os.clock()
        local elapsed = end_time - start_time

        print("Analysis time for large file: " .. elapsed .. " seconds")

        -- Set a reasonable time limit based on your performance requirements
        assert.is_true(elapsed < 2.0) -- Should analyze in under 2 seconds
    end)

    it("should handle deep nesting efficiently", function()
        local source = helper.load_fixture("performance/deep_nesting.lua")

        local start_time = os.clock()
        local results = verdict.analyze(source)
        local end_time = os.clock()
        local elapsed = end_time - start_time

        print("Analysis time for deeply nested code: " .. elapsed .. " seconds")

        assert.is_true(elapsed < 1.0) -- Should analyze in under 1 second
    end)

    it("should handle many functions efficiently", function()
        local source = helper.load_fixture("performance/many_functions.lua")

        local start_time = os.clock()
        local results = verdict.analyze(source)
        local end_time = os.clock()
        local elapsed = end_time - start_time

        print("Analysis time for many functions: " .. elapsed .. " seconds")

        assert.is_true(elapsed < 1.5) -- Should analyze in under 1.5 seconds
    end)

    -- TODO: Add more performance tests for specific scenarios
end)
