-- tests/unit/language_features/coroutines_spec.lua
describe("Coroutine Analysis", function()
    local verdict = require("verdict")

    it("should analyze coroutine creation", function()
        local source = [[
       local co = coroutine.create(function()
         local x = 10
         coroutine.yield(x)
         return x * 2
       end)
     ]]

        local results = verdict.analyze(source)
        assert.equal(0, #results.errors)
    end)

    it("should analyze coroutine usage", function()
        local source = [[
       local co = coroutine.create(function()
         return 42
       end)

       local status, result = coroutine.resume(co)
     ]]

        local results = verdict.analyze(source)
        assert.equal(0, #results.errors)
    end)

    -- TODO: Add more tests for coroutine features like yielding, resuming, and error handling
end)
