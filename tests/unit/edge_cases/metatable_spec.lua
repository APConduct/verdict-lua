-- tests/unit/edge_cases/metatable_spec.lua
describe("Metatable Analysis", function()
    local verdict = require("verdict")

    it("should analyze metatable usage", function()
        local source = [[
      local t = {}
      local mt = {
        __index = function(t, k)
          return k:upper()
        end,
        __newindex = function(t, k, v)
          rawset(t, k:lower(), v)
        end
      }
      setmetatable(t, mt)

      local value = t.name -- Uses __index
      t.NAME = "Alice"     -- Uses __newindex
    ]]

        local results = verdict.analyze(source)
        -- This might produce warnings but should not produce errors
        assert.equal(0, #results.errors)
    end)

    -- TODO: Add more tests for specific metatable features
end)
