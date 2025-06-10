-- tests/helpers/test_helper.lua
local helper = {}

-- Load a test fixture
function helper.load_fixture(path)
    local file = io.open("tests/fixtures/" .. path, "r")
    if not file then
        error("Could not load fixture: " .. path)
    end
    local content = file:read("*all")
    file:close()
    return content
end

-- Compare ASTs for structural equality
function helper.ast_equals(ast1, ast2)
    if type(ast1) ~= type(ast2) then return false end
    if type(ast1) ~= "table" then return ast1 == ast2 end

    -- Check if all keys in ast1 exist in ast2 with the same values
    for k, v in pairs(ast1) do
        if not helper.ast_equals(v, ast2[k]) then return false end
    end

    -- Check if all keys in ast2 exist in ast1
    for k, _ in pairs(ast2) do
        if ast1[k] == nil then return false end
    end

    return true
end

-- Generate test snippets for specific Lua constructs
function helper.generate_snippet(construct_type, params)
    -- Generate Lua code snippets for testing
    local snippets = {
        variable_declaration = function(name, value)
            return string.format("local %s = %s", name, value)
        end,
        function_declaration = function(name, params, body)
            return string.format("function %s(%s)\n  %s\nend",
                name, table.concat(params, ", "), body)
        end,
        -- Add more snippet generators as needed
    }

    if snippets[construct_type] then
        return snippets[construct_type](table.unpack(params))
    end
    error("Unknown construct type: " .. construct_type)
end

return helper
