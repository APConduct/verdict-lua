local function generate_large_file(file_path, size)
    local file = io.open(file_path, "w")
    if not file then
        error("Could not open file for writing: " .. file_path)
    end

    file:write("-- Large test file\n")
    file:write("local results = {}\n\n")

    for i = 1, size do
        file:write(string.format("local var_%d = %d\n", i, i))
        file:write(string.format("results[%d] = var_%d * 2\n", i, i))
    end

    file:write("\nreturn results\n")
    file:close()
end

local function generate_deep_nesting(file_path, depth)
    local file = io.open(file_path, "w")

    if not file then
        error("Could not open file for writing: " .. file_path)
    end

    file:write("-- Deeply nested code\n")
    file:write("local function deep(n)\n")

    -- Create nested if statements
    for i = 1, depth do
        local indent = string.rep("  ", i)
        file:write(indent .. string.format("if n > %d then\n", i))
    end

    -- Inner-most code
    local deepest_indent = string.rep("  ", depth + 1)
    file:write(deepest_indent .. "return 'deep'\n")

    -- Close all the if statements
    for i = depth, 1, -1 do
        local indent = string.rep("  ", i)
        file:write(indent .. "else\n")
        file:write(indent .. "  return 'not deep enough'\n")
        file:write(indent .. "end\n")
    end

    file:write("end\n\n")
    file:write("return deep(100)\n")
    file:close()
end

local function generate_many_functions(file_path, count)
    local file = io.open(file_path, "w")

    if not file then
        error("Could not open file for writing: " .. file_path)
    end

    file:write("-- Many functions\n")
    file:write("local module = {}\n\n")

    for i = 1, count do
        file:write(string.format("function module.func_%d(a, b)\n", i))
        file:write("  local result = a + b\n")
        file:write(string.format("  if result > %d then\n", i))
        file:write("    return result * 2\n")
        file:write("  else\n")
        file:write("    return result / 2\n")
        file:write("  end\n")
        file:write("end\n\n")
    end

    file:write("return module\n")
    file:close()
end

-- Create the fixtures directory if it doesn't exist
os.execute("mkdir -p tests/fixtures/performance")

-- Generate performance test fixtures
generate_large_file("tests/fixtures/performance/large_file.lua", 1000)
generate_deep_nesting("tests/fixtures/performance/deep_nesting.lua", 50)
generate_many_functions("tests/fixtures/performance/many_functions.lua", 200)

print("Generated performance test fixtures")
