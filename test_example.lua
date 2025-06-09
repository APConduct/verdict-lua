-- Test file for Verdict analyzer
local x = 42
local name = "Alice"

local function add(a, b)
    return a + b
end

local function greet(person)
    local message = "Hello, " .. person .. "!"
    print(message)
    return message
end

local result = add(10, 5)
local greeting = greet(name)
