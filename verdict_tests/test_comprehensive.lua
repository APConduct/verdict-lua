-- Comprehensive test file for enhanced Verdict analyzer

-- Basic variable declarations
local x = 42
local name = "Alice"
local flag = true
local empty = nil

-- Table operations
local person = {
    name = "Bob",
    age = 30,
    address = {
        street = "123 Main St",
        city = "Anytown"
    }
}

-- Field access
local person_name = person.name
local street = person.address.street

-- Index access
local first_char = name[1]
local dynamic_field = person["name"]

-- Array-style table
local numbers = { 1, 2, 3, 4, 5 }
local first_number = numbers[1]

-- Function definitions with various parameter patterns
function add(a, b)
    return a + b
end

local function multiply(x, y)
    local result = x * y
    return result
end

-- Function with varargs
function print_all(...)
    local args = { ... }
    for i = 1, #args do
        print(args[i])
    end
end

-- Method-style function definition
function person:greet()
    return "Hello, I'm " .. self.name
end

-- Function expressions
local square = function(n)
    return n * n
end

-- Complex expressions
local complex_result = (x + 10) * 2 - multiply(3, 4)
local concatenated = "Result: " .. tostring(complex_result)

-- Control flow structures
if x > 20 then
    print("x is large")
elseif x > 10 then
    print("x is medium")
else
    print("x is small")
end

-- Loops
for i = 1, 10 do
    local temp = i * 2
    print(temp)
end

for key, value in pairs(person) do
    print(key, value)
end

local i = 1
while i <= 5 do
    print("Count: " .. i)
    i = i + 1
end

repeat
    local input = "test"
    print(input)
until true

-- Do blocks
do
    local scoped_var = "only visible here"
    print(scoped_var)
end

-- Advanced table operations
local matrix = {
    { 1, 2, 3 },
    { 4, 5, 6 },
    { 7, 8, 9 }
}

local cell = matrix[2][3]

-- String operations
local text = "Hello World"
local length = #text
local upper_text = string.upper(text)
local sub_text = string.sub(text, 1, 5)

-- Mathematical operations
local math_result = math.sqrt(16) + math.abs(-10)
local power_result = 2 ^ 3

-- Error cases (should be caught by analyzer)
-- local undefined_result = undefined_var + 5  -- Undefined variable
-- local type_error = "string" + 42             -- Type mismatch
-- local bad_call = "not a function"()          -- Call on non-function

-- Unused variables (should generate warnings)
local unused_var = "this is never used"
local _ignored_var = "underscore variables are typically ignored"

-- Functions with type inference opportunities
function process_data(data)
    -- Parameter 'data' used as table - should infer table type
    local count = #data
    for i = 1, count do
        local item = data[i]
        print(item)
    end
    return count
end

function string_processor(text)
    -- Parameter 'text' used with string operations - should infer string type
    local upper = string.upper(text)
    local result = "Processed: " .. upper
    return result
end

function number_calculator(num)
    -- Parameter 'num' used in arithmetic - should infer number type
    local doubled = num * 2
    local squared = num ^ 2
    return doubled + squared
end

-- Method calls
local greeting = person:greet()
print(greeting)

-- Table method calls
table.insert(numbers, 6)
table.sort(numbers)

-- Complex nested access
local config = {
    database = {
        host = "localhost",
        port = 5432,
        credentials = {
            username = "admin",
            password = "secret"
        }
    }
}

local db_host = config.database.host
local username = config["database"]["credentials"]["username"]

-- Function calls with various argument patterns
print_all(1, 2, 3, "hello", true)
local sum = add(10, 20)
local product = multiply(5, 6)

-- Return statements with multiple values
function get_coordinates()
    return 10, 20
end

function get_info()
    return "John", 25, true
end

local x_coord, y_coord = get_coordinates()
local person_name, person_age, is_active = get_info()

-- Goto and labels (if supported)
::start::
local counter = 0
counter = counter + 1
if counter < 3 then
    goto start
end

-- Advanced string operations
local pattern_match = string.match(text, "(%w+)")
local formatted = string.format("Number: %d, String: %s", 42, "test")

-- File operations (if io library is available)
-- local file = io.open("test.txt", "r")
-- if file then
--     local content = file:read("*all")
--     file:close()
-- end

-- Coroutine usage (basic)
-- local co = coroutine.create(function()
--     print("Coroutine running")
--     coroutine.yield()
--     print("Coroutine resumed")
-- end)
--
-- coroutine.resume(co)
-- coroutine.resume(co)
