-- A more complex example with various Lua constructs
local math_ops = {}

function math_ops.add(a, b)
    return a + b
end

function math_ops.subtract(a, b)
    return a - b
end

function math_ops.multiply(a, b)
    return a * b
end

function math_ops.divide(a, b)
    if b == 0 then
        error("Division by zero")
    end
    return a / b
end

local function apply_operation(op_name, a, b)
    if math_ops[op_name] then
        return math_ops[op_name](a, b)
    else
        error("Unknown operation: " .. op_name)
    end
end

local numbers = { 10, 20, 30, 40, 50 }
local results = {}

for i, num in ipairs(numbers) do
    if i > 1 then
        results[i - 1] = apply_operation("add", num, numbers[i - 1])
    end
end

return results
