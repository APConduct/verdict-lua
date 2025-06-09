--- The analyzer module for the verdict Lua static analysis tool.
-- @module analizer
local analyzer = {}

--- Represents a lua type.
-- @type Type
local Type = {}
Type.__index = Type

--- Creates a new Type instance.
---@param kind string The kind of the type (e.g., "primitive", "function", "table", "unknown")
---@param data any Additional data associated with the type (optional)
---@return Type A new Type instance.
function Type.new(kind, data)
    return setmetatable({
        kind = kind, -- "primitive", "function", "table", "unknown", "union"
        data = data or {}
    }, Type)
end

--- Checks if this type is assignable to another type
---@param other Type The target type
---@return boolean True if this type can be assigned to other
function Type:is_assignable_to(other)
    if self.kind == "unknown" or other.kind == "unknown" then
        return true -- Unknown types are compatible with everything
    end

    if self.kind == other.kind then
        if self.kind == "primitive" then
            return self.data.name == other.data.name
        elseif self.kind == "function" then
            -- Simplified function compatibility - could be more sophisticated
            return true
        elseif self.kind == "table" then
            -- Simplified table compatibility
            return true
        end
        return true
    end

    -- Special cases for Lua's dynamic nature
    if other.kind == "primitive" and other.data.name == "string" then
        -- Numbers can be converted to strings
        if self.kind == "primitive" and self.data.name == "number" then
            return true
        end
    end

    return false
end

--- Returns a string representation of the type.
---@return string A string representation of the type.
function Type:__tostring()
    if self.kind == "primitive" then
        return self.data.name or tostring(self.data)
    elseif self.kind == "function" then
        local params = {}
        for _, param in ipairs(self.data.params or {}) do
            table.insert(params, tostring(param))
        end
        local returns_str = self.data.returns and tostring(self.data.returns) or "unknown"
        return "function(" .. table.concat(params, ", ") .. ") -> " .. returns_str
    elseif self.kind == "table" then
        if self.data.fields and next(self.data.fields) then
            local field_strs = {}
            for key, value_type in pairs(self.data.fields) do
                table.insert(field_strs, key .. ": " .. tostring(value_type))
            end
            return "table{" .. table.concat(field_strs, ", ") .. "}"
        else
            return "table"
        end
    elseif self.kind == "union" then
        local types = {}
        for _, t in ipairs(self.data.types) do
            table.insert(types, tostring(t))
        end
        return table.concat(types, " | ")
    else
        return "unknown"
    end
end

-- Type factory functions
Type.number = function() return Type.new("primitive", { name = "number" }) end
Type.string = function() return Type.new("primitive", { name = "string" }) end
Type.boolean = function() return Type.new("primitive", { name = "boolean" }) end
Type.nil_type = function() return Type.new("primitive", { name = "nil" }) end
Type.unknown = function() return Type.new("unknown") end

--- Creates a union type
---@param types table Array of types
---@return Type Union type
function Type.union(types)
    if #types == 1 then
        return types[1]
    end
    return Type.new("union", { types = types })
end

--- Creates a function type
---@param params table Array of parameter types
---@param returns Type Return type
---@return Type Function type
function Type.function_type(params, returns)
    return Type.new("function", {
        params = params or {},
        returns = returns or Type.unknown()
    })
end

--- Creates a table type
---@param fields table Map of field names to types
---@return Type Table type
function Type.table_type(fields)
    return Type.new("table", { fields = fields or {} })
end

--- Represents a symbol with additional metadata
-- @type Symbol
local Symbol = {}
Symbol.__index = Symbol

function Symbol.new(name, type, line, column)
    return setmetatable({
        name = name,
        type = type,
        line = line,
        column = column,
        used = false,
        defined = true
    }, Symbol)
end

--- Represents a symbol table for type inference.
-- @type SymbolTable
local SymbolTable = {}
SymbolTable.__index = SymbolTable

--- Creates a new SymbolTable instance.
---@param parent SymbolTable | nil The parent symbol table (optional)
function SymbolTable.new(parent)
    return setmetatable({
        parent = parent,
        symbols = {},
        scope_name = parent and "child" or "global"
    }, SymbolTable)
end

--- Defines a symbol in the symbol table.
---@param name string The name of the symbol
---@param type Type The type of the symbol
---@param line number Line number where defined
---@param column number Column number where defined
---@return nil
function SymbolTable:define(name, type, line, column)
    print("DEBUG: Defining variable '" .. name .. "' in scope")
    self.symbols[name] = Symbol.new(name, type, line, column)
end

--- Looks up a symbol in the symbol table.
---@param name string The name of the symbol
---@return Symbol | nil The symbol if found, otherwise nil
function SymbolTable:lookup(name)
    if self.symbols[name] then
        print("DEBUG: Using variable '" .. name .. "'") -- Add this debug line
        self.symbols[name].used = true
        return self.symbols[name]
    elseif self.parent then
        return self.parent:lookup(name)
    else
        return nil
    end
end

--- Gets unused variables in this scope
---@return table Array of unused symbols
function SymbolTable:get_unused_variables()
    local unused = {}
    print("DEBUG: Checking unused variables in scope:")
    for name, symbol in pairs(self.symbols) do
        print("  " .. name .. " -> used: " .. tostring(symbol.used))
        if not symbol.used and symbol.name ~= "_" then -- Ignore underscore variables
            table.insert(unused, symbol)
        end
    end
    return unused
end

--- Enters a new scope in the symbol table.
---@param scope_name string Optional name for the scope
---@return SymbolTable A new SymbolTable instance representing the new scope
function SymbolTable:enter_scope(scope_name)
    local new_scope = SymbolTable.new(self)
    new_scope.scope_name = scope_name or "block"
    return new_scope
end

--- The type inference module for the verdict Lua static analysis tool.
-- @type TypeInference
local TypeInference = {}
TypeInference.__index = TypeInference

--- Creates a new TypeInference instance.
---@return table A new TypeInference instance
function TypeInference.new()
    return setmetatable({
        global_scope = SymbolTable.new(),
        current_scope = nil,
        errors = {},
        warnings = {},
        function_returns = {} -- Track return types in functions
    }, TypeInference)
end

--- Attaches an error message to the TypeInference instance.
---@param msg string The error message
---@param node table The AST node associated with the error (optional)
---@return nil
function TypeInference:error(msg, node)
    table.insert(self.errors, {
        message = msg,
        line = node and node.line,
        column = node and node.column,
        type = "error"
    })
end

--- Attaches a warning message to the TypeInference instance.
---@param msg string The warning message
---@param node table The AST node associated with the warning (optional)
---@return nil
function TypeInference:warning(msg, node)
    table.insert(self.warnings, {
        message = msg,
        line = node and node.line,
        column = node and node.column,
        type = "warning"
    })
end

--- Infers the type of a literal node.
---@param literal_data table The literal data from the AST node
---@return Type The inferred type of the literal
function TypeInference:infer_literal(literal_data)
    if literal_data.type == "number" then
        return Type.number()
    elseif literal_data.type == "string" then
        return Type.string()
    elseif literal_data.type == "boolean" then
        return Type.boolean()
    elseif literal_data.type == "nil" then
        return Type.nil_type()
    else
        return Type.unknown()
    end
end

--- Infers the type of a unary operation.
---@param expr_type Type The type of the operand expression
---@param op string The unary operator
---@param node table The AST node for error reporting
---@return Type The result type of the unary operation
function TypeInference:infer_unary_op(expr_type, op, node)
    if op == "-" then
        if expr_type.kind == "primitive" and expr_type.data.name == "number" then
            return Type.number()
        elseif expr_type.kind == "unknown" then
            -- Assume numeric negation for unknown types during inference
            return Type.number()
        else
            self:error("Attempt to perform arithmetic on a " .. tostring(expr_type) .. " value", node)
            return Type.unknown()
        end
    elseif op == "not" then
        return Type.boolean()
    else
        return Type.unknown()
    end
end

--- Infers the type of a binary operation.
---@param left_type Type The type of the left operand
---@param right_type Type The type of the right operand
---@param op string The binary operator
---@param node table The AST node for error reporting
---@return Type The result type of the binary operation
function TypeInference:infer_binary_op(left_type, right_type, op, node)
    -- Handle unknown types more gracefully during inference
    local left_unknown = left_type.kind == "unknown"
    local right_unknown = right_type.kind == "unknown"

    -- Arithmetic operations
    if op == "+" or op == "-" or op == "*" or op == "/" or op == "%" or op == "^" then
        -- If both types are known and valid
        if left_type.kind == "primitive" and left_type.data.name == "number" and
            right_type.kind == "primitive" and right_type.data.name == "number" then
            return Type.number()
            -- If either type is unknown, assume arithmetic operation and return number
        elseif left_unknown or right_unknown then
            return Type.number()
        else
            self:error("Attempt to perform arithmetic on " .. tostring(left_type) ..
                " and " .. tostring(right_type), node)
            return Type.unknown()
        end
        -- String concatenation
    elseif op == ".." then
        -- In Lua, numbers can be concatenated as strings
        if (left_type.kind == "primitive" and (left_type.data.name == "string" or left_type.data.name == "number")) and
            (right_type.kind == "primitive" and (right_type.data.name == "string" or right_type.data.name == "number")) then
            return Type.string()
            -- If either type is unknown, assume string concatenation
        elseif left_unknown or right_unknown then
            return Type.string()
        else
            self:error("Attempt to concatenate " .. tostring(left_type) ..
                " and " .. tostring(right_type), node)
            return Type.string() -- Return string anyway as Lua is forgiving
        end
        -- Comparison operations
    elseif op == "==" or op == "~=" then
        return Type.boolean()
    elseif op == "<" or op == ">" or op == "<=" or op == ">=" then
        -- These work on numbers and strings
        if (left_type.kind == "primitive" and (left_type.data.name == "number" or left_type.data.name == "string")) and
            (right_type.kind == "primitive" and (right_type.data.name == "number" or right_type.data.name == "string")) and
            left_type.data.name == right_type.data.name then
            return Type.boolean()
        elseif left_unknown or right_unknown then
            return Type.boolean()
        else
            self:warning("Comparison between " .. tostring(left_type) ..
                " and " .. tostring(right_type) .. " may not work as expected", node)
            return Type.boolean()
        end
        -- Logical operations
    elseif op == "and" then
        -- In Lua, 'and' returns the first falsy value or the last value
        -- Simplified: if left is falsy, return left, otherwise return right
        return Type.union({ left_type, right_type })
    elseif op == "or" then
        -- In Lua, 'or' returns the first truthy value or the last value
        return Type.union({ left_type, right_type })
    else
        return Type.unknown()
    end
end

--- Infers the type of an expression.
---@param node table The AST node to infer the type for
---@param scope SymbolTable The current scope
---@return Type The inferred type of the expression
function TypeInference:infer_expression(node, scope)
    if not node then
        return Type.unknown()
    end

    if node.type == "literal" then
        return self:infer_literal(node.data)
    elseif node.type == "identifier" then
        local symbol = scope:lookup(node.data.name)
        if not symbol then
            self:error("Undefined variable: " .. node.data.name, node)
            return Type.unknown()
        end
        print("DEBUG: Looking up '" .. node.data.name .. "' -> type: " .. tostring(symbol.type))
        return symbol.type
    elseif node.type == "binary_op" then
        local left_type = self:infer_expression(node.data.left, scope)
        local right_type = self:infer_expression(node.data.right, scope)
        return self:infer_binary_op(left_type, right_type, node.data.op, node)
    elseif node.type == "unary_op" then
        local expr_type = self:infer_expression(node.data.expr, scope)
        return self:infer_unary_op(expr_type, node.data.op, node)
    elseif node.type == "function_call" then
        local func_type = self:infer_expression(node.data.func, scope)

        -- Type check arguments and try to infer parameter types
        if func_type.kind == "function" then
            local expected_params = func_type.data.params or {}
            local actual_args = node.data.args or {}

            -- Infer parameter types from actual arguments
            for i, arg in ipairs(actual_args) do
                local arg_type = self:infer_expression(arg, scope)
                if expected_params[i] and expected_params[i].kind == "unknown" and arg_type.kind ~= "unknown" then
                    expected_params[i] = arg_type
                end
            end

            -- Check argument count
            if #actual_args ~= #expected_params then
                self:warning("Function expects " .. #expected_params ..
                    " arguments but got " .. #actual_args, node)
            end

            return func_type.data.returns or Type.unknown()
        else
            self:error("Attempt to call " .. tostring(func_type) .. " (not a function)", node)
            return Type.unknown()
        end
    elseif node.type == "table_constructor" then
        local fields = {}
        local array_type = nil

        for i, field in ipairs(node.data.fields or {}) do
            local field_type = self:infer_expression(field, scope)

            -- For now, treat as array-like table
            if not array_type then
                array_type = field_type
            elseif not field_type:is_assignable_to(array_type) then
                array_type = Type.union({ array_type, field_type })
            end

            fields[tostring(i)] = field_type
        end

        return Type.table_type(fields)
    else
        return Type.unknown()
    end
end

--- Attempts to infer parameter type from usage patterns
---@param param_name string The parameter name
---@param body table Function body statements
---@param scope SymbolTable Function scope
---@return Type Inferred type
function TypeInference:infer_parameter_type_from_usage(param_name, body, scope)
    print("DEBUG: Inferring type for parameter '" .. param_name .. "'")

    -- Look through all statements in the function body for parameter usage
    for _, stmt in ipairs(body or {}) do
        local inferred_type = self:analyze_statement_for_param_inference(stmt, param_name)
        if inferred_type.kind ~= "unknown" then
            print("DEBUG: Inferred type for '" .. param_name .. "': " .. tostring(inferred_type))
            return inferred_type
        end
    end

    print("DEBUG: Could not infer type for '" .. param_name .. "', defaulting to unknown")
    return Type.unknown()
end

--- Analyzes a statement to infer parameter types
---@param stmt table Statement AST node
---@param param_name string Parameter name to look for
---@return Type Inferred type
function TypeInference:analyze_statement_for_param_inference(stmt, param_name)
    if not stmt then return Type.unknown() end

    print("DEBUG: Checking statement type '" .. (stmt.type or "nil") .. "' for parameter '" .. param_name .. "'")

    if stmt.type == "return_statement" and stmt.data.expr then
        return self:analyze_expression_for_param_inference(stmt.data.expr, param_name)
    elseif stmt.type == "local_assignment" and stmt.data.expr then
        return self:analyze_expression_for_param_inference(stmt.data.expr, param_name)
    elseif stmt.type == "assignment" and stmt.data.expr then
        return self:analyze_expression_for_param_inference(stmt.data.expr, param_name)
    elseif stmt.type == "expression_statement" and stmt.data.expr then
        return self:analyze_expression_for_param_inference(stmt.data.expr, param_name)
    end

    return Type.unknown()
end

--- Helper to analyze expressions for parameter type inference (enhanced)
---@param expr table Expression AST node
---@param param_name string Parameter name to look for
---@return Type Inferred type
function TypeInference:analyze_expression_for_param_inference(expr, param_name)
    if not expr then return Type.unknown() end

    if expr.type == "binary_op" then
        -- Check if parameter is used in arithmetic operations
        if (expr.data.left and expr.data.left.type == "identifier" and expr.data.left.data.name == param_name) or
            (expr.data.right and expr.data.right.type == "identifier" and expr.data.right.data.name == param_name) then
            if expr.data.op == "+" or expr.data.op == "-" or expr.data.op == "*" or expr.data.op == "/" or expr.data.op == "%" then
                return Type.number()
            elseif expr.data.op == ".." then
                return Type.string()
            end
        end

        -- Recursively check nested expressions
        local left_type = self:analyze_expression_for_param_inference(expr.data.left, param_name)
        if left_type.kind ~= "unknown" then return left_type end

        local right_type = self:analyze_expression_for_param_inference(expr.data.right, param_name)
        if right_type.kind ~= "unknown" then return right_type end
    elseif expr.type == "function_call" then
        -- Check function arguments for parameter usage
        for _, arg in ipairs(expr.data.args or {}) do
            local arg_type = self:analyze_expression_for_param_inference(arg, param_name)
            if arg_type.kind ~= "unknown" then
                return arg_type
            end
        end
    end

    return Type.unknown()
end

--- Analyzes a statement.
---@param node table The AST node representing the statement
---@param scope SymbolTable The current scope
---@return Type|nil Return type if this is a return statement
function TypeInference:analyze_statement(node, scope)
    if not node then return end

    if node.type == "local_assignment" then
        local expr_type = self:infer_expression(node.data.expr, scope)
        scope:define(node.data.name, expr_type, node.line, node.column)
    elseif node.type == "local_declaration" then
        -- local x (without assignment)
        scope:define(node.data.name, Type.nil_type(), node.line, node.column)
    elseif node.type == "assignment" then
        local expr_type = self:infer_expression(node.data.expr, scope)
        local symbol = scope:lookup(node.data.name)

        if symbol then
            -- Check type compatibility
            if not expr_type:is_assignable_to(symbol.type) then
                self:error("Cannot assign " .. tostring(expr_type) ..
                    " to variable of type " .. tostring(symbol.type), node)
            end
        else
            -- Global assignment
            self.global_scope:define(node.data.name, expr_type, node.line, node.column)
        end
    elseif node.type == "function_def" then
        -- Create parameter types (assume unknown for now)
        local param_types = {}
        local func_scope = scope:enter_scope("function")

        for _, param_name in ipairs(node.data.params or {}) do
            local param_type = Type.unknown()
            table.insert(param_types, param_type)
            func_scope:define(param_name, param_type, node.line, node.column)
        end

        -- Analyze function body
        local return_types = {}
        for _, stmt in ipairs(node.data.body or {}) do
            local return_type = self:analyze_statement(stmt, func_scope)
            if return_type then
                table.insert(return_types, return_type)
            end
        end

        -- Determine return type
        local return_type = Type.nil_type() -- Default return type
        if #return_types > 0 then
            if #return_types == 1 then
                return_type = return_types[1]
            else
                return_type = Type.union(return_types)
            end
        end

        local func_type = Type.function_type(param_types, return_type)
        scope:define(node.data.name, func_type, node.line, node.column)

        -- Check for unused parameters
        for _, symbol in pairs(func_scope.symbols) do
            if not symbol.used and symbol.name ~= "_" then
                self:warning("Unused parameter: " .. symbol.name, node)
            end
        end
    elseif node.type == "local_function_def" then
        -- local function name(params) body end
        local param_types = {}
        local func_scope = scope:enter_scope("function")

        -- First pass: define parameters with unknown types
        for _, param_name in ipairs(node.data.params or {}) do
            local param_type = Type.unknown()
            table.insert(param_types, param_type)
            func_scope:define(param_name, param_type, node.line, node.column)
        end

        -- Second pass: infer parameter types from usage patterns
        for i, param_name in ipairs(node.data.params or {}) do
            local param_symbol = func_scope.symbols[param_name]
            if param_symbol then
                local inferred_type = self:infer_parameter_type_from_usage(param_name, node.data.body, func_scope)
                if inferred_type.kind ~= "unknown" then
                    param_types[i] = inferred_type
                    param_symbol.type = inferred_type
                end
            end
        end

        -- Third pass: analyze function body with inferred parameter types
        local return_types = {}
        for _, stmt in ipairs(node.data.body or {}) do
            local return_type = self:analyze_statement(stmt, func_scope)
            if return_type then
                table.insert(return_types, return_type)
            end
        end

        -- Fourth pass: create function type and define it in parent scope
        local return_type = Type.nil_type()
        if #return_types > 0 then
            return_type = #return_types == 1 and return_types[1] or Type.union(return_types)
        end

        local func_type = Type.function_type(param_types, return_type)
        scope:define(node.data.name, func_type, node.line, node.column)

        -- Check for unused parameters
        for _, symbol in pairs(func_scope.symbols) do
            if not symbol.used and symbol.name ~= "_" then
                self:warning("Unused parameter: " .. symbol.name, node)
            end
        end
    elseif node.type == "return_statement" then
        if node.data.expr then
            return self:infer_expression(node.data.expr, scope)
        else
            return Type.nil_type()
        end
    elseif node.type == "if_statement" then
        local cond_type = self:infer_expression(node.data.condition, scope)

        -- Analyze then block
        for _, stmt in ipairs(node.data.then_block or {}) do
            self:analyze_statement(stmt, scope)
        end

        -- Analyze else block
        if node.data.else_block then
            for _, stmt in ipairs(node.data.else_block) do
                self:analyze_statement(stmt, scope)
            end
        end
    elseif node.type == "while_statement" then
        local cond_type = self:infer_expression(node.data.condition, scope)

        local loop_scope = scope:enter_scope("while")
        for _, stmt in ipairs(node.data.body or {}) do
            self:analyze_statement(stmt, loop_scope)
        end
    elseif node.type == "for_statement" then
        -- Simplified for loop analysis
        local for_scope = scope:enter_scope("for")

        if node.data.var then
            for_scope:define(node.data.var, Type.unknown(), node.line, node.column)
        end

        for _, stmt in ipairs(node.data.body or {}) do
            self:analyze_statement(stmt, for_scope)
        end
    elseif node.type == "expression_statement" then
        self:infer_expression(node.data.expr, scope)
    elseif node.type == "block" then
        local block_scope = scope:enter_scope("block")
        for _, stmt in ipairs(node.data.statements or {}) do
            self:analyze_statement(stmt, block_scope)
        end

        -- Check for unused variables in this block
        for _, unused in ipairs(block_scope:get_unused_variables()) do
            self:warning("Unused variable: " .. unused.name,
                { line = unused.line, column = unused.column })
        end
    end
end

--- Performs static analysis on the AST.
---@param ast table The AST to analyze
---@return table Array of errors found during analysis
function TypeInference:analyze(ast)
    self.current_scope = self.global_scope

    -- Add built-in functions
    self.global_scope:define("print", Type.function_type({ Type.unknown() }, Type.nil_type()))
    self.global_scope:define("type", Type.function_type({ Type.unknown() }, Type.string()))
    self.global_scope:define("tostring", Type.function_type({ Type.unknown() }, Type.string()))
    self.global_scope:define("tonumber",
        Type.function_type({ Type.string() }, Type.union({ Type.number(), Type.nil_type() })))
    self.global_scope:define("pairs", Type.function_type({ Type.table_type() }, Type.function_type()))
    self.global_scope:define("ipairs", Type.function_type({ Type.table_type() }, Type.function_type()))
    self.global_scope:define("next", Type.function_type({ Type.table_type(), Type.unknown() }, Type.unknown()))

    -- Standard library globals
    self.global_scope:define("string", Type.table_type())
    self.global_scope:define("table", Type.table_type())
    self.global_scope:define("math", Type.table_type())
    self.global_scope:define("io", Type.table_type())
    self.global_scope:define("os", Type.table_type())

    -- Analyze the AST
    for _, stmt in ipairs(ast or {}) do
        self:analyze_statement(stmt, self.current_scope)
    end

    -- Check for unused variables (exclude built-ins)
    local built_ins = {
        "print", "type", "tostring", "tonumber", "pairs", "ipairs", "next",
        "string", "table", "math", "io", "os", "_G"
    }

    for _, unused in ipairs(self.global_scope:get_unused_variables()) do
        local is_builtin = false
        for _, builtin in ipairs(built_ins) do
            if unused.name == builtin then
                is_builtin = true
                break
            end
        end

        if not is_builtin then
            self:warning("Unused variable: " .. unused.name,
                { line = unused.line, column = unused.column })
        end
    end



    -- Return errors and warnings separately
    return {
        errors = self.errors,
        warnings = self.warnings
    }
end

-- Export the main components
analyzer.Type = Type
analyzer.Symbol = Symbol
analyzer.SymbolTable = SymbolTable
analyzer.TypeInference = TypeInference

-- Keep the existing test and example functions for compatibility
function analyzer.create_example_ast()
    local function create_node(type, data)
        return { type = type, data = data or {} }
    end

    return {
        create_node("local_assignment", {
            name = "x",
            expr = create_node("literal", { type = "number", value = 42 })
        }),
        create_node("function_call", {
            func = create_node("identifier", { name = "print" }),
            args = {
                create_node("binary_op", {
                    left = create_node("identifier", { name = "x" }),
                    right = create_node("literal", { type = "number", value = 1 }),
                    op = "+"
                })
            }
        })
    }
end

function analyzer.test()
    print("Testing enhanced Lua static analyzer")

    local inference = TypeInference.new()
    local ast = analyzer.create_example_ast()
    local issues = inference:analyze(ast)

    print("Analysis complete. Issues found:", #issues)
    for _, issue in ipairs(issues) do
        local prefix = issue.type == "error" and "Error" or "Warning"
        print(prefix .. ": " .. issue.message)
    end

    -- Test symbol lookup
    local x_symbol = inference.global_scope:lookup("x")
    if x_symbol then
        print("Variable 'x' has type: " .. tostring(x_symbol.type))
    end
end

return analyzer
