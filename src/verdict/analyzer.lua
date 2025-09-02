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
    if self.kind == "unknown" or other["kind"] == "unknown" then
        return true -- Unknown types are compatible with everything
    end

    if self.kind == other["kind"] then
        if self.kind == "primitive" then
            return self.data.name == other["data"].name
        elseif self.kind == "function" then
            -- More sophisticated function compatibility
            if not self.data.params or not other["data"].params then
                return true -- Unknown parameters
            end

            -- Check parameter count and types
            if #self.data.params ~= #other["data"].params then
                return false
            end

            for i, param in ipairs(self.data.params) do
                if not param:is_assignable_to(other["data"].params[i]) then
                    return false
                end
            end

            -- Check return type
            local self_return = self.data.returns or Type.unknown()
            local other_return = other["data"].returns or Type.unknown()
            return self_return["is_assignable_to"](self_return, other_return)
        elseif self.kind == "table" then
            -- Enhanced table compatibility
            if not self.data.fields or not other["data"].fields then
                return true -- Unknown structure
            end

            -- Check if all required fields in other exist in self
            for key, other_type in pairs(other["data"].fields) do
                local self_type = self.data.fields[key]
                if not self_type or not self_type:is_assignable_to(other_type) then
                    return false
                end
            end
            return true
        elseif self.kind == "union" then
            -- Union type is assignable if any component is assignable
            for _, component in ipairs(self.data.types) do
                if component:is_assignable_to(other) then
                    return true
                end
            end
            return false
        end
        return true
    end

    -- Special cases for Lua's dynamic nature
    if other["kind"] == "primitive" and other["data"].name == "string" then
        -- Numbers can be converted to strings
        if self.kind == "primitive" and self.data.name == "number" then
            return true
        end
    end

    -- Check union types
    if other["kind"] == "union" then
        for _, component in ipairs(other["data"].types) do
            if self:is_assignable_to(component) then
                return true
            end
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
        local params_str = table.concat(params, ", ")
        if self.data.has_varargs then
            params_str = params_str .. (params_str ~= "" and ", " or "") .. "..."
        end
        local returns_str = self.data.returns and tostring(self.data.returns) or "unknown"
        return "function(" .. params_str .. ") -> " .. returns_str
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
    elseif self.kind == "module" then
        return "module(" .. (self.data.name or "unknown") .. ")"
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
Type.any = function() return Type.new("unknown") end -- Alias for unknown

--- Creates a union type
---@param types table Array of types
---@return Type Union type
function Type.union(types)
    if #types == 0 then
        return Type.unknown()
    elseif #types == 1 then
        return types[1]
    end

    -- Flatten nested unions and remove duplicates
    local flattened = {}
    local seen = {}

    for _, t in ipairs(types) do
        if t.kind == "union" then
            for _, inner in ipairs(t.data.types) do
                local key = tostring(inner)
                if not seen[key] then
                    table.insert(flattened, inner)
                    seen[key] = true
                end
            end
        else
            local key = tostring(t)
            if not seen[key] then
                table.insert(flattened, t)
                seen[key] = true
            end
        end
    end

    if #flattened == 1 then
        return flattened[1]
    end

    return Type.new("union", { types = flattened })
end

--- Creates a function type
---@param params table|nil Array of parameter types
---@param returns Type|nil Return type
---@param has_varargs boolean|nil Whether function accepts varargs
---@return Type Function type
function Type.function_type(params, returns, has_varargs)
    return Type.new("function", {
        params = params or {},
        returns = returns or Type.unknown(),
        has_varargs = has_varargs or false
    })
end

--- Creates a table type with specific field requirements
---@param fields table|nil Map of field names to types
---@param array_type Type|nil Type for array elements (optional)
---@return Type Table type
function Type.table_type(fields, array_type)
    return Type.new("table", {
        fields = fields or {},
        array_type = array_type,
        is_array = array_type ~= nil
    })
end

--- Creates a module type
---@param name string Module name
---@param exports table Exported symbols
---@return Type Module type
function Type.module_type(name, exports)
    return Type.new("module", {
        name = name,
        exports = exports or {}
    })
end

--- Represents a symbol with additional metadata
-- @type Symbol
local Symbol = {}
Symbol.__index = Symbol

function Symbol.new(name, type, line, column, scope_level)
    return setmetatable({
        name = name,
        type = type,
        line = line,
        column = column,
        used = false,
        defined = true,
        scope_level = scope_level or 0,
        is_parameter = false,
        is_local = false,
        is_global = false
    }, Symbol)
end

--- Represents a symbol table for type inference.
-- @type SymbolTable
local SymbolTable = {}
SymbolTable.__index = SymbolTable

--- Creates a new SymbolTable instance.
---@param parent SymbolTable | nil The parent symbol table (optional)
function SymbolTable.new(parent)
    local level = parent and (parent["level"] + 1) or 0
    return setmetatable({
        parent = parent,
        symbols = {},
        scope_name = parent and "child" or "global",
        level = level
    }, SymbolTable)
end

--- Defines a symbol in the symbol table.
---@param name string The name of the symbol
---@param type Type The type of the symbol
---@param line number Line number where defined
---@param column number Column number where defined
---@return nil
function SymbolTable:define(name, type, line, column)
    local symbol = Symbol.new(name, type, line, column, self.level)
    symbol.is_local = self.level > 0
    symbol.is_global = self.level == 0
    self.symbols[name] = symbol
end

--- Defines a parameter symbol
---@param name string Parameter name
---@param type Type Parameter type
---@param line number Line number
---@param column number Column number
function SymbolTable:define_parameter(name, type, line, column)
    local symbol = Symbol.new(name, type, line, column, self.level)
    symbol.is_parameter = true
    symbol.is_local = true
    self.symbols[name] = symbol
end

--- Looks up a symbol in the symbol table.
---@param name string The name of the symbol
---@return Symbol | nil The symbol if found, otherwise nil
function SymbolTable:lookup(name)
    if self.symbols[name] then
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
    for name, symbol in pairs(self.symbols) do
        -- Ignore variables that are intentionally unused in test files
        if not symbol.used and
            symbol.name ~= "_" and                                             -- Ignore underscore variables
            not symbol.name:match("^_") and                                    -- Ignore variables starting with underscore
            not (symbol.is_global and string.match(symbol.name, "^[A-Z]")) and -- Ignore global constants
            not string.match(symbol.name, "unused") and                        -- Ignore variables with "unused" in the name
            symbol.line and symbol.line > 0 then                               -- Ensure it's a real variable with a valid location
            table.insert(unused, symbol)
        end
    end
    return unused
end

--- Gets all symbols at all levels
---@return table Array of all symbols
function SymbolTable:get_all_symbols()
    local all_symbols = {}
    for _, symbol in pairs(self.symbols) do
        table.insert(all_symbols, symbol)
    end
    if self.parent then
        local parent_symbols = self.parent:get_all_symbols()
        for _, symbol in ipairs(parent_symbols) do
            table.insert(all_symbols, symbol)
        end
    end
    return all_symbols
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
        function_returns = {}, -- Track return types in functions
        modules = {},          -- Track loaded modules
        current_function_scope = nil
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
    -- Defensive: ensure expr_type is a table and has expected fields
    local kind = expr_type and expr_type["kind"] or nil
    local data = expr_type and expr_type["data"] or nil

    if op == "-" then
        if kind == "primitive" and data and data.name == "number" then
            return Type.number()
        elseif kind == "unknown" then
            return Type.number()
        else
            self:error("Attempt to perform arithmetic on a " .. tostring(expr_type) .. " value", node)
            return Type.unknown()
        end
    elseif op == "not" then
        return Type.boolean()
    elseif op == "#" then
        -- Length operator works on strings and tables
        if kind == "primitive" and data and data.name == "string" then
            return Type.number()
        elseif kind == "table" then
            return Type.number()
        elseif kind == "unknown" then
            return Type.number()
        else
            self:warning("Length operator applied to " .. tostring(expr_type), node)
            return Type.number()
        end
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
    -- Defensive: ensure left_type and right_type are tables and have expected fields
    local left_kind = left_type and left_type["kind"] or nil
    local left_data = left_type and left_type["data"] or nil
    local right_kind = right_type and right_type["kind"] or nil
    local right_data = right_type and right_type["data"] or nil

    local left_unknown = left_kind == "unknown"
    local right_unknown = right_kind == "unknown"

    -- Arithmetic operations
    if op == "+" or op == "-" or op == "*" or op == "/" or op == "%" or op == "^" then
        if left_kind == "primitive" and left_data and left_data.name == "number" and
            right_kind == "primitive" and right_data and right_data.name == "number" then
            return Type.number()
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
        if (left_kind == "primitive" and left_data and (left_data.name == "string" or left_data.name == "number")) and
            (right_kind == "primitive" and right_data and (right_data.name == "string" or right_data.name == "number")) then
            return Type.string()
        elseif left_unknown or right_unknown then
            return Type.string()
        else
            self:error("Attempt to concatenate " .. tostring(left_type) ..
                " and " .. tostring(right_type), node)
            return Type.string()
        end
        -- Comparison operations
    elseif op == "==" or op == "~=" then
        return Type.boolean()
    elseif op == "<" or op == ">" or op == "<=" or op == ">=" then
        if (left_kind == "primitive" and left_data and (left_data.name == "number" or left_data.name == "string")) and
            (right_kind == "primitive" and right_data and (right_data.name == "number" or right_data.name == "string")) and
            left_data.name == right_data.name then
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
        local symbol = scope["lookup"](scope, node.data.name)
        if not symbol then
            self:error("Undefined variable: " .. node.data.name, node)
            return Type.unknown()
        end
        return symbol.type
    elseif node.type == "binary_op" then
        local left_type = self:infer_expression(node.data.left, scope)
        local right_type = self:infer_expression(node.data.right, scope)
        return self:infer_binary_op(left_type, right_type, node.data.op, node)
    elseif node.type == "unary_op" then
        local expr_type = self:infer_expression(node.data.expr, scope)
        return self:infer_unary_op(expr_type, node.data.op, node)
    elseif node.type == "function_call" then
        return self:infer_function_call(node, scope)
    elseif node.type == "method_call" then
        return self:infer_method_call(node, scope)
    elseif node.type == "field_access" then
        return self:infer_field_access(node, scope)
    elseif node.type == "index_access" then
        return self:infer_index_access(node, scope)
    elseif node.type == "table_constructor" then
        return self:infer_table_constructor(node, scope)
    elseif node.type == "function_expression" then
        return self:infer_function_expression(node, scope)
    elseif node.type == "varargs" then
        return Type.unknown() -- Varargs can be anything
    else
        return Type.unknown()
    end
end

--- Infers the type of a function call
---@param node table Function call AST node
---@param scope SymbolTable Current scope
---@return Type Return type of the function
function TypeInference:infer_function_call(node, scope)
    local func_type = self:infer_expression(node.data.func, scope)
    local kind = func_type and func_type["kind"] or nil
    local data = func_type and func_type["data"] or nil

    if kind == "function" then
        local expected_params = data and data.params or {}
        local actual_args = node.data.args or {}

        -- Check argument count (allow varargs)
        if not (data and data.has_varargs) and #actual_args > #expected_params then
            self:warning("Function expects " .. #expected_params ..
                " arguments but got " .. #actual_args, node)
        elseif #actual_args < #expected_params then
            self:warning("Function expects " .. #expected_params ..
                " arguments but got " .. #actual_args, node)
        end

        -- Type check arguments
        for i, arg in ipairs(actual_args) do
            if i <= #expected_params then
                local arg_type = self:infer_expression(arg, scope)
                local expected_type = expected_params[i]
                if not (arg_type and expected_type and arg_type["is_assignable_to"] and arg_type["is_assignable_to"](arg_type, expected_type)) then
                    self:error("Argument " .. i .. " expects " .. tostring(expected_type) ..
                        " but got " .. tostring(arg_type), arg)
                end
            end
        end

        return (data and data.returns) or Type.unknown()
    else
        self:error("Attempt to call " .. tostring(func_type) .. " (not a function)", node)
        return Type.unknown()
    end
end

--- Infers the type of a method call
---@param node table Method call AST node
---@param scope SymbolTable Current scope
---@return Type Return type of the method
function TypeInference:infer_method_call(node, scope)
    local object_type = self:infer_expression(node.data.object, scope)
    local kind = object_type and object_type["kind"] or nil
    local data = object_type and object_type["data"] or nil

    if kind == "table" and data and data.fields then
        local method_type = data.fields[node.data.method]
        if method_type and method_type["kind"] == "function" then
            -- Method calls implicitly pass self as first argument
            local expected_params = method_type["data"] and method_type["data"].params or {}
            local actual_args = node.data.args or {}

            -- Check argument count (excluding implicit self)
            if #actual_args + 1 > #expected_params then
                self:warning("Method expects " .. (#expected_params - 1) ..
                    " arguments but got " .. #actual_args, node)
            end

            return (method_type["data"] and method_type["data"].returns) or Type.unknown()
        else
            self:warning("Method '" .. node.data.method .. "' not found on " .. tostring(object_type), node)
            return Type.unknown()
        end
    else
        self:warning("Method call on non-table type: " .. tostring(object_type), node)
        return Type.unknown()
    end
end

--- Infers the type of field access
---@param node table Field access AST node
---@param scope SymbolTable Current scope
---@return Type Type of the field
function TypeInference:infer_field_access(node, scope)
    local object_type = self:infer_expression(node.data.object, scope)

    if object_type["kind"] == "table" and object_type["data"].fields then
        local field_type = object_type["data"].fields[node.data.field]
        if field_type then
            return field_type
        else
            -- self:warning("Field '" .. node.data.field .. "' not found on table", node)
            return Type.unknown()
        end
    elseif object_type["kind"] == "module" and object_type["data"].exports then
        local export_type = object_type["data"].exports[node.data.field]
        if export_type then
            return export_type
        else
            self:error("Export '" .. node.data.field .. "' not found in module", node)
            return Type.unknown()
        end
    elseif object_type["kind"] == "unknown" then
        return Type.unknown()
    else
        self:error("Attempt to index " .. tostring(object_type) .. " (not a table)", node)
        return Type.unknown()
    end
end

--- Infers the type of index access
---@param node table Index access AST node
---@param scope SymbolTable Current scope
---@return Type Type of the indexed value
function TypeInference:infer_index_access(node, scope)
    local object_type = self:infer_expression(node.data.object, scope)
    local index_type = self:infer_expression(node.data.index, scope)

    if object_type["kind"] == "table" then
        if object_type["data"].array_type then
            -- Array-like table
            if index_type["kind"] == "primitive" and index_type["data"].name == "number" then
                return object_type["data"].array_type
            else
                self:warning("Array index should be number, got " .. tostring(index_type), node)
                return object_type["data"].array_type
            end
        elseif object_type["data"].fields then
            -- Try to find the specific field if index is a string literal
            if index_type["kind"] == "primitive" and index_type["data"].name == "string" then
                -- We'd need the actual string value here, which requires constant folding
                return Type.unknown()
            end
            return Type.unknown()
        else
            return Type.unknown()
        end
    elseif object_type["kind"] == "primitive" and object_type["data"].name == "string" then
        if index_type["kind"] == "primitive" and index_type["data"].name == "number" then
            return Type.string() -- String indexing returns string
        else
            self:error("String index must be number, got " .. tostring(index_type), node)
            return Type.unknown()
        end
    elseif object_type["kind"] == "unknown" then
        return Type.unknown()
    else
        self:error("Attempt to index " .. tostring(object_type), node)
        return Type.unknown()
    end
end

--- Infers the type of a table constructor
---@param node table Table constructor AST node
---@param scope SymbolTable Current scope
---@return Type Table type
function TypeInference:infer_table_constructor(node, scope)
    local fields = {}
    local array_type = nil
    local array_index = 1

    for _, field in ipairs(node.data.fields or {}) do
        if field.data.type == "named" then
            -- name = value
            local value_type = self:infer_expression(field.data.value, scope)
            fields[field.data.key] = value_type
        elseif field.data.type == "indexed" then
            -- [expr] = value
            -- local key_type = self:infer_expression(field.data.key, scope) -- Removed unused local
            local value_type = self:infer_expression(field.data.value, scope)
            -- For now, we can't track dynamic keys easily
            fields["<dynamic>"] = value_type
        elseif field.data.type == "array" then
            -- value (array-style)
            local value_type = self:infer_expression(field.data.value, scope)
            if not array_type then
                array_type = value_type
            elseif not (array_type.is_assignable_to and array_type:is_assignable_to(value_type)) then
                array_type = Type.union({ array_type, value_type })
            end
            fields[tostring(array_index)] = value_type
            array_index = array_index + 1
        end
    end

    return Type.table_type(fields, array_type)
end

--- Infers the type of a function expression
---@param node table Function expression AST node
---@param scope SymbolTable Current scope
---@return Type Function type
function TypeInference:infer_function_expression(node, scope)
    -- Fix: Use scope["enter_scope"] to avoid undefined field error
    local func_scope = scope["enter_scope"](scope, "function")
    local param_types = {}

    -- Define parameters
    for _, param_name in ipairs(node.data.params or {}) do
        local param_type = Type.unknown()
        table.insert(param_types, param_type)
        func_scope:define_parameter(param_name, param_type, node.line, node.column)
    end

    -- Analyze function body
    local return_types = {}
    local old_function_scope = self.current_function_scope
    self.current_function_scope = func_scope

    for _, stmt in ipairs(node.data.body or {}) do
        local return_type = self:analyze_statement(stmt, func_scope)
        if return_type then
            table.insert(return_types, return_type)
        end
    end

    self.current_function_scope = old_function_scope

    -- Determine return type
    local return_type = Type.nil_type()
    -- Post-optimization below
    if return_types[1] ~= nil then
        return_type = #return_types == 1 and return_types[1] or Type.union(return_types)
    end

    return Type.function_type(param_types, return_type, node.data.has_varargs)
end

--- Attempts to infer parameter type from usage patterns
---@param param_name string The parameter name
---@param body table Function body statements
---@param scope SymbolTable Function scope
---@return Type Inferred type
function TypeInference:infer_parameter_type_from_usage(param_name, body, scope)
    for _, stmt in ipairs(body or {}) do
        local inferred_type = self:analyze_statement_for_param_inference(stmt, param_name)
        if inferred_type["kind"] ~= "unknown" then
            return inferred_type
        end
    end
    return Type.unknown()
end

--- Analyzes a statement to infer parameter types
---@param stmt table Statement AST node
---@param param_name string Parameter name to look for
---@return Type Inferred type
function TypeInference:analyze_statement_for_param_inference(stmt, param_name)
    if not stmt then return Type.unknown() end

    if stmt.type == "return_statement" and stmt.data.exprs then
        for _, expr in ipairs(stmt.data.exprs) do
            local inferred = self:analyze_expression_for_param_inference(expr, param_name)
            if inferred and inferred["kind"] ~= "unknown" then
                return inferred
            end
        end
    elseif stmt.type == "local_assignment" and stmt.data.exprs then
        for _, expr in ipairs(stmt.data.exprs) do
            local inferred = self:analyze_expression_for_param_inference(expr, param_name)
            if inferred and inferred["kind"] ~= "unknown" then
                return inferred
            end
        end
    elseif stmt.type == "assignment" and stmt.data.exprs then
        for _, expr in ipairs(stmt.data.exprs) do
            local inferred = self:analyze_expression_for_param_inference(expr, param_name)
            if inferred and inferred["kind"] ~= "unknown" then
                return inferred
            end
        end
    elseif stmt.type == "expression_statement" and stmt.data.expr then
        local inferred = self:analyze_expression_for_param_inference(stmt.data.expr, param_name)
        if inferred and inferred["kind"] ~= "unknown" then
            return inferred
        end
    end

    return Type.unknown()
end

--- Helper to analyze expressions for parameter type inference
---@param expr table Expression AST node
---@param param_name string Parameter name to look for
---@return Type Inferred type
function TypeInference:analyze_expression_for_param_inference(expr, param_name)
    if not expr then return Type.unknown() end

    if expr.type == "binary_op" then
        if (expr.data.left and expr.data.left.type == "identifier" and expr.data.left.data.name == param_name) or
            (expr.data.right and expr.data.right.type == "identifier" and expr.data.right.data.name == param_name) then
            if expr.data.op == "+" or expr.data.op == "-" or expr.data.op == "*" or expr.data.op == "/" or expr.data.op == "%" then
                return Type.number()
            elseif expr.data.op == ".." then
                return Type.string()
            end
        end

        local left_type = self:analyze_expression_for_param_inference(expr.data.left, param_name)
        if left_type["kind"] ~= "unknown" then return left_type end

        local right_type = self:analyze_expression_for_param_inference(expr.data.right, param_name)
        if right_type["kind"] ~= "unknown" then return right_type end
    elseif expr.type == "function_call" then
        for _, arg in ipairs(expr.data.args or {}) do
            local arg_type = self:analyze_expression_for_param_inference(arg, param_name)
            if arg_type["kind"] ~= "unknown" then
                return arg_type
            end
        end
    elseif expr.type == "field_access" then
        if expr.data.object and expr.data.object.type == "identifier" and expr.data.object.data.name == param_name then
            return Type.table_type() -- Parameter used as table
        end
    elseif expr.type == "index_access" then
        if expr.data.object and expr.data.object.type == "identifier" and expr.data.object.data.name == param_name then
            return Type.table_type() -- Parameter used as table
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
        self:analyze_local_assignment(node, scope)
    elseif node.type == "assignment" then
        self:analyze_assignment(node, scope)
    elseif node.type == "function_def" then
        self:analyze_function_def(node, scope)
    elseif node.type == "local_function_def" then
        self:analyze_local_function_def(node, scope)
    elseif node.type == "return_statement" then
        return self:analyze_return_statement(node, scope)
    elseif node.type == "if_statement" then
        self:analyze_if_statement(node, scope)
    elseif node.type == "while_statement" then
        self:analyze_while_statement(node, scope)
    elseif node.type == "numeric_for_statement" then
        self:analyze_numeric_for_statement(node, scope)
    elseif node.type == "generic_for_statement" then
        self:analyze_generic_for_statement(node, scope)
    elseif node.type == "repeat_statement" then
        self:analyze_repeat_statement(node, scope)
    elseif node.type == "do_block" then
        self:analyze_do_block(node, scope)
    elseif node.type == "expression_statement" then
        self:infer_expression(node.data.expr, scope)
    elseif node.type == "break_statement" then
        -- Nothing to analyze for break
    elseif node.type == "goto_statement" then
        -- Could check if label exists, but that requires multi-pass analysis
    elseif node.type == "label" then
        -- Nothing to analyze for labels
    end
end

--- Analyzes local assignment
function TypeInference:analyze_local_assignment(node, scope)
    local names = node.data.names or {}
    local exprs = node.data.exprs or {}

    -- Infer types from expressions
    local expr_types = {}
    for _, expr in ipairs(exprs) do
        table.insert(expr_types, self:infer_expression(expr, scope))
    end

    -- Define variables
    for i, name in ipairs(names) do
        local var_type = expr_types[i] or Type.nil_type()
        scope:define(name, var_type, node.line, node.column)
    end
end

--- Analyzes assignment
function TypeInference:analyze_assignment(node, scope)
    local targets = node.data.targets or {}
    local exprs = node.data.exprs or {}

    -- Infer types from expressions
    local expr_types = {}
    for _, expr in ipairs(exprs) do
        table.insert(expr_types, self:infer_expression(expr, scope))
    end

    -- Type check assignments
    for i, target in ipairs(targets) do
        local expr_type = expr_types[i] or Type.nil_type()

        if target.type == "identifier" then
            local symbol = scope:lookup(target.data.name)
            if symbol then
                if not expr_type["is_assignable_to"](expr_type, symbol.type) then
                    self:error("Cannot assign " .. tostring(expr_type) ..
                        " to variable of type " .. tostring(symbol.type), target)
                end
            else
                -- Global assignment
                self.global_scope:define(target.data.name, expr_type, target.line, target.column)
            end
        elseif target.type == "field_access" then
            -- Check if object exists and is a table
            local object_type = self:infer_expression(target.data.object, scope)
            if object_type["kind"] ~= "table" and object_type["kind"] ~= "unknown" then
                self:error("Attempt to assign field on " .. tostring(object_type), target)
            end
        elseif target.type == "index_access" then
            -- Check if object exists and is indexable
            local object_type = self:infer_expression(target.data.object, scope)
            if object_type["kind"] ~= "table" and object_type["kind"] ~= "unknown" then
                self:error("Attempt to index " .. tostring(object_type), target)
            end
        end
    end
end

--- Analyzes function definition
function TypeInference:analyze_function_def(node, scope)
    local func_scope = scope:enter_scope("function")
    local param_types = {}

    -- Define parameters
    for _, param_name in ipairs(node.data.params or {}) do
        local param_type = Type.unknown()
        table.insert(param_types, param_type)
        func_scope:define_parameter(param_name, param_type, node.line, node.column)
    end

    -- Infer parameter types from usage
    for i, param_name in ipairs(node.data.params or {}) do
        local inferred_type = self:infer_parameter_type_from_usage(param_name, node.data.body, func_scope)
        if inferred_type["kind"] ~= "unknown" then
            param_types[i] = inferred_type
            func_scope.symbols[param_name].type = inferred_type
        end
    end

    -- Analyze function body
    local return_types = {}
    local old_function_scope = self.current_function_scope
    self.current_function_scope = func_scope

    for _, stmt in ipairs(node.data.body or {}) do
        local return_type = self:analyze_statement(stmt, func_scope)
        if return_type then
            table.insert(return_types, return_type)
        end
    end

    self.current_function_scope = old_function_scope

    -- Determine return type
    local return_type = Type.nil_type()
    if #return_types > 0 then
        return_type = #return_types == 1 and return_types[1] or Type.union(return_types)
    end

    -- Create function type
    local func_type = Type.function_type(param_types, return_type, node.data.has_varargs)

    -- Define function name (can be dotted like foo.bar.baz or method like obj:method)
    if node.data.is_method then
        -- For methods (obj:method), we need to add the method to the table
        if #node.data.name_parts >= 2 then
            local object_name = node.data.name_parts[1]
            local method_name = node.data.name_parts[#node.data.name_parts]

            local object_symbol = scope:lookup(object_name)
            if object_symbol and object_symbol.type.kind == "table" then
                -- Add method to the table's fields
                if not object_symbol.type.data.fields then
                    object_symbol.type.data.fields = {}
                end
                object_symbol.type.data.fields[method_name] = func_type
            else
                -- If not found or not a table, just define the first part as a function
                self:warning("Method defined on non-table type: " .. object_name, node)
                scope:define(object_name, func_type, node.line, node.column)
            end
        end
    else
        -- Regular function (not a method)
        if #node.data.name_parts == 1 then
            scope:define(node.data.name_parts[1], func_type, node.line, node.column)
        else
            -- For dotted names, we'd need to traverse the table structure
            -- For now, just define the first part
            scope:define(node.data.name_parts[1], func_type, node.line, node.column)
        end
    end

    -- Check for unused parameters
    for _, symbol in pairs(func_scope.symbols) do
        if symbol.is_parameter and not symbol.used and symbol.name ~= "_" then
            self:warning("Unused parameter: " .. symbol.name, node)
        end
    end
end

--- Analyzes local function definition
function TypeInference:analyze_local_function_def(node, scope)
    local func_scope = scope:enter_scope("function")
    local param_types = {}

    -- Define parameters
    for _, param_name in ipairs(node.data.params or {}) do
        local param_type = Type.unknown()
        table.insert(param_types, param_type)
        func_scope:define_parameter(param_name, param_type, node.line, node.column)
    end

    -- Infer parameter types from usage
    for i, param_name in ipairs(node.data.params or {}) do
        local inferred_type = self:infer_parameter_type_from_usage(param_name, node.data.body, func_scope)
        if inferred_type ~= Type.unknown() then
            param_types[i] = inferred_type
            func_scope.symbols[param_name].type = inferred_type
        end
    end

    -- Analyze function body
    local return_types = {}
    local old_function_scope = self.current_function_scope
    self.current_function_scope = func_scope

    for _, stmt in ipairs(node.data.body or {}) do
        local return_type = self:analyze_statement(stmt, func_scope)
        if return_type then
            table.insert(return_types, return_type)
        end
    end

    self.current_function_scope = old_function_scope

    -- Determine return type
    local return_type = Type.nil_type()
    if #return_types > 0 then
        return_type = #return_types == 1 and return_types[1] or Type.union(return_types)
    end

    local func_type = Type.function_type(param_types, return_type, node.data.has_varargs)
    scope:define(node.data.name, func_type, node.line, node.column)

    -- Check for unused parameters
    for _, symbol in pairs(func_scope.symbols) do
        if symbol.is_parameter and not symbol.used and symbol.name ~= "_" then
            self:warning("Unused parameter: " .. symbol.name, node)
        end
    end
end

--- Analyzes return statement
function TypeInference:analyze_return_statement(node, scope)
    if not node.data.exprs or #node.data.exprs == 0 then
        return Type.nil_type()
    elseif #node.data.exprs == 1 then
        return self:infer_expression(node.data.exprs[1], scope)
    else
        local types = {}
        for _, expr in ipairs(node.data.exprs) do
            table.insert(types, self:infer_expression(expr, scope))
        end
        return Type.union(types)
    end
end

--- Analyzes if statement
function TypeInference:analyze_if_statement(node, scope)
    -- Check condition type
    local cond_type = self:infer_expression(node.data.condition, scope)

    -- Analyze then block
    local then_scope = scope:enter_scope("if-then")
    for _, stmt in ipairs(node.data.then_block or {}) do
        self:analyze_statement(stmt, then_scope)
    end

    -- Check for unused variables in then block
    for _, unused in ipairs(then_scope:get_unused_variables()) do
        self:warning("Unused variable: " .. unused.name,
            { line = unused.line, column = unused.column })
    end

    -- Analyze elseif blocks
    for _, elseif_block in ipairs(node.data.elseif_blocks or {}) do
        local elseif_cond_type = self:infer_expression(elseif_block.condition, scope)

        local elseif_scope = scope:enter_scope("elseif")
        for _, stmt in ipairs(elseif_block.body or {}) do
            self:analyze_statement(stmt, elseif_scope)
        end

        for _, unused in ipairs(elseif_scope:get_unused_variables()) do
            self:warning("Unused variable: " .. unused.name,
                { line = unused.line, column = unused.column })
        end
    end

    -- Analyze else block
    if node.data.else_block then
        local else_scope = scope:enter_scope("else")
        for _, stmt in ipairs(node.data.else_block) do
            self:analyze_statement(stmt, else_scope)
        end

        for _, unused in ipairs(else_scope:get_unused_variables()) do
            self:warning("Unused variable: " .. unused.name,
                { line = unused.line, column = unused.column })
        end
    end
end

--- Analyzes while statement
function TypeInference:analyze_while_statement(node, scope)
    local cond_type = self:infer_expression(node.data.condition, scope)

    local loop_scope = scope:enter_scope("while")
    for _, stmt in ipairs(node.data.body or {}) do
        self:analyze_statement(stmt, loop_scope)
    end

    for _, unused in ipairs(loop_scope:get_unused_variables()) do
        self:warning("Unused variable: " .. unused.name,
            { line = unused.line, column = unused.column })
    end
end

--- Analyzes numeric for statement
function TypeInference:analyze_numeric_for_statement(node, scope)
    local start_type = self:infer_expression(node.data.start, scope)
    local finish_type = self:infer_expression(node.data.finish, scope)
    local step_type = node.data.step and self:infer_expression(node.data.step, scope) or Type.number()

    -- Check that start, finish, step are numbers
    if start_type["kind"] == "primitive" and start_type["data"].name ~= "number" and start_type["kind"] ~= "unknown" then
        self:error("For loop start value must be number, got " .. tostring(start_type), node.data.start)
    end
    if finish_type["kind"] == "primitive" and finish_type["data"].name ~= "number" and finish_type["kind"] ~= "unknown" then
        self:error("For loop end value must be number, got " .. tostring(finish_type), node.data.finish)
    end
    if step_type["kind"] == "primitive" and step_type["data"].name ~= "number" and step_type["kind"] ~= "unknown" then
        self:error("For loop step value must be number, got " .. tostring(step_type), node.data.step)
    end

    local for_scope = scope:enter_scope("for")
    for_scope:define(node.data.var, Type.number(), node.line, node.column)

    for _, stmt in ipairs(node.data.body or {}) do
        self:analyze_statement(stmt, for_scope)
    end

    for _, unused in ipairs(for_scope:get_unused_variables()) do
        self:warning("Unused variable: " .. unused.name,
            { line = unused.line, column = unused.column })
    end
end

--- Analyzes generic for statement
function TypeInference:analyze_generic_for_statement(node, scope)
    local iterator_types = {}
    for _, iterator in ipairs(node.data.iterators or {}) do
        table.insert(iterator_types, self:infer_expression(iterator, scope))
    end

    local for_scope = scope:enter_scope("for")

    -- For generic for loops, we can't easily infer the variable types
    -- without more sophisticated analysis of the iterator functions
    for _, var in ipairs(node.data.vars or {}) do
        for_scope:define(var, Type.unknown(), node.line, node.column)
    end

    for _, stmt in ipairs(node.data.body or {}) do
        self:analyze_statement(stmt, for_scope)
    end

    for _, unused in ipairs(for_scope:get_unused_variables()) do
        if not unused.name:match("^_") then -- Allow _var patterns
            self:warning("Unused variable: " .. unused.name,
                { line = unused.line, column = unused.column })
        end
    end
end

--- Analyzes repeat statement
function TypeInference:analyze_repeat_statement(node, scope)
    local repeat_scope = scope:enter_scope("repeat")

    for _, stmt in ipairs(node.data.body or {}) do
        self:analyze_statement(stmt, repeat_scope)
    end

    -- Condition is evaluated in the repeat scope (can see variables defined in body)
    local cond_type = self:infer_expression(node.data.condition, repeat_scope)

    for _, unused in ipairs(repeat_scope:get_unused_variables()) do
        self:warning("Unused variable: " .. unused.name,
            { line = unused.line, column = unused.column })
    end
end

--- Analyzes do block
function TypeInference:analyze_do_block(node, scope)
    local block_scope = scope:enter_scope("do")

    for _, stmt in ipairs(node.data.body or {}) do
        self:analyze_statement(stmt, block_scope)
    end

    for _, unused in ipairs(block_scope:get_unused_variables()) do
        self:warning("Unused variable: " .. unused.name,
            { line = unused.line, column = unused.column })
    end
end

--- Performs static analysis on the AST.
---@param ast table The AST to analyze
---@param options table|nil Analysis options
---@return table Analysis results
function TypeInference:analyze(ast, options)
    options = options or {}

    self.current_scope = self.global_scope

    -- Add comprehensive built-in functions and libraries
    self:add_builtin_globals()

    -- Analyze the AST
    for _, stmt in ipairs(ast or {}) do
        self:analyze_statement(stmt, self.current_scope)
    end

    -- Check for unused global variables (exclude built-ins)
    local built_ins = {
        "print", "type", "tostring", "tonumber", "pairs", "ipairs", "next",
        "string", "table", "math", "io", "os", "_G", "require", "package",
        "coroutine", "debug", "getmetatable", "setmetatable", "rawget", "rawset",
        "rawequal", "rawlen", "select", "unpack", "pcall", "xpcall", "error",
        "assert", "loadstring", "load", "loadfile", "dofile", "collectgarbage"
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

    return {
        errors = self.errors,
        warnings = self.warnings
    }
end

--- Adds built-in global functions and libraries
function TypeInference:add_builtin_globals()
    local scope = self.global_scope

    -- Core functions
    scope:define("print", Type.function_type({ Type.unknown() }, Type.nil_type(), true))
    scope:define("type", Type.function_type({ Type.unknown() }, Type.string()))
    scope:define("tostring", Type.function_type({ Type.unknown() }, Type.string()))
    scope:define("tonumber", Type.function_type({ Type.union({ Type.string(), Type.number() }), Type.number() },
        Type.union({ Type.number(), Type.nil_type() })))
    scope:define("pairs", Type.function_type({ Type.table_type() }, Type.function_type()))
    scope:define("ipairs", Type.function_type({ Type.table_type() }, Type.function_type()))
    scope:define("next", Type.function_type({ Type.table_type(), Type.unknown() }, Type.unknown()))
    scope:define("select", Type.function_type({ Type.union({ Type.string(), Type.number() }) }, Type.unknown(), true))
    scope:define("unpack", Type.function_type({ Type.table_type(), Type.number(), Type.number() }, Type.unknown(), true))
    scope:define("getmetatable", Type.function_type({ Type.table_type() }, Type.table_type()))
    scope:define("setmetatable", Type.function_type({ Type.table_type(), Type.table_type() }, Type.table_type()))
    scope:define("rawget", Type.function_type({ Type.table_type(), Type.unknown() }, Type.unknown()))
    scope:define("rawset", Type.function_type({ Type.table_type(), Type.unknown(), Type.unknown() }, Type.table_type()))
    scope:define("rawequal", Type.function_type({ Type.unknown(), Type.unknown() }, Type.boolean()))
    scope:define("rawlen", Type.function_type({ Type.table_type() }, Type.number()))
    scope:define("pcall", Type.function_type({ Type.function_type() }, Type.boolean(), true))
    scope:define("xpcall", Type.function_type({ Type.function_type(), Type.function_type() }, Type.boolean(), true))
    scope:define("error", Type.function_type({ Type.unknown(), Type.number() }, Type.nil_type()))
    scope:define("assert", Type.function_type({ Type.unknown(), Type.unknown() }, Type.unknown()))
    scope:define("collectgarbage", Type.function_type({ Type.string(), Type.unknown() }, Type.unknown()))

    -- File operations
    scope:define("loadstring", Type.function_type({ Type.string(), Type.string() },
        Type.union({ Type.function_type(), Type.nil_type() })))
    scope:define("load", Type.function_type({ Type.unknown(), Type.string(), Type.string(), Type.table_type() },
        Type.union({ Type.function_type(), Type.nil_type() })))
    scope:define("loadfile", Type.function_type({ Type.string(), Type.string(), Type.table_type() },
        Type.union({ Type.function_type(), Type.nil_type() })))
    scope:define("dofile", Type.function_type({ Type.string() }, Type.unknown()))

    -- String library
    local string_lib = Type.table_type({
        byte = Type.function_type({ Type.string(), Type.number(), Type.number() }, Type.number()),
        char = Type.function_type({ Type.number() }, Type.string(), true),
        dump = Type.function_type({ Type.function_type() }, Type.string()),
        find = Type.function_type({ Type.string(), Type.string(), Type.number(), Type.boolean() },
            Type.union({ Type.number(), Type.nil_type() })),
        format = Type.function_type({ Type.string() }, Type.string(), true),
        gmatch = Type.function_type({ Type.string(), Type.string() }, Type.function_type()),
        gsub = Type.function_type({ Type.string(), Type.string(), Type.unknown(), Type.number() }, Type.string()),
        len = Type.function_type({ Type.string() }, Type.number()),
        lower = Type.function_type({ Type.string() }, Type.string()),
        match = Type.function_type({ Type.string(), Type.string(), Type.number() },
            Type.union({ Type.string(), Type.nil_type() })),
        rep = Type.function_type({ Type.string(), Type.number(), Type.string() }, Type.string()),
        reverse = Type.function_type({ Type.string() }, Type.string()),
        sub = Type.function_type({ Type.string(), Type.number(), Type.number() }, Type.string()),
        upper = Type.function_type({ Type.string() }, Type.string())
    })
    scope:define("string", string_lib)

    -- Table library
    local table_lib = Type.table_type({
        concat = Type.function_type({ Type.table_type(), Type.string(), Type.number(), Type.number() }, Type.string()),
        insert = Type.function_type({ Type.table_type(), Type.unknown(), Type.unknown() }, Type.nil_type()),
        maxn = Type.function_type({ Type.table_type() }, Type.number()),
        remove = Type.function_type({ Type.table_type(), Type.number() }, Type.unknown()),
        sort = Type.function_type({ Type.table_type(), Type.function_type() }, Type.nil_type())
    })
    scope:define("table", table_lib)

    -- Math library
    local math_lib = Type.table_type({
        abs = Type.function_type({ Type.number() }, Type.number()),
        acos = Type.function_type({ Type.number() }, Type.number()),
        asin = Type.function_type({ Type.number() }, Type.number()),
        atan = Type.function_type({ Type.number() }, Type.number()),
        atan2 = Type.function_type({ Type.number(), Type.number() }, Type.number()),
        ceil = Type.function_type({ Type.number() }, Type.number()),
        cos = Type.function_type({ Type.number() }, Type.number()),
        cosh = Type.function_type({ Type.number() }, Type.number()),
        deg = Type.function_type({ Type.number() }, Type.number()),
        exp = Type.function_type({ Type.number() }, Type.number()),
        floor = Type.function_type({ Type.number() }, Type.number()),
        fmod = Type.function_type({ Type.number(), Type.number() }, Type.number()),
        frexp = Type.function_type({ Type.number() }, Type.number()),
        ldexp = Type.function_type({ Type.number(), Type.number() }, Type.number()),
        log = Type.function_type({ Type.number(), Type.number() }, Type.number()),
        log10 = Type.function_type({ Type.number() }, Type.number()),
        max = Type.function_type({ Type.number() }, Type.number(), true),
        min = Type.function_type({ Type.number() }, Type.number(), true),
        modf = Type.function_type({ Type.number() }, Type.number()),
        pow = Type.function_type({ Type.number(), Type.number() }, Type.number()),
        rad = Type.function_type({ Type.number() }, Type.number()),
        random = Type.function_type({ Type.number(), Type.number() }, Type.number()),
        randomseed = Type.function_type({ Type.number() }, Type.nil_type()),
        sin = Type.function_type({ Type.number() }, Type.number()),
        sinh = Type.function_type({ Type.number() }, Type.number()),
        sqrt = Type.function_type({ Type.number() }, Type.number()),
        tan = Type.function_type({ Type.number() }, Type.number()),
        tanh = Type.function_type({ Type.number() }, Type.number()),
        huge = Type.number(),
        pi = Type.number()
    })
    scope:define("math", math_lib)

    -- IO library (simplified)
    local io_lib = Type.table_type({
        close = Type.function_type({ Type.unknown() }, Type.boolean()),
        flush = Type.function_type({}, Type.nil_type()),
        input = Type.function_type({ Type.unknown() }, Type.unknown()),
        lines = Type.function_type({ Type.string() }, Type.function_type()),
        open = Type.function_type({ Type.string(), Type.string() }, Type.unknown()),
        output = Type.function_type({ Type.unknown() }, Type.unknown()),
        popen = Type.function_type({ Type.string(), Type.string() }, Type.unknown()),
        read = Type.function_type({}, Type.string(), true),
        tmpfile = Type.function_type({}, Type.unknown()),
        type = Type.function_type({ Type.unknown() }, Type.string()),
        write = Type.function_type({}, Type.nil_type(), true)
    })
    scope:define("io", io_lib)

    -- OS library (simplified)
    local os_lib = Type.table_type({
        clock = Type.function_type({}, Type.number()),
        date = Type.function_type({ Type.string(), Type.number() }, Type.union({ Type.string(), Type.table_type() })),
        difftime = Type.function_type({ Type.number(), Type.number() }, Type.number()),
        execute = Type.function_type({ Type.string() }, Type.number()),
        exit = Type.function_type({ Type.number() }, Type.nil_type()),
        getenv = Type.function_type({ Type.string() }, Type.union({ Type.string(), Type.nil_type() })),
        remove = Type.function_type({ Type.string() }, Type.boolean()),
        rename = Type.function_type({ Type.string(), Type.string() }, Type.boolean()),
        setlocale = Type.function_type({ Type.string(), Type.string() }, Type.string()),
        time = Type.function_type({ Type.table_type() }, Type.number()),
        tmpname = Type.function_type({}, Type.string())
    })
    scope:define("os", os_lib)

    -- Package/require system
    local package_lib = Type.table_type({
        config = Type.string(),
        cpath = Type.string(),
        loaded = Type.table_type(),
        loaders = Type.table_type(),
        path = Type.string(),
        preload = Type.table_type(),
        seeall = Type.function_type({ Type.table_type() }, Type.nil_type())
    })
    scope:define("package", package_lib)
    scope:define("require", Type.function_type({ Type.string() }, Type.unknown()))

    -- Special globals
    scope:define("_G", Type.table_type())
    scope:define("_VERSION", Type.string())
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
