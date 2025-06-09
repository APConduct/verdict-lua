--- The analyzer module for the verdict Lua static analysis tool.
-- @module analizer
local analyzer = {}


--- Represents a lua type.
-- @type Type
local Type = {}
Type.__index = Type

--- Creates a new Type instance.
---@param kind string The kind of the type (e.g., "string", "number", etc.)
---@param data any Additional data associated with the type (optional)
---@return Type A new Type instance.
function Type.new(kind, data)
    return setmetatable({
        kind = kind, -- "primative", "function", "table", "unknown"
        data = data or {}
    })
end

--- Returns a string representation of the type.
---@return string A string representation of the type.
function Type:__tostring()
    if self.kind == "primative" then
        return self.data.name or tostring(self.data)
    elseif self.kind == "function" then
        local params = {}
        for _, param in ipairs(self.data.params or {}) do
            table.insert(params, tostring(param))
        end
        return "(" .. table.concat(params, ", ") .. ") -> " .. tostring(self.data.returns or Type.unknown())
    elseif self.kind == "table" then
        return "table" -- TODO: Implement better table string representation
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

Type.number = function() return Type.new("primative", { name = "number" }) end

Type.string = function() return Type.new("primative", { name = "string" }) end

Type.boolean = function() return Type.new("primative", { name = "boolean" }) end

Type.nil_type = function() return Type.new("primative", { name = "nil" }) end

Type.unknown = function() return Type.new("unknown") end

--- Represents a symbol table for type inference.
-- @type SymbolTable
local SymbolTable = {}
SymbolTable.__index = SymbolTable

--- Creates a new SymbolTable instance.
---@param parent SymbolTable | nil The parent symbol table (optional)
function SymbolTable.new(parent)
    return setmetatable({
        parent = parent,
        symbols = {}
    }, SymbolTable)
end

--- Defines a symbol in the symbol table.
---@param name string The name of the symbol
---@param type Type The type of the symbol
---@return nil
function SymbolTable:define(name, type)
    self.symbols[name] = type
end

--- Looks up a symbol in the symbol table.
---@param name string The name of the symbol
---@return Type | nil The type of the symbol if found, otherwise nil
function SymbolTable:lookup(name)
    if self.symbols[name] then
        return self.symbols[name]
    elseif self.parent then
        return self.parent:lookup(name)
    else
        return nil
    end
end

--- Enters a new scope in the symbol table.
---@return SymbolTable A new SymbolTable instance representing the new scope
function SymbolTable:enter_scope()
    return SymbolTable.new(self)
end

-- AST node types (simplified)
--- Creates a new AST node.
---@param type string The type of the node (e.g., "number", "string", etc.)
---@param data table Additional data associated with the node (optional)
---@return table A new AST node
local function create_node(type, data)
    return { type = type, data = data or {} }
end

--- The type inference module for the verdict Lua static analysis tool.
-- @type TypeInference
local TypeInference = {}

--- Creates a new TypeInference instance.
---@return table A new TypeInference instance
function TypeInference.new()
    return {
        global_scope = SymbolTable.new(),
        current_scope = nil,
        errors = {}
    }
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
    })
end

--- Infers the type of a given AST node.
---@param node table The AST node to infer the type for
---@return Type The inferred type of the node
function TypeInference:infer_literal(node)
    if node.type == "number" then
        return Type.number()
    elseif node.type == "string" then
        return Type.string()
    elseif node.type == "boolean" then
        return Type.boolean()
    elseif node.type == "nil" then
        return Type.nil_type()
    else
        return Type.unknown()
    end
end

function TypeInference:infer_binary_op(left_type, right_type, op)
    -- Simplified binary operation type rules
    if op == "+" or op == "-" or op == "*" or op == "/" or op == "%" then
        if left_type.kind == "primative" and left_type.data.name == "number"
            and right_type.kind == "primative" and right_type.data.name == "number" then
            return Type.number()
        else
            return Type.unknown()
        end
    elseif op == ".." then
        -- String concatenation
        return Type.string()
    elseif op == "==" or op == "~=" or op == "<" or op == ">" or op == "<=" or op == ">=" then
        return Type.boolean()
    elseif op == "and" or op == "or" then
        -- Simplified: return right type for now
        return right_type
    else
        return Type.unknown()
    end
end

function TypeInference:infer_expression(node, scope)
    if not node then
        return Type.unknown()
    end

    if node.type == "literal" then
        return self:infer_literal(node.data)
    elseif node.type == "identifier" then
        local var_type = scope:lookup(node.data.name)
        if not var_type then
            self:error("Unidentified variable: " .. node.data.name, node)
            return Type.unknown()
        end
        return var_type
    elseif node.type == "binary_op" then
        local left_type = self:infer_expression(node.data.left, scope)
        local right_type = self:infer_expression(node.data.right, scope)
        return self:infer_binary_op(left_type, right_type, node.data.op)
    elseif node.type == "function_call" then
        local func_type = self:infer_expression(node.data.func, scope)
        if func_type.kind == "function" then
            return func_type.data.returns or Type.unknown()
        else
            self:error("Attempting to call non-function", node)
            return Type.unknown()
        end
    elseif node.type == "table_constructor" then
        -- Simplified table inference
        return Type.new("table", { fields = {} })
    else
        return Type.unknown()
    end
end

function TypeInference:analyze_statement(node, scope)
    if not node then return end

    if node.type == "local_assignment" then
        -- local x = expr
        local expr_type = self:infer_expression(node.data.expr, scope)
        scope:define(node.data.name, expr_type)
    elseif node.type == "assignment" then
        -- x = expr
        local expr_type = self:infer_expression(node.data.expr, scope)
        local var_type = scope:lookup(node.data.name)
        if var_type then
            -- TODO: Check type compatability
        else
            -- Global assignment
            self.global_scope:define(node.data.name, expr_type)
        end
    elseif node.type == "function_def" then
        -- function name(params) body end
        local func_type = Type.new("function", {
            params = {},             -- TODO: infer parameter types
            returns = Type.unknown() -- TODO: infer return type
        })
        scope:define(node.data.name, func_type)

        -- Analyze function body in new scope
        local func_scope = scope:enter_scope()
        for _, stmt in ipairs(node.data.body or {}) do
            self:analyze_statement(stmt, func_scope)
        end
    elseif node.type == "if_statement" then
        local cond_type = self:infer_expression(node.data.condition, scope)
        -- TODO: Type narrowing based on condition
        for _, stmt in ipairs(node.data.then_block or {}) do
            self:analyze_statement(stmt, scope)
        end

        if node.data.else_block then
            for _, stmt in ipairs(node.data.else_block) do
                self:analyze_statement(stmt, scope)
            end
        end
    elseif node.type == "block" then
        local block_scope = scope:enter_scope()
        for _, stmt in ipairs(node.data.statements or {}) do
            self:analyze_statement(stmt, block_scope)
        end
    end
end

function TypeInference:analyze(ast)
    self.current_scope = self.global_scope

    -- Add built-in functions
    self.global_scope:define("print", Type.new("function", {
        params = { Type.unknown() },
        returns = Type.nil_type()
    }))

    self.global_scope:define("type", Type.new("function", {
        params = { Type.unknown() },
        returns = Type.string()
    }))

    -- Analyze the AST
    for _, stmt in ipairs(ast or {}) do
        self:analyze_statement(stmt, self.current_scope)
    end

    return self.errors
end

-- Example usage and testing
function analyzer.create_example_ast()
    -- Example ast for: local x = 42 print(x + 1)
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
    print("Testing lua static analyzer")

    local inference = TypeInference.new()
    local ast = analyzer.create_example_ast()
    local errors = inference:analyze(ast)

    print("Analysis complete. Errors found:", #errors)
    for _, error in ipairs(errors) do
        print("Error: " .. error.message)
    end

    -- Test symbol lookup
    local x_type = inference.global_scope:lookup("x")
    if x_type then
        print("Variable 'x' has type: " .. tostring(x_type))
    end
end

-- Export the main components
analyzer.Type = Type
analyzer.SymbolTable = SymbolTable
analyzer.TypeInference = TypeInference
analyzer.test = analyzer.test

return analyzer
