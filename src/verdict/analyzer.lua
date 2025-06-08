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
        return self.data
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

return analyzer
