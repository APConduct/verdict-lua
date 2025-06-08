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

function Type.__tostring(self)
    -- TODO: Implement a __tostring method
end

Type.number = function() return Type.new("primative", { name = "number" }) end

Type.string = function() return Type.new("primative", { name = "string" }) end

Type.boolean = function() return Type.new("primative", { name = "boolean" }) end

Type.nil_type = function() return Type.new("primative", { name = "nil" }) end

Type.unknown = function() return Type.new("unknown") end



return analyzer
