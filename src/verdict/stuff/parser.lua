--- Parser module for parsing Lua code.
-- @module parser
local Parser = {}
Parser.__index = Parser

--- Creates a new AST node
---@param type string The node type
---@param data table Additional node data
---@param line number Line number
---@param column number Column number
---@return table A new AST node
local function create_node(type, data, line, column)
    return {
        type = type,
        data = data or {},
        line = line,
        column = column
    }
end

--- Creates a new Parser instance
---@param tokens table Array of tokens from lexer
---@return table a new Parser instance
function Parser.new(tokens)
    return setmetatable({
        tokens = tokens,
        position = 1,
        current_token = tokens[1]
    }, Parser)
end

return Parser
