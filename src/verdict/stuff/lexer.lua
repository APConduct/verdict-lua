--- A simple lexer for parsing Lua code.
-- @module lexer
local Lexer = {}
Lexer.__index = Lexer

-- Token types
--
local TOKEN_TYPES = {
    -- Literals
    NUMBER = "NUMBER",
    STRING = "STRING",
    BOOLEAN = "BOOLEAN",
    NIL = "NIL",

    -- Identifiers and keywords
    IDENTIFIER = "IDENTIFIER",
    KEYWORD = "KEYWORD",

    -- Operators
    PLUS = "PLUS",         -- +
    MINUS = "MINUS",       -- -
    MULTIPLY = "MULTIPLY", -- *
    DIVIDE = "DIVIDE",     -- /
    MODULO = "MODULO",     -- %
    POWER = "POWER",       -- ^
    CONCAT = "CONCAT",     -- ..
    VARARGS = "VARARGS",   -- ...

    -- Comparison
    EQUAL = "EQUAL",                 -- ==
    NOT_EQUAL = "NOT_EQUAL",         -- ~=
    LESS_THAN = "LESS_THAN",         -- <
    LESS_EQUAL = "LESS_EQUAL",       -- <=
    GREATER_THAN = "GREATER_THAN",   -- >
    GREATER_EQUAL = "GREATER_EQUAL", -- >=
    LENGTH = "LENGTH",               -- #

    -- Assignment
    ASSIGN = "ASSIGN",

    -- Logical
    AND = "AND",
    OR = "OR",
    NOT = "NOT",

    -- Delimiters
    LEFT_PAREN = "LEFT_PAREN",       -- (
    RIGHT_PAREN = "RIGHT_PAREN",     -- )
    LEFT_BRACE = "LEFT_BRACE",       -- {
    RIGHT_BRACE = "RIGHT_BRACE",     -- }
    LEFT_BRACKET = "LEFT_BRACKET",   -- [
    RIGHT_BRACKET = "RIGHT_BRACKET", -- ]
    SEMICOLON = "SEMICOLON",         -- ;
    COMMA = "COMMA",                 -- ,
    DOT = "DOT",                     -- .
    COLON = "COLON",                 -- :
    DOUBLE_COLON = "DOUBLE_COLON",   -- ::

    -- Special
    EOF = "EOF",
    NEWLINE = "NEWLINE"
}

-- Lua keywords
local KEYWORDS = {
    ["and"] = true,
    ["break"] = true,
    ["do"] = true,
    ["else"] = true,
    ["elseif"] = true,
    ["end"] = true,
    ["false"] = true,
    ["for"] = true,
    ["function"] = true,
    ["goto"] = true,
    ["if"] = true,
    ["in"] = true,
    ["local"] = true,
    ["nil"] = true,
    ["not"] = true,
    ["or"] = true,
    ["repeat"] = true,
    ["return"] = true,
    ["then"] = true,
    ["true"] = true,
    ["until"] = true,
    ["while"] = true,
}

--- Creates a new token
---@param type string The token type
---@param value string|any The token value
---@param line number The line number
---@param column number The column number
---@return table A new token
local function create_token(type, value, line, column)
    return {
        type = type,
        value = value,
        line = line,
        column = column
    }
end

--- Creates a new Lexer instance
---@param source string The source code to tokenize
---@return table A new Lexer instance
function Lexer.new(source)
    return setmetatable({
        source = source,
        position = 1,
        line = 1,
        column = 1,
        tokens = {}
    }, Lexer)
end

--- Gets the current character
---@return string|nil The character or nil if at end
function Lexer:current_char()
    if self.position > #self.source then
        return nil
    end
    return self.source:sub(self.position, self.position)
end

--- Peeks at the next character without advancing
---@param offset number|nil Optional offset (default 1)
---@return string|nil The character at position + offset
function Lexer:peek_char(offset)
    offset = offset or 1
    local pos = self.position + offset
    if pos > #self.source then
        return nil
    end
    return self.source:sub(pos, pos)
end

--- Advances to the next character
function Lexer:advance()
    if self.position <= #self.source then
        if self:current_char() == '\n' then
            self.line = self.line + 1
            self.column = 1
        else
            self.column = self.column + 1
        end
        self.position = self.position + 1
    end
end

--- Skips comments (lines starting with --)
function Lexer:skip_comment()
    -- Skip the -- characters
    self:advance()
    self:advance()

    -- Skip everything until end of line
    while self:current_char() and self:current_char() ~= '\n' do
        self:advance()
    end
end

--- Skips whitespace characters
function Lexer:skip_whitespace()
    while self:current_char() and self:current_char():match("%s") and self:current_char() ~= '\n' do
        self:advance()
    end
end

--- Reads a number token (including hex and scientific notation)
function Lexer:read_number()
    local start_line, start_column = self.line, self.column
    local number_str = ""

    -- Handle hex numbers (0x...)
    if self:current_char() == '0' and (self:peek_char() == 'x' or self:peek_char() == 'X') then
        number_str = number_str .. self:current_char() -- 0
        self:advance()
        number_str = number_str .. self:current_char() -- x
        self:advance()

        while self:current_char() and self:current_char():match("[%da-fA-F]") do
            number_str = number_str .. self:current_char()
            self:advance()
        end
    else
        -- Regular decimal numbers
        while self:current_char() and (self:current_char():match("%d") or self:current_char() == ".") do
            number_str = number_str .. self:current_char()
            self:advance()
        end

        -- Scientific notation (e.g., 1e5, 1.2e-3)
        if self:current_char() and (self:current_char():lower() == 'e') then
            number_str = number_str .. self:current_char()
            self:advance()

            if self:current_char() and (self:current_char() == '+' or self:current_char() == '-') then
                number_str = number_str .. self:current_char()
                self:advance()
            end

            while self:current_char() and self:current_char():match("%d") do
                number_str = number_str .. self:current_char()
                self:advance()
            end
        end
    end

    return create_token(TOKEN_TYPES.NUMBER, tonumber(number_str), start_line, start_column)
end

--- Reads a string token (including long strings)
---@return table the String token
function Lexer:read_string()
    local start_line, start_column = self.line, self.column
    local quote = self:current_char()

    -- Handle long strings [[...]]
    if quote == '[' then
        local equals = ""
        local pos = self.position + 1

        -- Count equals signs [===[...]==]
        while pos <= #self.source and self.source:sub(pos, pos) == '=' do
            equals = equals .. '='
            pos = pos + 1
        end

        if pos <= #self.source and self.source:sub(pos, pos) == '[' then
            -- This is a long string
            self.position = pos + 1
            self.column = self.column + #equals + 2

            local string_value = ""
            local close_pattern = ']' .. equals .. ']'

            while self.position <= #self.source do
                local remaining = self.source:sub(self.position)
                if remaining:sub(1, #close_pattern) == close_pattern then
                    self.position = self.position + #close_pattern
                    self.column = self.column + #close_pattern
                    break
                end

                string_value = string_value .. self:current_char()
                self:advance()
            end

            return create_token(TOKEN_TYPES.STRING, string_value, start_line, start_column)
        end
    end

    -- Regular string
    self:advance() -- Skip opening quote

    local string_value = ""
    while self:current_char() and self:current_char() ~= quote do
        if self:current_char() == '\\' then
            self:advance()
            local escape_char = self:current_char()
            if escape_char == 'n' then
                string_value = string_value .. '\n'
            elseif escape_char == 't' then
                string_value = string_value .. '\t'
            elseif escape_char == 'r' then
                string_value = string_value .. '\r'
            elseif escape_char == '\\' then
                string_value = string_value .. "\\"
            elseif escape_char == quote then
                string_value = string_value .. quote
            elseif escape_char == '0' then
                string_value = string_value .. '\0'
            else
                string_value = string_value .. escape_char
            end
        else
            string_value = string_value .. self:current_char()
        end
        self:advance()
    end

    if self:current_char() == quote then
        self:advance() -- Skip closing quote
    end

    return create_token(TOKEN_TYPES.STRING, string_value, start_line, start_column)
end

--- Reads an identifier or keyword token
---@return table the Identifier keyword token
function Lexer:read_identifier()
    local start_line, start_column = self.line, self.column
    local identifier = ""

    while self:current_char() and (self:current_char():match("[%w_]")) do
        identifier = identifier .. self:current_char()
        self:advance()
    end

    local token_type = KEYWORDS[identifier] and TOKEN_TYPES.KEYWORD or TOKEN_TYPES.IDENTIFIER

    -- Handle special keyword tokens
    if identifier == "true" or identifier == "false" then
        return create_token(TOKEN_TYPES.BOOLEAN, identifier == "true", start_line, start_column)
    elseif identifier == "nil" then
        return create_token(TOKEN_TYPES.NIL, nil, start_line, start_column)
    end

    return create_token(token_type, identifier, start_line, start_column)
end

--- Tokenizes the source code
---@return table Array of tokens
function Lexer:tokenize()
    self.tokens = {}

    while self:current_char() do
        local char = self:current_char()

        if char and char:match("%s") then
            if char == '\n' then
                table.insert(self.tokens, create_token(TOKEN_TYPES.NEWLINE, char, self.line, self.column))
            end
            self:advance()
            self:skip_whitespace()
        elseif char == '-' then
            -- Check if it's a comment (--)
            if self:peek_char() == '-' then
                self:skip_comment()
            else
                table.insert(self.tokens, create_token(TOKEN_TYPES.MINUS, char, self.line, self.column))
                self:advance()
            end
        elseif char and char:match("%d") then
            table.insert(self.tokens, self:read_number())
        elseif char == '"' or char == "'" then
            table.insert(self.tokens, self:read_string())
        elseif char and char:match("[%a_]") then
            table.insert(self.tokens, self:read_identifier())
        elseif char == '+' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.PLUS, char, self.line, self.column))
            self:advance()
        elseif char == '*' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.MULTIPLY, char, self.line, self.column))
            self:advance()
        elseif char == '/' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.DIVIDE, char, self.line, self.column))
            self:advance()
        elseif char == '%' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.MODULO, char, self.line, self.column))
            self:advance()
        elseif char == '^' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.POWER, char, self.line, self.column))
            self:advance()
        elseif char == '.' then
            if self:peek_char() == '.' then
                if self:peek_char(2) == '.' then
                    -- Varargs ...
                    table.insert(self.tokens, create_token(TOKEN_TYPES.VARARGS, "...", self.line, self.column))
                    self:advance()
                    self:advance()
                    self:advance()
                else
                    -- Concatenation ..
                    table.insert(self.tokens, create_token(TOKEN_TYPES.CONCAT, "..", self.line, self.column))
                    self:advance()
                    self:advance()
                end
            else
                table.insert(self.tokens, create_token(TOKEN_TYPES.DOT, char, self.line, self.column))
                self:advance()
                --- Handle dot followed by a number (e.g., .5)
            end
        elseif char == ':' then
            if self:peek_char() == ':' then
                table.insert(self.tokens, create_token(TOKEN_TYPES.DOUBLE_COLON, "::", self.line, self.column))
                self:advance()
                self:advance()
            else
                table.insert(self.tokens, create_token(TOKEN_TYPES.COLON, char, self.line, self.column))
                self:advance()
            end
        elseif char == '=' then
            if self:peek_char() == '=' then
                table.insert(self.tokens, create_token(TOKEN_TYPES.EQUAL, "==", self.line, self.column))
                self:advance()
                self:advance()
            else
                table.insert(self.tokens, create_token(TOKEN_TYPES.ASSIGN, char, self.line, self.column))
                self:advance()
            end
        elseif char == '~' then
            if self:peek_char() == '=' then
                table.insert(self.tokens, create_token(TOKEN_TYPES.NOT_EQUAL, "~=", self.line, self.column))
                self:advance()
                self:advance()
            else
                error("Unexpected character: " .. char .. " at line " .. self.line .. ", column " .. self.column)
            end
        elseif char == '<' then
            if self:peek_char() == '=' then
                table.insert(self.tokens, create_token(TOKEN_TYPES.LESS_EQUAL, "<=", self.line, self.column))
                self:advance()
                self:advance()
            else
                table.insert(self.tokens, create_token(TOKEN_TYPES.LESS_THAN, char, self.line, self.column))
                self:advance()
            end
        elseif char == '>' then
            if self:peek_char() == '=' then
                table.insert(self.tokens, create_token(TOKEN_TYPES.GREATER_EQUAL, ">=", self.line, self.column))
                self:advance()
                self:advance()
            else
                table.insert(self.tokens, create_token(TOKEN_TYPES.GREATER_THAN, char, self.line, self.column))
                self:advance()
            end
        elseif char == '(' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.LEFT_PAREN, char, self.line, self.column))
            self:advance()
        elseif char == ')' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.RIGHT_PAREN, char, self.line, self.column))
            self:advance()
        elseif char == '{' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.LEFT_BRACE, char, self.line, self.column))
            self:advance()
        elseif char == '}' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.RIGHT_BRACE, char, self.line, self.column))
            self:advance()
        elseif char == '[' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.LEFT_BRACKET, char, self.line, self.column))
            self:advance()
        elseif char == ']' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.RIGHT_BRACKET, char, self.line, self.column))
            self:advance()
        elseif char == ';' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.SEMICOLON, char, self.line, self.column))
            self:advance()
        elseif char == '#' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.LENGTH, char, self.line, self.column))
            self:advance()
        elseif char == ',' then
            table.insert(self.tokens, create_token(TOKEN_TYPES.COMMA, char, self.line, self.column))
            self:advance()
        else
            error("Unexpected character: " .. char .. " at line " .. self.line .. ", column " .. self.column)
        end
    end

    table.insert(self.tokens, create_token(TOKEN_TYPES.EOF, nil, self.line, self.column))
    return self.tokens
end

-- Export token types and lexer
Lexer.TOKEN_TYPES = TOKEN_TYPES
return Lexer

--- TODO: Add unit tests for the lexer
