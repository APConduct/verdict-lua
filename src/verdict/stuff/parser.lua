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

--- Advances to the next token
function Parser:advance()
    self.position = self.position + 1
    if self.position <= #self.tokens then
        self.current_token = self.tokens[self.position]
    else
        self.current_token = { type = "EOF" }
    end
end

--- Peeks at the next token without advancing
---@param offset number|nil Optional offset (default 1)
---@return table The token at position + offset
function Parser:peek_token(offset)
    offset = offset or 1
    local pos = self.position + offset
    if pos <= #self.tokens then
        return self.tokens[pos]
    else
        return { type = "EOF" }
    end
end

--- Consumes a token of the expected type
---@param expected_type string The expected token type
---@return table The consumed token
function Parser:consume(expected_type)
    if self.current_token == expected_type then
        local token = self.current_token
        self:advance()
        return token
    else
        error("Expected " .. expected_type .. " but got " .. self.current_token.type ..
            " at line " .. (self.current_token.line or "?") ..
            ", column " .. (self.current_token.column or "?"))
    end
end

--- Skip newline tokens
function Parser:skip_newlines()
    while self.current_token.type == "NEWLINE" do
        self:advance()
    end
end

--- Parses the entire program
---@return table The AST for the program
function Parser:parse()
    local statements = {}

    self:skip_newlines()

    while self.current_token.type ~= "EOF" do
        local stmt = self:parse_statement()
        if stmt then
            table.insert(statements, stmt)
        end
        self:skip_newlines()
    end
    return statements
end

--- Parses a statement
---@return table|nil The AST node for the statement
function Parser:parse_statement()
    self:skip_newlines()

    if self.current_token.type == "EOF" then
        return nil
    end

    if self.current_token.type == "KEYWORD" then
        if self.current_token.value == "local" then
            return self:parse_local_statement()
        elseif self.current_token.value == "function" then
            return self:parse_function_definition()
        elseif self.current_token.value == "if" then
            return self:parse_if_statement()
        elseif self.current_token.value == "while" then
            return self:parse_while_statement()
        elseif self.current_token.value == "for" then
            return self:parse_for_statement()
        elseif self.current_token.value == "return" then
            return self:parse_return_statement()
        end
    elseif self.current_token.type == "IDENTIFIER" then
        -- Check if it's an assignment or function call
        if self:peek_token().type == "ASSIGN" then
            return self:parse_assignment()
        else
            -- Treat as expression statement (function call)
            local expr = self:parse_expression()
            return create_node("expression_statement", { expr = expr },
                self.current_token.line, self.current_token.column)
        end
    end

    -- If we can't parse a statement, try parsing as expression
    local expr = self:parse_expression()
    return create_node("expression_statement", { expr = expr },
        self.current_token.line, self.current_token.column)
end

--- Parses a local statement
---@return table The AST node for the local statement
function Parser:parse_local_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'local'

    local name_token = self:consume("IDENTIFIER")

    if self.current_token.type == "ASSIGN" then
        self:consume("ASSIGN")
        local expr = self:parse_expression()
        return create_node("local_assignment", {
            name = name_token.value,
            expr = expr
        }, line, column)
    else
        return create_node("local_declaration", {
            name = name_token.value
        }, line, column)
    end
end

--- Parses an assignment statement
---@return table The AST node for the assignment
function Parser:parse_assignment()
    local line, column = self.current_token.line, self.current_token.column
    local name_token = self:consume("IDENTIFIER")
    self:consume("ASSIGN")
    local expr = self:parse_expression()

    return create_node("assignment", {
        name = name_token.value,
        expr = expr
    }, line, column)
end

--- Parses a function definition
---@return table The AST node for the function definition
function Parser:parse_function_definition()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'function'

    local name_token = self:consume("IDENTIFIER")
    self:consume("LEFT_PAREN")

    local params = {}
    while self.current_token.type == "IDENTIFIER" do
        table.insert(params, self.current_token.value)
        self:advance()
        if self.current_token.type == "COMMA" then
            self:advance()
        end
    end

    self:consume("RIGHT_PAREN")
    self:skip_newlines()

    local body = {}
    while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
        local stmt = self:parse_statement()
        if stmt then
            table.insert(body, stmt)
        end
        self:skip_newlines()
    end

    self:consume("KEYWORD") -- consume 'end'

    return create_node("function_def", {
        name = name_token.value,
        params = params,
        body = body
    }, line, column)
end

--- Parses an if statement
---@return table The AST node for the if statement
function Parser:parse_if_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'if'

    local condition = self:parse_expression()
    self:consume("KEYWORD") -- consume 'then'
    self:skip_newlines()

    local then_block = {}
    while self.current_token.type ~= "KEYWORD" or
        (self.current_token.value ~= "end" and
            self.current_token.value ~= "else" and
            self.current_token.value ~= "elseif") do
        local stmt = self:parse_statement()
        if stmt then
            table.insert(then_block, stmt)
        end
        self:skip_newlines()
    end

    local else_block = nil
    if self.current_token.type == "KEYWORD" and self.current_token.value == "else" then
        self:advance() -- consume 'else'
        self:skip_newlines()
        else_block = {}
        while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
            local stmt = self:parse_statement()
            if stmt then
                table.insert(else_block, stmt)
            end
            self:skip_newlines()
        end
    end

    self:consume("KEYWORD") -- consume 'end'

    return create_node("if_statement", {
        condition = condition,
        then_block = then_block,
        else_block = else_block
    }, line, column)
end

--- Parses a return statement
---@return table The AST node for the return statement
function Parser:parse_return_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'return'

    local expr = nil
    if self.current_token.type ~= "NEWLINE" and self.current_token.type ~= "EOF" then
        expr = self:parse_expression()
    end

    return create_node("return_statement", {
        expr = expr
    }, line, column)
end

--- Parses an expression
---@return table The AST node for the expression
function Parser:parse_expression()
    return self:parse_or_expression()
end

--- Parses an OR expression
---@return table The AST node for the expression
function Parser:parse_or_expression()
    local left = self:parse_and_expression()

    while self.current_token.type == "KEYWORD" and self.current_token.value == "or" do
        local op_token = self.current_token
        self:advance()
        local right = self:parse_and_expression()
        left = create_node("binary_op", {
            left = left,
            right = right,
            op = op_token.value
        }, op_token.line, op_token.column)
    end

    return left
end

--- Parses an AND expression
---@return table The AST node for the expression
function Parser:parse_and_expression()
    local left = self:parse_equality_expression()

    while self.current_token.type == "KEYWORD" and self.current_token.value == "and" do
        local op_token = self.current_token
        self:advance()
        local right = self:parse_equality_expression()
        left = create_node("binary_op", {
            left = left,
            right = right,
            op = op_token.value
        }, op_token.line, op_token.column)
    end

    return left
end

--- Parses an equality expression
---@return table The AST node for the expression
function Parser:parse_equality_expression()
    local left = self:parse_relational_expression()

    while self.current_token.type == "EQUAL" or self.current_token.type == "NOT_EQUAL" do
        local op_token = self.current_token
        self:advance()
        local right = self:parse_relational_expression()
        left = create_node("binary_op", {
            left = left,
            right = right,
            op = op_token.value
        }, op_token.line, op_token.column)
    end

    return left
end

--- Parses a relational expression
---@return table The AST node for the expression
function Parser:parse_relational_expression()
    local left = self:parse_concatenation_expression()

    while self.current_token.type == "LESS_THAN" or
        self.current_token.type == "LESS_EQUAL" or
        self.current_token.type == "GREATER_THAN" or
        self.current_token.type == "GREATER_EQUAL" do
        local op_token = self.current_token
        self:advance()
        local right = self:parse_concatenation_expression()
        left = create_node("binary_op", {
            left = left,
            right = right,
            op = op_token.value
        }, op_token.line, op_token.column)
    end

    return left
end

--- Parses a concatenation expression
---@return table The AST node for the expression
function Parser:parse_concatenation_expression()
    local left = self:parse_additive_expression()

    while self.current_token.type == "CONCAT" do
        local op_token = self.current_token
        self:advance()
        local right = self:parse_additive_expression()
        left = create_node("binary_op", {
            left = left,
            right = right,
            op = op_token.value
        }, op_token.line, op_token.column)
    end

    return left
end

--- Parses an additive expression
---@return table The AST node for the expression
function Parser:parse_additive_expression()
    local left = self:parse_multiplicative_expression()

    while self.current_token.type == "PLUS" or self.current_token.type == "MINUS" do
        local op_token = self.current_token
        self:advance()
        local right = self:parse_multiplicative_expression()
        left = create_node("binary_op", {
            left = left,
            right = right,
            op = op_token.value
        }, op_token.line, op_token.column)
    end

    return left
end

--- Parses a multiplicative expression
---@return table The AST node for the expression
function Parser:parse_multiplicative_expression()
    local left = self:parse_unary_expression()

    while self.current_token.type == "MULTIPLY" or
        self.current_token.type == "DIVIDE" or
        self.current_token.type == "MODULO" do
        local op_token = self.current_token
        self:advance()
        local right = self:parse_unary_expression()
        left = create_node("binary_op", {
            left = left,
            right = right,
            op = op_token.value
        }, op_token.line, op_token.column)
    end

    return left
end

--- Parses a unary expression
---@return table The AST node for the expression
function Parser:parse_unary_expression()
    if self.current_token.type == "MINUS" or
        (self.current_token.type == "KEYWORD" and self.current_token.value == "not") then
        local op_token = self.current_token
        self:advance()
        local expr = self:parse_unary_expression()
        return create_node("unary_op", {
            expr = expr,
            op = op_token.value
        }, op_token.line, op_token.column)
    else
        return self:parse_primary_expression()
    end
end

--- Parses a primary expression
---@return table The AST node for the expression
function Parser:parse_primary_expression()
    local token = self.current_token

    if token.type == "NUMBER" then
        self:advance()
        return create_node("literal", {
            type = "number",
            value = token.value
        }, token.line, token.column)
    elseif token.type == "STRING" then
        self:advance()
        return create_node("literal", {
            type = "string",
            value = token.value
        }, token.line, token.column)
    elseif token.type == "BOOLEAN" then
        self:advance()
        return create_node("literal", {
            type = "boolean",
            value = token.value
        }, token.line, token.column)
    elseif token.type == "NIL" then
        self:advance()
        return create_node("literal", {
            type = "nil",
            value = nil
        }, token.line, token.column)
    elseif token.type == "IDENTIFIER" then
        self:advance()

        -- Check for function call
        if self.current_token.type == "LEFT_PAREN" then
            self:advance() -- consume '('
            local args = {}

            while self.current_token.type ~= "RIGHT_PAREN" do
                table.insert(args, self:parse_expression())
                if self.current_token.type == "COMMA" then
                    self:advance()
                end
            end

            self:consume("RIGHT_PAREN")

            return create_node("function_call", {
                func = create_node("identifier", { name = token.value }, token.line, token.column),
                args = args
            }, token.line, token.column)
        else
            return create_node("identifier", {
                name = token.value
            }, token.line, token.column)
        end
    elseif token.type == "LEFT_PAREN" then
        self:advance() -- consume '('
        local expr = self:parse_expression()
        self:consume("RIGHT_PAREN")
        return expr
    elseif token.type == "LEFT_BRACE" then
        return self:parse_table_constructor()
    else
        error("Unexpected token: " .. token.type .. " at line " .. (token.line or "?") ..
            ", column " .. (token.column or "?"))
    end
end

--- Parses a table constructor
---@return table The AST node for the table constructor
function Parser:parse_table_constructor()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("LEFT_BRACE")

    local fields = {}
    while self.current_token.type ~= "RIGHT_BRACE" do
        -- For now, just parse expressions as array elements
        table.insert(fields, self:parse_expression())
        if self.current_token.type == "COMMA" then
            self:advance()
        end
    end

    self:consume("RIGHT_BRACE")

    return create_node("table_constructor", {
        fields = fields
    }, line, column)
end

return Parser
