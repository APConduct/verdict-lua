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
        current_token = tokens[1],
        errors = {}
    }, Parser)
end

--- Records a parse error but continues parsing
---@param message string Error message
function Parser:error(message)
    table.insert(self.errors, {
        message = message,
        line = self.current_token.line,
        column = self.current_token.column
    })
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
    if self.current_token.type == expected_type then
        local token = self.current_token
        self:advance()
        return token
    else
        self:error("Expected " .. expected_type .. " but got " .. self.current_token.type)
        return self.current_token -- Return current token and continue
    end
end

--- Skip newline tokens
function Parser:skip_newlines()
    while self.current_token and
        (self.current_token.type == "NEWLINE" or
            (self.current_token.type == "KEYWORD" and self.current_token.value == "then") or
            (self.current_token.type == "KEYWORD" and self.current_token.value == "do")) do
        if self.current_token.type == "NEWLINE" then
            self:advance()
        else
            break -- Keep keywords 'then' and 'do' for processing
        end
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

    -- Return both AST and any parse errors
    return {
        ast = statements,
        errors = self.errors
    }
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
        elseif self.current_token.value == "repeat" then
            return self:parse_repeat_statement()
        elseif self.current_token.value == "return" then
            return self:parse_return_statement()
        elseif self.current_token.value == "break" then
            self:advance()
            return create_node("break_statement", {}, self.current_token.line, self.current_token.column)
        elseif self.current_token.value == "goto" then
            return self:parse_goto_statement()
        elseif self.current_token.value == "do" then
            return self:parse_do_block()
        end
    elseif self.current_token.type == "DOUBLE_COLON" then
        return self:parse_label()
    elseif self.current_token.type == "IDENTIFIER" then
        -- Check if it's an assignment or function call
        if self:peek_token().type == "ASSIGN" or
            self:peek_token().type == "DOT" or
            self:peek_token().type == "LEFT_BRACKET" then
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
    if expr then
        return create_node("expression_statement", { expr = expr },
            self.current_token.line, self.current_token.column)
    end

    return nil
end

--- Parses a label ::name::
function Parser:parse_label()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("DOUBLE_COLON")
    local name = self:consume("IDENTIFIER")
    self:consume("DOUBLE_COLON")

    return create_node("label", { name = name.value }, line, column)
end

--- Parses a goto statement
function Parser:parse_goto_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- goto
    local label = self:consume("IDENTIFIER")

    return create_node("goto_statement", { label = label.value }, line, column)
end

--- Parses a do block
function Parser:parse_do_block()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- do
    self:skip_newlines()

    local body = {}
    while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
        if self.current_token.type == "EOF" then
            self:error("Expected 'end' to close 'do' block")
            break
        end
        local stmt = self:parse_statement()
        if stmt then
            table.insert(body, stmt)
        end
        self:skip_newlines()
    end

    self:consume("KEYWORD") -- end

    return create_node("do_block", { body = body }, line, column)
end

--- Parses a repeat statement
function Parser:parse_repeat_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- repeat
    self:skip_newlines()

    local body = {}
    while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "until" do
        if self.current_token.type == "EOF" then
            self:error("Expected 'until' to close 'repeat' block")
            break
        end
        local stmt = self:parse_statement()
        if stmt then
            table.insert(body, stmt)
        end
        self:skip_newlines()
    end

    self:consume("KEYWORD") -- until
    local condition = self:parse_expression()

    return create_node("repeat_statement", {
        body = body,
        condition = condition
    }, line, column)
end

--- Parses a local statement
---@return table The AST node for the local statement
function Parser:parse_local_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'local'

    -- Check if it's a local function declaration
    if self.current_token.type == "KEYWORD" and self.current_token.value == "function" then
        self:consume("KEYWORD") -- consume 'function'

        local name_token = self:consume("IDENTIFIER")
        self:consume("LEFT_PAREN")

        local params = {}
        local has_varargs = false

        while self.current_token.type == "IDENTIFIER" or self.current_token.type == "VARARGS" do
            if self.current_token.type == "VARARGS" then
                has_varargs = true
                self:advance()
                break
            else
                table.insert(params, self.current_token.value)
                self:advance()
                if self.current_token.type == "COMMA" then
                    self:advance()
                end
            end
        end

        self:consume("RIGHT_PAREN")
        self:skip_newlines()

        local body = {}
        while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
            if self.current_token.type == "EOF" then
                self:error("Expected 'end' to close function")
                break
            end
            local stmt = self:parse_statement()
            if stmt then
                table.insert(body, stmt)
            end
            self:skip_newlines()
        end

        self:consume("KEYWORD") -- consume 'end'

        return create_node("local_function_def", {
            name = name_token.value,
            params = params,
            has_varargs = has_varargs,
            body = body
        }, line, column)
    else
        -- Regular local variable declaration/assignment
        local names = {}

        repeat
            local name_token = self:consume("IDENTIFIER")
            table.insert(names, name_token.value)

            if self.current_token.type == "COMMA" then
                self:advance()
            else
                break
            end
        until false

        local exprs = {}
        if self.current_token.type == "ASSIGN" then
            self:consume("ASSIGN")

            repeat
                table.insert(exprs, self:parse_expression())

                if self.current_token.type == "COMMA" then
                    self:advance()
                else
                    break
                end
            until false
        end

        return create_node("local_assignment", {
            names = names,
            exprs = exprs
        }, line, column)
    end
end

--- Parses an assignment statement
---@return table The AST node for the assignment
function Parser:parse_assignment()
    local line, column = self.current_token.line, self.current_token.column
    local targets = {}

    -- Parse left-hand side (can be multiple targets)
    repeat
        local target = self:parse_primary_expression()
        table.insert(targets, target)

        self:skip_newlines()
        if self.current_token.type == "COMMA" then
            self:advance()
            self:skip_newlines()
        else
            break
        end
    until false

    self:skip_newlines()
    if self.current_token.type == "ASSIGN" then
        self:advance()
        self:skip_newlines()

        -- Parse right-hand side expressions
        local exprs = {}
        repeat
            table.insert(exprs, self:parse_expression())
            self:skip_newlines()

            if self.current_token.type == "COMMA" then
                self:advance()
                self:skip_newlines()
            else
                break
            end
        until false

        return create_node("assignment", {
            targets = targets,
            exprs = exprs
        }, line, column)
    else
        -- This was just a primary expression, not an assignment
        if #targets == 1 then
            return create_node("expression_statement", {
                expr = targets[1]
            }, line, column)
        else
            self:error("Expected '=' after variable list")
            return create_node("error", {}, line, column)
        end
    end
end

--- Parses a function definition
---@return table The AST node for the function definition
function Parser:parse_function_definition()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'function'

    -- Parse function name (can be dotted like foo.bar.baz)
    local name_parts = {}
    table.insert(name_parts, self:consume("IDENTIFIER").value)

    while self.current_token.type == "DOT" do
        self:advance()
        table.insert(name_parts, self:consume("IDENTIFIER").value)
    end

    -- Check for method definition (colon)
    local is_method = false
    if self.current_token.type == "COLON" then
        self:advance()
        table.insert(name_parts, self:consume("IDENTIFIER").value)
        is_method = true
    end

    self:consume("LEFT_PAREN")

    local params = {}
    local has_varargs = false

    -- If it's a method, add 'self' as the first parameter
    if is_method then
        table.insert(params, "self")
    end

    while self.current_token.type == "IDENTIFIER" or self.current_token.type == "VARARGS" do
        if self.current_token.type == "VARARGS" then
            has_varargs = true
            self:advance()
            break
        else
            table.insert(params, self.current_token.value)
            self:advance()
            if self.current_token.type == "COMMA" then
                self:advance()
            end
        end
    end

    self:consume("RIGHT_PAREN")
    self:skip_newlines()

    local body = {}
    while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
        if self.current_token.type == "EOF" then
            self:error("Expected 'end' to close function")
            break
        end
        local stmt = self:parse_statement()
        if stmt then
            table.insert(body, stmt)
        end
        self:skip_newlines()
    end

    self:consume("KEYWORD") -- consume 'end'

    return create_node("function_def", {
        name_parts = name_parts,
        is_method = is_method,
        params = params,
        has_varargs = has_varargs,
        body = body
    }, line, column)
end

--- Parses a while statement
---@return table The AST node for the while statement
function Parser:parse_while_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'while'

    local condition = self:parse_expression()
    self:consume("KEYWORD") -- consume 'do'
    self:skip_newlines()

    local body = {}
    while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
        if self.current_token.type == "EOF" then
            self:error("Expected 'end' to close while loop")
            break
        end
        local stmt = self:parse_statement()
        if stmt then
            table.insert(body, stmt)
        end
        self:skip_newlines()
    end

    self:consume("KEYWORD") -- consume 'end'

    return create_node("while_statement", {
        condition = condition,
        body = body
    }, line, column)
end

--- Parses a for statement
---@return table The AST node for the for statement
function Parser:parse_for_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'for'

    local var_token = self:consume("IDENTIFIER")

    -- Check if it's a numeric for loop (for i = 1, 10 do) or generic for loop (for k, v in pairs(t) do)
    if self.current_token.type == "ASSIGN" then
        -- Numeric for loop: for i = start, end [, step] do
        self:consume("ASSIGN")
        local start_expr = self:parse_expression()
        self:consume("COMMA")
        local end_expr = self:parse_expression()

        local step_expr = nil
        if self.current_token.type == "COMMA" then
            self:advance()
            step_expr = self:parse_expression()
        end

        self:consume("KEYWORD") -- consume 'do'
        self:skip_newlines()

        local body = {}
        while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
            if self.current_token.type == "EOF" then
                self:error("Expected 'end' to close for loop")
                break
            end
            local stmt = self:parse_statement()
            if stmt then
                table.insert(body, stmt)
            end
            self:skip_newlines()
        end

        self:consume("KEYWORD") -- consume 'end'

        return create_node("numeric_for_statement", {
            var = var_token.value,
            start = start_expr,
            finish = end_expr,
            step = step_expr,
            body = body
        }, line, column)
    else
        -- Generic for loop: for k, v in iterator do
        local vars = { var_token.value }

        while self.current_token.type == "COMMA" do
            self:advance()
            table.insert(vars, self:consume("IDENTIFIER").value)
        end

        self:consume("KEYWORD") -- consume 'in'

        local iterators = {}
        repeat
            table.insert(iterators, self:parse_expression())
            if self.current_token.type == "COMMA" then
                self:advance()
            else
                break
            end
        until false

        self:consume("KEYWORD") -- consume 'do'
        self:skip_newlines()

        local body = {}
        while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
            if self.current_token.type == "EOF" then
                self:error("Expected 'end' to close for loop")
                break
            end
            local stmt = self:parse_statement()
            if stmt then
                table.insert(body, stmt)
            end
            self:skip_newlines()
        end

        self:consume("KEYWORD") -- consume 'end'

        return create_node("generic_for_statement", {
            vars = vars,
            iterators = iterators,
            body = body
        }, line, column)
    end
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
        if self.current_token.type == "EOF" then
            self:error("Expected 'end', 'else', or 'elseif'")
            break
        end
        local stmt = self:parse_statement()
        if stmt then
            table.insert(then_block, stmt)
        end
        self:skip_newlines()
    end

    local elseif_blocks = {}
    while self.current_token.type == "KEYWORD" and self.current_token.value == "elseif" do
        self:advance()          -- consume 'elseif'
        local elseif_condition = self:parse_expression()
        self:consume("KEYWORD") -- consume 'then'
        self:skip_newlines()

        local elseif_body = {}
        while self.current_token.type ~= "KEYWORD" or
            (self.current_token.value ~= "end" and
                self.current_token.value ~= "else" and
                self.current_token.value ~= "elseif") do
            if self.current_token.type == "EOF" then
                self:error("Expected 'end', 'else', or 'elseif'")
                break
            end
            local stmt = self:parse_statement()
            if stmt then
                table.insert(elseif_body, stmt)
            end
            self:skip_newlines()
        end

        table.insert(elseif_blocks, {
            condition = elseif_condition,
            body = elseif_body
        })
    end

    local else_block = nil
    if self.current_token.type == "KEYWORD" and self.current_token.value == "else" then
        self:advance() -- consume 'else'
        self:skip_newlines()
        else_block = {}
        while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
            if self.current_token.type == "EOF" then
                self:error("Expected 'end' to close if statement")
                break
            end
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
        elseif_blocks = elseif_blocks,
        else_block = else_block
    }, line, column)
end

--- Parses a return statement
---@return table The AST node for the return statement
function Parser:parse_return_statement()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- consume 'return'

    local exprs = {}
    if self.current_token.type ~= "NEWLINE" and
        self.current_token.type ~= "EOF" and
        self.current_token.type ~= "KEYWORD" then
        repeat
            table.insert(exprs, self:parse_expression())
            if self.current_token.type == "COMMA" then
                self:advance()
            else
                break
            end
        until false
    end

    return create_node("return_statement", {
        exprs = exprs
    }, line, column)
end

--- Parses an expression
---@return table The AST node for the expression
function Parser:parse_expression()
    -- Skip any newlines before starting expression parsing
    self:skip_newlines()

    if self.current_token.type == "NEWLINE" or self.current_token.type == "EOF" then
        self:error("Unexpected token in expression: " .. self.current_token.type)
        return create_node("error", {}, self.current_token.line, self.current_token.column)
    end

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

    -- Right-associative
    if self.current_token.type == "CONCAT" then
        local op_token = self.current_token
        self:advance()
        local right = self:parse_concatenation_expression() -- Recursive for right-associativity
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
        (self.current_token.type == "KEYWORD" and self.current_token.value == "not") or
        self.current_token.type == "LENGTH" then -- Handle # operator
        local op_token = self.current_token
        self:advance()
        local expr = self:parse_unary_expression()
        return create_node("unary_op", {
            expr = expr,
            op = op_token.value or "#" -- Use # for LENGTH tokens
        }, op_token.line, op_token.column)
    else
        return self:parse_power_expression()
    end
end

--- Parses a power expression (right-associative)
---@return table The AST node for the expression
function Parser:parse_power_expression()
    local left = self:parse_primary_expression()

    if self.current_token.type == "POWER" then
        local op_token = self.current_token
        self:advance()
        local right = self:parse_unary_expression() -- Right-associative
        left = create_node("binary_op", {
            left = left,
            right = right,
            op = op_token.value
        }, op_token.line, op_token.column)
    end

    return left
end

--- Parses a primary expression with field access and calls
---@return table|nil The AST node for the expression
function Parser:parse_primary_expression()
    local node = self:parse_base_primary()

    if not node then
        return nil
    end

    -- Handle postfix operations (field access, indexing, function calls)
    while true do
        self:skip_newlines()
        if self.current_token.type == "DOT" then
            local line, column = self.current_token.line, self.current_token.column
            self:advance()
            local field = self:consume("IDENTIFIER")
            node = create_node("field_access", {
                object = node,
                field = field.value
            }, line, column)
        elseif self.current_token.type == "LEFT_BRACKET" then
            local line, column = self.current_token.line, self.current_token.column
            self:advance()
            local index = self:parse_expression()
            self:consume("RIGHT_BRACKET")
            node = create_node("index_access", {
                object = node,
                index = index
            }, line, column)
        elseif self.current_token.type == "LEFT_PAREN" then
            local line, column = self.current_token.line, self.current_token.column
            self:advance()
            local args = {}

            while self.current_token.type ~= "RIGHT_PAREN" do
                if self.current_token.type == "EOF" then
                    self:error("Expected ')' to close function call")
                    break
                end
                table.insert(args, self:parse_expression())
                if self.current_token.type == "COMMA" then
                    self:advance()
                elseif self.current_token.type ~= "RIGHT_PAREN" then
                    self:error("Expected ',' or ')' in function call")
                    break
                end
            end

            self:consume("RIGHT_PAREN")
            node = create_node("function_call", {
                func = node,
                args = args
            }, line, column)
        elseif self.current_token.type == "COLON" then
            -- Method call syntax obj:method(args)
            local line, column = self.current_token.line, self.current_token.column
            self:advance()
            local method = self:consume("IDENTIFIER")

            if self.current_token.type == "LEFT_PAREN" then
                self:advance()
                local args = {}

                while self.current_token.type ~= "RIGHT_PAREN" do
                    if self.current_token.type == "EOF" then
                        self:error("Expected ')' to close method call")
                        break
                    end
                    table.insert(args, self:parse_expression())
                    if self.current_token.type == "COMMA" then
                        self:advance()
                    elseif self.current_token.type ~= "RIGHT_PAREN" then
                        self:error("Expected ',' or ')' in method call")
                        break
                    end
                end

                self:consume("RIGHT_PAREN")
                node = create_node("method_call", {
                    object = node,
                    method = method.value,
                    args = args
                }, line, column)
            else
                -- obj:method without parentheses (single argument)
                node = create_node("method_call", {
                    object = node,
                    method = method.value,
                    args = {}
                }, line, column)
            end
        else
            break
        end
    end

    return node
end

--- Parses base primary expressions
---@return table The AST node for the expression
function Parser:parse_base_primary()
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
    elseif token.type == "VARARGS" then
        self:advance()
        return create_node("varargs", {}, token.line, token.column)
    elseif token.type == "IDENTIFIER" then
        self:advance()
        return create_node("identifier", {
            name = token.value
        }, token.line, token.column)
    elseif token.type == "LEFT_PAREN" then
        self:advance() -- consume '('
        local expr = self:parse_expression()
        self:consume("RIGHT_PAREN")
        return expr
    elseif token.type == "LEFT_BRACE" then
        return self:parse_table_constructor()
    elseif token.type == "KEYWORD" and token.value == "function" then
        return self:parse_function_expression()
    else
        self:error("Unexpected token in expression: " .. token.type)
        self:advance() -- Skip the problematic token
        return create_node("error", {}, token.line, token.column)
    end
end

--- Parses a function expression
---@return table The AST node for the function expression
function Parser:parse_function_expression()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("KEYWORD") -- function
    self:consume("LEFT_PAREN")

    local params = {}
    local has_varargs = false

    while self.current_token.type == "IDENTIFIER" or self.current_token.type == "VARARGS" do
        if self.current_token.type == "VARARGS" then
            has_varargs = true
            self:advance()
            break
        else
            table.insert(params, self.current_token.value)
            self:advance()
            if self.current_token.type == "COMMA" then
                self:advance()
            end
        end
    end

    self:consume("RIGHT_PAREN")
    self:skip_newlines()

    local body = {}
    while self.current_token.type ~= "KEYWORD" or self.current_token.value ~= "end" do
        if self.current_token.type == "EOF" then
            self:error("Expected 'end' to close function expression")
            break
        end
        local stmt = self:parse_statement()
        if stmt then
            table.insert(body, stmt)
        end
        self:skip_newlines()
    end

    self:consume("KEYWORD") -- end

    return create_node("function_expression", {
        params = params,
        has_varargs = has_varargs,
        body = body
    }, line, column)
end

--- Parses a table constructor
---@return table The AST node for the table constructor
function Parser:parse_table_constructor()
    local line, column = self.current_token.line, self.current_token.column
    self:consume("LEFT_BRACE")
    self:skip_newlines() -- Skip newlines after opening brace

    local fields = {}

    -- Empty table case
    if self.current_token.type == "RIGHT_BRACE" then
        self:advance()
        return create_node("table_constructor", {
            fields = fields
        }, line, column)
    end

    while true do
        -- Skip any newlines before field parsing
        self:skip_newlines()

        -- Check if we've reached the end of the table
        if self.current_token.type == "RIGHT_BRACE" then
            self:advance() -- Consume the closing brace
            break
        end

        if self.current_token.type == "EOF" then
            self:error("Expected '}' to close table constructor")
            break
        end

        local field = nil

        -- Check for [expr] = expr syntax
        if self.current_token.type == "LEFT_BRACKET" then
            self:advance()
            local key = self:parse_expression()
            self:consume("RIGHT_BRACKET")
            self:consume("ASSIGN")
            local value = self:parse_expression()
            field = create_node("table_field", {
                type = "indexed",
                key = key,
                value = value
            }, line, column)
            -- Check for name = expr syntax
        elseif self.current_token.type == "IDENTIFIER" and self:peek_token().type == "ASSIGN" then
            local key = self.current_token.value
            self:advance()
            self:consume("ASSIGN")
            local value = self:parse_expression()
            field = create_node("table_field", {
                type = "named",
                key = key,
                value = value
            }, line, column)
        else
            -- Just an expression (array-style)
            local value = self:parse_expression()
            field = create_node("table_field", {
                type = "array",
                value = value
            }, line, column)
        end

        table.insert(fields, field)

        -- Handle field separators (comma or semicolon)
        self:skip_newlines() -- Skip newlines before looking for separator
        if self.current_token.type == "COMMA" or self.current_token.type == "SEMICOLON" then
            self:advance()
            self:skip_newlines() -- Skip newlines after separator
        elseif self.current_token.type ~= "RIGHT_BRACE" then
            -- If not a separator and not the end, report error but continue
            self:error("Expected ',', ';', or '}' in table constructor")
            -- Try to advance to recover
            if self.current_token.type ~= "EOF" then
                self:advance()
            else
                break
            end
        end
    end

    return create_node("table_constructor", {
        fields = fields
    }, line, column)
end

return Parser
