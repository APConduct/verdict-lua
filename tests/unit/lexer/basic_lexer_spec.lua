describe("Lexer", function()
    local Lexer = require("verdict.stuff.lexer")
    local helper = require("tests.helpers.test_helper")

    it("should tokenize a simple variable declaration", function()
        local source = "local x = 42"
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()

        assert.is_table(tokens)
        assert.equal("KEYWORD", tokens[1].type)
        assert.equal("local", tokens[1].value)
        assert.equal("IDENTIFIER", tokens[2].type)
        assert.equal("x", tokens[2].value)
        assert.equal("ASSIGN", tokens[3].type)
        assert.equal("NUMBER", tokens[4].type)
        assert.equal(42, tokens[4].value)
    end)

    it("should tokenize string literals", function()
        local source = 'local str = "hello world"'
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()

        assert.equal("STRING", tokens[4].type)
        assert.equal("hello world", tokens[4].value)
    end)

    it("should tokenize operators", function()
        local source = "local result = a + b * c / d"
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()

        -- Find the operators in the token stream
        local operators = {}
        for i, token in ipairs(tokens) do
            if token.type == "PLUS" or token.type == "MULTIPLY" or token.type == "DIVIDE" then
                table.insert(operators, token.type)
            end
        end

        assert.same({ "PLUS", "MULTIPLY", "DIVIDE" }, operators)
    end)

    it("should handle complex expressions", function()
        local source = "local x = (a + b) * (c - d) / 2.5"
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()

        -- Verify we have the right number of tokens and they're the right types
        local expected_types = {
            "KEYWORD", "IDENTIFIER", "ASSIGN", "LEFT_PAREN", "IDENTIFIER",
            "PLUS", "IDENTIFIER", "RIGHT_PAREN", "MULTIPLY", "LEFT_PAREN",
            "IDENTIFIER", "MINUS", "IDENTIFIER", "RIGHT_PAREN", "DIVIDE", "NUMBER", "EOF"
        }

        local actual_types = {}
        for i, token in ipairs(tokens) do
            table.insert(actual_types, token.type)
        end

        assert.same(expected_types, actual_types)
    end)

    it("should report line and column information", function()
        local source = "local x = 10\nlocal y = 20"
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()

        assert.equal(1, tokens[1].line) -- 'local' on line 1
        assert.equal(1, tokens[1].column)

        -- Find the second 'local' token
        local second_local = nil
        for i, token in ipairs(tokens) do
            if token.type == "KEYWORD" and token.value == "local" and token.line == 2 then
                second_local = token
                break
            end
        end

        assert.is_not_nil(second_local)
        assert.equal(2, second_local.line)
        assert.equal(1, second_local.column)
    end)

    it("should handle comments", function()
        local source = "local x = 10 -- This is a comment\nlocal y = 20"
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()

        -- Check that the comment is skipped
        local has_comment = false
        for i, token in ipairs(tokens) do
            if token.type == "COMMENT" then
                has_comment = true
                break
            end
        end

        assert.is_false(has_comment)

        -- Check that we still get the tokens after the comment
        local found_y = false
        for i, token in ipairs(tokens) do
            if token.type == "IDENTIFIER" and token.value == "y" then
                found_y = true
                break
            end
        end

        assert.is_true(found_y)
    end)

    -- TODO: add more lexer tests for various token types and edge cases
end)
