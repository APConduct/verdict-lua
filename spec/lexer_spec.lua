describe("Lexer", function()
    local Lexer = require("verdict.stuff.lexer")

    it("should tokenize a simple variable declaration", function()
        local source = "local x = 42"
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()

        assert.is_table(tokens)
        assert.is_not_nil(tokens)

        -- Find the relevant tokens
        local found_local = false
        local found_x = false
        local found_assign = false
        local found_number = false

        for _, token in ipairs(tokens) do
            if token.type == "KEYWORD" and token.value == "local" then
                found_local = true
            elseif token.type == "IDENTIFIER" and token.value == "x" then
                found_x = true
            elseif token.type == "ASSIGN" then
                found_assign = true
            elseif token.type == "NUMBER" and token.value == 42 then
                found_number = true
            end
        end

        assert.is_true(found_local, "Should find 'local' keyword")
        assert.is_true(found_x, "Should find 'x' identifier")
        assert.is_true(found_assign, "Should find assignment operator")
        assert.is_true(found_number, "Should find number 42")
    end)

    it("should tokenize string literals", function()
        local source = 'local str = "hello world"'
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()

        local found_string = false
        for _, token in ipairs(tokens) do
            if token.type == "STRING" and token.value == "hello world" then
                found_string = true
            end
        end

        assert.is_true(found_string, "Should find string literal")
    end)
end)
