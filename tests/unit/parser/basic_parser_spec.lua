describe("Parser", function()
    local Parser = require("verdict.stuff.parser")
    local Lexer = require("verdict.stuff.lexer")
    local helper = require("tests.helpers.test_helper")

    local function parse(source)
        local lexer = Lexer.new(source)
        local tokens = lexer:tokenize()
        local parser = Parser.new(tokens)
        return parser:parse()
    end

    it("should parse a simple variable declaration", function()
        local source = "local x = 42"
        local result = parse(source)

        assert.is_table(result)
        assert.is_table(result.ast)
        assert.equal(1, #result.ast)
        assert.equal("local_assignment", result.ast[1].type)
        assert.same({ "x" }, result.ast[1].data.names)
        assert.equal(1, #result.ast[1].data.exprs)
        assert.equal("literal", result.ast[1].data.exprs[1].type)
        assert.equal("number", result.ast[1].data.exprs[1].data.type)
        assert.equal(42, result.ast[1].data.exprs[1].data.value)
    end)

    it("should parse a function declaration", function()
        local source = "function add(a, b)\n  return a + b\nend"
        local result = parse(source)

        assert.is_table(result)
        assert.is_table(result.ast)
        assert.equal(1, #result.ast)
        assert.equal("function_def", result.ast[1].type)
        assert.same({ "add" }, result.ast[1].data.name_parts)
        assert.same({ "a", "b" }, result.ast[1].data.params)
        assert.equal(1, #result.ast[1].data.body)
        assert.equal("return_statement", result.ast[1].data.body[1].type)
    end)

    it("should parse control flow structures", function()
        local source = "if x > 10 then\n  print(x)\nelse\n  print('too small')\nend"
        local result = parse(source)

        assert.is_table(result)
        assert.is_table(result.ast)
        assert.equal(1, #result.ast)
        assert.equal("if_statement", result.ast[1].type)
        assert.is_table(result.ast[1].data.condition)
        assert.is_table(result.ast[1].data.then_block)
        assert.is_table(result.ast[1].data.else_block)
    end)

    it("should parse loop structures", function()
        local source = "for i = 1, 10 do\n  print(i)\nend"
        local result = parse(source)

        assert.is_table(result)
        assert.is_table(result.ast)
        assert.equal(1, #result.ast)
        assert.equal("numeric_for_statement", result.ast[1].type)
        assert.equal("i", result.ast[1].data.var)
        assert.is_table(result.ast[1].data.start)
        assert.is_table(result.ast[1].data.finish)
        assert.is_table(result.ast[1].data.body)
    end)

    it("should parse table constructors", function()
        local source = "local t = { a = 1, b = 2, [3] = 'three', 'four' }"
        local result = parse(source)

        assert.is_table(result)
        assert.is_table(result.ast)
        assert.equal(1, #result.ast)
        assert.equal("local_assignment", result.ast[1].type)
        assert.equal("table_constructor", result.ast[1].data.exprs[1].type)
        assert.equal(4, #result.ast[1].data.exprs[1].data.fields)
    end)

    it("should handle syntax errors gracefully", function()
        local source = "function bad_syntax(\n  return 'oops'\nend"
        local result = parse(source)

        assert.is_table(result)
        assert.is_table(result.errors)
        assert.is_true(#result.errors > 0)
        -- The parser should still return a partial AST even with errors
        assert.is_table(result.ast)
    end)

    -- TODO: add more parser tests for various constructs and edge cases
end)
