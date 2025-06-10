-- verdict-lua/spec/parser_spec.lua
describe("Parser", function()
    local Parser = require("verdict.stuff.parser")
    local Lexer = require("verdict.stuff.lexer")

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
        assert.is_true(#result.ast > 0, "AST should have at least one node")

        local found_local_assignment = false
        for _, node in ipairs(result.ast) do
            if node.type == "local_assignment" then
                found_local_assignment = true
                -- Basic check that the node has the expected structure
                assert.is_table(node.data)
                assert.is_table(node.data.names)
                assert.is_table(node.data.exprs)
            end
        end

        assert.is_true(found_local_assignment, "Should find local assignment node")
    end)
end)
