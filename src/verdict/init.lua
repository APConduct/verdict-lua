local Parser = require("verdict.stuff.parser")
local Lexer = require("verdict.stuff.lexer")

local analyzer = require("verdict.analyzer")

local verdict = {}

verdict.Lexer = Lexer
verdict.Parser = Parser

verdict.analyzer = analyzer


print("verdict module loaded")

return verdict
