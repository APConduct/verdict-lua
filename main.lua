-- Set up the module path
package.path = "./src/?.lua;./src/?/init.lua;" .. package.path

-- Load your module
local verdict = require("verdict")
