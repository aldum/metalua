local describe = require("busted").describe
local assert = require("busted").assert
local it = require("busted").it

require("stringutils")

local inputs = require("spec.ast_inputs")
local mlc = require('metalua.compiler').new()

local w = 64

local show_code = os.getenv("SHOW_CODE")

local parse_prot = function(code)
  local c = string.unlines(code)
  return pcall(mlc.src_to_ast, mlc, c)
end

describe('ast_to_src #ast', function()
  local function do_code(ast, seen_comments)
    local code, comments = mlc:ast_to_src(ast, seen_comments, w)
    local seen = seen_comments or {}
    for k, v in pairs(comments or {}) do
      --- if a table was passed in, this modifies it
      seen[k] = v
    end
    if show_code then
      print(string.quote(code))
    end

    return code, seen_comments
  end

  describe('produces ASTs', function()
    for _, test_t in pairs(inputs) do
      local tag = test_t[1]
      local tests = test_t[2]
      describe('for ' .. tag, function()
        for i, tc in ipairs(tests) do
          local input = tc[1]
          local output = tc[2]

          local ok, r = parse_prot(input)
          local result = {}
          if ok then
            local has_lines = false
            local seen_comments = {}
            for _, v in ipairs(r) do
              has_lines = true
              local ct, _ = do_code(v, seen_comments)
              for _, cl in ipairs(string.lines(ct) or {}) do
                table.insert(result, cl)
              end
            end
            --- corner case, e.g comments only
            --- it is valid code, but gets parsed a bit differently
            if not has_lines then
              result = string.lines(do_code(r)) or {}
            end

            --- remove trailing newline
            if result[#result] == '' then
              table.remove(result)
            end
            it('matches ' .. i, function()
              assert.same(output, result)
              assert.is_true(parse_prot(output))
            end)
          else
            print('syntax error in input #' .. i)
            print(r)
          end
        end
      end)
    end
  end)
end)
