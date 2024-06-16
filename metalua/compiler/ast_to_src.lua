-------------------------------------------------------------------------------
-- Copyright (c) 2006-2013 Fabien Fleutot and others.
--
-- All rights reserved.
--
-- This program and the accompanying materials are made available
-- under the terms of the Eclipse Public License v1.0 which
-- accompanies this distribution, and is available at
-- http://www.eclipse.org/legal/epl-v10.html
--
-- This program and the accompanying materials are also made available
-- under the terms of the MIT public license which accompanies this
-- distribution, and is available at http://www.lua.org/license.html
--
-- Contributors:
--     Fabien Fleutot - API and implementation
--
-------------------------------------------------------------------------------

--- @class M
--- @field _acc table
--- @field current_indent integer
--- @field indent_step string
--- @field comment_ids table
--- @field wrap integer
local M = {}
M.__index = M

local pp = require("metalua.pprint")
require("stringutils")

-- Instantiate a new AST->source synthetizer
function M.new(seen_comments, w)
   local self = {
      -- Accumulates pieces of source as strings
      _acc = {},
      -- Current level of line indentation
      current_indent = 0,
      -- Indentation symbol, normally spaces or '\t'
      indent_step = "  ",
      -- Comments index accumulator
      comment_ids = seen_comments or {},
      -- wrap length
      wrap = w or 80
   }
   return setmetatable(self, M)
end

--------------------------------------------------------------------------------
-- Run a synthetizer on the `ast' arg and return the source as a string.
-- Can also be used as a static method `M.run (ast)'; in this case,
-- a temporary Metizer is instantiated on the fly.
--------------------------------------------------------------------------------
function M:run(ast)
   if not ast then
      self, ast = M.new(), self
   end
   self._acc = {}
   self:node(ast)
   return table.concat(self._acc), self.comment_ids
end

--------------------------------------------------------------------------------
-- Accumulate a piece of source file in the synthetizer.
--------------------------------------------------------------------------------
function M:acc(x)
   if x then
      table.insert(self._acc, x)
   end
end

--------------------------------------------------------------------------------
-- Accumulate an indented newline.
-- Jumps an extra line if indentation is 0, so that
-- toplevel definitions are separated by an extra empty line.
--------------------------------------------------------------------------------
--- @param comment? boolean
function M:nl(comment)
   if self.current_indent == 0 and not comment then
      self:acc("\n")
   end
   self:acc("\n" .. self.indent_step:rep(self.current_indent))
end

--------------------------------------------------------------------------------
-- Increase indentation and accumulate a new line.
--------------------------------------------------------------------------------
function M:nlindent()
   self.current_indent = self.current_indent + 1
   self:nl()
end

--------------------------------------------------------------------------------
-- Decrease indentation and accumulate a new line.
--------------------------------------------------------------------------------
function M:nldedent()
   self.current_indent = self.current_indent - 1
   self:acc("\n" .. self.indent_step:rep(self.current_indent))
end

--------------------------------------------------------------------------------
-- Keywords, which are illegal as identifiers.
--------------------------------------------------------------------------------
local keywords_list = {
   "and",
   "break",
   "do",
   "else",
   "elseif",
   "end",
   "false",
   "for",
   "function",
   "if",
   "in",
   "local",
   "nil",
   "not",
   "or",
   "repeat",
   "return",
   "then",
   "true",
   "until",
   "while",
}
local keywords = {}
for _, kw in pairs(keywords_list) do
   keywords[kw] = true
end

--------------------------------------------------------------------------------
-- Return true iff string `id' is a legal identifier name.
--------------------------------------------------------------------------------
local function is_ident(id)
   -- HACK:
   if type(id) ~= "string" then
      return false
   end
   return string["match"](id, "^[%a_][%w_]*$") and not keywords[id]
end

-- Return true iff ast represents a legal function name for
-- syntax sugar ``function foo.bar.gnat() ... end'':
-- a series of nested string indexes, with an identifier as
-- the innermost node.
local function is_idx_stack(ast)
   local tag = ast.tag
   if tag == "Index" then
      return is_idx_stack(ast[1])
   elseif tag == "Id" then
      return true
   else
      return false
   end
end

--------------------------------------------------------------------------------
-- Operator precedences, in increasing order.
-- This is not directly used, it's used to generate op_prec below.
--------------------------------------------------------------------------------
local op_preprec = {
   { "or",    "and" },
   { "lt",    "le",    "eq",  "ne" },
   { "concat" },
   { "add",   "sub" },
   { "mul",   "div",   "mod" },
   { "unm",   "unary", "not", "len" }, ---TODO:
   { "pow" },
   { "index" },
}

--------------------------------------------------------------------------------
-- operator --> precedence table, generated from op_preprec.
--------------------------------------------------------------------------------
local op_prec = {}

for prec, ops in ipairs(op_preprec) do
   for _, op in ipairs(ops) do
      op_prec[op] = prec
   end
end

--------------------------------------------------------------------------------
-- operator --> source representation.
--------------------------------------------------------------------------------
local op_symbol = {
   add = " + ",
   sub = " - ",
   mul = " * ",
   div = " / ",
   mod = " % ",
   pow = " ^ ",
   concat = " .. ",
   eq = " == ",
   ne = " ~= ",
   lt = " < ",
   le = " <= ",
   ["and"] = " and ",
   ["or"] = " or ",
   ["not"] = "not ",
   len = "# ",
   unm = "-",
}

--- @param node token
--- @return table
function M:extract_comments(node)
   if not node.lineinfo then return {} end
   local lfi = node.lineinfo.first
   local lla = node.lineinfo.last
   local comments = {}

   --- @param c table
   --- @param pos 'first'|'last'
   local function add_comment(c, pos)
      local idf = c.lineinfo.first.id
      local idl = c.lineinfo.last.id
      local present = self.comment_ids[idf] or self.comment_ids[idl]
      if not present then
         local comment_text    = c[1]
         local has_next        = c[2]
         local n_l             = #(string.lines(comment_text))
         local cfi             = c.lineinfo.first
         local cla             = c.lineinfo.last
         local cfirst          = { l = cfi.line, c = cfi.column }
         local clast           = { l = cla.line, c = cla.column }
         local l_d             = cla.line - cfi.line
         local newline         = (n_l ~= 0 and n_l == l_d)
         local li              = {
            idf = idf,
            idl = idl,
            first = cfirst,
            last = clast,
            text = comment_text,
            multiline = has_next,
            position = pos,
            prepend_newline = newline
         }
         self.comment_ids[idf] = true
         self.comment_ids[idl] = true
         table.insert(comments, li)
      end
   end
   if lfi.comments then
      for _, c in ipairs(lfi.comments) do
         add_comment(c, 'first')
      end
   end
   if lla.comments then
      for _, c in ipairs(lla.comments) do
         add_comment(c, 'last')
      end
   end

   return comments
end

-- Accumulate the source representation of AST `node' in
-- the synthetizer. Most of the work is done by delegating to
-- the method having the name of the AST tag.
-- If something can't be converted to normal sources, it's
-- instead dumped as a `-{ ... }' splice in the source accumulator.
function M:node(node)
   -- p(node)
   assert(self ~= M and self._acc)
   if node == nil then
      self:acc("<<error>>")
      return
   end
   local comments = self:extract_comments(node)
   --- @param pos 'first'|'last'
   local function show_comments(pos)
      for _, co in pairs(comments) do
         if co.position == pos then
            if co.position == 'last' then self:nl(true) end
            local lines = string.lines(co.text)
            if co.multiline then
               local wrapped = string.wrap_array(lines, self.wrap - 4)
               self:acc('--[[')
               if co.prepend_newline then self:nl(true) end
               for i, l in ipairs(wrapped) do
                  self:acc(l)
                  if i ~= #wrapped then self:nl(true) end
               end
               self:acc(']]')
            else
               local ls = co.first.l
               local le = co.last.l
               local wrapped = string.wrap_array(lines, self.wrap - 3)
               if ls == le then
                  --- single line comment
                  if co.text == '' then
                     self:acc('--')
                  else
                     for i, l in ipairs(wrapped) do
                        local first = string.sub(l, 1, 1)
                        local pre = '--'
                        if i == 1 and first == ' ' or first == '-'
                        then
                        else
                           pre = pre .. ' '
                        end
                        self:acc(pre .. l)
                        if i ~= #wrapped then self:nl(true) end
                     end
                  end
               else
                  for i, l in ipairs(wrapped) do
                     local first = string.sub(l, 1, 1)
                     local pre = '--'
                     if first == ' ' or first == '-'
                     then
                     else
                        pre = pre .. ' '
                     end
                     self:acc(pre .. l)
                     if i ~= #wrapped then self:nl(true) end
                  end
               end
            end

            if co.position == 'first' then self:nl(true) end
         end
      end
   end

   show_comments('first')
   if not node.tag then -- tagless block.
      self:list(node, self.nl)
   else
      local f = M[node.tag]
      if type(f) == "function" then   -- Delegate to tag method.
         f(self, node, unpack(node))
      elseif type(f) == "string" then -- tag string.
         self:acc(f)
      else                            -- No appropriate method, fall back to splice dumping.
         -- This cannot happen in a plain Lua AST.
         self:acc(" -{ ")
         self:acc(pp.tostring(node,
            { metalua_tag = 1, hide_hash = 1, line_max = 80 }))
         self:acc(" }")
      end
   end
   show_comments('last')
end

--------------------------------------------------------------------------------
-- Convert every node in the AST list `list' passed as 1st arg.
-- `sep' is an optional separator to be accumulated between each list element,
-- it can be a string or a synth method.
-- `start' is an optional number (default == 1), indicating which is the
-- first element of list to be converted, so that we can skip the begining
-- of a list.
--------------------------------------------------------------------------------
function M:list(list, sep, start)
   for i = start or 1, #list do
      self:node(list[i])
      if list[i + 1] then
         if not sep then
         elseif type(sep) == "function" then
            sep(self)
         elseif type(sep) == "string" then
            self:acc(sep)
         else
            error("Invalid list separator")
         end
      end
   end
end

--------------------------------------------------------------------------------
--
-- Tag methods.
-- ------------
--
-- Specific AST node dumping methods, associated to their node kinds
-- by their name, which is the corresponding AST tag.
-- synth:node() is in charge of delegating a node's treatment to the
-- appropriate tag method.
--
-- Such tag methods are called with the AST node as 1st arg.
-- As a convenience, the n node's children are passed as args #2 ... n+1.
--
-- There are several things that could be refactored into common subroutines
-- here: statement blocks dumping, function dumping...
-- However, given their small size and linear execution
-- (they basically perform series of :acc(), :node(), :list(),
-- :nl(), :nlindent() and :nldedent() calls), it seems more readable
-- to avoid multiplication of such tiny functions.
--
-- To make sense out of these, you need to know metalua's AST syntax, as
-- found in the reference manual or in metalua/doc/ast.txt.
--
--------------------------------------------------------------------------------

function M:Do(node)
   self:acc("do")
   self:nlindent()
   self:list(node, self.nl)
   self:nldedent()
   self:acc("end")
end

function M:Set(node)
   local lhs = node[1]
   local rhs = node[2]
   -- ``function foo:bar(...) ... end'' --
   if
       lhs[1].tag == "Index"
       and rhs[1].tag == "Function"
       and rhs[1][1][1] == "self"
       and is_idx_stack(lhs)
       and is_ident(lhs[1][2][1])
   then
      local method = lhs[1][2][1]
      local params = rhs[1][1]
      local body = rhs[1][2]
      -- TODO:
      -- local params = node[2]
      self:acc("function ")
      self:node(lhs)
      self:acc(":")
      self:acc(method)
      self:acc("(")
      self:list(params, ", ", 2)
      self:acc(")")
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc("end")
   elseif rhs[1].tag == "Function" and is_idx_stack(lhs) then
      print("here?")
      print(is_idx_stack(lhs))
      -- | `Set{ { lhs }, { `Function{ params, body } } } if is_idx_stack (lhs) ->
      --    -- ``function foo(...) ... end'' --
      local params = rhs[1][1]
      local body = rhs[1][2]
      self:acc("function ")
      self:node(lhs)
      self:acc("(")
      self:list(params, ", ")
      self:acc(")")
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc("end")
   else
      self:list(lhs, ", ")
      self:acc(" = ")
      self:list(rhs, ", ")

      --
      -- | `Set{ { `Id{ lhs1name } == lhs1, ... } == lhs, rhs }
      --       if not is_ident (lhs1name) ->
      --    -- ``foo, ... = ...'' when foo is *not* a valid identifier.
      --    -- In that case, the spliced 1st variable must get parentheses,
      --    -- to be distinguished from a statement splice.
      --    -- This cannot happen in a plain Lua AST.
      --    self:acc      "("
      --    self:node     (lhs1)
      --    self:acc      ")"
      --    if lhs[2] then -- more than one lhs variable
      --       self:acc   ", "
      --       self:list  (lhs, ", ", 2)
      --    end
      --    self:acc      " = "
      --    self:list     (rhs, ", ")
      --
      -- | `Set{ lhs, rhs } ->
      --    -- ``... = ...'', no syntax sugar --
      --    self:list  (lhs, ", ")
      --    self:acc   " = "
      --    self:list  (rhs, ", ")
      -- | `Set{ lhs, rhs, annot } ->
      --    -- ``... = ...'', no syntax sugar, annotation --
      --    local n = #lhs
      --    for i=1,n do
      --        local ell, a = lhs[i], annot[i]
      --        self:node (ell)
      --        if a then
      --            self:acc ' #'
      --            self:node(a)
      --        end
      --        if i~=n then self:acc ', ' end
      --    end
      --    self:acc   " = "
      --    self:list  (rhs, ", ")
   end
end

function M:While(_, cond, body)
   self:acc("while ")
   self:node(cond)
   self:acc(" do")
   self:nlindent()
   self:list(body, self.nl)
   self:nldedent()
   self:acc("end")
end

function M:Repeat(_, body, cond)
   self:acc("repeat")
   self:nlindent()
   self:list(body, self.nl)
   self:nldedent()
   self:acc("until ")
   self:node(cond)
end

function M:If(node)
   for i = 1, #node - 1, 2 do
      -- for each ``if/then'' and ``elseif/then'' pair --
      local cond, body = node[i], node[i + 1]
      self:acc(i == 1 and "if " or "elseif ")
      self:node(cond)
      self:acc(" then")
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
   end
   -- odd number of children --> last one is an `else' clause --
   if #node % 2 == 1 then
      self:acc("else")
      self:nlindent()
      self:list(node[#node], self.nl)
      self:nldedent()
   end
   self:acc("end")
end

function M:Fornum(node, var, first, last)
   local body = node[#node]
   self:acc("for ")
   self:node(var)
   self:acc(" = ")
   self:node(first)
   self:acc(", ")
   self:node(last)
   if #node == 5 then -- 5 children --> child #4 is a step increment.
      self:acc(", ")
      self:node(node[4])
   end
   self:acc(" do")
   self:nlindent()
   self:list(body, self.nl)
   self:nldedent()
   self:acc("end")
end

function M:Forin(_, vars, generators, body)
   self:acc("for ")
   self:list(vars, ", ")
   self:acc(" in ")
   self:list(generators, ", ")
   self:acc(" do")
   self:nlindent()
   self:list(body, self.nl)
   self:nldedent()
   self:acc("end")
end

function M:Local(node, lhs, rhs, annots)
   if next(lhs) then
      self:acc("local ")
      if annots then
         local n = #lhs
         for i = 1, n do
            self:node(lhs)
            local a = annots[i]
            if a then
               self:acc(" #")
               self:node(a)
            end
            if i ~= n then
               self:acc(", ")
            end
         end
      else
         self:list(lhs, ", ")
      end
      if rhs[1] then
         self:acc(" = ")
         self:list(rhs, ", ")
      end
   else -- Can't create a local statement with 0 variables in plain Lua
      self:acc(pp.tostring(node, "nohash"))
   end
end

function M:Localrec(_, lhs, rhs)
   -- ``local function name() ... end'' --
   self:acc("local function ")
   self:acc(lhs[1][1])
   self:acc("(")
   self:list(rhs[1][1], ", ")
   self:acc(")")
   self:nlindent()
   self:list(rhs[1][2], self.nl)
   self:nldedent()
   self:acc("end")
   --
   -- | _ ->
   --    -- Other localrec are unprintable ==> splice them --
   --        -- This cannot happen in a plain Lua AST. --
   --    self:acc "-{ "
   --    self:acc (table.tostring (node, 'nohash', 80))
   --    self:acc " }"
   -- end
end

function M:Call(node, f)
   self:node(f)
   self:acc("(")
   self:list(node, ", ", 2) -- skip `f'.
   self:acc(")")
end

function M:Invoke(node, f, method)
   -- single string or table literal arg ==> no need for parentheses. --
   -- local parens
   -- if node[2].tag == "String" or node[2].tag == "Table" then
   --    parens = false
   -- else
   --    parens = true
   -- end
   self:node(f)
   self:acc(":")
   self:acc(method[1])
   self:acc("(")
   -- self:acc(parens and "(" or " ")
   self:list(node, ", ", 3) -- Skip args #1 and #2, object and method name.
   -- self:acc(parens and ")")
   self:acc(")")
end

function M:Return(node)
   self:acc("return ")
   self:list(node, ", ")
end

M.Break = "break"
M.Nil = "nil"
M.False = "false"
M.True = "true"
M.Dots = "..."

function M:Number(_, n)
   self:acc(tostring(n))
end

function M:String(_, str)
   local fl        = string.len('"" ..' .. self.indent_step)
   local wl        = self.wrap - fl
   local multiline = string.ulen(str) > wl
   local rendered  = ''
   --- format "%q" prints '\n' in an umpractical way IMO,
   --- so this is fixed with the :gsub( ) call.
   if multiline then
      -- local nl_esc = str:gsub("\n", [[\n]])
      --- split the raw text
      local split = string.lines(str)
      --- add newline placeholders
      for i, v in ipairs(split) do
         if i ~= #split then
            split[i] = v .. '\\n'
         end
      end
      --- wrap
      local ls = string.wrap_array(split, wl)
      for i, v in ipairs(ls) do
         rendered = rendered .. "\n" .. self.indent_step
         rendered = rendered .. string.format("%q", v):gsub("\\\\", [[\]])
         if i ~= #ls then
            rendered = rendered .. ' ..'
         end
      end
   else
      rendered = string.format("%q", str):gsub("\\\n", [[\n]])
   end
   self:acc(rendered)
end

function M:Function(_, params, body, annots)
   self:acc("function(")
   if annots then
      local n = #params
      for i = 1, n do
         local p, a = params[i], annots[i]
         self:node(p)
         if annots then
            self:acc(" #")
            self:node(a)
         end
         if i ~= n then
            self:acc(", ")
         end
      end
   else
      self:list(params, ", ")
   end
   self:acc(")")
   self:nlindent()
   self:list(body, self.nl)
   self:nldedent()
   self:acc("end")
end

function M:Table(node)
   if not node[1] then
      self:acc("{ }")
   else
      self:acc("{")
      if #node > 1 then
         self:nlindent()
      else
         self:acc(" ")
      end
      for i, elem in ipairs(node) do
         if elem.tag == "Pair" then
            -- `Pair{ `String{ key }, value }
            if elem[1].tag == "String" and is_ident(elem[1][1]) then
               self:acc(elem[1][1])
               self:acc(" = ")
               self:node(elem[2])
               -- `Pair{ key, value }
            else
               self:acc("[")
               self:node(elem[1])
               self:acc("] = ")
               self:node(elem[2])
            end
         else
            self:node(elem)
         end
         if node[i + 1] then
            self:acc(",")
            self:nl()
         end
      end
      if #node > 1 then
         self:nldedent()
      else
         self:acc(" ")
      end
      self:acc("}")
   end
end

-- TODO: understand associativity
function M:Op(node, op, a, b)
   if op == "not" and (node[2][1][1] == "eq") then ---TODO:???
      op, a, b = "ne", node[2][1][2], node[2][1][3]
   end
   if b then -- binary operator.
      local left_paren, right_paren
      if a.tag == "Op" and op_prec[op] >= op_prec[a[1]] then
         left_paren = true
      else
         left_paren = false
      end
      if b.tag == "Op" and op_prec[op] >= op_prec[b[1]] then
         right_paren = true
      else
         right_paren = false
      end
      self:acc(left_paren and "(")
      self:node(a)
      self:acc(left_paren and ")")

      self:acc(op_symbol[op])

      self:acc(right_paren and "(")
      self:node(b)
      self:acc(right_paren and ")")
   else -- unary operator.
      local paren
      if a.tag == "Op" and op_prec[op] >= op_prec[a[1]] then
         paren = true
      else
         paren = false
      end
      self:acc(op_symbol[op])
      self:acc(paren and "(")
      self:node(a)
      self:acc(paren and ")")
   end
end

function M:Paren(_, content)
   self:acc("(")
   self:node(content)
   self:acc(")")
end

function M:Index(_, table, key)
   local paren_table
   if table.tag == "Op" and op_prec[table[1][1]] < op_prec.index then
      paren_table = true
   else
      paren_table = false
   end

   self:acc(paren_table and "(")
   self:node(table)
   self:acc(paren_table and ")")

   -- ``table [key]''
   if key.tag == "True" or key.tag == "False" or (not is_ident(key[1])) then
      self:acc("[")
      self:node(key)
      self:acc("]")
   else
      -- ``table.key''
      self:acc(".")
      self:acc(key[1])
   end
end

function M:Id(node, name)
   if is_ident(name) then
      self:acc(name)
   else -- Unprintable identifier, fall back to splice representation.
      -- This cannot happen in a plain Lua AST.
      self:acc("-{`Id ")
      self:String(node, name)
      self:acc("}")
   end
end

-- M.TDyn    = '*'
-- M.TDynbar = '**'
-- M.TPass   = 'pass'
-- M.TField  = 'field'
-- M.TIdbar  = M.TId
-- M.TReturn = M.Return
--
--
-- function M:TId (node, name) self:acc(name) end
--
--
-- function M:TCatbar(node, te, tebar)
--     self:acc'('
--     self:node(te)
--     self:acc'|'
--     self:tebar(tebar)
--     self:acc')'
-- end
--
-- function M:TFunction(node, p, r)
--     self:tebar(p)
--     self:acc '->'
--     self:tebar(r)
-- end
--
-- function M:TTable (node, default, pairs)
--     self:acc '['
--     self:list (pairs, ', ')
--     if default.tag~='TField' then
--         self:acc '|'
--         self:node (default)
--     end
--     self:acc ']'
-- end
--
-- function M:TPair (node, k, v)
--     self:node (k)
--     self:acc '='
--     self:node (v)
-- end
--
-- function M:TIdbar (node, name)
--     self :acc (name)
-- end
--
-- function M:TCatbar (node, a, b)
--     self:node(a)
--     self:acc ' ++ '
--     self:node(b)
-- end
--
-- function M:tebar(node)
--     if node.tag then self:node(node) else
--         self:acc '('
--         self:list(node, ', ')
--         self:acc ')'
--     end
-- end
--
-- function M:TUnkbar(node, name)
--     self:acc '~~'
--     self:acc (name)
-- end
--
-- function M:TUnk(node, name)
--     self:acc '~'
--     self:acc (name)
-- end
--
-- for name, tag in pairs{ const='TConst', var='TVar', currently='TCurrently', just='TJust' } do
--     M[tag] = function(self, node, te)
--         self:acc (name..' ')
--         self:node(te)
--     end
-- end

-- print(M.run(+{stat: local function add(a, b) local c = a + b; return add(a,c) end}))

return function(x, ...)
   local a2s = M.new(...)
   return a2s:run(x)
end
