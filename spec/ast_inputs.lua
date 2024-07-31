--- @param s string|string[]
--- @param canonized string|string[]?
--- @return table {string[], string[]}
local prep = function(s, canonized)
  local orig = (function()
    if type(s) == 'string' then
      return string.lines(s)
    elseif type(s) == 'table' then
      local ret = {}
      for i, v in ipairs(s) do
        ret[i] = v
      end
      return ret
    end
  end)()
  local canon = canonized and string.lines(canonized) or orig

  return { orig, canon }
end

local sierpinski = [[function sierpinski(depth)
  lines = { '*' }
  for i = 2, depth + 1 do
    sp = string.rep(' ', 2 ^ (i - 2))
    tmp = {} -- comment
    for idx, line in ipairs(lines) do
      tmp[idx] = sp .. line .. sp
      tmp[idx + #lines] = line .. ' ' .. line
    end
    lines = tmp
  end
  return table.concat(lines, '\n')
end

print(sierpinski(4))]]

local sierpinski_res = {
  'function sierpinski(depth)',
  -- --- [[ ]] version
  -- '  lines = { [[*]] }',
  '  lines = { "*" }',

  '  for i = 2, depth + 1 do',
  -- --- [[ ]] version
  -- '    sp = string.rep([[ ]], 2 ^ (i - 2))',
  '    sp = string.rep(" ", 2 ^ (i - 2))',

  '    tmp = { }',
  '    -- comment',
  '    for idx, line in ipairs(lines) do',
  -- '      tmp[idx] = sp .. (line .. sp)',
  '      tmp[idx] = sp .. line .. sp',
  -- --- [[ ]] version
  -- '      tmp.add = line .. ([[ ]] .. line)',
  --- TODO what's up with the paren
  -- '      tmp[idx + #lines] = line .. (" " .. line)',
  '      tmp[idx + #lines] = line .. " " .. line',

  '    end',
  '    lines = tmp',
  '  end',
  -- --- [[ ]] version
  -- '  return table.concat(lines, [[',
  -- ']])',
  -- ---  '' version
  -- "  return table.concat(lines, '\\n')",
  ---  "" version
  '  return table.concat(lines, "\\n")',
  'end',
  -- '',
  'print(sierpinski(4))',
}

local meta =
[[
--- @param node token
--- @return table
function M:extract_comments(node)
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
         local len             = string.len(comment_text)
         local n_l             = #(string.lines(comment_text))
         local cfi             = c.lineinfo.first
         local cla             = c.lineinfo.last
         local cfirst          = { l = cfi.line, c = cfi.column }
         local clast           = { l = cla.line, c = cla.column }
         local off             = cla.offset - cfi.offset
         local d               = off - len
         local l_d             = cla.line - cfi.line
         local newline         = (n_l ~= 0 and n_l == l_d)
         local li              = {
            idf = idf,
            idl = idl,
            first = cfirst,
            last = clast,
            text = comment_text,
            multiline = (d > 4),
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
]]
local meta_res = {
  '--- @param node token',
  '--- @return table',
  --- functions are values
  --- 'M.extract_comments = function(self, node)',
  --- self syntax sugar
  'function M:extract_comments(node)',
  -- 'function M.extract_comments(self, node)',
  '  local lfi = node.lineinfo.first',
  '  local lla = node.lineinfo.last',
  '  local comments = { }',
  -- '',
  '  --- @param c table',
  "  --- @param pos 'first'|'last'",
  '  local function add_comment(c, pos)',
  '    local idf = c.lineinfo.first.id',
  '    local idl = c.lineinfo.last.id',
  -- '    local present = self.comment_ids[idf] or self.comment_ids[idl]',
  '    local present = self.comment_ids[idf]',
  '         or self.comment_ids[idl]',
  '    if not present then',
  '      local comment_text = c[1]',
  '      local len = string.len(comment_text)',
  '      local n_l = #(string.lines(comment_text))',
  '      local cfi = c.lineinfo.first',
  '      local cla = c.lineinfo.last',
  --- splits tables
  '      local cfirst = {',
  '        l = cfi.line,',
  '        c = cfi.column',
  '      }',
  '      local clast = {',
  '        l = cla.line,',
  '        c = cla.column',
  '      }',
  '      local off = cla.offset - cfi.offset',
  '      local d = off - len',
  '      local l_d = cla.line - cfi.line',
  --- normalizes logic conditions
  '      local newline = (n_l ~= 0 and n_l == l_d)',
  '      local li = {',
  '        idf = idf,',
  '        idl = idl,',
  '        first = cfirst,',
  '        last = clast,',
  '        text = comment_text,',
  '        multiline = (4 < d),',
  '        position = pos,',
  '        prepend_newline = newline',
  '      }',
  '      self.comment_ids[idf] = true',
  '      self.comment_ids[idl] = true',
  '      table.insert(comments, li)',
  '    end',
  '  end',
  '  if lfi.comments then',
  '    for _, c in ipairs(lfi.comments) do',
  '      add_comment(c, "first")',
  '    end',
  '  end',
  '  if lla.comments then',
  '    for _, c in ipairs(lla.comments) do',
  '      add_comment(c, "last")',
  '    end',
  '  end',
  -- '',
  '  return comments',
  'end',
}

local clock = {
  'love.draw = function()',
  '  draw()',
  'end',
  '',
  'function love.update(dt)',
  '  t = t + dt',
  '  s = math.floor(t)',
  '  if s > midnight then s = 0 end',
  'end',
  '',
  'function cycle(c)',
  '  if c > 7 then return 1 end',
  '  return c + 1',
  'end',
  '',
  'love.keyreleased = function (k)',
  '  if k == \'space\' then',
  '    if love.keyboard.isDown("lshift", "rshift") then',
  '      bg_color = cycle(bg_color)',
  '    else',
  '      color = cycle(color)',
  '    end',
  '  end',
  '  if k == \'s\' then',
  '    stop(\'STOP THE CLOCKS!\')',
  '  end',
  'end' }
local clock_res = {
  --- functions are values
  -- 'love.draw = function()',
  --- syntax sugar
  'function love.draw()',
  '  draw()',
  'end',
  -- '',
  --- functions are values
  -- 'love.update = function(dt)',
  --- syntax sugar
  'function love.update(dt)',
  '  t = t + dt',
  '  s = math.floor(t)',
  -- canonized compare order
  '  if midnight < s then',
  '    s = 0',
  '  end',
  'end',
  -- '',
  --- functions are values
  -- 'cycle = function(c)',
  --- syntax sugar
  'function cycle(c)',
  -- canonized compare order
  '  if 7 < c then',
  '    return 1',
  '  end',
  '  return c + 1',
  'end',
  -- '',
  --- functions are values
  -- 'love.keyreleased = function(k)',
  --- syntax sugar
  'function love.keyreleased(k)',
  '  if k == "space" then',
  '    if love.keyboard.isDown("lshift", "rshift") then',
  '      bg_color = cycle(bg_color)',
  '    else',
  '      color = cycle(color)',
  '    end',
  '  end',
  '  if k == "s" then',
  '    stop("STOP THE CLOCKS!")',
  '  end',
  'end',
}

local basics = {
  prep('local str = "asd"'),
  prep("local str = 'asd'", { 'local str = "asd"' }),
  prep('local n = 3.0e2', 'local n = 300'),
}

local operators = {
  prep('local x = 2 + 3'),
  prep('local x = (2 + 3) * 5'),
  prep('local x = 2 + 3 * 5'),
  prep('x = 3 * 5 + 6 * 9'),
  prep('x = 3 + 5 * 6 + 9'),
  prep('x = 3 * 5 * 6 * 9'),
  prep('p = 4 ^ 2 ^ 3', 'p = 4 ^ (2 ^ 3)'),
  prep('x = 2 + - 1', 'x = 2 + -1'),
  prep('unm = -10'),
  prep('y = 15 / 5 * 3', 'y = (15 / 5) * 3'),
  prep('x = 3 - 2 - 1', 'x = (3 - 2) - 1'),
  prep('G.setColor(Color[color + Color.bright])'),
  prep('G.setColor(Color[(color + Color.bright)])'),
  prep('v = t[i + 3]'),
  prep('v = t[(i + 3)]'),
  prep('local v = #t'),
  prep('v = #t'),
  prep('tmp[idx + #lines] = line .. (" " .. line)'),
  prep('con_line = line .. " " .. line'),
  prep('bool = true and false or true'),
  prep('bool = not false or false'),
  prep('bool = x ~= y'),
  prep('bool = not (x == y)', 'bool = x ~= y'),
  prep('len = #t1'),
  prep('len = #t1.t2', 'len = #(t1.t2)'),
  prep('len = #(t1.t2)'),
  prep('len = #V:get_text()', 'len = #(V:get_text())'),
  prep('len = #V:get_input():get_text()',
    'len = #(V:get_input():get_text())'),
  prep('len = # "asd" .. "vfds"', 'len = #"asd" .. "vfds"'),
  prep('len = # ("asd" .. "vfds")', 'len = #("asd" .. "vfds")'),
  prep('bool = 2 >= 3 ~= 4 < 5', 'bool = (3 <= 2) ~= 4 < 5'),
}

local comments = {
  prep({
    'y = 10',
    '--- comment',
    'z = 99'
  }),

  prep({
    'y = 10',
    '--- comment1',
    '--- comment2',
    '--- comment3',
    'z = 99'
  }),

  prep({
    'x = 0',
    '-- comment1',
    '-- comment2',
    '-- comment3',
    'a = 1'
  }),

  prep({
    'x = 1',
    '--[[ comment1',
    ' comment2',
    ' comment3]]',
    'a = 3'
  }),

  prep({
    'x = 0',
    '--[[ comment1',
    ' comment2]]--',
    'a = 2',
  }, {
    'x = 0',
    '--[[ comment1',
    ' comment2]]',
    '--', --- this is canonical now, no following `--` after `]]`
    'a = 2',
  }),

  prep({
    'x = 0',
    '--[[ comment1',
    ' comment2]]',
    'a = 2',
  }, {
    'x = 0',
    '--[[ comment1',
    ' comment2]]',
    'a = 2',
  }),

  prep({
    'x = 0',
    '--[[line1',
    'line2--]]',
    'a = 2',
  }
  -- , {
  --   'x = 0',
  --   '--[[ comment1',
  --   ' comment2]]',
  --   'a = 2',
  --   }
  ),

  prep({
    'x = 0',
    '-- comment1',
    '-- comment2',
    'a = 2',
  }),

  prep([[-- comment]]),
  prep({ '-- interesting comment', 'a = 1' }),
  prep({
    '-- comment1',
    '-- comment2'
  }),

  prep({
    '--[[ comment1',
    ' comment2--]]',
    'a = 2',
    '-- asd', -- canonical space after comment marker
    '--[[]]',
  }),
  prep({ '--[[]]', 'a = 1' }),
  prep({ 'a = 1', '--[[]]' }),
  prep({
    '-- comment1',
    '-- comment2',
    'a = 1'
  }),
  prep({
    'local head_r = 8',
    'local leg_r = 5',
    '-- move offsets',
    'local x_r = 15',
    'local y_r = 20',
  }),
  prep({
    'y = 3',
    '--[[',
    'multi-',
    'line',
    'comment]]',
  }),
  prep({ '--[[',
    'multi-',
    'line',
    'comment]]',
  }),
  prep({ '--[[multi-',
    'line',
    'comment',
    ']]',
  }),
  prep({ '--[[multi-',
    'line',
    'comment]]',
  }),
  prep({
    '--[[ comment1',
    ' comment2]]--',
    'a = 2',
    '--asd',
    '--[[]]',
  }, {
    '--[[ comment1',
    ' comment2]]',
    '--',     --- this is canonical now, no following `--` after `]]`
    'a = 2',
    '-- asd', -- canonical space after comment marker
    '--[[]]',
  }),
  prep({
      'local x = 2',
      '--[[',
      'multi-',
      'line',
      'comment]]--',
    },
    {
      'local x = 2',
      '--[[',
      'multi-',
      'line',
      'comment]]',
      '--', --- the proper mlc closing is `--]]`
    }
  ),
  prep({ 'a = 3 --cmt',
  }, {
    'a = 3',
    '-- cmt',
  }),
}

local wrapping = {
  prep('local t = { b = 2, 3, 4 }', {
    'local t = {',
    '  b = 2,',
    '  3,',
    '  4',
    '}' }),
  prep('a = 1 ; b = 2', { 'a = 1', 'b = 2' }),
  --- string literals and comments
  prep(
    'local long_string = "яяяяяяяяяяяяяяяяяяя22222222222222222eeeeeeeeeeeeeeeeeee6666666666666666666666666sssssssssss"',
    {
      'local long_string = ',
      '  "яяяяяяяяяяяяяяяяяяя22222222222222222eeeeeeeeeeeeeeeeeee66" ..',
      '  "66666666666666666666666sssssssssss"',
    }),
  prep(
    '-- яяяяяяяяяяяяяяяяяяя22222222222222222eeeeeeeeeeeeeeeeeee6666666666666666666666666sssssssssss',
    {
      '-- яяяяяяяяяяяяяяяяяяя22222222222222222eeeeeeeeeeeeeeeeeee66666',
      '-- 66666666666666666666sssssssssss',
    }),
  prep(
    {
      '--[[Bacon ipsum dolor amet sint meatball pork loin, shankle kiel',
      'basa nulla mollit quis elit dolore tenderloin swine.',
      'Elit beef pancetta, lorem sirloin spare ribs tenderloin exercitation laborum tongue eiusmod dolor fatback.',
      'In ut dolore corned beef flank eiusmod, burgdoggen capicola ham enim culpa hamburger chuck. Beef burgdoggen qui meatloaf cupidatat sunt. Lorem spare ribs dolor mollit porchetta. Nostrud pig shoulder beef veniam shank pork loin landjaeger chuck ball tip.',
      'Tri-tip elit culpa deserunt.]]' },
    {
      '--[[Bacon ipsum dolor amet sint meatball pork loin, shankle kiel',
      'basa nulla mollit quis elit dolore tenderloin swine.',
      'Elit beef pancetta, lorem sirloin spare ribs tenderloin exercita',
      'tion laborum tongue eiusmod dolor fatback.',
      'In ut dolore corned beef flank eiusmod, burgdoggen capicola ham ',
      'enim culpa hamburger chuck. Beef burgdoggen qui meatloaf cupidat',
      'at sunt. Lorem spare ribs dolor mollit porchetta. Nostrud pig sh',
      'oulder beef veniam shank pork loin landjaeger chuck ball tip.',
      'Tri-tip elit culpa deserunt.]]',
    }),
  prep({
      '-- яяяяяяяяяяяяяяяяяяя22222222222222222eeeeeeeeeeeeeeeeeee6666666666666666666666666sssssssssss',
      '-- цэфлаэфцжфдэжафдукзщфкхз2щ3х54з2ьахажщд2хфладжьяхадыхжахдхыжхахдыалджлождлод' },
    {
      '-- яяяяяяяяяяяяяяяяяяя22222222222222222eeeeeeeeeeeeeeeeeee66666',
      '-- 66666666666666666666sssssssssss',
      '-- цэфлаэфцжфдэжафдукзщфкхз2щ3х54з2ьахажщд2хфладжьяхадыхжахдхыж',
      '-- хахдыалджлождлод',
    }),
  prep({
    'function fun()',
    '  doSomething() -- very long comment that will go over the line length',
    'end',
  }, {
    'function fun()',
    '  doSomething()',
    '  -- very long comment that will go over the line length',
    'end' }
  ),
  prep({
    'local s = [[asd',
    'string',
    ']]',
  }, { [[local s = "asd\nstring\n"]] }),
  prep({
    'local s = [[asd',
    'string',
    '',
    '',
    ']]',
  }, { [[local s = "asd\nstring\n\n\n"]] }),

  prep({
      'local ms=  [[█Bacon ipsum dolor amet ribeye hamburger',
      'c█hislic pork short ribs',
      'po█rchetta. Pork loin meatball ball tip',
      'por█k chop pork capicola fatback andouille beef sausage short',
      'loin█ bresaola venison.\\t]]',
    },
    -- --- [[ ]] version
    -- {
    --   'local ms = [[█Bacon ipsum dolor amet ribeye hamburger',
    --   'c█hislic pork short ribs',
    --   'po█rchetta. Pork loin meatball ball tip',Debug.text(comment_text)
    -- }
    --- " " version
    {
      'local ms = ',
      [[  "█Bacon ipsum dolor amet ribeye hamburger\n" ..]],
      [[  "c█hislic pork short ribs\n" ..]],
      [[  "po█rchetta. Pork loin meatball ball tip\n" ..]],
      [[  "por█k chop pork capicola fatback andouille beef sausage s" ..]],
      [[  "hort\n" ..]],
      [[  "loin█ bresaola venison.\t"]],
    }
  ),
  prep({
      'local str = "asd\\nbgf"',
      'local mstr = [[rty',
      'qwe]]',
      'local ms = [[ms]]',
      'local m_s = [[m\ns]]',
    },
    -- --- [[ ]] version
    -- {
    --   'local str = [[asd',
    --   'bgf]]',
    --   'local mstr = [[rty',
    --   'qwe]]',
    --   'local ms = "ms"',
    -- }
    --- " " version
    {
      [[local str = "asd\nbgf"]],
      [[local mstr = "rty\nqwe"]],
      'local ms = "ms"',
      [[local m_s = "m\ns"]],
    }
  ),
  --- compound conditions
  prep({
    'if type(w) ~= "number" or w < 1 then',
    '  fun()',
    'end',
  }),
  prep({
    'if type(w) ~= "number"',
    '     or w < 1 then',
    '  fun()',
    'end',
  }, {
    'if type(w) ~= "number"',
    '     or w < 1',
    'then',
    '  fun()',
    'end',
  }),
  prep({
    'local f = function()',
    '  for k, v in pairs(x) do',
    '    if love.keyboard.isDown("lshift", "rshift") and not love.keyboard.isDown("lalt", "ralt") then',
    '      bg_color = cycle(bg_color)',
    '    else',
    '      bg_color = Color.blue',
    '    end',
    '  end',
    'end',
  }, {
    'local f = function()',
    '  for k, v in pairs(x) do',
    --- wrong:
    --  '    if love.keyboard.isDown("lshift", "rshift") and not love.'
    --  '        keyboard.isDown("lalt", "ralt") then'
    '    if love.keyboard.isDown("lshift", "rshift")',
    '         and not love.keyboard.isDown("lalt", "ralt")',
    '    then',
    '      bg_color = cycle(bg_color)',
    '    else',
    '      bg_color = Color.blue',
    '    end',
    '  end',
    'end',
  }
  ),
  prep({
    'local f = function()',
    '  for k, v in pairs(x) do',
    '    if love.keyboard.isDown("lshift", "rshift") and not love.keyboard.isDown("lalt", "ralt") and not love.keyboard.isDown("lctrl", "rctrl") then',
    '      bg_color = cycle(bg_color)',
    '    else',
    '      bg_color = Color.blue',
    '    end',
    '  end',
    'end',
  }, {
    'local f = function()',
    '  for k, v in pairs(x) do',
    '    if love.keyboard.isDown("lshift", "rshift")',
    --- wrong:
    --  '    if love.keyboard.isDown("lshift", "rshift") and not love.'
    --  '        keyboard.isDown("lalt", "ralt") and not love.keyboard.isDown('
    --  '        "lctrl", "rctrl") then'
    '         and not love.keyboard.isDown("lalt", "ralt")',
    '         and not love.keyboard.isDown("lctrl", "rctrl")',
    '    then',
    '      bg_color = cycle(bg_color)',
    '    else',
    '      bg_color = Color.blue',
    '    end',
    '  end',
    'end',
  }
  ),
  prep({
      'function M:extract_comments(node)',
      '  local function add_comment(c, pos)',
      '    local idf = c.lineinfo.first.id',
      '    local idl = c.lineinfo.last.id',
      '    local present = self.comment_ids[idf] or self.comment_ids[idl]',
      '  end',
      'end',
    },
    {
      'function M:extract_comments(node)',
      '  local function add_comment(c, pos)',
      '    local idf = c.lineinfo.first.id',
      '    local idl = c.lineinfo.last.id',
      -- '    local present = self.comment_ids[idf] or self.comment_ids['
      -- 'idl]',
      '    local present = self.comment_ids[idf]',
      '         or self.comment_ids[idl]',
      '  end',
      'end',
    }),
  prep({
    'function M:extract_comments(node)',
    '  local function add_comment(c, pos)',
    '    local idf = c.lineinfo.first.id',
    '    local idl = c.lineinfo.last.id',
    '    local present = self.comment_ids[idf]',
    '         or self.comment_ids[idl]',
    '  end',
    'end',
  }),

  --- complicated calculations that should probably be broken up
  prep('local longcomp = (3749182734 + 1928340918 - 239420985) * (274927 + 820479 - 2973842)', {
    'local longcomp = (3749182734 + 1928340918 - 239420985) * ',
    '  (274927 + 820479 - 2973842)',
  }),
  prep('local longcomp2 = 1001 + 1002 + 1003 + 1004 + 1005 + 1006 + 1007 + 1008 + 1009', {
    'local longcomp2 = 1001 + 1002 + 1003 + 1004 + 1005 + 1006 + 1007',
    '     + 1008 + 1009'
  }),

  --- multi-assignments that again, probably should be broken up
  prep('local declaring, a, lot, of, variables, in_, one, go = 1, 2, 3, 4, 5, 6, 7, 8', {
    'local declaring, a, lot, of, variables, in_, one, go = 1, 2, 3, ',
    '    4, 5, 6, 7, 8' }),
  prep({
      'local declaring, a, lot, of, ',
      '    variables, in_, one, go = 1, 2, 3,',
      '     4, 5, 6, 7, 8',
    }
    ,
    {
      'local declaring, a, lot, of, variables, in_, one, go = 1, 2, 3, ',
      '    4, 5, 6, 7, 8' }
  ),

  prep(
    'local assigning, an, amount, of, variables, that, cannot, possibly, fit, on, one, line  = 101, 102, 103, 4, 5, 6, 7, 8, 9, 10, 11, 12'
    , {
      'local assigning, an, amount, of, variables, that, cannot, ',
      '    possibly, fit, on, one, line = 101, 102, 103, 4, 5, 6, 7, 8, 9, 10, ',
      '    11, 12', }
  ),
  prep({
      'Globally, declaring, a, lot, of, ',
      '    variables, in_, one, go = 1, 2, 3,',
      '     4, 5, 6, 7, 8',
    }
    ,
    {
      'Globally, declaring, a, lot, of, variables, in_, one, go = 1, 2',
      '    , 3, 4, 5, 6, 7, 8' }
  ),

  --- returning too many values
  prep({
    'function ret_gaming()',
    '  return a, ridiculous, amouns, of, named, values, cannot, possibly, fit',
    'end' }, {
    'function ret_gaming()',
    '  return a, ridiculous, amouns, of, named, values, cannot, ',
    '      possibly, fit',
    'end' }
  ),

  --- Very Large Numbers
  prep(
    'N = 398492087598247598237529834759827345928375928734958729387459283787459837452987345982375',
    'N = 3.9849208759825e+86'
  ),


  --- functions with way too many parameters
  prep({
      'function fun(copious, amounts, of, parameters, which, cannot, possibly, fit)',
      '  doSomething()',
      'end',
    }
    ,
    --- naiive solution
    -- {
    --   'function fun(copious, amounts, of, parameters, which, cannot, ',
    --   '    possibly, fit)',
    --   '  doSomething()',
    --   'end',
    --     },
    {
      'function fun(',
      '  copious,',
      '  amounts,',
      '  of,',
      '  parameters,',
      '  which,',
      '  cannot,',
      '  possibly,',
      '  fit',
      ')',
      '  doSomething()',
      'end',
    }
  ),
  prep({
      'function O:method(copious, amounts, of, parameters, which, cannot, possibly, fit)',
      '  methodBody()',
      'end',
    },
    {
      'function O:method(',
      '  copious,',
      '  amounts,',
      '  of,',
      '  parameters,',
      '  which,',
      '  cannot,',
      '  possibly,',
      '  fit',
      ')',
      '  methodBody()',
      'end',
    }
  ),
  prep({
      'local function localfun(inordinate, amounts, of, parameters, which, cannot, possibly, fit)',
      '  localFunBody()',
      'end',
    },
    {
      'local function localfun(',
      '  inordinate,',
      '  amounts,',
      '  of,',
      '  parameters,',
      '  which,',
      '  cannot,',
      '  possibly,',
      '  fit',
      ')',
      '  localFunBody()',
      'end',
    }
  ),

  prep({
      'local ft = {',
      '  zx = 3,',
      '  tablefun = function(inordinate, amounts, of, parameters, which, cannot, and_, will, not_, fit)',
      '    ',
      '  end',
      '}',
    },
    {
      'local ft = {',
      '  zx = 3,',
      '  tablefun = function(',
      '    inordinate,',
      '    amounts,',
      '    of,',
      '    parameters,',
      '    which,',
      '    cannot,',
      '    and_,',
      '    will,',
      '    not_,',
      '    fit',
      '  )',
      '    ',
      '  end',
      '  ',
      '}',
    }
  ),

  prep({
    'local t1 = {',
    '  a1 = 1,',
    '  t2 = {',
    '    a2 = 2,',
    '    t3 = {',
    '      a3 = 3,',
    '      t4 = {',
    '        tablefun = function(deeply, nested, tables, lots, of, params)',
    '         ',
    '        end',
    '      }',
    '    }',
    '  }',
    '}',
  }, {
    'local t1 = {',
    '  a1 = 1,',
    '  t2 = {',
    '    a2 = 2,',
    '    t3 = {',
    '      a3 = 3,',
    '      t4 = {',
    '        tablefun = function(',
    '          deeply,',
    '          nested,',
    '          tables,',
    '          lots,',
    '          of,',
    '          params',
    '        )',
    '          ',
    '        end',
    '        ',
    '      }',
    '    }',
    '  }',
    '}',
  }),

  prep({
    'local call = localfun(inordinate, amounts, of, parameters, which, cannot, possibly, fit)',
  }, {
    'local call = localfun(',
    '  inordinate,',
    '  amounts,',
    '  of,',
    '  parameters,',
    '  which,',
    '  cannot,',
    '  possibly,',
    '  fit',
    ')',
  }
  ),
  prep({
    'local invoke = O:method(inordinate, amounts, of, parameters, which, cannot, possibly, fit)',
  }, {
    'local invoke = O:method(',
    '  inordinate,',
    '  amounts,',
    '  of,',
    '  parameters,',
    '  which,',
    '  cannot,',
    '  possibly,',
    '  fit',
    ')',
  }
  ),

  --- nested calls
  prep('local computedValue = function1(function2(function3(function4(function5()))))', {
    'local computedValue = function1(',
    '  function2(function3(function4(function5())))',
    ')',
  }),
  prep('local computedValue = there(really(should(be(a(pipe(operator()))))))', {
    'local computedValue = there(',
    '  really(should(be(a(pipe(operator())))))',
    ')',
  }),
  prep(
    'local computedValue = there(really(should(be(a(pipe(operator(so, this, could, go, more, smoothly, and_, in_, a, readable, fashion)))))))',
    {
      'local computedValue = there(really(should(be(a(pipe(operator(',
      '  so,',
      '  this,',
      '  could,',
      '  go,',
      '  more,',
      '  smoothly,',
      '  and_,',
      '  in_,',
      '  a,',
      '  readable,',
      '  fashion',
      ')))))))',
    }),
}

local functions = {
  prep({ 'fun(1)' }),
  prep({ 'fun(1)', 'fun(3)' }),
  prep({
    'love.draw = function()',
    '  draw()',
    'end',
  }, {
    'function love.draw()',
    '  draw()',
    'end',
  }),
  prep({
    'function love.draw()',
    '  draw()',
    'end',
  }),
  prep('local function x() end', {
    'local function x()',
    '  ',
    'end',
  }),
  prep('local x = function() end', {
    'local x = function()',
    '  ',
    'end',
    --- TODO #46 consistent function sugar
    --   'local function x()',
    --   '  ',
    --   'end',
  }
  ),
}
local self = {
  prep({
    '--- @param t string|string[]',
    'function InputController:set_text(t)',
    '  self.model:set_text(t)',
    'end',
  }),
  prep({
    '--- @protected',
    '--- @param w integer',
    '--- @param text string[]?',
    'function WrappedText:_init(w, text)',
    '  if type(w) ~= "number" or w < 1 then',
    "    error('invalid wrap length')",
    '  end',
    '  self.text = {}',
    '  self.wrap_w = w',
    '  self.wrap_forward = {}',
    '  self.wrap_reverse = {}',
    '  self.n_breaks = 0',
    '  if text then',
    '    self:wrap(text)',
    '  end',
    'end',
  }, {
    '--- @protected',
    '--- @param w integer',
    '--- @param text string[]?',
    'function WrappedText:_init(w, text)',
    '  if type(w) ~= "number" or w < 1 then',
    '    error("invalid wrap length")',
    '  end',
    '  self.text = { }',
    '  self.wrap_w = w',
    '  self.wrap_forward = { }',
    '  self.wrap_reverse = { }',
    '  self.n_breaks = 0',
    '  if text then',
    '    self:wrap(text)',
    '  end',
    'end',
  }
  ),
  prep({
    '--- @return Range',
    'function VisibleContent:get_range()',
    '  return self.range',
    'end',
  }),

  prep({
    'local t = {',
    '  draw = function(value)',
    '    V:dibujar()',
    '  end',
    '  ',
    '}'
  }),
  prep({
    'local t = {',
    '  draw = function(value)',
    '    V:dibujar()',
    '  end',
    '  ',
    '}'
  }),
  prep({
    'if a == 2 then',
    '  function love.draw(sugar)',
    '    V:dibujar()',
    '  end',
    -- '  ',
    'end',
  }),
  prep({
    'if a == 2 then',
    '  love.draw = function(value)',
    '    V:dibujar()',
    '  end',
    -- '  ',
    'end',
  }, {
    'if a == 2 then',
    '  function love.draw(value)',
    -- '  love.draw = function(value)',
    '    V:dibujar()',
    '  end',
    -- '  ',
    'end',
  }),

  prep({
    'function love.draw(sugar)',
    '  V:dibujar()',
    'end',
  }),
  prep({
    'love.draw = function(value)',
    '  V:dibujar()',
    'end',
  }, {
    'function love.draw(value)',
    -- '  love.draw = function(value)',
    '  V:dibujar()',
    'end',
  }),
}

local canon = {
  prep('if a > b then     return a else return b end', {
    'if b < a then',
    '  return a',
    'else',
    '  return b',
    'end' }
  ),

  prep({
    'local draw = function()    x:draw()    end',
  }, {
    'local draw = function()',
    '  x:draw()',
    'end',
  }),
  --- operators
  prep('if not (a == b) then end',
    {
      'if a ~= b then',
      '  ',
      'end'
    }),
}

local full = {
  prep(sierpinski, sierpinski_res),
  prep(clock, clock_res),
  prep(meta, meta_res),
}

local todo = {
  --- comment
  -- prep({
  --   'love.draw = function()',
  --   '  -- f',
  --   'end',
  -- }),
}
return {
  { 'basics',    basics },
  { 'operators', operators },
  { 'functions', functions },
  { 'self',      self },
  { 'comments',  comments },
  { 'wrap',      wrapping },
  { 'canon',     canon },
  { 'full',      full },

  { 'todo',      todo },
}
