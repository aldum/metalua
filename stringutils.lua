string.normalize = function(s)
  return string.gsub(s, "%s+", "")
end

string.is_non_empty_string = function(s, no_trim)
  if s and type(s) == 'string' and s ~= '' then
    local str = (function()
      if no_trim then
        return s
      else
        return string.normalize(s)
      end
    end)()
    if str ~= '' then
      return true
    end
  end
  return false
end

string.is_non_empty_string_array = function(sa)
  if type(sa) ~= 'table' then
    return false
  else
    for _, s in ipairs(sa) do
      if string.is_non_empty_string(s) then
        return true
      end
    end
    return false
  end
end

-- https://stackoverflow.com/a/51893646
string.split = function(str, delimiter)
  local del = delimiter or ' '
  if str and type(str) == 'string' and string.is_non_empty_string(str, true) then
    local result               = {}
    local from                 = 1
    local delim_from, delim_to = string.find(str, del, from)
    while delim_from do
      table.insert(result, string.sub(str, from, delim_from - 1))
      from                 = delim_to + 1
      delim_from, delim_to = string.find(str, del, from)
    end
    table.insert(result, string.sub(str, from))
    return result
  else
    return {}
  end
end

string.split_array = function(str_arr, char)
  if not type(str_arr) == 'table' then return {} end
  local words = {}
  for _, line in ipairs(str_arr) do
    local ws = string.split(line, char)
    for _, word in ipairs(ws) do
      table.insert(words, word)
    end
  end
  return words
end

string.lines = function(s)
  if type(s) == 'string' then
    return string.split(s, '\n')
  end
  if type(s) == 'table' then
    return string.split_array(s, '\n')
  end
end

string.join = function(strs, char)
  local res = ''
  if type(strs) == 'table' then
    local j = char or ' '
    for i, word in ipairs(strs) do
      res = res .. word
      if i ~= #strs then
        res = res .. j
      end
    end
  end
  if type(strs) == 'string' then
    res = strs
  end
  return res
end
