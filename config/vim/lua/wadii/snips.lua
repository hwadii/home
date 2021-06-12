local s = require('snippets')
local U = require('snippets.utils')

-- s.use_suggested_mappings()

s.snippets = {
  _global = {
    todo = "TODO(wadii)",
    uname = function() return vim.loop.os_uname().sysname end,
  },
  lua = {
    req = [[local ${2:$1} = require('$1')]]
  },
  typescript = {
    ['if'] = U.match_indentation [[
if ($1) {
  $0
}]],
    ife = U.match_indentation [[
if ($1) {
  $2
} else {
  $3
}
$0]]
  }
}
