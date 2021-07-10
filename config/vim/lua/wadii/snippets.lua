local s = require('snippets')
local U = require('snippets.utils')

-- s.use_suggested_mappings()
vim.api.nvim_set_keymap('i', '<a-j>', '<cmd>lua return require"snippets".expand_or_advance(1)<CR>', {expr = false, noremap = true})
vim.api.nvim_set_keymap('i', '<a-k>', '<cmd>lua return require"snippets".advance_snippet(-1)<CR>', {expr = false, noremap = true})

s.snippets = {
  _global = {
    todo = "TODO(wadii)",
    uname = function() return vim.loop.os_uname().sysname end,
  },
  lua = {
    req = [[local ${2:$1} = require('$1')]]
  },
  ruby = {
    rld = 'Rails.logger.debug $1',
    rli = 'Rails.logger.info $1'
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
