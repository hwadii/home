local M = {}
local h = require('null-ls.helpers')
local methods = require('null-ls').methods

M.sqlfmt = {
  method = methods.FORMATTING,
  filetypes = { 'sql' },
  meta = {
    name = 'sqlfmt',
  },
  generator = h.formatter_factory({
    command = 'sqlfmt',
    args = { '--use-spaces', '--tab-width=2' },
    to_stdin = true,
  }),
}

return M
