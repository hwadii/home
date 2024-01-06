vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank({ higroup = 'IncSearch' })
  end,
  group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
  pattern = '*',
})

vim.api.nvim_create_autocmd({ 'BufEnter', 'WinEnter' }, {
  callback = require('wadii.statusline').define,
  group = vim.api.nvim_create_augroup('statusline', { clear = true })
})
