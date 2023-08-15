vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank({ higroup = 'IncSearch', timeout = 1000 })
  end,
  group = vim.api.nvim_create_augroup('yank_post_group', { clear = true }),
})

vim.api.nvim_create_autocmd({ 'BufEnter', 'WinEnter' }, {
  callback = require('wadii.statusline').define,
  group = vim.api.nvim_create_augroup('statusline', { clear = true })
})
