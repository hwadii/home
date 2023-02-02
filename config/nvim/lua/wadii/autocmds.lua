vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank({ higroup = 'IncSearch', timeout = 1000 })
  end,
  group = vim.api.nvim_create_augroup('yank_post_group', { clear = true }),
})

