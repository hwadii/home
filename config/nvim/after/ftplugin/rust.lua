vim.api.nvim_create_user_command('Cargo', function(opts)
  local cmd = 'cargo ' .. vim.trim(opts.args)
  local buf = vim.api.nvim_create_buf(true, true)
  vim.api.nvim_command("below split")
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_command('startinsert')
  vim.fn.termopen(cmd)
end, { nargs = '+' })

vim.api.nvim_create_user_command('Crun', function(opts)
  vim.api.nvim_command("Cargo run " .. vim.trim(opts.args))
end, { nargs = '*' })

vim.api.nvim_create_user_command('Ccheck', 'Cargo check', { nargs = 0 })
