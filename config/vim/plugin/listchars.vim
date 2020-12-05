lua << EOF
  vim.wo.list = true
  vim.api.nvim_command('set listchars=')
  vim.api.nvim_command('set listchars+=nbsp:⦸')
  vim.api.nvim_command('set listchars+=tab:▷-')
  vim.api.nvim_command('set listchars+=trail:·')
  vim.api.nvim_command('set listchars+=extends:↷')
  vim.api.nvim_command('set listchars+=precedes:↶')
EOF
