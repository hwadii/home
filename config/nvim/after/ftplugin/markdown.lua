vim.opt_local.spell = true
vim.opt_local.spelllang = "en"
vim.opt_local.textwidth = 80
vim.opt_local.shiftwidth = 2
vim.opt_local.conceallevel = 2
vim.api.nvim_buf_create_user_command(0, "ToggleCheckbox", function()
  vim.cmd("s/- \\[ \\]/- [x]/")
  vim.cmd.nohl()
end, {})
