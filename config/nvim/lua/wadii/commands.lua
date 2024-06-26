vim.api.nvim_create_user_command("Gpf", "Git push --force-if-includes --force-with-lease", {})
vim.api.nvim_create_user_command("Gpff", "Git push --force", {})
vim.api.nvim_create_user_command("Gupa", "Git pull --rebase --autostash", {})
vim.api.nvim_create_user_command("Gfa", "Git fetch --all --prune", {})
vim.api.nvim_create_user_command("Gh", "!gh <f-args>", { nargs = 1 })
vim.api.nvim_create_user_command("Gbr", "Gh browse", {})
vim.api.nvim_create_user_command("Gpr", "Gh p", {})
vim.api.nvim_create_user_command("Browse", function(opts)
  vim.ui.open(opts.args)
end, { nargs = 1 })
vim.api.nvim_create_user_command("Drop", function(opts)
  if opts.args == "show" then
    require("drop").show()
  elseif opts.args == "hide" then
    require("drop").hide()
  end
end, {
  nargs = 1,
  complete = function()
    return { "show", "hide" }
  end,
})
