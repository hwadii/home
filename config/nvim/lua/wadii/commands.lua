vim.api.nvim_create_user_command("Gpf", "Git push --force-if-includes --force-with-lease", {})
vim.api.nvim_create_user_command("Gpff", "Git push --force", {})
vim.api.nvim_create_user_command("Gupa", "Git pull --rebase --autostash", {})
vim.api.nvim_create_user_command("Gfa", "Git fetch --all --prune", {})
vim.api.nvim_create_user_command("Gh", "!gh <f-args>", { nargs = 1 })
vim.api.nvim_create_user_command("Gbr", "Gh browse", {})
vim.api.nvim_create_user_command("Gpr", "Gh p", {})
vim.api.nvim_create_user_command("Browse", function(opts)
  if opts.args == "" then
    vim.ui.open(vim.api.nvim_buf_get_name(0))
  else
    vim.ui.open(opts.args)
  end
end, { nargs = "?" })
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
vim.api.nvim_create_user_command("Calc", function()
  local highlight_input = function(input)
    local parser = vim.treesitter.get_string_parser(input, "lua")
    local tree = parser:parse()[1]
    local query = vim.treesitter.query.get("lua", "highlights")
    local highlights = {}
    for id, node in query:iter_captures(tree:root(), input) do
      local _, cstart, _, cend = node:range()
      table.insert(highlights, { cstart, cend, "@" .. query.captures[id] })
    end
    return highlights
  end
  vim.ui.input({ prompt = "Quick calc: ", highlight = highlight_input }, function(input)
    local r = require("wadii.calc").quick(input)
    vim.cmd.redraw()
    vim.fn.setreg("+", r)
    vim.notify("Result: " .. input .. " => " .. r)
  end)
end, {})
