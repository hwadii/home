local M = {}
local Path = require("plenary.path")

M.yank_path = function(bufnr, register)
  bufnr = bufnr or 0
  register = register or vim.v.register or "\""
  -- Make relative in case "%" does not contain the relative path for some
  -- reason.
  local relative_path = Path:new(vim.api.nvim_buf_get_name(bufnr)):make_relative(vim.loop.cwd())
  vim.fn.setreg(register, relative_path)
  if #relative_path ~= 0 then
    vim.notify('Copied ' .. relative_path .. ' into "' .. register)
  end
end

M.yank_current_path = function(register)
  M.yank_path(0, register)
end

M.make_unix = function()
  vim.cmd("set ff=unix")
  vim.opt_local.bomb = true
  vim.opt_local.bomb = false
  vim.cmd("%s/\r//ge")
  vim.notify("File format is now " .. vim.bo.fileformat)
end

M.zoom = function()
  vim.cmd("wincmd |")
  vim.cmd("wincmd _")
end

return M
