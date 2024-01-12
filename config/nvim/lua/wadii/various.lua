local M = {}
local Path = require('plenary.path')

M.yank_path = function(bufnr, register)
  bufnr = bufnr or 0
  register = register or "+"
  local relative_path = Path:new(vim.api.nvim_buf_get_name(bufnr)):make_relative(vim.loop.cwd())
  vim.fn.setreg(register, relative_path)
  if #relative_path ~= 0 then
    vim.notify('Path yanked into "' .. register)
  end
end

M.yank_current_path = function(register)
  register = register or "+"
  -- Make relative in case "%" does not contain the relative path for some
  -- reason.
  local relative_path = Path:new(vim.fn.getreg("%")):make_relative(vim.loop.cwd())
  vim.fn.setreg(register, relative_path)
  if #relative_path ~= 0 then
    vim.notify('Path yanked into "' .. register)
  end
end

M.make_unix = function()
  vim.cmd("set ff=unix")
  vim.opt_local.bomb = true
  vim.opt_local.bomb = false
  vim.cmd("%s/\r//ge")
end

M.toggle_minifiles = function(...)
  local minifile = require('mini.files')
  if not minifile.close() then minifile.open(..., nil) end
end

return M
