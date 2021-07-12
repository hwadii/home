local Path = require('plenary.path')
local strings = require('plenary.strings')

local statusline = {}

statusline.gutter_padding = function()
  local signcolumn = 0
  local option = vim.wo.signcolumn
  if option == 'yes' then
    signcolumn = 2
  elseif option == 'auto' then
    local signs = vim.fn.sign_getplaced('')
    if #signs[1].signs > 0 then
      signcolumn = 2
    end
  end

  local minwidth = 2
  local numberwidth = vim.wo.numberwidth
  local row = vim.api.nvim_buf_line_count(0)
  local gutterwidth = math.max(
    (#tostring(row) + 1),
    minwidth,
    numberwidth
  ) + signcolumn
  local padding = (' '):rep(gutterwidth - 1)
  return padding
end

statusline.fileprefix = function()
  local basename = vim.fn.expand('%:h')
  if basename ~= '' then
    local homedir = vim.loop.os_homedir() .. '/'
    local without_home = string.gsub(basename, homedir, '')
    return Path:new(without_home):shorten() .. '/'
  else
    return ''
  end
end


statusline.branch = function()
  local current_branch = vim.fn['gina#component#repo#branch']()
  if #current_branch > 15 then
    return strings.truncate(current_branch, 15, '>') .. ' · '
  else
    return current_branch .. ' · '
  end
end


statusline.right_hand_side = function()
  local rhs = ''

  if vim.fn.winwidth(0) > 80 then
    local column = vim.fn.virtcol('.')
    local width = vim.fn.virtcol('$')
    local line = vim.api.nvim_win_get_cursor(0)[1]
    local height = vim.api.nvim_buf_line_count(0)

    -- Add padding to stop RHS from changing too much as we move the cursor.
    local padding = #tostring(height) - #tostring(line)
    if padding > 0 then
      rhs = rhs .. (' '):rep(padding)
    end

    rhs = rhs .. 'ℓ ' -- (Literal, \u2113 "SCRIPT SMALL L").
    rhs = rhs .. line
    rhs = rhs .. '/'
    rhs = rhs .. height
    rhs = rhs .. ' c ' -- (Literal, \u1d68c "MATHEMATICAL MONOSPACE SMALL C").
    rhs = rhs .. column
    rhs = rhs .. '/'
    rhs = rhs .. width
    rhs = rhs .. ' '

    -- Add padding to stop rhs from changing too much as we move the cursor.
    if #tostring(column) < 2 then
      rhs = rhs .. ' '
    end
    if #tostring(width) < 2 then
      rhs = rhs .. ' '
    end
  end

  return rhs
end

statusline.gitstatus = function()
  local status = vim.b.gitsigns_status or ''
  if #status > 0 then
    return status .. ' ·'
  end
  return status
end

statusline.active = function()
  vim.opt.statusline = '🌸 %<'
  vim.opt.statusline = vim.opt.statusline + '%{luaeval("require\'wadii.statusline\'.fileprefix()")}'
  vim.opt.statusline = vim.opt.statusline + '%1*'
  vim.opt.statusline = vim.opt.statusline + '%t'
  vim.opt.statusline = vim.opt.statusline + '%*'
  vim.opt.statusline = vim.opt.statusline + ' '
  vim.opt.statusline = vim.opt.statusline + '%m'
  vim.opt.statusline = vim.opt.statusline + '%y'
  vim.opt.statusline = vim.opt.statusline + '%r'
  vim.opt.statusline = vim.opt.statusline + ' '
  vim.opt.statusline = vim.opt.statusline + '%='
  vim.opt.statusline = vim.opt.statusline + '%{luaeval("require\'wadii.statusline\'.gitstatus()")}'
  vim.opt.statusline = vim.opt.statusline + ' '
  vim.opt.statusline = vim.opt.statusline + ('%{luaeval("require(\'wadii.statusline\').branch()")}')
  vim.opt.statusline = vim.opt.statusline + '%*'
  vim.opt.statusline = vim.opt.statusline + '%{luaeval("require(\'wadii.statusline\').right_hand_side()")}'
  if vim.opt.cursorline:get() == false then
    vim.opt.cursorline = true
  end
end

statusline.inactive = function()
  vim.opt.statusline = ''
  vim.opt.statusline = vim.opt.statusline + '%{luaeval("require(\'wadii.statusline\').gutter_padding()")}'
  vim.opt.statusline = vim.opt.statusline + '%2*'
  vim.opt.statusline = vim.opt.statusline + '%f'
  if vim.opt.cursorline:get() == true then
    vim.opt.cursorline = false
  end
  vim.opt.statusline = vim.opt.statusline + '%*'
end

return statusline
