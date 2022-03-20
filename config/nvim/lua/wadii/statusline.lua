local Path = require('plenary.path')
local strings = require('plenary.strings')
local gps = require('nvim-gps')

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
  local basename = vim.fn.fnamemodify(vim.fn.expand('%:h'), ':p:~:.')
  if basename == '' or basename == '.' then
    return ''
  else
    return Path:new(basename:gsub('/$', '') .. '/'):shorten()
  end
end


statusline.branch = function()
  local current_branch = vim.fn['gina#component#repo#branch']() or ''
  if current_branch == '' then
    return current_branch
  else
    return current_branch .. ' • '
  end
end

statusline.gps = function()
  if not gps.is_available() then
    return ''
  end
  local location = gps.get_location()
  if #location > 0 then return location .. ' • ' else return '' end
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
    return status .. ' • '
  end
  return status
end

statusline.active = function()
  vim.opt_local.statusline = ''
  .. '🌸 %<'
  .. '%{v:lua.wadii.statusline.fileprefix()}'
  .. '%1*'
  .. '%t'
  .. '%*'
  .. ' '
  .. '%m'
  .. '%y'
  .. '%r'
  .. ' '
  .. '%='
  .. '%{v:lua.wadii.statusline.gps()}'
  .. '%{v:lua.wadii.statusline.gitstatus()}'
  .. '%{v:lua.wadii.statusline.branch()}'
  .. '%{v:lua.wadii.statusline.right_hand_side()}'
  if vim.opt_local.cursorline:get() == false then
    vim.opt_local.cursorline = true
  end
end

statusline.inactive = function()
  vim.opt_local.statusline = ''
  .. '%{v:lua.wadii.statusline.gutter_padding()}'
  .. '%2*'
  .. '%f'
  .. '%*'
  if vim.opt_local.cursorline:get() == true then
    vim.opt_local.cursorline = false
  end
end

local statusline_group = vim.api.nvim_create_augroup('statusline', { clear = true })

vim.api.nvim_create_autocmd(
  { 'WinEnter', 'BufEnter' },
  {
    callback = function()
      statusline.active()
    end,
    group = statusline_group,
  }
)

return statusline
