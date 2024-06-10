local statusline = {}

statusline.fileprefix = function()
  local basename = vim.fn.fnamemodify(vim.fn.expand("%:h"), ":p:~:.")
  if basename == "" or basename == "." then
    return ""
  else
    return require("plenary.path"):new(basename:gsub("/$", "") .. "/"):shorten()
  end
end

statusline.branch = function()
  return vim.b.gitsigns_head
end

statusline.navic = function()
  return require("nvim-navic").get_location()
end

statusline.line_and_column = function()
  local rhs = ""

  if vim.fn.winwidth(0) > 80 then
    local column = vim.fn.virtcol(".")
    local width = vim.fn.virtcol("$")
    local line = vim.api.nvim_win_get_cursor(0)[1]
    local height = vim.api.nvim_buf_line_count(0)

    rhs = rhs .. math.floor(line / height * 100) .. "%" .. " • "

    -- Add padding to stop RHS from changing too much as we move the cursor.
    local padding = #tostring(height) - #tostring(line)
    if padding > 0 then
      rhs = rhs .. (" "):rep(padding)
    end

    rhs = rhs .. "ℓ " -- (Literal, \u2113 "SCRIPT SMALL L").
    rhs = rhs .. line
    rhs = rhs .. " c " -- (Literal, \u1d68c "MATHEMATICAL MONOSPACE SMALL C").
    rhs = rhs .. column
    rhs = rhs .. "/"
    rhs = rhs .. width
    rhs = rhs .. " "

    -- Add padding to stop rhs from changing too much as we move the cursor.
    if #tostring(column) < 2 then
      rhs = rhs .. " "
    end
    if #tostring(width) < 2 then
      rhs = rhs .. " "
    end
  end

  return rhs
end

statusline.gitstatus = function()
  return vim.b.gitsigns_status
end

statusline.fileformat = function()
  return vim.bo.fileformat
end

statusline.right_hand_side = function()
  local all_components = {
    statusline.gitstatus(),
    statusline.fileformat(),
    statusline.branch(),
    statusline.line_and_column(),
  }

  local usable_components = vim.tbl_filter(function(component)
    return component and #component > 0
  end, all_components)

  return table.concat(usable_components, " • ")
end

statusline.define = function()
  vim.opt_local.statusline = ""
    .. "» "
    .. "%{v:lua.require'wadii.statusline'.fileprefix()}"
    .. "%t"
    .. " "
    .. "%m"
    .. "%y"
    .. "%r"
    .. " "
    .. "%="
    .. "%{v:lua.require'wadii.statusline'.right_hand_side()}"
end

return statusline
