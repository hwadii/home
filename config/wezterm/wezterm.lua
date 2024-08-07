local wezterm = require("wezterm")
local colors = require("colorschemes")
local tmux_keys = require("keys")
local keys = require("default")
local mux = wezterm.mux

wezterm.on("gui-startup", function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

wezterm.on("update-right-status", function(window)
  window:set_right_status(window:active_workspace())
end)

for key, value in pairs(tmux_keys) do
  keys[key] = value
end
local default_key_tables = wezterm.gui.default_key_tables()
local copy_mode = default_key_tables.copy_mode
table.insert(copy_mode, {
  key = "y",
  mods = "NONE",
  action = wezterm.action.Multiple({
    wezterm.action.CopyTo("ClipboardAndPrimarySelection"),
    wezterm.action.CopyMode("ClearSelectionMode"),
  }),
})
table.insert(copy_mode, {
  key = "/",
  mods = "NONE",
  action = wezterm.action.Search("CurrentSelectionOrEmptyString"),
})
table.insert(copy_mode, {
  key = "?",
  mods = "SHIFT",
  action = wezterm.action.Search("CurrentSelectionOrEmptyString"),
})
table.insert(copy_mode, {
  key = "[",
  mods = "CTRL",
  action = wezterm.action.CopyMode("ClearSelectionMode"),
})
table.insert(copy_mode, {
  key = "Escape",
  mods = "NONE",
  action = wezterm.action.CopyMode("ClearSelectionMode"),
})
table.insert(copy_mode, {
  key = "n",
  mods = "NONE",
  action = wezterm.action.CopyMode("NextMatch"),
})
table.insert(copy_mode, {
  key = "N",
  mods = "NONE",
  action = wezterm.action.CopyMode("PriorMatch"),
})
local search_mode = default_key_tables.search_mode
table.insert(search_mode, {
  key = "c",
  mods = "CTRL",
  action = wezterm.action.Multiple({
    wezterm.action.ActivateCopyMode,
    wezterm.action.CopyMode("ClearSelectionMode"),
  }),
})
table.insert(search_mode, {
  key = "Enter",
  mods = "NONE",
  action = wezterm.action.CopyMode("AcceptPattern"),
})

return {
  term = "wezterm",
  font = wezterm.font({
    family = "Iosevka Comfy Motion Fixed",
    weight = "Regular",
    stretch = "Normal",
    -- harfbuzz_features = { "ss01", "ss04" },
  }),
  font_size = 15,
  underline_thickness = 1,
  default_cursor_style = "SteadyBlock",
  enable_tab_bar = true,
  tab_bar_at_bottom = true,
  window_close_confirmation = "AlwaysPrompt",
  use_ime = false,
  use_fancy_tab_bar = true,
  line_height = 1,
  enable_scroll_bar = false,
  selection_word_boundary = "\t\n {}[]()\"'`.,;:",
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
  quick_select_alphabet = "jfkdls;ahgurieowpq",
  enable_kitty_keyboard = true,
  scrollback_lines = 10000,
  leader = { key = "q", mods = "CTRL", timeout_milliseconds = 5000 },
  keys = keys,
  key_tables = {
    copy_mode = copy_mode,
    search_mode = search_mode,
  },
  window_frame = {
    font = wezterm.font { family = 'Iosevka Aile', weight = 'Bold' },

    -- The size of the font in the tab bar.
    -- Default to 10.0 on Windows but 12.0 on other systems
    font_size = 12.0,

    -- The overall background color of the tab bar when
    -- the window is focused
    active_titlebar_bg = '#333333',

    -- The overall background color of the tab bar when
    -- the window is not focused
    inactive_titlebar_bg = '#333333',
  },
  color_schemes = colors,
  color_scheme = "ploy_light",
}
