local wezterm = require("wezterm")
local mux = wezterm.mux
local config = wezterm.config_builder()

config.term = "wezterm"
config.font = wezterm.font_with_fallback({ "Berkeley Mono", "Symbols Nerd Font Mono" })
config.font_size = 11
config.enable_wayland = false
config.underline_thickness = 1
config.enable_tab_bar = true
config.tab_bar_at_bottom = true
config.window_close_confirmation = "NeverPrompt"
config.use_ime = false
config.line_height = 1
config.enable_scroll_bar = false
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}
config.quick_select_alphabet = "jfkdls;ahgurieowpq"
config.enable_kitty_keyboard = true

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local title = string.gsub(tab.active_pane.foreground_process_name, "(.*[/\\])(.*)", "%2")
  return {
    { Text = " " .. title .. " " },
  }
end)

wezterm.on("update-right-status", function(window, pane)
  window:set_right_status(window:active_workspace())
end)

wezterm.on("gui-startup", function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

config.leader = { key = "q", mods = "CTRL", timeout = 1000 }

config.keys = {
  {
    key = "q",
    mods = "LEADER|CTRL",
    action = wezterm.action.SendKey({ key = "q", mods = "CTRL" }),
  },
  {
    key = "O",
    mods = "CTRL",
    action = wezterm.action.QuickSelectArgs({
      label = "open url",
      patterns = {
        "https://\\S+|s3://\\S+",
      },
      action = wezterm.action_callback(function(window, pane)
        local url = window:get_selection_text_for_pane(pane)
        wezterm.log_info("opening: " .. url)
        wezterm.open_with(url)
      end),
    }),
  },
  {
    key = "Y",
    mods = "CTRL",
    action = wezterm.action.QuickSelectArgs({
      label = "copy url",
      patterns = {
        "https://\\S+|s3://\\S+",
      },
      action = wezterm.action.CopyTo("ClipboardAndPrimarySelection"),
    }),
  },
}

config.colors = {
  -- The default text color
  foreground = "#d1d1d1",
  -- The default background color
  background = "#1a1a19",

  -- Overrides the cell background color when the current cell is occupied by the
  -- cursor and the cursor style is set to Block
  cursor_bg = "#dbdbdb",
  -- Overrides the text color when the current cell is occupied by the cursor
  cursor_fg = "#1a1a19",

  cursor_border = "#dbdbdb",

  -- the foreground color of selected text
  selection_fg = "#1a1a19",
  -- the background color of selected text
  selection_bg = "#fffacd",

  -- The color of the scrollbar "thumb"; the portion that represents the current viewport
  scrollbar_thumb = "#222222",

  -- The color of the split lines between panes
  split = "#444444",

  ansi = {
    "#1a1a19",
    "#ff968c",
    "#7bb099",
    "#ffc591",
    "#97B2C9",
    "#de9bc8",
    "#61957f",
    "#d1d1d1",
  },
  brights = {
    "#4c4c4b",
    "#ffafa5",
    "#94c9b2",
    "#ffdeaa",
    "#a6cded",
    "#f7b4e1",
    "#7aae98",
    "#eaeaea",
  },

  -- Since: 20220319-142410-0fcdea07
  -- When the IME, a dead key or a leader key are being processed and are effectively
  -- holding input pending the result of input composition, change the cursor
  -- to this color to give a visual cue about the compose state.
  compose_cursor = "#ffc591",

  -- Colors for copy_mode and quick_select
  -- available since: 20220807-113146-c2fee766
  -- In copy_mode, the color of the active text is:
  -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
  -- 2. selection_* otherwise
  copy_mode_active_highlight_bg = { Color = "#000000" },
  -- use `AnsiColor` to specify one of the ansi color palette values
  -- (index 0-15) using one of the names "Black", "Maroon", "Green",
  --  "Olive", "Navy", "Purple", "Teal", "Silver", "Grey", "Red", "Lime",
  -- "Yellow", "Blue", "Fuchsia", "Aqua" or "White".
  copy_mode_active_highlight_fg = { Color = "#1a1a19" },
  copy_mode_inactive_highlight_bg = { Color = "#d1d1d1" },
  copy_mode_inactive_highlight_fg = { Color = "#1a1a19" },

  quick_select_label_bg = { Color = "#ffc591" },
  quick_select_label_fg = { Color = "#1a1a19" },
  quick_select_match_bg = { Color = "#444444" },
  quick_select_match_fg = { Color = "#d1d1d1" },
}

return config
