local M = {}

M.ploy_dark = {
  -- The default text color
  foreground = "#e0e0e0",
  -- The default background color
  background = "#0d0d0d",

  -- Overrides the cell background color when the current cell is occupied by the
  -- cursor and the cursor style is set to Block
  cursor_bg = "#e0e0e0",
  -- Overrides the text color when the current cell is occupied by the cursor
  cursor_fg = "#1a1a19",

  cursor_border = "#dbdbdb",

  -- the foreground color of selected text
  selection_fg = "#e0e0e0",
  -- the background color of selected text
  selection_bg = "#614444",

  -- The color of the scrollbar "thumb"; the portion that represents the current viewport
  scrollbar_thumb = "#222222",

  -- The color of the split lines between panes
  split = "#444444",

  ansi = {
    "#222222",
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
  copy_mode_active_highlight_bg = { Color = "#ffc591" },
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

M.ploy_light = {
  -- The default text color
  foreground = "#222222",
  -- The default background color
  background = "#fff8f0",

  -- Overrides the cell background color when the current cell is occupied by the
  -- cursor and the cursor style is set to Block
  cursor_bg = "#085eff",
  -- Overrides the text color when the current cell is occupied by the cursor
  cursor_fg = "#feffff",

  cursor_border = "#085eff",

  -- the foreground color of selected text
  selection_fg = "#222222",
  -- the background color of selected text
  selection_bg = "#b4cfff",

  -- The color of the scrollbar "thumb"; the portion that represents the current viewport
  scrollbar_thumb = "#222222",

  -- The color of the split lines between panes
  split = "#baafba",

  ansi = {
    "#000000",
    "#cc3333",
    "#217a3c",
    "#8f5a3a",
    "#375cd8",
    "#ba35af",
    "#1f6fbf",
    "#cbbcaf",
  },
  brights = {
    "#222222",
    "#a2403f",
    "#61756c",
    "#765640",
    "#6060d0",
    "#af569f",
    "#406f90",
    "#c7c0ba",
  },

  -- Since: 20220319-142410-0fcdea07
  -- When the IME, a dead key or a leader key are being processed and are effectively
  -- holding input pending the result of input composition, change the cursor
  -- to this color to give a visual cue about the compose state.
  compose_cursor = "#856f4a",

  -- Colors for copy_mode and quick_select
  -- available since: 20220807-113146-c2fee766
  -- In copy_mode, the color of the active text is:
  -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
  -- 2. selection_* otherwise
  copy_mode_active_highlight_bg = { Color = "#ffc591" },
  -- use `AnsiColor` to specify one of the ansi color palette values
  -- (index 0-15) using one of the names "Black", "Maroon", "Green",
  --  "Olive", "Navy", "Purple", "Teal", "Silver", "Grey", "Red", "Lime",
  -- "Yellow", "Blue", "Fuchsia", "Aqua" or "White".
  copy_mode_active_highlight_fg = { Color = "#1a1a19" },
  copy_mode_inactive_highlight_bg = { Color = "#d1d1d1" },
  copy_mode_inactive_highlight_fg = { Color = "#1a1a19" },

  quick_select_label_bg = { Color = "#ffcfbf" },
  quick_select_label_fg = { Color = "#dd1100" },
  quick_select_match_bg = { Color = "#e7e0da" },
  quick_select_match_fg = { Color = "#856f4a" },
}

return M
