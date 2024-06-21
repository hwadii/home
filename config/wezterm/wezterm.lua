local wezterm = require("wezterm")
local mux = wezterm.mux

wezterm.on("gui-startup", function(cmd)
  local _, _, window = mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

wezterm.on("update-right-status", function(window)
  window:set_right_status(window:active_workspace())
end)

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
    family = "Berkeley Mono",
    weight = "Regular",
    stretch = "Normal",
    harfbuzz_features = { "ss03", "ss04" },
  }),
  font_size = 14.5,
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
  keys = {
    {
      key = "O",
      mods = "CTRL",
      action = wezterm.action.QuickSelectArgs({
        label = "open url",
        patterns = {
          "https?://\\S+|s3://\\S+|file:\\S+",
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
          "https?://\\S+|s3://\\S+|file:\\S+",
        },
        action = wezterm.action.CopyTo("ClipboardAndPrimarySelection"),
      }),
    },
    {
      key = "w",
      mods = "LEADER|SHIFT",
      action = wezterm.action.PromptInputLine({
        description = wezterm.format({
          { Attribute = { Intensity = "Bold" } },
          { Text = "Enter name for new workspace" },
        }),
        action = wezterm.action_callback(function(window, pane, line)
          if line then
            window:perform_action(
              wezterm.action.SwitchToWorkspace({
                name = line,
              }),
              pane
            )
          end
        end),
      }),
    },
    {
      key = "s",
      mods = "LEADER",
      action = wezterm.action.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }),
    },
    {
      key = ".",
      mods = "LEADER",
      action = wezterm.action.PromptInputLine({
        description = wezterm.format({
          { Attribute = { Intensity = "Bold" } },
          { Text = "Enter new name for workspace" },
        }),
        action = wezterm.action_callback(function(_, _, line)
          wezterm.mux.rename_workspace(wezterm.mux.get_active_workspace(), line)
        end),
      }),
    },
    {
      key = "q",
      mods = "LEADER|CTRL",
      action = wezterm.action({ SendKey = { key = "q", mods = "LEADER|CTRL" } }),
    },
    {
      key = '"',
      mods = "LEADER|SHIFT",
      action = wezterm.action({ SplitVertical = { domain = "CurrentPaneDomain" } }),
    },
    {
      key = "%",
      mods = "LEADER|SHIFT",
      action = wezterm.action({ SplitHorizontal = { domain = "CurrentPaneDomain" } }),
    },
    { key = "z", mods = "LEADER", action = "TogglePaneZoomState" },
    { key = "a", mods = "LEADER", action = "ActivateLastTab" },
    { key = "c", mods = "LEADER", action = wezterm.action({ SpawnTab = "CurrentPaneDomain" }) },
    { key = "p", mods = "LEADER", action = wezterm.action({ ActivateTabRelative = -1 }) },
    { key = "p", mods = "LEADER|CTRL", action = wezterm.action({ ActivateTabRelative = -1 }) },
    { key = "n", mods = "LEADER", action = wezterm.action({ ActivateTabRelative = 1 }) },
    { key = "n", mods = "LEADER|CTRL", action = wezterm.action({ ActivateTabRelative = 1 }) },
    { key = "h", mods = "LEADER", action = wezterm.action({ ActivatePaneDirection = "Left" }) },
    {
      key = "h",
      mods = "LEADER|CTRL",
      action = wezterm.action({ ActivatePaneDirection = "Left" }),
    },
    { key = "j", mods = "LEADER", action = wezterm.action({ ActivatePaneDirection = "Down" }) },
    {
      key = "j",
      mods = "LEADER|CTRL",
      action = wezterm.action({ ActivatePaneDirection = "Down" }),
    },
    { key = "k", mods = "LEADER", action = wezterm.action({ ActivatePaneDirection = "Up" }) },
    { key = "k", mods = "LEADER|CTRL", action = wezterm.action({ ActivatePaneDirection = "Up" }) },
    { key = "l", mods = "LEADER", action = wezterm.action({ ActivatePaneDirection = "Right" }) },
    {
      key = "l",
      mods = "LEADER|CTRL",
      action = wezterm.action({ ActivatePaneDirection = "Right" }),
    },
    { key = "1", mods = "LEADER", action = wezterm.action({ ActivateTab = 0 }) },
    { key = "2", mods = "LEADER", action = wezterm.action({ ActivateTab = 1 }) },
    { key = "3", mods = "LEADER", action = wezterm.action({ ActivateTab = 2 }) },
    { key = "4", mods = "LEADER", action = wezterm.action({ ActivateTab = 3 }) },
    { key = "5", mods = "LEADER", action = wezterm.action({ ActivateTab = 4 }) },
    { key = "6", mods = "LEADER", action = wezterm.action({ ActivateTab = 5 }) },
    { key = "7", mods = "LEADER", action = wezterm.action({ ActivateTab = 6 }) },
    { key = "8", mods = "LEADER", action = wezterm.action({ ActivateTab = 7 }) },
    { key = "9", mods = "LEADER", action = wezterm.action({ ActivateTab = 8 }) },
    {
      key = "&",
      mods = "LEADER|SHIFT",
      action = wezterm.action({ CloseCurrentTab = { confirm = true } }),
    },
    {
      key = "d",
      mods = "LEADER",
      action = wezterm.action({ CloseCurrentPane = { confirm = true } }),
    },
    {
      key = "x",
      mods = "LEADER",
      action = wezterm.action({ CloseCurrentPane = { confirm = true } }),
    },
    {
      key = "[",
      mods = "LEADER",
      action = wezterm.action.ActivateCopyMode,
    },
    { key = "]", mods = "LEADER", action = wezterm.action.PasteFrom("Clipboard") },
    { key = "w", mods = "LEADER", action = wezterm.action.ShowTabNavigator },
  },
  key_tables = {
    copy_mode = copy_mode,
    search_mode = search_mode,
  },
  colors = {
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
  },
}
