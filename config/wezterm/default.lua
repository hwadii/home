local wezterm = require("wezterm")

return {
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
}
