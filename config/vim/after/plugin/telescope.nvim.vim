lua << EOF
local actions = require('telescope.actions')
require('telescope').setup{
  defaults = {
    sorting_strategy = "ascending",
    layout_strategy = "center",
    results_title = false,
    preview_title = "Preview",
    preview_cutoff = 1, -- Preview should always show (unless previewer = false)
    width = 80,
    results_height = 15,
    scroll_strategy = 'cycle',
    borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-q>"] = actions.send_to_qflist,
        ["<esc>"] = actions.close,
        ["<C-[>"] = actions.close,
        ["<C-c>"] = actions.close,
      },
      n = {
        ["<esc>"] = actions.close
      },
    },
  },
}
require('telescope').load_extension('fzy_native')

local M = {}

function M.grep_prompt()
  require('telescope.builtin').grep_string {
    shorten_path = true,
    search = vim.fn.input("Grep String > "),
  }
end

return setmetatable({}, {
  __index = function(_, k)
    reloader()

    if M[k] then
      return M[k]
    else
      return require('telescope.builtin')[k]
    end
  end
  })
EOF
