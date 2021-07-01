pcall(require('telescope').load_extension, 'fzy_native')
local actions = require('telescope.actions')
require('telescope').setup{
  defaults = {
    sorting_strategy = "ascending",
    winblend = 0,
    preview_title = "Preview",
    scroll_strategy = 'cycle',
    layout_strategy = 'vertical',
    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-q>"] = actions.send_to_qflist,
        ["<esc>"] = actions.close,
        ["<C-[>"] = actions.close,
        ["<C-c>"] = actions.close,
        ["<c-d>"] = actions.delete_buffer,
      },
      n = {
        ["<esc>"] = actions.close
      },
    },
  },
  pickers = {
    find_files = {
      theme = 'dropdown',
    },
    live_grep = {
      theme = 'dropdown',
    },
    buffers = {
      theme = 'dropdown',
    },
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = true,
      override_file_sorter = true,
    },
  },
}

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
