pcall(require('telescope').load_extension, 'fzy_native')
local actions = require('telescope.actions')
require('telescope').setup{
  defaults = {
    sorting_strategy = "ascending",
    winblend = 0,
    preview_title = "Preview",
    scroll_strategy = 'cycle',
    layout_strategy = 'vertical',
    path_display = { 'shorten' },
    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-q>"] = actions.send_to_qflist,
        ["<a-q>"] = actions.send_selected_to_qflist,
        ["<esc>"] = actions.close,
        ["<C-[>"] = actions.close,
        ["<C-c>"] = actions.close,
      },
      n = {
        ["<esc>"] = actions.close
      },
    },
  },
  pickers = {
    find_files = {
      theme = 'ivy',
      find_command = { "fd", "--hidden", "-E.git", "-tf" }
    },
    git_files = {
      theme = 'ivy',
    },
    live_grep = {
      theme = 'ivy',
    },
    buffers = {
      theme = 'dropdown',
      previewer = false,
      sort_lastused = true,
      mappings = {
        i = {
          ["<c-d>"] = actions.delete_buffer,
        }
      }
    },
    lsp_code_actions = {
      theme = 'cursor',
    },
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = false, -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
    },
  },
}
