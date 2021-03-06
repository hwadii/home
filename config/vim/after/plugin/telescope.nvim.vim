lua << EOF
local actions = require('telescope.actions')
require('telescope').setup{
  defaults = {
    winblend = 0,
    preview_cutoff = 120,
    layout_strategy = 'horizontal',
    selection_strategy = 'reset',
    sorting_strategy = 'descending',
    scroll_strategy = 'cycle',
    prompt_position = 'top',
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
    file_previewer = require('telescope.previewers').vim_buffer_cat.new,
    grep_previewer = require('telescope.previewers').vim_buffer_vimgrep.new,
    qlfit_previewer = require('telescope.previewers').vim_buffer_qflist.new,
  }
}
require('telescope').load_extension('fzy_native')
EOF
