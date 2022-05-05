local telescope = require('telescope')
local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
telescope.load_extension('fzf')
telescope.setup{
  defaults = {
    path_display = { 'shorten' },
    mappings = {
      i = {
        ['<C-j>'] = actions.move_selection_next,
        ['<C-k>'] = actions.move_selection_previous,
        ['<C-q>'] = actions.send_to_qflist,
        ['<a-q>'] = actions.send_selected_to_qflist,
        ['<esc>'] = actions.close,
        ['<C-[>'] = actions.close,
        ['<C-c>'] = actions.close,
      },
      n = {
        ['<esc>'] = actions.close
      },
    },
  },
  pickers = {
    find_files = {
      theme = 'ivy',
      find_command = { 'fd', '--hidden', '-E.git', '-tf' },
      path_display = { 'truncate' },
      layout_config = {
        height = 25,
      },
    },
    buffers = {
      theme = 'dropdown',
      previewer = false,
      sort_lastused = true,
      layout_config = {
        height = 20,
      },
      mappings = {
        i = {
          ['<c-d>'] = actions.delete_buffer,
          ['<a-d>'] = function(prompt_bufnr)
            local current_picker = action_state.get_current_picker(prompt_bufnr)
            current_picker:delete_selection(function(selection)
              vim.api.nvim_buf_delete(selection.bufnr, { force = true })
            end)
          end,
        }
      }
    },
  },
  extensions = {
    fuzzy = true,                    -- false will only do exact matching
    override_generic_sorter = true,  -- override the generic sorter
    override_file_sorter = true,     -- override the file sorter
    case_mode = 'smart_case',        -- or 'ignore_case' or 'respect_case'
  },
}
