return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-ui-select.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
    },
    cmd = "Telescope",
    keys = {
      { "<C-p>", "<cmd>Telescope find_files<cr>" },
      {
        "<Leader>sf",
        function()
          require("telescope.builtin").find_files({ no_ignore = true })
        end,
      },
      { "<leader>:", "<cmd>Telescope command_history<cr>" },
      { "<Leader>?", "<cmd>Telescope builtin<cr>" },
      { "<Leader>/", "<cmd>Telescope live_grep<cr>" },
      { "<localleader><localleader>", "<cmd>Telescope buffers<cr>" },
      { "<leader>sH", "<cmd>Telescope highlights<cr>" },
      { "<Leader>s?", "<cmd>Telescope oldfiles<cr>" },
      { "<Leader>sc", "<cmd>Telescope git_commits<cr>" },
      { "<Leader>se", "<cmd>Telescope resume<cr>" },
      { "<Leader>sd", "<cmd>Telescope diagnostics<cr>" },
      { "<Leader>sh", "<cmd>Telescope help_tags<cr>" },
      { "<Leader>sk", "<cmd>Telescope keymaps<cr>" },
      { '<Leader>s"', "<cmd>Telescope registers<cr>" },
      { "<C-s>", "<cmd>Telescope current_buffer_fuzzy_find<cr>" },
      {
        "<Leader>s-",
        function()
          require("telescope").extensions.file_browser.file_browser({
            path = "%:p:h",
            select_buffer = true,
          })
        end,
      },
      {
        "<Leader>sw",
        function()
          vim.ui.input({ prompt = "Grep For > " }, function(input)
            require("telescope.builtin").grep_string({ search = input, word_match = "-w" })
          end)
        end,
      },
      {
        "<Leader>sW",
        function()
          require("telescope.builtin").grep_string({ word_match = "-w" })
        end,
      },
    },
    config = function()
      local telescope = require("telescope")
      local actions = require("telescope.actions")
      local themes = require("telescope.themes")
      local action_state = require("telescope.actions.state")
      local action_layout = require("telescope.actions.layout")

      telescope.setup({
        defaults = {
          layout_strategy = "flex",
          path_display = { "shorten" },
          mappings = {
            i = {
              ["<C-k>"] = actions.nop,
              ["<C-space>"] = actions.nop,
              ["<C-s>"] = actions.select_horizontal,
              ["<C-f>"] = actions.preview_scrolling_right,
              ["<C-b>"] = actions.preview_scrolling_left,
              ["<a-l>"] = actions.smart_send_to_loclist + actions.open_loclist,
              ["<a-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
              ["<a-p>"] = action_layout.toggle_preview,
              ["<a-f>"] = actions.to_fuzzy_refine,
            },
            n = {
              ["<M-p>"] = action_layout.toggle_preview,
            },
          },
        },
        pickers = {
          find_files = {
            theme = "ivy",
            find_command = { "fd", "--type", "f", "--color", "never" },
            hidden = true,
            path_display = { "filename_first" },
            previewer = false,
          },
          fzf = {
            fuzzy = true, -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or 'ignore_case' or 'respect_case'
          },
          lsp_references = { theme = "dropdown" },
          lsp_definitions = { theme = "dropdown" },
          lsp_implementations = { theme = "dropdown" },
          lsp_code_actions = { theme = "cursor" },
          code_action = { theme = "cursor" },
          buffers = {
            path_display = { shorten = 3 },
            ignore_current_buffer = false,
            sort_mru = true,
            theme = "dropdown",
            previewer = false,
            layout_config = {
              height = 20,
            },
            mappings = {
              i = {
                ["<c-d>"] = actions.delete_buffer,
                ["<a-d>"] = function(prompt_bufnr)
                  local current_picker = action_state.get_current_picker(prompt_bufnr)
                  current_picker:delete_selection(function(selection)
                    vim.api.nvim_buf_delete(selection.bufnr, { force = true })
                  end)
                end,
              },
            },
          },
          marks = {
            mappings = {
              i = {
                ["<a-d>"] = actions.delete_mark,
              },
            },
          },
        },
        extensions = {
          ["ui-select"] = {
            themes.get_cursor(),
          },
          file_browser = {
            hijack_netrw = false,
            grouped = true,
          },
        },
      })
      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")
      telescope.load_extension("file_browser")
    end,
  },
}
