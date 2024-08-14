return {
  {
    "Wansmer/treesj",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      use_default_keymaps = false,
      notify = false,
    },
    keys = {
      { "gS", "<cmd>TSJToggle<cr>", desc = "Join Toggle", mode = "n" },
    },
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    enabled = false,
    opts = {
      fast_wrap = {},
    },
  },
  {
    "ledger/vim-ledger",
    enabled = false,
    init = function()
      vim.g.ledger_align_at = 52
    end,
  },
  {
    "jpalardy/vim-slime",
    enabled = false,
    keys = {
      { "gz", "<Plug>SlimeMotionSend", mode = "n", remap = true, silent = false },
      { "gzz", "<Plug>SlimeLineSend", mode = "n", remap = true, silent = false },
      { "gz", "<Plug>SlimeRegionSend", mode = "x", remap = true, silent = false },
      { "gzc", "<Plug>SlimeConfig", mode = "n", remap = true, silent = false },
    },
    init = function()
      vim.g.slime_target = "neovim"
      vim.g.slime_no_mappings = true
    end,
    config = function()
      vim.g.slime_input_pid = false
      vim.g.slime_suggest_default = true
      vim.g.slime_menu_config = false
      vim.g.slime_neovim_ignore_unlisted = false
    end,
  },
  {
    "uga-rosa/ccc.nvim",
    opts = { empty_point_bg = false, highlight_mode = "virtual" },
    cmd = { "CccHighlighterToggle", "CccPick" },
    keys = {
      { "<leader>cc", "<cmd>CccHighlighterToggle<cr>", mode = "n" },
      { "<leader>cp", "<cmd>CccPick<cr>", mode = "n" },
    },
  },
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-emoji",
      "dcampos/cmp-snippy",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-nvim-lua",
      "onsails/lspkind.nvim",
    },
    opts = function(_, opts)
      local cmp = require("cmp")
      local snippy = require("snippy")
      local lspkind = require("lspkind")
      opts.snippet = {
        expand = function(args)
          snippy.expand_snippet(args.body)
        end,
      }
      opts.mapping = cmp.mapping.preset.insert({
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-e>"] = cmp.mapping.abort({ select = true }),
        ["<C-y>"] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = true }),
        ["<C-]>"] = cmp.mapping(function()
          if snippy.can_expand() then
            snippy.expand()
          end
        end, { "i", "s" }),
        ["<C-l>"] = cmp.mapping(function()
          if snippy.can_jump(1) then
            snippy.next()
          end
        end, { "i", "s" }),

        ["<C-h>"] = cmp.mapping(function()
          if snippy.can_jump(-1) then
            snippy.previous()
          end
        end, { "i", "s" }),
      })
      opts.window = {
        documentation = vim.tbl_extend(
          "force",
          cmp.config.window.bordered({
            winhighlight = "Normal:Normal,FloatBorder:Normal,CursorLine:Visual,Search:None",
          }),
          { max_width = 100 }
        ),
      }
      opts.formatting = {
        format = lspkind.cmp_format({ mode = "text", show_labelDetails = true }),
      }
      opts.sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "nvim_lua" },
        { name = "vim-dadbod-completion" },
        { name = "snippy" },
        { name = "buffer", max_item_count = 10 },
        { name = "path" },
        { name = "emoji" },
      })
      opts.experimental = {
        ghost_text = false,
      }
    end,
  },
  {
    "dcampos/nvim-snippy",
    event = "BufReadPre",
    opts = {},
  },
  { "lewis6991/github_dark.nvim", enabled = false },
  {
    "lewis6991/gitsigns.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    event = "BufReadPre",
    opts = {
      attach_to_untracked = true,
      on_attach = function(bufnr)
        local gitsigns = require("gitsigns")

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map("n", "]c", function()
          if vim.wo.diff then
            vim.cmd.normal({ "]c", bang = true })
          else
            gitsigns.nav_hunk("next")
          end
        end)

        map("n", "[c", function()
          if vim.wo.diff then
            vim.cmd.normal({ "[c", bang = true })
          else
            gitsigns.nav_hunk("prev")
          end
        end)

        map("n", "[C", function()
          gitsigns.nav_hunk("first")
        end)

        map("n", "]C", function()
          gitsigns.nav_hunk("last")
        end)

        -- Actions
        map("n", "<leader>hs", gitsigns.stage_hunk)
        map("n", "<leader>hr", gitsigns.reset_hunk)
        map("v", "<leader>hs", function()
          gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end)
        map("v", "<leader>hr", function()
          gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end)
        map("n", "<leader>hS", gitsigns.stage_buffer)
        map("n", "<leader>hu", gitsigns.undo_stage_hunk)
        map("n", "<leader>hR", gitsigns.reset_buffer)
        map("n", "<leader>hp", gitsigns.preview_hunk)
        map("n", "<leader>hb", function()
          gitsigns.blame_line({ full = true })
        end)
        map("n", "<leader>tb", gitsigns.toggle_current_line_blame)
        map("n", "<leader>hd", gitsigns.diffthis)
        map("n", "<leader>hD", function()
          gitsigns.diffthis("~")
        end)
        map("n", "<leader>td", gitsigns.toggle_deleted)
        map("n", "<leader>gB", gitsigns.blame)

        -- Text object
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
      end,
    },
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
  },
  { "Hoffs/omnisharp-extended-lsp.nvim", ft = "csharp" },
  { "aklt/plantuml-syntax", ft = "plantuml" },
  { "terrastruct/d2-vim", ft = "d2" },
  {
    "stevearc/conform.nvim",
    cmd = { "ConformInfo" },
    opts = {
      formatters_by_ft = {
        ["*"] = { "trim_whitespace", "trim_newlines" },
        sql = { "pg_format" },
        yaml = { "yamlfmt" },
        xml = { "xmllint" },
        json = { "jq" },
        ruby = { "rubocop" },
        lua = { "stylua" },
        cs = { "csharpier" },
        markdown = { "prettierd" },
        python = { "ruff_format" },
      },
    },
    keys = {
      {
        "<localleader>f",
        function()
          require("conform").format({ async = true, lsp_fallback = true })
        end,
        desc = "Format buffer",
        mode = { "n", "v" },
      },
    },
  },
  {
    "echasnovski/mini.operators",
    event = "VeryLazy",
    opts = {
      evaluate = { prefix = "" },
      exchange = { prefix = "" },
      multiply = { prefix = "" },
      replace = { prefix = "" },
      sort = { prefix = "gs" },
    },
  },
  {
    "echasnovski/mini.ai",
    event = "InsertEnter",
    opts = { search_method = "cover_or_next", n_lines = 200 },
  },
  {
    "echasnovski/mini.move",
    event = "VeryLazy",
    opts = {},
  },
  {
    "echasnovski/mini.align",
    event = "VeryLazy",
    opts = {
      mappings = {
        start = "<leader>a",
        start_with_preview = "<leader>A",
      },
    },
  },
  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VeryLazy",
    keys = {
      {
        "-",
        function()
          require("oil").open()
        end,
        mode = "n",
        desc = "Open parent directory in a buffer.",
      },
      {
        "<localleader>-",
        function()
          require("oil").toggle_float()
        end,
        mode = "n",
        desc = "Open parent directory in a buffer.",
      },
    },
    opts = {
      view_options = {
        show_hidden = true,
      },
      columns = { "icon" },
      buf_options = {
        buflisted = false,
        bufhidden = "hide",
      },
      keymaps = {
        ["!"] = "actions.open_terminal",
        ["."] = "actions.open_cmdline",
        ["<C-p>"] = false,
        ["gp"] = "actions.preview",
      },
    },
  },
  {
    "chrisbra/unicode.vim",
    cmd = { "UnicodeSearch", "Digraphs" },
  },
  {
    "preservim/vim-markdown",
    ft = "markdown",
    init = function()
      vim.g.vim_markdown_override_foldtext = 1
      vim.g.vim_markdown_no_default_key_mappings = 1
      vim.g.vim_markdown_emphasis_multiline = 0
      vim.g.vim_markdown_conceal = 1
      vim.g.vim_markdown_conceal_code_blocks = 0
      vim.g.vim_markdown_frontmatter = 1
      vim.g.vim_markdown_borderless_table = 0
      vim.g.vim_markdown_folding_disabled = 1
    end,
  },
  {
    "kylechui/nvim-surround",
    event = "VeryLazy",
    opts = {},
  },
  {
    "andymass/vim-matchup",
    enabled = false,
    event = "BufReadPost",
    opts = {},
    init = function()
      vim.g.matchup_matchparen_offscreen = {}
    end,
  },
  {
    "dstein64/nvim-scrollview",
    enabled = false,
    event = "UIEnter",
    opts = {
      current_only = true,
      winblend_gui = 40,
      signs_on_startup = { "diagnostics", "search", "spell", "marks", "conflicts", "changelist" },
      diagnostics_error_symbol = "×",
      diagnostics_warn_symbol = "!",
      diagnostics_info_symbol = "i",
      diagnostics_hint_symbol = "H",
      excluded_filetypes = { "fugitiveblame", "gitsigns.blame" },
    },
  },
  {
    "tris203/precognition.nvim",
    opts = {},
    cmd = "Precognition",
  },
  {
    "zbirenbaum/copilot.lua",
    dependencies = { "zbirenbaum/copilot-cmp" },
    cmd = "Copilot",
    keys = {
      { "<leader>pt", "<cmd>Copilot toggle<cr>", mode = "n", desc = "Copilot toggle" },
      { "<leader>ps", "<cmd>Copilot status<cr>", mode = "n", desc = "Copilot status" },
    },
    build = ":Copilot auth",
    opts = {
      panel = { enabled = false },
      suggestion = {
        auto_trigger = true,
        keymap = {
          accept = "<M-e>",
          accept_word = "<M-w>",
          accept_line = "<M-l>",
          next = "<M-]>",
          prev = "<M-[>",
          dismiss = "<C-]>",
        },
      },
    },
  },
  {
    "zbirenbaum/copilot-cmp",
    opts = {},
  },
  { "tpope/vim-dispatch", cmd = { "Start", "Make", "Dispatch", "Copen" } },
  { "tpope/vim-abolish", cmd = { "Abolish", "Subvert", "S" }, keys = "cr" },
  { "tpope/vim-unimpaired", keys = { "[", "]", "yo", "=", "<", ">" } },
  { "tpope/vim-sleuth", lazy = false },
  {
    "tpope/vim-fugitive",
    cmd = {
      "Git",
      "GBrowse",
      "GRemove",
      "GRename",
      "Gread",
      "Gwrite",
      "Ggrep",
      "GMove",
      "Gdiffsplit",
      "Gvdiffsplit",
    },
    keys = {
      { "<Leader>gs", "<cmd>Git<cr>", mode = "n" },
      { "<Leader>gb", "<cmd>Git blame<cr>", mode = "n" },
    },
    dependencies = "tpope/vim-rhubarb",
  },
  {
    "NeogitOrg/neogit",
    enabled = false,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    cmd = "Neogit",
    keys = {
      { "<Leader>gs", "<cmd>Neogit<cr>", mode = "n" },
    },
    opts = {
      disable_hint = true,
      kind = "tab",
    },
  },
  {
    "linrongbin16/gitlinker.nvim",
    opts = {},
    cmd = { "GitLink" },
    keys = {
      { "<Leader>gO", "<cmd>GitLink!<cr>", mode = { "n", "v" } },
      { "<Leader>go", "<cmd>GitLink<cr>", mode = { "n", "v" } },
      { "<Leader>gd", "<cmd>GitLink default_branch<cr>", mode = { "v" } },
      { "<Leader>gD", "<cmd>GitLink! default_branch<cr>", mode = { "n", "v" } },
    },
  },
  {
    "tpope/vim-rsi",
    event = { "InsertEnter", "CmdLineEnter" },
  },
  { "tpope/vim-eunuch", event = "CmdLineEnter" },
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = {
      { "tpope/vim-dadbod", lazy = true },
      { "kristijanhusak/vim-dadbod-completion", ft = { "sql", "mysql", "plsql" }, lazy = true },
    },
    cmd = {
      "DBUI",
      "DBUIToggle",
      "DBUIAddConnection",
      "DBUIFindBuffer",
    },
    init = function()
      vim.g.db_ui_use_nerd_fonts = 1
    end,
  },
  {
    "lewis6991/satellite.nvim",
    opts = {
      excluded_filetypes = { "fugitiveblame", "gitsigns.blame" },
      handlers = {
        gitsigns = { enable = false },
      },
      current_only = true,
    },
    event = "VeryLazy",
    enabled = false,
  },
  { "zenbones-theme/zenbones.nvim", dependencies = "rktjmp/lush.nvim", enabled = false },
  {
    dir = "~/code/ploy.nvim",
    dependencies = "rktjmp/lush.nvim",
    init = function()
      vim.opt.background = "light"
      vim.cmd("colorscheme ploy_light")
    end,
  },
  {
    "rktjmp/paperplanes.nvim",
    opts = {
      provider = "paste.rs",
    },
    cmd = "PP",
  },
  {
    "pmizio/typescript-tools.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {},
    ft = { "typescript", "javascript", "html", "typescriptreact" },
  },
  { "j-hui/fidget.nvim", opts = {}, event = "BufReadPre" },
  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewClose", "DiffviewToggleFiles", "DiffviewFocusFiles" },
    opts = {},
    keys = {
      { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "DiffviewOpen" },
      { "<leader>gh", "<cmd>DiffviewFileHistory %<cr>", desc = "DiffviewFileHistory" },
    },
  },
  {
    "stevearc/aerial.nvim",
    cmd = "AerialToggle",
    keys = {
      { "<leader>cs", "<cmd>AerialToggle right<CR>", desc = "Toggle outline" },
    },
    opts = {},
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
  },
  {
    "folke/drop.nvim",
    opts = {
      screensaver = false,
      theme = "summer",
    },
  },
}
