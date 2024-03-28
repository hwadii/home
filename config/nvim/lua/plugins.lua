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
    init = function()
      vim.g.slime_target = "tmux"
      vim.g.slime_paste_file = "/tmp/.slime_paste"
      vim.g.slime_default_config = { socket_name = "default", target_pane = "{last}" }
      vim.g.slime_dont_ask_default = 1
      vim.g.slime_no_mappings = 1
    end,
  },
  { "godlygeek/tabular", cmd = "Tabularize" },
  {
    "numToStr/Comment.nvim",
    opts = {},
    keys = {
      { "gc", mode = { "n", "x" } },
      { "gb", mode = { "n", "x" } },
    },
  },
  {
    "uga-rosa/ccc.nvim",
    opts = { empty_point_bg = false },
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
    },
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.snippet = {
        expand = function(args)
          require("snippy").expand_snippet(args.body)
        end,
      }
      opts.mapping = cmp.mapping.preset.insert({
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-e>"] = cmp.mapping.abort({ select = true }),
        ["<C-y>"] = cmp.mapping.confirm({ select = true }),
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
        ["<C-l>"] = cmp.mapping(function()
          if require("snippy").can_expand_or_advance() then
            require("snippy").expand_or_advance()
          end
        end, { "i", "s" }),

        ["<C-h>"] = cmp.mapping(function()
          if require("snippy").can_jump(-1) then
            require("snippy").previous()
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
      opts.sources = cmp.config.sources({
        { name = "nvim_lsp" },
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
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map({ "n", "v" }, "]c", function()
          if vim.wo.diff then
            return "]c"
          end
          vim.schedule(function()
            gs.next_hunk()
          end)
          return "<Ignore>"
        end, { expr = true, desc = "Jump to next hunk" })

        map({ "n", "v" }, "[c", function()
          if vim.wo.diff then
            return "[c"
          end
          vim.schedule(function()
            gs.prev_hunk()
          end)
          return "<Ignore>"
        end, { expr = true, desc = "Jump to previous hunk" })

        -- Actions
        -- visual mode
        map("v", "<leader>hs", function()
          gs.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end, { desc = "stage git hunk" })
        map("v", "<leader>hr", function()
          gs.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end, { desc = "reset git hunk" })
        -- normal mode
        map("n", "<leader>hs", gs.stage_hunk, { desc = "git stage hunk" })
        map("n", "<leader>hr", gs.reset_hunk, { desc = "git reset hunk" })
        map("n", "<leader>hS", gs.stage_buffer, { desc = "git Stage buffer" })
        map("n", "<leader>hu", gs.undo_stage_hunk, { desc = "undo stage hunk" })
        map("n", "<leader>hR", gs.reset_buffer, { desc = "git Reset buffer" })
        map("n", "<leader>hp", gs.preview_hunk, { desc = "preview git hunk" })
        map("n", "<leader>hb", function()
          gs.blame_line({ full = false })
        end, { desc = "git blame line" })
        map("n", "<leader>hd", gs.diffthis, { desc = "git diff against index" })
        map("n", "<leader>hD", function()
          gs.diffthis("~")
        end, { desc = "git diff against last commit" })

        -- Toggles
        map("n", "<leader>tb", gs.toggle_current_line_blame, { desc = "toggle git blame line" })
        map("n", "<leader>td", gs.toggle_deleted, { desc = "toggle git show deleted" })

        -- Text object
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", { desc = "select git hunk" })
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
        ["*"] = { "trim_whitespace" },
        sql = { "pg_format" },
        yaml = { "yamlfmt" },
        xml = { "xmllint" },
        json = { "jq" },
        ruby = { "rubocop" },
        lua = { "stylua" },
        cs = { "csharpier" },
        markdown = { "prettierd" },
      },
    },
    keys = {
      {
        "<localleader>f",
        function()
          require("conform").format({ async = true, lsp_fallback = true })
        end,
        desc = "Format buffer",
      },
    },
  },
  {
    "stevearc/stickybuf.nvim",
    opts = {},
    ft = { "fugitive", "NeogitCommitMessage", "help" },
    cmd = { "PinBuffer", "PinFiletype", "PinBuftype" },
  },
  {
    "echasnovski/mini.nvim",
    event = "BufReadPre",
    config = function()
      require("mini.ai").setup({ search_method = "cover_or_next", n_lines = 200 })
      require("mini.move").setup()
      require("mini.operators").setup({
        evaluate = { prefix = "" },
        exchange = { prefix = "" },
        multiply = { prefix = "" },
        replace = { prefix = "" },
        sort = { prefix = "gs" },
      })
    end,
  },
  {
    "stevearc/oil.nvim",
    cmd = "Oil",
    keys = {
      {
        "-",
        function()
          require("oil").open()
        end,
        mode = "n",
        desc = "Open parent directory in a buffer.",
      },
    },
    opts = {
      view_options = {
        show_hidden = true,
      },
      buf_options = {
        buflisted = false,
        bufhidden = "hide",
      },
      keymaps = {
        ["!"] = "actions.open_terminal",
        ["."] = "actions.open_cmdline",
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
      vim.g.vim_markdown_conceal = 0
      vim.g.vim_markdown_conceal_code_blocks = 0
      vim.g.vim_markdown_frontmatter = 1
      vim.g.vim_markdown_borderless_table = 0
      vim.g.vim_markdown_folding_disabled = 1
    end,
  },
  {
    "kylechui/nvim-surround",
    event = "VimEnter",
    opts = {},
  },
  {
    "dstein64/nvim-scrollview",
    event = "UIEnter",
    opts = {
      current_only = true,
      winblend_gui = 40,
      signs_on_startup = { "diagnostics", "search", "spell", "marks", "conflicts" },
      excluded_filetypes = { "fugitiveblame" },
    },
  },
  {
    "zbirenbaum/copilot.lua",
    dependencies = { "zbirenbaum/copilot-cmp" },
    cmd = "Copilot",
    keys = {
      { "<leader>pt", "<cmd>Copilot toggle<cr>", mode = "n", desc = "Copilot enable" },
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
  { "tpope/vim-abolish", cmd = { "Abolish", "Subvert" }, keys = "cr" },
  { "tpope/vim-unimpaired", keys = { "[", "]", "yo", "=", "<", ">" } },
  { "tpope/vim-sleuth", event = "VimEnter" },
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
      { "<Leader>gp", "<cmd>Git push<cr>", mode = "n" },
      { "<Leader>gl", "<cmd>Git log<cr>", mode = "n" },
      { "<Leader>gb", "<cmd>Git blame<cr>", mode = "n" },
    },
    dependencies = "tpope/vim-rhubarb",
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    cmd = "Neogit",
    keys = {
      { "<Leader>gS", "<cmd>Neogit<cr>", mode = "n" },
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
    init = function()
      vim.g.rsi_no_meta = 1
    end,
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
  { "lewis6991/satellite.nvim", opts = {}, enabled = false },
  { "mcchrish/zenbones.nvim", dependencies = "rktjmp/lush.nvim", enabled = false },
  {
    dir = "~/code/ploy.nvim",
    dependencies = "rktjmp/lush.nvim",
    init = function()
      vim.g.colors_name = "ploy"
    end,
  },
  {
    "rktjmp/paperplanes.nvim",
    opts = {
      provider = "paste.rs",
    },
    cmd = "PP",
  },
  { "alaviss/nim.nvim", ft = "nim" },
  {
    "pmizio/typescript-tools.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {},
    ft = { "typescript", "javascript", "html" },
  },
  "nvim-tree/nvim-web-devicons",
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
}
