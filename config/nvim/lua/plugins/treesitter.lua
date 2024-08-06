return {
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-refactor",
      "nvim-treesitter/nvim-treesitter-textobjects",
      "nvim-treesitter/nvim-treesitter-context",
      "nushell/tree-sitter-nu",
    },
    opts = {
      ensure_installed = {
        "angular",
        "bash",
        "c",
        "c_sharp",
        "comment",
        "cpp",
        "css",
        "csv",
        "diff",
        "dockerfile",
        "dot",
        "editorconfig",
        "fish",
        "git_config",
        "git_rebase",
        "gitattributes",
        "gitcommit",
        "gitignore",
        "go",
        "groovy",
        "hare",
        "hcl",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "markdown_inline",
        "nim",
        "nix",
        "nu",
        "org",
        "python",
        "query",
        "racket",
        "ruby",
        "rust",
        "scheme",
        "sql",
        "terraform",
        "tmux",
        "toml",
        "tsx",
        "typescript",
        "vim",
        "vimdoc",
        "vue",
        "xml",
        "yaml",
        "zig",
      },
      autoinstall = true,
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            ["aa"] = "@parameter.outer",
            ["ia"] = "@parameter.inner",
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@class.outer",
            ["ic"] = "@class.inner",
            ["ad"] = "@block.outer",
            ["id"] = "@block.inner",
          },
        },
        matchup = {
          enable = true
        },
        swap = {
          enable = true,
          swap_next = {
            ["<localleader>a"] = "@parameter.inner",
          },
          swap_previous = {
            ["<localleader>A"] = "@parameter.inner",
          },
        },
        lsp_interop = {
          enable = true,
          peek_definition_code = {
            ["<leader>df"] = "@function.outer",
            ["<leader>dF"] = "@class.outer",
          },
        },
        move = {
          enable = true,
          set_jumps = true, -- whether to set jumps in the jumplist
          goto_next_start = {
            ["]m"] = "@function.outer",
            ["]]"] = "@class.outer",
          },
          goto_next_end = {
            ["]M"] = "@function.outer",
            ["]["] = "@class.outer",
          },
          goto_previous_start = {
            ["[m"] = "@function.outer",
            ["[["] = "@class.outer",
          },
          goto_previous_end = {
            ["[M"] = "@function.outer",
            ["[]"] = "@class.outer",
          },
        },
      },
      highlight = {
        enable = true,
        disable = function(_, buf)
          local max_filesize = 100 * 1024 -- 100 KB
          local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
          if ok and stats and stats.size > max_filesize then
            return true
          end
        end,
      },
      fold = {
        enable = true,
      },
      refactor = {
        highlight_definitions = { enable = false },
        highlight_current_scope = { enable = false },
        navigation = {
          enable = true,
          keymaps = {
            goto_definition = false,
            list_definitions = false,
            list_definitions_toc = false,
            goto_next_usage = "<a-*>",
            goto_previous_usage = "<a-#>",
          },
        },
        smart_rename = {
          enable = false,
          keymaps = {
            -- mapping to rename reference under cursor
            smart_rename = "grr",
          },
        },
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<a-o>", -- maps in normal mode to init the node/scope selection
          node_incremental = "<a-o>", -- increment to the upper named parent
          node_decremental = "<a-i>", -- decrement to the previous node
          scope_incremental = "<a-e>", -- increment to the upper scope (as defined in locals.scm)
        },
      },
      indent = {
        disable = { "ruby" },
        enable = true,
      },
    },
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)
      require("treesitter-context").setup({ enable = true, max_lines = 1 })
      require("nvim-treesitter").define_modules({
        fold = {
          attach = function(_, _)
            vim.cmd("set foldmethod=expr foldexpr=nvim_treesitter#foldexpr() nofoldenable")
          end,
          detach = function() end,
        },
      })
      local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
      parser_config.powershell = {
        install_info = {
          url = "https://github.com/jrsconfitto/tree-sitter-powershell",
          files = { "src/parser.c" },
        },
        filetype = "ps1",
        used_by = { "psm1", "psd1", "pssc", "psxml", "cdxml" },
      }
    end,
  },
}
