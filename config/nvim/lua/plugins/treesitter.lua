return {
  {
    "nvim-treesitter/nvim-treesitter",
    event = "VimEnter",
    build = ":TSUpdate",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-refactor",
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    opts = {
      ensure_installed = {
        "c",
        "cpp",
        "ruby",
        "typescript",
        "scheme",
        "go",
        "javascript",
        "tsx",
        "vim",
        "lua",
        "rust",
        "css",
        "dot",
        "json",
        "python",
        "yaml",
        "vue",
        "org",
        "markdown",
        "markdown_inline",
        "html",
        "sql",
        "c_sharp",
        "fish",
        "diff",
        "git_rebase",
        "gitignore",
        "gitattributes",
        "vimdoc",
        "nix",
        "comment",
        "hare",
        "hcl",
        "groovy",
        "csv",
        "racket",
        "terraform",
        "angular",
        "gitcommit",
        "query",
        "bash",
        "toml",
        "dockerfile",
        "tmux",
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
        swap = {
          enable = true,
          swap_next = {
            ["<leader>a"] = "@parameter.inner",
          },
          swap_previous = {
            ["<leader>A"] = "@parameter.inner",
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
        additional_vim_regex_highlighting = false,
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
            goto_definition = "gnd",
            list_definitions = "gnD",
            list_definitions_toc = "gO",
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
      require("nvim-treesitter").define_modules({
        fold = {
          attach = function(_, _)
            vim.cmd("set foldmethod=expr foldexpr=nvim_treesitter#foldexpr() nofoldenable")
          end,
          detach = function() end,
        },
      })
    end,
  },
}
