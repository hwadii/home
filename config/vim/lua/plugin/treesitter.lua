lua << EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "typescript", "ruby", "bash", "javascript", "css", "html", "jsdoc", "json", "yaml", "python", "rust", "comment" },
  textobjects = {
    keymaps = {
      ["af"] = "@function.outer",
      ["if"] = "@function.inner",
      ["ac"] = "@class.outer",
      ["ic"] = "@class.inner",
    },
  },
  highlight = {
    enable = true,
    custom_captures = {
      ["TextYankPost"] = "IncSearch",
    }
  },
  refactor = {
    highlight_definitions = { enable = true },
  };
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  indent = {
    enable = false,
  }
}
EOF
