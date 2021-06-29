require'nvim-treesitter.configs'.setup {
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
      init_selection = "<a-w>", -- maps in normal mode to init the node/scope selection
      node_incremental = "<a-w>", -- increment to the upper named parent
      node_decremental = "<a-C-w>", -- decrement to the previous node
      scope_incremental = "<a-e>", -- increment to the upper scope (as defined in locals.scm)
    },
  },
  indent = {
    enable = false,
  }
}
