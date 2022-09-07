require'nvim-treesitter.configs'.setup {
  ensure_installed = {
    "ruby", "typescript", "scheme", "go", "javascript", "tsx", "vim", "lua",
    "rust", "css", "dot", "json", "python", "yaml", "vue", "markdown", "html",
    "sql"
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
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
  },
  highlight = {
    enable = true,
    custom_captures = {
      ["TextYankPost"] = "IncSearch",
    },
    additional_vim_regex_highlighting = false,
    disable = { "typescript" },
  },
  fold = {
    enable = true,
  },
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },
  refactor = {
    highlight_definitions = { enable = false },
    highlight_current_scope = { enable = false },
    navigation = {
      enable = false,
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
  },
}

require'nvim-treesitter'.define_modules {
  fold = {
    attach = function(_, _)
      vim.cmd'set foldmethod=expr foldexpr=nvim_treesitter#foldexpr()'
    end,
    detach = function() end,
  }
}
