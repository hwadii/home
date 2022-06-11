local lspconfig = require('lspconfig')
local null_ls = require('null-ls')
local telescope = require('telescope.builtin')
local themes = require('telescope.themes')
local navic = require('nvim-navic')

local custom_attach = function(client, bufnr)
  vim.diagnostic.config({
    underline = true,
    update_in_insert = false,
    float = { header = false },
    virtual_text = { prefix = '»' },
  })

  local opts = { buffer = bufnr }

  local function map(mode, keys, func)
    vim.keymap.set(mode, keys, func, opts)
  end

  map('n', 'gA', vim.lsp.buf.code_action)
  map('n', 'gd', vim.lsp.buf.definition)
  map('n', 'K', vim.lsp.buf.hover)
  map('n', '<leader>i', vim.lsp.buf.implementation)
  map('n', 'gy', vim.lsp.buf.type_definition)
  map('n', 'gr', vim.lsp.buf.references)
  map('n', '<localleader>ws', vim.lsp.buf.workspace_symbol)
  map('n', '<localleader>wa', vim.lsp.buf.add_workspace_folder)
  map('n', '<localleader>wr', vim.lsp.buf.remove_workspace_folder)
  map('n', '<localleader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end)
  map('n', 'gR', vim.lsp.buf.rename)
  map('n', '<localleader>f', vim.lsp.buf.format)
  map('n', '<localleader>q', vim.diagnostic.setloclist)
  map('n', ']d', vim.diagnostic.goto_next)
  map('n', '[d', vim.diagnostic.goto_prev)
  map('n', '<c-]>', vim.lsp.buf.signature_help)
  map('i', '<c-]>', vim.lsp.buf.signature_help)
  map('n', '<Leader>sd', function() telescope.diagnostics(themes.get_dropdown({ previewer = false })) end)
  map('n', '<Leader>so', telescope.lsp_document_symbols)
  map('n', '<Leader>sr', telescope.lsp_references)

  map('n', '<localleader>o', vim.lsp.buf.document_symbol)
  map('n', '<localleader>d', vim.diagnostic.get)
  map('n', '<localleader>i', vim.diagnostic.open_float)

  vim.api.nvim_buf_create_user_command(bufnr, 'Format', vim.lsp.buf.format, { desc = 'Format current buffer with LSP' })

  if client.server_capabilities.documentHighlightProvider then
    local group = vim.api.nvim_create_augroup('lsp_autcmds', { clear = true })
    vim.api.nvim_create_autocmd('CursorHold', {
      pattern = '<buffer>',
      callback = vim.lsp.buf.document_highlight,
      group = group,
    })
    vim.api.nvim_create_autocmd('CursorMoved', {
      pattern = '<buffer>',
      callback = vim.lsp.buf.clear_references,
      group = group,
    })
  end
  navic.attach(client, bufnr)
end

null_ls.setup({
  sources = {
    null_ls.builtins.code_actions.gitsigns,
    null_ls.builtins.code_actions.eslint_d,
    null_ls.builtins.formatting.eslint_d,
  },
})

lspconfig.tsserver.setup({
  init_options = {
    hostInfo = "neovim",
    preferences = {
      importModuleSpecifierPreference = "relative",
      includeCompletionsForImportStatements = true,
      includeCompletionsWithSnippetText = true,
      includeInlayParameterNameHints = "none",
      includeInlayParameterNameHintsWhenArgumentMatchesName = false,
      includeInlayFunctionParameterTypeHints = false,
      includeInlayVariableTypeHints = false,
      includeInlayPropertyDeclarationTypeHints = false,
      includeInlayFunctionLikeReturnTypeHints = false,
      includeInlayEnumMemberValueHints = false,
    },
  },
  on_attach = function(client, bufnr)
    custom_attach(client, bufnr)
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
    local ts_utils = require('nvim-lsp-ts-utils')
    ts_utils.setup({
      eslint_bin = "eslint_d",
      eslint_config_fallback = nil,
      eslint_enable_diagnostics = false,
      enable_formatting = true,
      formatter = "prettier",
    })
    ts_utils.setup_client(client)
    vim.api.nvim_buf_set_option(0, 'formatexpr', 'v:lua.vim.lsp.formatexpr()')
  end,
  flags = {
    allow_incremental_sync = true,
  }
})

local libpath = vim.fn.expand('~/.config/nvm/versions/node/v16.14.2/lib/node_modules/typescript/lib')
local cmd = {"ngserver", "--stdio", "--tsProbeLocations", libpath, "", "--ngProbeLocations", libpath, ""}
lspconfig.angularls.setup({
  on_attach = custom_attach,
  cmd = cmd,
  on_new_config = function(new_config, _)
    new_config.cmd = cmd
  end,
})
lspconfig.html.setup({
    on_attach = custom_attach,
  })
lspconfig.cssls.setup({
    on_attach = custom_attach,
  })
lspconfig.rust_analyzer.setup({
  on_attach = custom_attach,
  settings = {
    ["rust-analyzer"] = {
      assist = {
        importGranularity = "module",
        importPrefix = "by_self",
      },
      cargo = {
        loadOutDirsFromCheck = true
      },
      procMacro = {
        enable = true
      },
    }
  }
})
lspconfig.omnisharp.setup({
  on_attach = custom_attach,
  cmd = { "OmniSharp", "--languageserver", "--hostPID", tostring(vim.fn.getpid()) }
})

local servers = {
  'solargraph',
  'vuels',
  'jsonls',
  'bashls',
  'pylsp',
  'racket_langserver',
  'emmet_ls',
  'sumneko_lua',
  'gopls',
}
for _, server in pairs(servers) do
  lspconfig[server].setup {
    on_attach = custom_attach,
  }
end

local signs = {
  { name = "DiagnosticSignError", text = "×" },
  { name = "DiagnosticSignWarn", text = "!" },
  { name = "DiagnosticSignHint", text = "i" },
  { name = "DiagnosticSignInfo", text = "H" },
}

for _, value in ipairs(signs) do
  vim.fn.sign_define(value.name, { text = value.text, texthl = value.name })
end
