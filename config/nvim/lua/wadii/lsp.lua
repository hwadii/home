local lspconfig = require('lspconfig')
local null_ls = require('null-ls')

null_ls.setup({
  sources = {
    null_ls.builtins.code_actions.gitsigns,
    null_ls.builtins.code_actions.eslint_d,
    null_ls.builtins.formatting.eslint_d,
  }
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local custom_attach = function(_, bufnr)
  vim.diagnostic.config({
    underline = true,
    update_in_insert = true,
    virtual_text = { prefix = '•' },
  })

  local opts = { buffer = bufnr }
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
  vim.keymap.set('n', '<leader>i', vim.lsp.buf.implementation, opts)
  vim.keymap.set('n', 'gy', vim.lsp.buf.type_definition, opts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
  vim.keymap.set('n', '<localleader>ws', vim.lsp.buf.workspace_symbol, opts)
  vim.keymap.set('n', '<localleader>wa', vim.lsp.buf.add_workspace_folder, opts)
  vim.keymap.set('n', '<localleader>wr', vim.lsp.buf.remove_workspace_folder, opts)
  vim.keymap.set('n', '<localleader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)
  vim.keymap.set('n', 'gR', vim.lsp.buf.rename, opts)
  vim.keymap.set('n', '<localleader>f', vim.lsp.buf.formatting, opts)
  vim.keymap.set('n', '<localleader>q', vim.diagnostic.setloclist, opts)
  vim.keymap.set('n', ']g', vim.diagnostic.goto_next, opts)
  vim.keymap.set('n', '[g', vim.diagnostic.goto_prev, opts)
  vim.keymap.set('n', '<c-]>', vim.lsp.buf.signature_help, opts)
  vim.keymap.set('i', '<c-]>', vim.lsp.buf.signature_help, opts)

  vim.keymap.set('n', '<localleader>o', vim.lsp.buf.document_symbol, opts)
  vim.keymap.set('n', '<localleader>d', vim.diagnostic.get, opts)
  vim.keymap.set('n', '<localleader>i', vim.diagnostic.open_float, opts)
end

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
    client.resolved_capabilities.document_formatting = false
    client.resolved_capabilities.document_range_formatting = false
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
  capabilities = capabilities,
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
  capabilities = capabilities,
})
lspconfig.html.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
  })
lspconfig.cssls.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
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

local servers = {
  'solargraph',
  'vuels',
  'jsonls',
  'bashls',
  'pylsp',
  'racket_langserver',
  'emmet_ls',
  'sumneko_lua',
}
for _, server in pairs(servers) do
  lspconfig[server].setup {
    on_attach = custom_attach,
    capabilities = capabilities,
  }
end

local function set_lsp_sign(name, text)
  vim.fn.sign_define(name, {text = text, texthl = name})
end

set_lsp_sign('DiagnosticSignError', '×')
set_lsp_sign('DiagnosticSignWarn', '!')
set_lsp_sign('DiagnosticSignInfo', 'i')
set_lsp_sign('DiagnosticSignHint', 'H')
