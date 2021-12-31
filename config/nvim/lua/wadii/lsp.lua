local lspconfig = require('lspconfig')
local null_ls = require('null-ls')

null_ls.setup({
  sources = {
    null_ls.builtins.code_actions.gitsigns,
    null_ls.builtins.code_actions.eslint,
  }
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local custom_attach = function(_, bufnr)
  vim.diagnostic.config({
    underline = true,
    update_in_insert = true,
    virtual_text = { spacing = 2, prefix = '·' },
  })

  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function opts(cb) return { noremap = true, silent = true, callback = cb } end
  buf_set_keymap('n', 'gd', '', opts(vim.lsp.buf.definition))
  buf_set_keymap('n', 'K', '', opts(vim.lsp.buf.hover))
  buf_set_keymap('n', '<leader>i', '', opts(vim.lsp.buf.implementation))
  buf_set_keymap('n', 'gy', '', opts(vim.lsp.buf.type_definition))
  buf_set_keymap('n', 'gr', '', opts(vim.lsp.buf.references))
  buf_set_keymap('n', '<localleader>ws', '', opts(vim.lsp.buf.workspace_symbol))
  buf_set_keymap('n', '<localleader>wa', '', opts(vim.lsp.buf.add_workspace_folder))
  buf_set_keymap('n', '<localleader>wr', '', opts(vim.lsp.buf.remove_workspace_folder))
  buf_set_keymap('n', '<localleader>wl', '', opts(function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end))
  buf_set_keymap('n', 'gR', '', opts(vim.lsp.buf.rename))
  buf_set_keymap('n', '<localleader>f', '', opts(vim.lsp.buf.formatting))
  buf_set_keymap('n', '<localleader>q', '', opts(vim.diagnostic.setloclist))
  buf_set_keymap('n', ']g', '', opts(vim.diagnostic.goto_next))
  buf_set_keymap('n', '[g', '', opts(vim.diagnostic.goto_prev))
  buf_set_keymap('n', '<c-]>', '', opts(vim.lsp.buf.signature_help))
  buf_set_keymap('i', '<c-]>', '', opts(vim.lsp.buf.signature_help))

  buf_set_keymap('n', '<localleader>o', '', opts(vim.lsp.buf.document_symbol))
  buf_set_keymap('n', '<localleader>d', '', opts(vim.diagnostic.get))
  buf_set_keymap('n', '<localleader>i', '', opts(vim.diagnostic.open_float))
end

lspconfig.tsserver.setup({
  init_options = {
    hostInfo = "neovim",
    preferences = {
      importModuleSpecifierPreference = "shortest",
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
    debounce_text_changes = 150,
  }
})

local libpath = vim.fn.expand('~/.config/nvm/versions/node/v14.18.1/lib/node_modules/typescript/lib')
local cmd = {"ngserver", "--stdio", "--tsProbeLocations", libpath, "", "--ngProbeLocations", libpath, ""}
lspconfig.angularls.setup({
  on_attach = custom_attach,
  cmd = cmd,
  on_new_config = function(new_config, _)
    new_config.cmd = cmd
  end,
  capabilities = capabilities,
  flags = {
    debounce_text_changes = 150,
  }
})
lspconfig.html.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    }
  })
lspconfig.cssls.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = {
      debounce_text_changes = 150,
    }
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
    flags = {
      debounce_text_changes = 150,
    }
  }
end

local function set_lsp_sign(name, text)
  vim.fn.sign_define(name, {text = text, texthl = name})
end

set_lsp_sign('DiagnosticSignError', '×')
set_lsp_sign('DiagnosticSignWarn', '!')
set_lsp_sign('DiagnosticSignInfo', 'i')
set_lsp_sign('DiagnosticSignHint', 'H')
