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

local custom_attach = function(client, bufnr)
  vim.diagnostic.config({
    underline = true,
    update_in_insert = true,
    virtual_text = { spacing = 2, prefix = '·' },
  })

  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local opts = { noremap = true, silent = true }
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
  buf_set_keymap('n', '<leader>i', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)
  buf_set_keymap('n', 'gy', '<cmd>lua vim.lsp.buf.type_definition()<cr>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', opts)
  buf_set_keymap('n', '<localleader>ws', '<cmd>lua vim.lsp.buf.workspace_symbol()<cr>', opts)
  buf_set_keymap('n', '<localleader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<localleader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<localleader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', 'gR', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
  buf_set_keymap('n', '<localleader>f', '<cmd>lua vim.lsp.buf.formatting()<cr>', opts)
  buf_set_keymap('n', '<localleader>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
  buf_set_keymap('n', ']g', '<cmd>lua vim.diagnostic.goto_next()<cr>', opts)
  buf_set_keymap('n', '[g', '<cmd>lua vim.diagnostic.goto_prev()<cr>', opts)
  buf_set_keymap('n', '<c-]>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
  buf_set_keymap('i', '<c-]>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)

  buf_set_keymap('n', '<localleader>o', '<cmd>lua vim.lsp.buf.document_symbol()<cr>', opts)
  buf_set_keymap('n', '<localleader>d', '<cmd>lua vim.diagnostic.get()<cr>', opts)
  buf_set_keymap('n', '<localleader>i', '<cmd>lua vim.diagnostic.open_float()<cr>', opts)
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
  on_new_config = function(new_config, new_root_dir)
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

local servers = { 'solargraph', 'vuels', 'jsonls', 'bashls', 'pylsp', 'racket_langserver', 'emmet_ls' }
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
