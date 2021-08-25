local lspconfig = require('lspconfig')

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local custom_attach = function(client, bufnr)
  client.resolved_capabilities.document_formatting = false
  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      signs = {
        priority = 11,
      },
      underline = true,
      update_in_insert = true,
      virtual_text = { spacing = 2, prefix = '·' },
    }
  )

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
  buf_set_keymap('n', '<localleader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', ']g', '<cmd>lua vim.lsp.diagnostic.goto_next()<cr>', opts)
  buf_set_keymap('n', '[g', '<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>', opts)
  buf_set_keymap('n', '<c-]>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)
  buf_set_keymap('i', '<c-]>', '<cmd>lua vim.lsp.buf.signature_help()<cr>', opts)

  buf_set_keymap('n', '<localleader>o', '<cmd>lua vim.lsp.buf.document_symbol()<cr>', opts)
  buf_set_keymap('n', '<localleader>d', '<cmd>lua vim.lsp.diagnostic.get_all()<cr>', opts)
  buf_set_keymap('n', '<localleader>i', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>', opts)
end

lspconfig.tsserver.setup({
  init_options = {
    preferences = {
      importModuleSpecifierPreference = "relative"
    },
  },
  on_attach = custom_attach,
  capabilities = capabilities,
  flags = {
    debounce_text_changes = 150,
  }
})

local libpath = "/home/wadii/.config/nvm/versions/node/v14.17.4/lib/node_modules/typescript/lib"
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

local servers = { 'solargraph', 'vuels', 'jsonls', 'bashls', 'pylsp', 'racket_langserver' }
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

set_lsp_sign('LspDiagnosticsSignError', '×')
set_lsp_sign('LspDiagnosticsSignWarning', '!')
set_lsp_sign('LspDiagnosticsSignInformation', 'i')
set_lsp_sign('LspDiagnosticsSignHint', 'H')
