local lspconfig = require('lspconfig')
local completion = require('completion')

vim.g.diagnostic_show_virtual_text = 1
vim.g.completion_sorting = 'length'
vim.g.completion_matching_strategy_list = { "exact", "substring", "fuzzy" }
vim.g.matching_smart_case = 1
vim.g.completion_enable_auto_popup = 1
vim.g.completion_auto_change_source = 1
vim.g.completion_enable_snippet = 'snippets.nvim'
vim.g.completion_enable_auto_paren = 1
vim.g.completion_chain_complete_list = {
  default = {
    {complete_items = {'path'}, triggered_only = {'/'}},
    {complete_items = {'buffers'}},
  },
  comment = {},
  string = {
    {complete_items = {'path'}, triggered_only = {'/'}},
    {complete_items = {'words'}},
  }
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

local custom_attach = function(client)
  vim.bo.omnifunc = 'v:lua.vim.lsp.omnifunc'
  completion.on_attach(client)
end

lspconfig.util.default_config = vim.tbl_extend(
  "force",
  lspconfig.util.default_config,
  {
    on_attach = custom_attach
  }
)

lspconfig.tsserver.setup({
    on_attach = custom_attach,
  })

lspconfig.solargraph.setup({
    on_attach = custom_attach,
  })

lspconfig.pyls.setup({
  enable = true,
  plugins = {
    pyls_mypy = {
      enabled = true,
      live_mode = false
    }
  },
  on_attach = custom_attach
})
local project_library_path = "."
local path_to_angularls = "/home/wadii/.config/nvm/versions/node/v14.15.1/lib/node_modules/@angular/language-server/index.js"
local angularlscmd = {"node", path_to_angularls, "--stdio", "--tsProbeLocations", project_library_path, "--ngProbeLocations", project_library_path}
lspconfig.angularls.setup({
    filetypes = { "typescript", "html" },
    capabilities = capabilities,
    cmd = angularlscmd,
    on_new_config = function(new_config,new_root_dir)
      new_config.cmd = angularlscmd
    end,
    on_attach = custom_attach,
  })
lspconfig.html.setup({
    capabilities = capabilities,
    on_attach = custom_attach,
  })
lspconfig.cssls.setup({
    capabilities = capabilities,
    on_attach = custom_attach
  })
lspconfig.jsonls.setup({
    on_attach = custom_attach
  })
lspconfig.rls.setup({
    on_attach = custom_attach
  })
lspconfig.vuels.setup({
    on_attach = custom_attach
  })

lspconfig.efm.setup {
  default_config = {
    cmd = {
      "efm-langserver",
      "-c",
      [["$HOME/.config/efm-langserver/config.yaml"]]
    }
  },
  filetypes = {
    "javascript",
    "javascriptreact",
    "javascript.jsx",
    "typescript",
    "typescript.tsx",
    "typescriptreact",
    "vue"
  }
}

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

local sign_error = '>';
local sign_warning = '~';
local sign_information = '- ';
local sign_hint = 'H';

vim.fn.sign_define('LspDiagnosticsSignError', {
  text = sign_error,
  texthl = 'LspDiagnosticsSignError'
})

vim.fn.sign_define('LspDiagnosticsSignWarning', {
  text = sign_warning,
  texthl = 'LspDiagnosticsSignWarning'
})

vim.fn.sign_define('LspDiagnosticsSignInformation', {
  text = sign_information,
  texthl = 'LspDiagnosticsSignInformation'
})

vim.fn.sign_define('LspDiagnosticsSignHint', {
  text = sign_hint,
  texthl = 'LspDiagnosticsSignHint'
})
