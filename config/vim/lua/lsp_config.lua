local lspconfig = require('lspconfig')

require('compe').setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;
  source = {
    path = true;
    buffer = false;
    calc = true;
    vsnip = false;
    nvim_lsp = true;
    nvim_lua = false;
    spell = true;
    tags = false;
    snippets_nvim = true;
    treesitter = false;
  };
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    'documentation',
    'detail',
    'additionalTextEdits',
  }
}

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<C-Space>", "compe#complete()", {expr = true})

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
end

lspconfig.tsserver.setup({
  init_options = {
    preferences = {
      importModuleSpecifier = "relative",
    }
  },
  on_attach = custom_attach,
  capabilities = capabilities,
})

lspconfig.pyls.setup({
  on_attach = custom_attach,
  enable = true,
  plugins = {
    pyls_mypy = {
      enabled = true,
      live_mode = false
    }
  },
  capabilities = capabilities,
})
local libpath = "/home/wadii/.config/nvm/versions/node/v14.17.0/lib/node_modules/typescript/lib"
local cmd = {"ngserver", "--stdio", "--tsProbeLocations", libpath, "--ngProbeLocations", libpath}
lspconfig.angularls.setup({
  on_attach = custom_attach,
  cmd = cmd,
  on_new_config = function(new_config, new_root_dir)
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

local servers = { 'solargraph', 'rls', 'vuels', 'jsonls', 'bashls' }
for _, server in ipairs(servers) do
  lspconfig[server].setup {
    on_attach = custom_attach,
    capabilities = capabilities,
  }
end

local sign_error = '×';
local sign_warning = '!';
local sign_information = 'i';
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
