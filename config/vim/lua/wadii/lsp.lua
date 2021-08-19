local lspconfig = require('lspconfig')

require('compe').setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  resolve_timeout = 800;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = {
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
    min_width = 60;
    border = 'single',
  };
  source = {
    path = true;
    buffer = true;
    calc = false;
    vsnip = false;
    nvim_lsp = true;
    nvim_lua = false;
    spell = false;
    tags = false;
    snippets_nvim = false;
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
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
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

vim.fn.sign_define('LspDiagnosticsSignError', {
  text = '×',
  texthl = 'LspDiagnosticsSignError'
})

vim.fn.sign_define('LspDiagnosticsSignWarning', {
  text = '!',
  texthl = 'LspDiagnosticsSignWarning'
})

vim.fn.sign_define('LspDiagnosticsSignInformation', {
  text = 'i',
  texthl = 'LspDiagnosticsSignInformation'
})

vim.fn.sign_define('LspDiagnosticsSignHint', {
  text = 'H',
  texthl = 'LspDiagnosticsSignHint'
})
