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
    buffer = true;
    nvim_lsp = true;
    treesitter = true;
  };
}

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

local custom_attach = function(_client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
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

  -- Override to add 
  vim.lsp.handlers['textDocument/hover'] = function(_, method, result)
    vim.lsp.util.focusable_float(method, function()
      if not (result and result.contents) then
        -- return { 'No information available' }
        return
      end
      local markdown_lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
      markdown_lines = vim.lsp.util.trim_empty_lines(markdown_lines)
      if vim.tbl_isempty(markdown_lines) then
        -- return { 'No information available' }
        return
      end
      local bufnr, winnr = vim.lsp.util.fancy_floating_markdown(markdown_lines, {
        pad_left = 1; pad_right = 1;
        max_width = 120
      })
      vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>wincmd p<CR>', {noremap = true, silent = true})
      vim.lsp.util.close_preview_autocmd({"CursorMoved", "BufHidden", "InsertCharPre"}, winnr)
      return bufnr, winnr
    end)
  end
  print('Language server is ready')
end

lspconfig.pyls.setup({
  on_attach = custom_attach,
  enable = true,
  plugins = {
    pyls_mypy = {
      enabled = true,
      live_mode = false
    }
  },
})
local libpath = "/home/wadii/.config/nvm/versions/node/v14.15.4/lib/node_modules/typescript/lib"
local cmd = {"ngserver", "--stdio", "--tsProbeLocations", libpath, "--ngProbeLocations", libpath}
lspconfig.angularls.setup({
  on_attach = custom_attach,
  cmd = cmd,
  on_new_config = function(new_config, new_root_dir)
    new_config.cmd = cmd
  end,
})
lspconfig.html.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
  })
lspconfig.cssls.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
  })

lspconfig.efm.setup {
  on_attach = custom_attach,
  default_config = {
    cmd = {
      "efm-langserver",
      "-c",
      [["$HOME/.config/efm-langserver/config.yaml"]]
    }
  },
  filetypes = {
    "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescript.tsx",
    "typescriptreact", "vue", "yaml", "json", "html", "scss", "css", "markdown",
  },
  init_options = {
    documentFormatting = true,
    codeAction = true,
  }
}

local servers = { 'solargraph', 'rls', 'vuels', 'jsonls', 'tsserver' }
for _, server in ipairs(servers) do
  lspconfig[server].setup {
    on_attach = custom_attach,
  }
end

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
