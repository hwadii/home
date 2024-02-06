local lspconfig = require('lspconfig')
local telescope = require('telescope.builtin')
local themes = require('telescope.themes')

local handlers = {
  ['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, { max_width = 100 }),
  ['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, { max_width = 100 }),
}
local capabalities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'Lsp actions',
  callback = function(event)
    local client = vim.lsp.get_client_by_id(event.data.client_id)
    local function map(mode, keys, func)
      vim.keymap.set(mode, keys, func, { buffer = event.buf })
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
    map({ 'n', 'v' }, '<localleader>f', function() vim.lsp.buf.format { async = true, timeout_ms = 10000 } end)
    map('n', '<localleader>q', vim.diagnostic.setloclist)
    map('n', ']d', vim.diagnostic.goto_next)
    map('n', '[d', vim.diagnostic.goto_prev)
    map('i', '<c-]>', vim.lsp.buf.signature_help)
    map('n', '<Leader>sd', function() telescope.diagnostics(themes.get_dropdown({ previewer = false })) end)
    map('n', '<Leader>so', telescope.lsp_document_symbols)
    map('n', '<Leader>sr', telescope.lsp_references)
    map('n', '<localleader>o', vim.lsp.buf.document_symbol)
    map('n', '<localleader>i', vim.diagnostic.open_float)

    vim.api.nvim_buf_create_user_command(event.buf, 'FormatLsp', function() vim.lsp.buf.format { async = true } end,
      { desc = 'Format current buffer with LSP' })

    if client.server_capabilities.documentHighlightProvider then
      vim.api.nvim_create_augroup('lsp_document_highlight', { clear = false })
      vim.api.nvim_clear_autocmds({ buffer = event.buf, group = 'lsp_document_highlight' })
      vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
        group = 'lsp_document_highlight',
        buffer = event.buf,
        callback = function()
          -- Ignore decompiled c# source files
          if not event.file:match('metadata') and vim.lsp.buf.server_ready() then
            vim.lsp.buf.document_highlight()
          end
        end,
      })
      vim.api.nvim_create_autocmd('CursorMoved', {
        group = 'lsp_document_highlight',
        buffer = event.buf,
        callback = vim.lsp.buf.clear_references,
      })
    end
  end,
})

local ngserver_cmd = function()
  local node_path = string.gsub(vim.fn.system('mise where node'), "\n", '')
  local libpath = vim.fn.expand(node_path .. '/lib/node_modules/typescript/lib')
  return { 'ngserver', '--stdio', '--tsProbeLocations', libpath, '--ngProbeLocations', libpath }
end
lspconfig.angularls.setup({
  cmd = ngserver_cmd(),
  capabalities = capabalities,
  on_new_config = function(new_config, _)
    new_config.cmd = ngserver_cmd()
  end,
  handlers = handlers,
})
lspconfig.cssls.setup({
  handlers = handlers,
  capabilities = capabalities,
})
lspconfig.rust_analyzer.setup({
  capabalities = capabalities,
  settings = {
    ['rust-analyzer'] = {
      assist = {
        importGranularity = 'module',
        importPrefix = 'by_self',
      },
      cargo = {
        loadOutDirsFromCheck = true
      },
      procMacro = {
        enable = true
      },
      checkOnSave = {
        command = 'clippy'
      }
    }
  },
  handlers = handlers,
})
lspconfig.omnisharp.setup({
  on_attach = function(client)
    client.server_capabilities.semanticTokensProvider = {
      full = vim.empty_dict(),
      legend = {
        tokenModifiers = { 'static_symbol' },
        tokenTypes = {
          'comment', 'excluded_code', 'identifier', 'keyword', 'keyword_control', 'number', 'operator',
          'operator_overloaded', 'preprocessor_keyword', 'string', 'whitespace', 'text', 'static_symbol',
          'preprocessor_text', 'punctuation', 'string_verbatim', 'string_escape_character', 'class_name',
          'delegate_name', 'enum_name', 'interface_name', 'module_name', 'struct_name', 'type_parameter_name',
          'field_name', 'enum_member_name', 'constant_name', 'local_name', 'parameter_name', 'method_name',
          'extension_method_name', 'property_name', 'event_name', 'namespace_name', 'label_name',
          'xml_doc_comment_attribute_name', 'xml_doc_comment_attribute_quotes', 'xml_doc_comment_attribute_value',
          'xml_doc_comment_cdata_section', 'xml_doc_comment_comment', 'xml_doc_comment_delimiter',
          'xml_doc_comment_entity_reference', 'xml_doc_comment_name', 'xml_doc_comment_processing_instruction',
          'xml_doc_comment_text', 'xml_literal_attribute_name', 'xml_literal_attribute_quotes',
          'xml_literal_attribute_value',
          'xml_literal_cdata_section', 'xml_literal_comment', 'xml_literal_delimiter', 'xml_literal_embedded_expression',
          'xml_literal_entity_reference', 'xml_literal_name', 'xml_literal_processing_instruction', 'xml_literal_text',
          'regex_comment', 'regex_character_class', 'regex_anchor', 'regex_quantifier', 'regex_grouping',
          'regex_alternation',
          'regex_text', 'regex_self_escaped_character', 'regex_other_escape',
        },
      },
      range = true,
    }
  end,
  capabalities = capabalities,
  cmd = { 'dotnet', vim.fn.expand('$HOME/.local/omnisharp/OmniSharp.dll') },
  enable_editorconfig_support = true,
  enable_ms_build_load_projects_on_demand = false,
  organize_imports_on_format = true,
  enable_import_completion = true,
  sdk_include_prereleases = true,
  enable_roslyn_analyzers = false,
  analyze_open_documents_only = false,
  handlers = vim.tbl_extend('keep', { ["textDocument/definition"] = require('omnisharp_extended').handler }, handlers),
})

local clangd_capabilities = vim.deepcopy(capabalities)
clangd_capabilities.offsetEncoding = 'utf-8'
lspconfig.clangd.setup({
  handlers = handlers,
  capabilities = clangd_capabilities,
  cmd = {
    'clangd',
    '--background-index',
    '--pch-storage=memory',
    '--clang-tidy',
    '--suggest-missing-includes',
    '--cross-file-rename',
    '--completion-style=detailed',
  },
  init_options = {
    clangdFileStatus = true,
    usePlaceholders = true,
    completeUnimported = true,
    semanticHighlighting = true,
  },
})
lspconfig.lua_ls.setup({
  handlers = handlers,
  capabalities = capabalities,
  settings = {
    Lua = {
      diagnostics = { globals = { 'vim' } },
    }
  }
})
lspconfig.ruby_ls.setup({
  enabled = false,
  handlers = handlers,
  capabalities = capabalities,
  settings = {
    init_options = {
      enabledFeatures = { 'documentHighlights', 'documentSymbols', 'foldingRanges', 'selectionRanges', 'formatting',
        'codeActions' }
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
  'gopls',
  'html',
  'zls',
  'docker_compose_language_service',
  'terraformls',
}
for _, server in pairs(servers) do
  lspconfig[server].setup {
    handlers = handlers,
    capabalities = capabalities,
  }
end
