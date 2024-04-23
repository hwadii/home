return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      diagnostics = {
        underline = true,
        signs = false,
        update_in_insert = false,
        virtual_text = {
          prefix = "■",
          spacing = 4,
          source = "if_many",
        },
        severity_sort = true,
      },
      capabilities = {},
    },
    config = function(_, opts)
      local lspconfig = require("lspconfig")
      local telescope = require("telescope.builtin")
      local themes = require("telescope.themes")
      local capabalities =
        require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

      vim.lsp.handlers["textDocument/hover"] =
        vim.lsp.with(vim.lsp.handlers.hover, { max_width = 100 })
      vim.lsp.handlers["textDocument/signatureHelp"] =
        vim.lsp.with(vim.lsp.handlers.signature_help, { max_width = 100 })

      vim.api.nvim_create_autocmd("LspAttach", {
        desc = "Lsp actions",
        callback = function(event)
          local client = vim.lsp.get_client_by_id(event.data.client_id)
          local function map(mode, keys, func)
            vim.keymap.set(mode, keys, func, { buffer = event.buf })
          end
          if client.name == "omnisharp" then
            map("n", "gr", require("omnisharp_extended").lsp_references)
            map("n", "gd", require("omnisharp_extended").lsp_definition)
            map("n", "<leader>i", require("omnisharp_extended").lsp_implementation)
            map("n", "<Leader>sr", require("omnisharp_extended").telescope_lsp_references)
          else
            map("n", "gr", vim.lsp.buf.references)
            map("n", "gd", vim.lsp.buf.definition)
            map("n", "<leader>i", vim.lsp.buf.implementation)
            map("n", "<Leader>sr", telescope.lsp_references)
          end
          map("n", "gA", vim.lsp.buf.code_action)
          map("n", "K", vim.lsp.buf.hover)
          map("n", "gy", vim.lsp.buf.type_definition)
          map("n", "<localleader>ws", vim.lsp.buf.workspace_symbol)
          map("n", "<localleader>wa", vim.lsp.buf.add_workspace_folder)
          map("n", "<localleader>wr", vim.lsp.buf.remove_workspace_folder)
          map("n", "<localleader>wl", function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          end)
          map("n", "gR", vim.lsp.buf.rename)
          map("n", "<localleader>q", vim.diagnostic.setloclist)
          map("n", "]d", vim.diagnostic.goto_next)
          map("n", "[d", vim.diagnostic.goto_prev)
          map("i", "<c-]>", vim.lsp.buf.signature_help)
          map("n", "<Leader>sd", function()
            telescope.diagnostics(themes.get_dropdown({ previewer = false }))
          end)
          map("n", "<Leader>so", telescope.lsp_document_symbols)
          map("n", "<localleader>o", vim.lsp.buf.document_symbol)
          map("n", "<localleader>i", function()
            vim.diagnostic.open_float({ max_width = 100 })
          end)

          vim.api.nvim_buf_create_user_command(event.buf, "FormatLsp", function()
            vim.lsp.buf.format({ async = true })
          end, { desc = "Format current buffer with LSP" })

          if client.server_capabilities.documentHighlightProvider then
            vim.api.nvim_create_augroup("lsp_document_highlight", { clear = false })
            vim.api.nvim_clear_autocmds({ buffer = event.buf, group = "lsp_document_highlight" })
            vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
              group = "lsp_document_highlight",
              buffer = event.buf,
              callback = function()
                -- Ignore decompiled c# source files
                if not event.file:match("metadata") and vim.lsp.buf.server_ready() then
                  vim.lsp.buf.document_highlight()
                end
              end,
            })
            vim.api.nvim_create_autocmd("CursorMoved", {
              group = "lsp_document_highlight",
              buffer = event.buf,
              callback = vim.lsp.buf.clear_references,
            })
          end
        end,
      })

      local ngserver_cmd = function()
        local node_path = string.gsub(vim.fn.system("mise where node"), "\n", "")
        local libpath = vim.fn.expand(node_path .. "/lib/node_modules/typescript/lib")
        return {
          "ngserver",
          "--stdio",
          "--tsProbeLocations",
          libpath,
          "--ngProbeLocations",
          libpath,
        }
      end
      lspconfig.angularls.setup({
        cmd = ngserver_cmd(),
        capabalities = capabalities,
        on_new_config = function(new_config, _)
          new_config.cmd = ngserver_cmd()
        end,
      })
      lspconfig.cssls.setup({
        capabilities = capabalities,
      })
      lspconfig.rust_analyzer.setup({
        capabalities = capabalities,
        settings = {
          ["rust-analyzer"] = {
            assist = {
              importGranularity = "module",
              importPrefix = "by_self",
            },
            cargo = {
              loadOutDirsFromCheck = true,
            },
            procMacro = {
              enable = true,
            },
            checkOnSave = {
              command = "clippy",
            },
          },
        },
      })
      lspconfig.omnisharp.setup({
        capabalities = capabalities,
        cmd = { "dotnet", vim.fn.expand("$HOME/.local/omnisharp/OmniSharp.dll") },
        settings = {
          FormattingOptions = {
            EnableEditorConfigSupport = true,
            OrganizeImports = true,
          },
          MsBuild = {
            LoadProjectsOnDemand = nil,
          },
          RoslynExtensionsOptions = {
            EnableAnalyzersSupport = nil,
            EnableImportCompletion = true,
            AnalyzeOpenDocumentsOnly = nil,
          },
          Sdk = {
            IncludePrereleases = true,
          },
        },
      })

      local clangd_capabilities = vim.deepcopy(capabalities)
      clangd_capabilities.offsetEncoding = "utf-8"
      lspconfig.clangd.setup({
        capabilities = clangd_capabilities,
        cmd = {
          "clangd",
          "--background-index",
          "--pch-storage=memory",
          "--clang-tidy",
          "--suggest-missing-includes",
          "--cross-file-rename",
          "--completion-style=detailed",
        },
        init_options = {
          clangdFileStatus = true,
          usePlaceholders = true,
          completeUnimported = true,
          semanticHighlighting = true,
        },
      })
      lspconfig.lua_ls.setup({
        capabalities = capabalities,
        settings = {
          Lua = {
            diagnostics = { globals = { "vim" } },
          },
        },
      })
      lspconfig.ruby_lsp.setup({
        enabled = false,
        capabalities = capabalities,
        settings = {
          init_options = {
            enabledFeatures = {
              "documentHighlights",
              "documentSymbols",
              "foldingRanges",
              "selectionRanges",
              "formatting",
              "codeActions",
            },
          },
        },
      })

      local servers = {
        "solargraph",
        "vuels",
        "jsonls",
        "bashls",
        "pylsp",
        "racket_langserver",
        "emmet_ls",
        "gopls",
        "html",
        "zls",
        "docker_compose_language_service",
        "terraformls",
        "gleam",
      }
      for _, server in pairs(servers) do
        lspconfig[server].setup({
          capabalities = capabalities,
        })
      end
      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
      vim.fn.sign_define("DiagnosticSignError", { text = "×", texthl = "DiagnosticSignError" })
      vim.fn.sign_define("DiagnosticSignWarn", { text = "!", texthl = "DiagnosticSignWarn" })
      vim.fn.sign_define("DiagnosticSignHint", { text = "i", texthl = "DiagnosticSignHint" })
      vim.fn.sign_define("DiagnosticSignInfo", { text = "H", texthl = "DiagnosticInfo" })
    end,
  },
}
