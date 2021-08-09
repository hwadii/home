local opts = { noremap = true, silent = true }
local set_keymap = vim.api.nvim_set_keymap

vim.g.mapleader = ','
vim.g.maplocalleader = ' '

-- general
set_keymap("n", "<A-j>", "<cmd>m .+1<CR>==", opts)
set_keymap("n", "<A-k>", "<cmd>m .-2<CR>==", opts)
set_keymap("x", "<A-j>", "<cmd>m '>+1<CR>gv=gv", opts)
set_keymap("x", "<A-k>", "<cmd>m '<-2<CR>gv=gv", opts)
set_keymap("i", "<C-d>", "<Del>", opts)
set_keymap("n", "<esc>", "<cmd>noh<return><esc>", opts)
set_keymap("n", "<C-/>", "<cmd>set hlsearch!<cr>", opts)
set_keymap("n", "<A-[>", "<cmd>vertical resize -5<cr>", opts)
set_keymap("n", "<A-]>", "<cmd>vertical resize +5<cr>", opts)
set_keymap("n", "[t", "gT", opts)
set_keymap("n", "]t", "gt", opts)
set_keymap("t", "<M-[>", "<C-\\><C-n>", opts)

-- telescope
set_keymap("n", "<C-p>", "<cmd>Telescope find_files find_command=fd,--hidden,-E.git,-tf<CR>", opts)
set_keymap("n", "<Leader>pp", "<cmd>Telescope<cr>", opts)
set_keymap("n", "<Leader>s", "<cmd>Telescope live_grep<cr>", opts)
set_keymap("n", "<Leader><Leader>s", "<cmd>Telescope grep_string<cr>", opts)
set_keymap("n", "<localleader>b", "<cmd>Telescope buffers<cr>", opts)
set_keymap("n", "<Leader>h", "<cmd>Telescope oldfiles<cr>", opts)
set_keymap("n", "<Leader>c", "<cmd>Telescope git_commits<cr>", opts)
set_keymap("n", "<Leader>o", "<cmd>Telescope lsp_document_symbols<cr>", opts)
set_keymap("n", "<Leader>r", "<cmd>Telescope lsp_references<cr>", opts)
set_keymap("n", "ga", "<cmd>Telescope lsp_code_actions<cr>", opts)
set_keymap("n", "<C-s>", "<cmd>Telescope current_buffer_fuzzy_find<cr>", opts)

-- compe
set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
set_keymap("i", "<C-Space>", "compe#complete()", {expr = true})
set_keymap("i", "<cr>", "compe#confirm('<cr>')", {expr = true})

-- gina
set_keymap("n", "<Leader>gs", "<cmd>Gina status -s<cr>", opts)
set_keymap("n", "<Leader>gc", "<cmd>Gina commit<cr>", opts)
set_keymap("n", "<Leader>gp", "<cmd>Gina push<cr>", opts)
set_keymap("n", "<Leader>gl", "<cmd>Gina log<cr>", opts)
set_keymap("n", "<Leader>gd", "<cmd>Gina diff<cr>", opts)
set_keymap("n", "<Leader>gb", "<cmd>Gina blame<cr>", opts)
set_keymap("n", "<Leader>geb", "<Plug>(gina-blame-echo)", opts)
set_keymap("n", "<Leader>gv",  "gina#component#repo#branch()", { noremap = true, silent = true, expr = true })

