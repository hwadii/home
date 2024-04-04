vim.keymap.set("n", "pp", "<cmd>Git push<cr>", { buffer = true })
vim.keymap.set("n", "pf", "<cmd>Git push --force-with-lease<cr>", { buffer = true })
vim.keymap.set("n", "p<space>", ":<C-U>Git push<space>", { buffer = true })

vim.keymap.set("n", "L", "<cmd>Git log<cr>", { buffer = true })

vim.keymap.set("n", "<Tab>", "=", { buffer = true, remap = true })

vim.keymap.set("n", "B", "<cmd>GBrowse<cr>", { buffer = true })
