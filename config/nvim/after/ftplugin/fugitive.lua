vim.keymap.set("n", "pp", "<cmd>Git push<cr>", { buffer = true })
vim.keymap.set("n", "p<space>", ":<C-U>Git push<space>", { buffer = true })

vim.keymap.set("n", "L", "<cmd>Git log<cr>", { buffer = true })
