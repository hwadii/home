vim.opt_local.spell = false

vim.keymap.set({ "n", "x" }, "<Enter>", function() require("mini.git").show_at_cursor() end, { buffer = true, desc = "Show at cursor" })
vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = true })
