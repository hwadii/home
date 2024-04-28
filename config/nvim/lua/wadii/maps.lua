local various = require("wadii.various")

-- general
vim.keymap.set("n", "<A-[>", "<cmd>vertical resize -5<cr>", { desc = "Decrease window width" })
vim.keymap.set("n", "<A-]>", "<cmd>vertical resize +5<cr>", { desc = "Increase window width" })
vim.keymap.set("n", "<A-Up>", "<cmd>horizontal resize +5<cr>", { desc = "Increase window height" })
vim.keymap.set(
  "n",
  "<A-Down>",
  "<cmd>horizontal resize -5<cr>",
  { desc = "Decrease window height" }
)
vim.keymap.set("n", "<Left>", "gT")
vim.keymap.set("n", "<Right>", "gt")
vim.keymap.set("t", "<M-[>", "<C-\\><C-n>")
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
vim.keymap.set("n", "<C-I>", "<C-I>", { noremap = true })

-- various
vim.keymap.set(
  "n",
  "y.",
  various.yank_current_path,
  { desc = "Yank the current relative path in clipboard register" }
)
vim.keymap.set("n", "<Leader>wu", various.make_unix, { desc = "Make fileformat unix" })
vim.keymap.set("n", "<Leader>w-", "<cmd>Explore<cr>", { desc = "Open netrw" })

vim.keymap.set("n", ",,", ",", { noremap = true, desc = "repeat last f/F t/T command" })
