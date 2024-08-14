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
vim.keymap.set("n", "<leader>w=", "<cmd>Calc<cr>", { desc = "Quick calc" })

vim.keymap.set("n", "<c-w>`", various.zoom, { desc = "Zoom in buffer" })

vim.keymap.set("n", ",,", ",", { noremap = true, desc = "repeat last f/F t/T command" })

vim.keymap.set("t", "<S-Space>", "<Space>", { desc = "Insert space when shift-space is pressed" })
vim.keymap.set("t", "<A-Space>", "<Space>", { desc = "Insert space when alt-space is pressed" })
vim.keymap.set("t", "<S-BS>", "<BS>", { desc = "Insert backspsace when shift-backspace is pressed" })
vim.keymap.set("t", "<C-BS>", "<BS>", { desc = "Insert backspsace when ctrl-backspace is pressed" })
vim.keymap.set("t", "<S-Enter>", "<Enter>", { desc = "Insert enter when shift-enter is pressed" })
vim.keymap.set("t", "<C-Enter>", "<Enter>", { desc = "Insert enter when ctrl-enter is pressed" })
vim.keymap.set("t", "<C-/>", "<C-_>", { desc = "Undo" })
