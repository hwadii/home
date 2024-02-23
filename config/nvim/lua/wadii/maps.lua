local map = vim.keymap.set
local various = require("wadii.various")

-- general
map("n", "<A-[>", "<cmd>vertical resize -5<cr>", { desc = "Decrease window width" })
map("n", "<A-]>", "<cmd>vertical resize +5<cr>", { desc = "Increase window width" })
map("n", "<A-Up>", "<cmd>horizontal resize +5<cr>", { desc = "Increase window height" })
map("n", "<A-Down>", "<cmd>horizontal resize -5<cr>", { desc = "Decrease window height" })
map("n", "<Left>", "gT")
map("n", "<Right>", "gt")
map("t", "<M-[>", "<C-\\><C-n>")
map({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- various
map(
  "n",
  "<Leader>wp",
  various.yank_current_path,
  { desc = "Yank the current absolute path in clipboard register" }
)
map("n", "<Leader>wu", various.make_unix, { desc = "Make fileformat unix" })
map("n", "<Leader>w-", "<cmd>Explore<cr>", { desc = "Open netrw" })
