local map = vim.keymap.set
local various = require('wadii.various')

-- general
map('n', '<esc>', '<cmd>noh<return><esc>')
map('n', '<C-/>', '<cmd>set hlsearch!<cr>')
map('n', '<A-[>', '<cmd>vertical resize -5<cr>', { desc = 'Decrease window width' })
map('n', '<A-]>', '<cmd>vertical resize +5<cr>', { desc = 'Increase window width' })
map('n', '<A-Up>', '<cmd>horizontal resize +5<cr>', { desc = 'Increase window height' })
map('n', '<A-Down>', '<cmd>horizontal resize -5<cr>', { desc = 'Decrease window height' })
map('n', '<Left>', 'gT')
map('n', '<Right>', 'gt')
map('t', '<M-[>', '<C-\\><C-n>')
map({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- various
map('n', '<Leader>wp', various.yank_current_path)
map('n', '<Leader>wu', various.make_unix)
