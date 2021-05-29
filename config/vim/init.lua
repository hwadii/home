require('lsp_config')
local fn = vim.fn
local opt = vim.opt
local execute = vim.api.nvim_command

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  execute 'packadd packer.nvim'
end

opt.termguicolors = true
require('plugins')
require('gitsigns').setup {
  signs = {
    add          = {hl = 'DiffAdd'   , text = ' ', numhl='GitSignsAddNr'},
    change       = {hl = 'DiffChange', text = ' ', numhl='GitSignsChangeNr'},
    delete       = {hl = 'DiffDelete', text = ' ', numhl='GitSignsDeleteNr'},
    topdelete    = {hl = 'DiffDelete', text = ' ', numhl='GitSignsDeleteNr'},
    changedelete = {hl = 'DiffChange', text = ' ', numhl='GitSignsChangeNr'},
  }
}

vim.cmd 'colorscheme github_dark'
opt.encoding = 'utf-8'
opt.magic = true
opt.autoindent = true
opt.background = 'dark'
opt.briopt = 'shift:2'
opt.completeopt = 'menuone,noselect'
opt.cursorline = true
opt.expandtab = true
opt.fileformats = 'unix,dos,mac'
opt.formatoptions = 'jcrqnl'
opt.fillchars = { eob = "~", vert = "|", stl = " ", stlnc = " " }
opt.list = true
opt.listchars = { tab = "┊ ", nbsp = "␣", trail = "·", extends = "↷", precedes = "↶" }
opt.hidden = true
opt.ignorecase = true
opt.inccommand = 'split'
opt.lazyredraw = true
opt.linebreak = true
opt.mouse = 'a'
opt.backup = false
opt.joinspaces = false
opt.swapfile = false
opt.writebackup = false
opt.number = true
opt.pumheight = 20
opt.redrawtime = 10000
opt.report = 0
opt.rnu = false
opt.shiftwidth = 2
opt.shortmess = 'filnxtToOFc'
opt.showbreak = '↳ '
opt.signcolumn = 'yes'
opt.smartcase = true -- make search case insensitive by default
opt.smartindent = true
opt.softtabstop = 2
opt.splitbelow = true  -- Splitting a window will put the new window below the current
opt.splitright = true  -- Splitting a window will put the new window right of the current
opt.tabstop = 2
opt.undofile = true        -- Persistent undo
opt.undolevels = 1000      -- Maximum number of changes that can be undone
opt.undoreload = 10000     -- Maximum number lines to save for undo on a buffer reload
opt.updatetime = 300
opt.wildmenu = true
opt.wildoptions = 'pum'
opt.wildignore = { '__pycache__', '*.o', '*~', '*.pyc', '*pycache*' }
opt.winminheight = 0
opt.pumblend = 17
opt.scrolloff = 3

if vim.g.enable_folding == 1 then
  opt.foldenable = true
  opt.foldmarker = '{,}'
  opt.foldlevel = 0
  opt.foldmethod = 'marker'
  opt.foldlevelstart = 99
end

vim.g.seoul256_background = 233
vim.g.seoul256_srgb = 1
vim.g.enable_folding = 1
execute [[autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank {higroup='IncSearch', timeout=1000}]]
execute [[autocmd BufWritePost plugins.lua PackerCompile]]
vim.g.mapleader = ','
vim.g.maplocalleader = ' '
vim.g.grepprg = 'rg --vimgrep --no-heading '
vim.g.grepformat = '%f:%l:%c:%m,%f:%l:%m'
