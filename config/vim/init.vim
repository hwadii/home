call plug#begin('~/.config/nvim/bundle/')
Plug 'andreypopp/vim-colors-plain'
Plug 'justinmk/vim-dirvish'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'lambdalisue/gina.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-abolish'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'ledger/vim-ledger'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'nvim-treesitter/nvim-treesitter', { 'commit': 'e45ab15' }
Plug 'sheerun/vim-polyglot'
Plug 'plasticboy/vim-markdown'
Plug 'axvr/org.vim'
Plug 'airblade/vim-rooter'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'wincent/loupe'
call plug#end()

lua << EOF
  require ('globals')
  local opt = vim.opt
  local command = vim.api.nvim_command
  command('colorscheme plain')
  opt.encoding = 'utf-8'
  opt.t_Co = '256'
  opt.background = 'dark'
  opt.termguicolors = true
  opt.briopt = 'shift:2'
  opt.completeopt = 'menuone,noinsert,noselect'
  opt.cursorline = true
  opt.expandtab = true
  opt.fileformats = 'unix,dos,mac'
  opt.formatoptions = 'jcrqnl'
  opt.fillchars = { eob = "~" }
  opt.hidden = true
  opt.ignorecase = true
  opt.inccommand = 'split'
  opt.lazyredraw = true
  opt.linebreak = true
  command('set lcs="nbsp:⦸,tab:→,eol:↵,trail:·,extends:↷,precedes:↶"')
  opt.mouse = 'a'
  opt.backup = false
  opt.joinspaces = false
  opt.swapfile = false
  opt.writebackup = false
  opt.number = true
  opt.pumheight = 20
  opt.redrawtime = 10000
  opt.report = 0
  opt.rnu = true
  opt.shiftwidth = 2
  opt.shortmess = 'atOI'
  opt.showbreak = '↳ '
  opt.signcolumn = 'yes'
  opt.smartcase = true -- make search case insensitive by default
  opt.smartindent = true
  opt.softtabstop = 2
  opt.splitbelow = true  -- Splitting a window will put the new window below the current
  opt.splitright = true  -- Splitting a window will put the new window right of the current
  opt.t_ut = ''
  opt.tabstop = 2
  opt.undofile = true        -- Persistent undo
  opt.undolevels = 1000      -- Maximum number of changes that can be undone
  opt.undoreload = 10000     -- Maximum number lines to save for undo on a buffer reload
  opt.updatetime = 300
  opt.wildmenu = true
  opt.wildmode = {'longest', 'full'}
  opt.wildoptions = 'pum'
  opt.winminheight = 0
  opt.pumblend = 17

  if vim.g.vim_better_default_enable_folding == 1 then
    opt.foldenable = true
    -- opt.foldmarker = '{,}'
    opt.foldlevel = 0
    opt.foldmethod = 'marker'
    opt.foldlevelstart = 99
  end

  vim.g.gitgutter_sign_modified = '!!'
  vim.g.gitgutter_sign_modified_removed = '!_'
  vim.g.vim_better_default_enable_folding = 1
  vim.g.maplocalleader = "<space>"
  vim.g.mapleader = ","
  -- command('iabbrev aa λ')
  -- command('autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank {higroup="IncSearch", timeout=1000}')
EOF
" command! -nargs=* -complete=shellcmd R new | setlocal buftype=nofile bufhidden=hide noswapfile | r !<args>
" command! Scratch lua require'tools'.makeScratch()
