local opt = vim.opt
opt.termguicolors = true

require('impatient')
require('wadii')
require('plugin.treesitter')

vim.g.zenbones = {
  lighten_noncurrent_window = true,
  colorize_diagnostic_underline_text = true,
  darkness = 'stark',
  lightness = 'dim'
}
vim.g.colors_name = 'zenbones'
opt.encoding = 'utf-8'
opt.magic = true
opt.autoindent = true
opt.background = 'dark'
opt.briopt = 'shift:2'
opt.completeopt = 'menuone,noselect'
opt.cursorline = true
opt.expandtab = true
opt.formatoptions = 'jcrqnl'
opt.fillchars = {
  diff = '∙',
  eob = '~',
  fold = '·',
  msgsep = '‾',
}
opt.list = true
opt.listchars = {
  tab = "┊ ",
  nbsp = '⦸',
  extends = '»',
  precedes = '«',
  trail = "·",
}
opt.emoji = false
opt.hidden = true
opt.ignorecase = true
opt.inccommand = 'split'
opt.laststatus = 3 -- Show global statusline as opposed to a statusline per window
opt.lazyredraw = true
opt.linebreak = true
opt.breakindent = true
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
opt.shortmess:append('actFTWI')
opt.matchpairs:append('<:>')
opt.showbreak = '↳ '
opt.signcolumn = 'auto'
opt.smartcase = true -- make search case insensitive by default
opt.smartindent = true
opt.softtabstop = 2
opt.splitbelow = true  -- Splitting a window will put the new window below the current
opt.splitright = true  -- Splitting a window will put the new window right of the current
opt.tabstop = 2
opt.undofile = true        -- Persistent undo
opt.undolevels = 1000      -- Maximum number of changes that can be undone
opt.undoreload = 10000     -- Maximum number lines to save for undo on a buffer reload
opt.updatetime = 250
opt.wildmenu = true
opt.wildoptions = 'pum'
opt.wildignore = { '__pycache__', '*.o', '*~', '*.pyc', '*pycache*' }
opt.winminheight = 0
opt.scrolloff = 3
opt.foldlevelstart = 99 -- start unfolded
opt.foldtext = 'v:lua.wadii.foldtext()'
opt.grepprg = 'rg --vimgrep --no-heading'
opt.grepformat = '%f:%l:%c:%m,%f:%l:%m'

vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank({ higroup = 'IncSearch', timeout = 1000 })
  end,
  group = vim.api.nvim_create_augroup('yank_post_group', { clear = true }),
})

vim.api.nvim_create_autocmd('BufWritePost', {
  pattern = 'plugins.lua',
  command = 'source <afile> | PackerCompile',
  group = vim.api.nvim_create_augroup('packer_user_config', { clear = true }),
})

vim.api.nvim_set_hl(0, 'User1', { fg = '#B4BDC3', bg = '#302825', bold = true })
vim.api.nvim_set_hl(0, 'User2', { fg = '#CAD0D4', bg = '#272321', italic = true, reverse = true })
vim.api.nvim_set_hl(0, 'MsgSeparator', { link = 'VertSplit' })

vim.g.vim_markdown_override_foldtext = 0
vim.g.vim_markdown_no_default_key_mappings = 1
vim.g.vim_markdown_emphasis_multiline = 0
vim.g.vim_markdown_conceal = 0
vim.g.vim_markdown_conceal_code_blocks = 0
vim.g.vim_markdown_frontmatter = 1
vim.g.rooter_silent_chdir = 1
vim.g.ledger_align_at = 52
vim.g.slime_target = 'tmux'
vim.g.slime_paste_file = '/tmp/.slime_paste'
vim.g.slime_default_config = { socket_name = "default", target_pane = "{last}" }
vim.g.slime_dont_ask_default = 1
vim.g.rsi_no_meta = 1
vim.g.rooter_manual_only = true
vim.g.did_load_filetypes = 0
vim.g.do_filetype_lua = 1
vim.g.dirvish_mode = [[ :sort ,^.*[\/], ]]
