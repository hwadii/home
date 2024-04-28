vim.opt.termguicolors = true
vim.opt.encoding = "utf-8"
vim.opt.magic = true
vim.opt.autoindent = true
vim.opt.background = "dark"
vim.opt.briopt = "shift:2"
vim.opt.completeopt = "menu,menuone,noinsert"
vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.formatoptions = "jcrqnl"
vim.opt.fillchars = {
  diff = "∙",
  eob = "~",
  fold = "·",
  msgsep = "‾",
}
vim.opt.list = true
vim.opt.listchars = {
  tab = "┊ ",
  nbsp = "␣",
  extends = "»",
  precedes = "«",
  trail = "·",
}
vim.opt.hidden = true
vim.opt.ignorecase = true
vim.opt.inccommand = "split"
vim.opt.laststatus = 3 -- Show global statusline as opposed to a statusline per window
vim.opt.lazyredraw = true
vim.opt.linebreak = true
vim.opt.breakindent = true
vim.opt.mouse = "a"
vim.opt.backup = false
vim.opt.joinspaces = false
vim.opt.swapfile = false
vim.opt.writebackup = false
vim.opt.number = false
vim.opt.pumheight = 20
vim.opt.redrawtime = 10000
vim.opt.report = 0
vim.opt.rnu = false
vim.opt.shiftwidth = 2
vim.opt.shortmess:append("acCtFTW")
vim.opt.matchpairs:append("<:>")
vim.opt.showbreak = "↪"
vim.opt.signcolumn = "yes"
vim.opt.smartcase = true -- make search case insensitive by default
vim.opt.smartindent = true
vim.opt.softtabstop = 2
vim.opt.splitbelow = true -- Splitting a window will put the new window below the current
vim.opt.splitright = true -- Splitting a window will put the new window right of the current
vim.opt.tabstop = 2
vim.opt.undofile = true -- Persistent undo
vim.opt.undolevels = 1000 -- Maximum number of changes that can be undone
vim.opt.undoreload = 10000 -- Maximum number lines to save for undo on a buffer reload
vim.opt.updatetime = 250
vim.opt.wildmenu = true
vim.opt.wildoptions = "pum"
vim.opt.wildignore = { "*.o", "*~" }
vim.opt.winminheight = 0
vim.opt.foldtext = "v:lua.wadii.foldtext()"
vim.o.grepprg = "rg --vimgrep -uu"
vim.o.grepformat = "%f:%l:%c:%m"
vim.cmd([[
  vnoremenu PopUp.Cut                     "+x
  vnoremenu PopUp.Copy                    "+y
  anoremenu PopUp.Paste                   "+gP
  vnoremenu PopUp.Paste                   "+P
  vnoremenu PopUp.Delete                  "_x
  nnoremenu PopUp.Select\ All             ggVG
  vnoremenu PopUp.Select\ All             gg0oG$
  inoremenu PopUp.Select\ All             <C-Home><C-O>VG
  anoremenu PopUp.Inspect                 <Cmd>Inspect<CR>
  anoremenu PopUp.-1-                     <Nop>
]])
