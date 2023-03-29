vim.g.mapleader = ','
vim.g.maplocalleader = ' '

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    '--single-branch',
    'https://github.com/folke/lazy.nvim.git',
    lazypath,
  }
end

vim.opt.runtimepath:prepend(lazypath)

require('lazy').setup('plugins', {
  defaults = { lazy = false },
  performance = {
    rtp = {
      disabled_plugins = {
        'gzip',
        'tar',
        'tarPlugin',
        'zip',
        'zipPlugin',

        'netrw',
        'tohtml',
        'tutor',
        'getscript',
        'getscriptPlugin',
        'vimball',
        'vimballPlugin',
        'netrwSettings',
        'rrhelper',
        'logiPat',
      },
    },
  },
})
vim.g.colors_name = 'ploy'

require('impatient')
require('wadii')
require('wadii.options')
require('wadii.autocmds')

vim.g.vim_markdown_override_foldtext = false
vim.g.vim_markdown_no_default_key_mappings = true
vim.g.vim_markdown_emphasis_multiline = false
vim.g.vim_markdown_conceal = false
vim.g.vim_markdown_conceal_code_blocks = false
vim.g.vim_markdown_frontmatter = true
vim.g.ledger_align_at = 52
vim.g.slime_target = 'tmux'
vim.g.slime_paste_file = '/tmp/.slime_paste'
vim.g.slime_default_config = { socket_name = "default", target_pane = "{last}" }
vim.g.slime_dont_ask_default = true
vim.g.slime_no_mappings = true
vim.g.rsi_no_meta = true
vim.g.dirvish_mode = [[ :sort ,^.*[\/], ]]
vim.g.navic_silence = true
vim.g.netrw_banner = false
vim.g.loaded_fzf = true
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_node_provider = 0
