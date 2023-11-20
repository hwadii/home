vim.g.mapleader = ','
vim.g.maplocalleader = ' '

vim.loader.enable()

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
  defaults = { lazy = true },
  change_detection = {
    enabled = true,
    notify = false,
  },
  performance = {
    rtp = {
      disabled_plugins = {
        'gzip',
        'tar',
        'tarPlugin',
        'zip',
        'zipPlugin',
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

require('wadii')
require('wadii.options')
require('wadii.autocmds')
require('wadii.filetype')

vim.g.vim_markdown_override_foldtext = 0
vim.g.vim_markdown_no_default_key_mappings = 1
vim.g.vim_markdown_emphasis_multiline = 0
vim.g.vim_markdown_conceal = 0
vim.g.vim_markdown_conceal_code_blocks = 0
vim.g.vim_markdown_frontmatter = 1
vim.g.vim_markdown_borderless_table = 0
vim.g.ledger_align_at = 52
vim.g.slime_target = 'tmux'
vim.g.slime_paste_file = '/tmp/.slime_paste'
vim.g.slime_default_config = { socket_name = "default", target_pane = "{last}" }
vim.g.slime_dont_ask_default = 1
vim.g.slime_no_mappings = 1
vim.g.rsi_no_meta = 1
vim.g.dirvish_mode = [[ :sort ,^.*[\/], ]]
vim.g.navic_silence = 1
vim.g.netrw_banner = 0
vim.g.loaded_fzf = 1
vim.g.loaded_python3_provider = 0
vim.g.loaded_netrwPlugin = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_node_provider = 0
vim.g.skip_ts_context_commentstring_module = true
