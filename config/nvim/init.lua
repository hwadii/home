vim.g.mapleader = ","
vim.g.maplocalleader = " "

vim.loader.enable()

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--single-branch",
    "https://github.com/folke/lazy.nvim.git",
    lazypath,
  })
end

vim.opt.runtimepath:prepend(lazypath)

require("lazy").setup("plugins", {
  defaults = { lazy = true },
  change_detection = {
    enabled = true,
    notify = false,
  },
  performance = {
    rtp = {
      disabled_plugins = {
        "tar",
        "tarPlugin",
        "zip",
        "zipPlugin",
        "tohtml",
        "tutor",
        "getscript",
        "getscriptPlugin",
        "vimball",
        "vimballPlugin",
        "netrwPlugin",
        "netrwSettings",
        "rrhelper",
        "logiPat",
      },
    },
  },
})

require("wadii")

vim.g.netrw_banner = 0
vim.g.netrw_keepdir = 1
vim.g.netrw_sort_options = "i"
vim.g.loaded_fzf = 1
vim.g.loaded_python3_provider = 0
vim.g.loaded_netrwPlugin = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_node_provider = 0
