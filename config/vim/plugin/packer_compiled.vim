" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif

packadd packer.nvim

try

lua << END
local package_path_str = "/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    print('Error running ' .. component .. ' for ' .. name)
    error(result)
  end
  return result
end

_G.packer_plugins = {
  ["auto-pairs"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/auto-pairs"
  },
  ["gina.vim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/gina.vim"
  },
  ["github_dark.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/github_dark.nvim"
  },
  ["gitsigns.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  loupe = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/loupe"
  },
  ["nvim-colorizer.lua"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua"
  },
  ["nvim-compe"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-compe"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["org.vim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/org.vim"
  },
  ["packer.nvim"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/opt/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["seoul256.vim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/seoul256.vim"
  },
  ["snippets.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/snippets.nvim"
  },
  ["splitjoin.vim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/splitjoin.vim"
  },
  ["telescope-fzy-native.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/telescope-fzy-native.nvim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["vim-abolish"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-abolish"
  },
  ["vim-commentary"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-commentary"
  },
  ["vim-dirvish"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-dirvish"
  },
  ["vim-ledger"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-ledger"
  },
  ["vim-markdown"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-markdown"
  },
  ["vim-rooter"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-rooter"
  },
  ["vim-sleuth"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-sleuth"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-surround"
  },
  ["vim-unimpaired"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-unimpaired"
  }
}

END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
