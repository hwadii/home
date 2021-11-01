-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/wadii/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  LuaSnip = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/LuaSnip"
  },
  ["auto-pairs"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/auto-pairs"
  },
  ["cmp-buffer"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/cmp-buffer"
  },
  ["cmp-nvim-lsp"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/cmp-nvim-lsp"
  },
  ["cmp-path"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/cmp-path"
  },
  cmp_luasnip = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/cmp_luasnip"
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
  ["gruber_darker.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/gruber_darker.nvim"
  },
  ["impatient.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/impatient.nvim"
  },
  ["lush.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/lush.nvim"
  },
  ["null-ls.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/null-ls.nvim"
  },
  ["nvim-cmp"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-cmp"
  },
  ["nvim-colorizer.lua"] = {
    config = { "\27LJ\2\nI\0\0\4\0\4\0\b6\0\0\0'\2\1\0B\0\2\0029\0\2\0004\2\0\0005\3\3\0B\0\3\1K\0\1\0\1\0\1\nnames\1\nsetup\14colorizer\frequire\0" },
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua"
  },
  ["nvim-lsp-ts-utils"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-lsp-ts-utils"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-treesitter-angular"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-treesitter-angular"
  },
  ["nvim-treesitter-refactor"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-treesitter-refactor"
  },
  ["nvim-treesitter-textobjects"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-treesitter-textobjects"
  },
  ["nvim-ts-context-commentstring"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/nvim-ts-context-commentstring"
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
  ["unicode.vim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/unicode.vim"
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
  ["vim-racket"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-racket"
  },
  ["vim-rooter"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-rooter"
  },
  ["vim-rsi"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-rsi"
  },
  ["vim-sleuth"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-sleuth"
  },
  ["vim-slime"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-slime"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-surround"
  },
  ["vim-unimpaired"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/vim-unimpaired"
  },
  ["zenbones.nvim"] = {
    loaded = true,
    path = "/home/wadii/.local/share/nvim/site/pack/packer/start/zenbones.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: nvim-colorizer.lua
time([[Config for nvim-colorizer.lua]], true)
try_loadstring("\27LJ\2\nI\0\0\4\0\4\0\b6\0\0\0'\2\1\0B\0\2\0029\0\2\0004\2\0\0005\3\3\0B\0\3\1K\0\1\0\1\0\1\nnames\1\nsetup\14colorizer\frequire\0", "config", "nvim-colorizer.lua")
time([[Config for nvim-colorizer.lua]], false)
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
