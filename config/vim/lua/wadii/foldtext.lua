-- Stolen from https://github.com/wincent/wincent/blob/f53783e767df8c4c3fedeefde07497d08cb8b525/aspects/nvim/files/.config/nvim/lua/wincent/foldtext.lua
local middot = '·'
local raquo = '»'
local small_l = 'ℓ'

local foldtext = function()
  local lines = '[' .. (vim.v.foldend - vim.v.foldstart + 1) .. small_l .. ']'
  local first = ({vim.api.nvim_buf_get_lines(0, vim.v.foldstart - 1, vim.v.foldstart, true)[1]:gsub(' *', '', 1)})[1]
  local dashes = ({vim.v.folddashes:gsub('-', middot)})[1]
  return raquo .. middot .. middot .. lines .. dashes .. ': ' .. first
end

return foldtext
