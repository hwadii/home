local wadii = {
  foldtext = require'wadii.foldtext',
  lsp = require'wadii.lsp',
  snippets = require'wadii.snippets',
  statusline = require'wadii.statusline',
  telescope = require'wadii.telescope',
  maps = require'wadii.maps',
  completion = require'wadii.completion',
}

_G.wadii = wadii

return wadii
