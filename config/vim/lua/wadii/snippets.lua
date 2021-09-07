local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node

vim.api.nvim_set_keymap('s', '<a-j>', '<Plug>luasnip-next-choice', {})
vim.api.nvim_set_keymap('i', '<a-j>', '<Plug>luasnip-next-choice', {})

ls.snippets = {
  all = {
    s('todo', {
      t('TODO(wadii): '),
      i(0),
    }),
    s('xxx', {
      t('XXX(wadii): '),
      i(0),
    })
  },
  ruby = {
    s('rld', {
      t('Rails.logger.debug'),
      i(0),
    }),
    s('rli', {
      t('Rails.logger.info'),
      i(0),
    })
  }
}
