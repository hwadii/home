local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

vim.api.nvim_set_keymap('s', '<a-j>', '<Plug>luasnip-next-choice', {})
vim.api.nvim_set_keymap('i', '<a-j>', '<Plug>luasnip-next-choice', {})

ls.add_snippets('all', {
  s('date', {
    t(os.date('%Y-%m-%d')),
  }),
  s('todo', {
    t('TODO(wadii): '),
  }),
  s('xxx', {
    t('XXX(wadii): '),
  }),
})

ls.add_snippets('ruby', {
  s('rld', {
    t('Rails.logger.debug '),
  }),
  s('rli', {
    t('Rails.logger.info '),
  }),
})
