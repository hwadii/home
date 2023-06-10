local cmp = require('cmp')

local feedkeys = vim.fn.feedkeys
local pumvisible = vim.fn.pumvisible
local replace_termcodes = function(key)
  return vim.api.nvim_replace_termcodes(key, true, true, true)
end

cmp.setup {
  snippet = {
    expand = function(args)
      require('snippy').expand_snippet(args.body)
    end
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    ["<Tab>"] = function(fallback)
      if pumvisible() == 1 then
        feedkeys(replace_termcodes("<C-n>"), "n")
      elseif cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if pumvisible() == 1 then
        feedkeys(replace_termcodes("<C-p>"), "n")
      elseif cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  }),
  window = {
    documentation = cmp.config.window.bordered(),
  },
  sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "snippy" },
    }, {
      { name = "buffer", max_item_count = 5 },
      { name = "path" },
    }),
}
