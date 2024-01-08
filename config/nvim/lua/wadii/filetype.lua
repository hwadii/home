vim.filetype.add({
  extension = {
    prr = 'prr',
  },
  pattern = {
    ['.*Jenkinsfile.*'] = 'groovy',
  },
})
