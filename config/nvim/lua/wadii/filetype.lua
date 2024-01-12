vim.filetype.add({
  extension = {
    prr = 'prr',
  },
  pattern = {
    ['.*Jenkinsfile.*'] = 'groovy',
    [".*docker%-compose%..*"] = "yaml.docker-compose",
  },
})
