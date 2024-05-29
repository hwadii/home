vim.filetype.add({
  pattern = {
    [".*Jenkinsfile.*"] = "groovy",
    [".*docker%-compose%..*"] = "yaml.docker-compose",
  },
})
