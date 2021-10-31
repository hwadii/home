# Re-source shell
function reset-shell() {
  source "$XDG_CONFIG_HOME/.zshrc"
}
zle -N reset-shell
bindkey '^[r' reset-shell
