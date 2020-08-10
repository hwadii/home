# Re-source shell
function reset-shell() {
  source "$HOME/.zshrc"
}
zle -N reset-shell
bindkey '^[r' reset-shell
