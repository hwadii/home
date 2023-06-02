function fish_user_key_bindings
  fzf_key_bindings
  bind --preset \el downcase-word
  bind \e\; fzf-file-widget
  bind \ec fzf-cd-widget
end
