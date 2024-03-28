function fish_user_key_bindings
    bind --erase \ct
    bind --erase \cr
    bind --preset \el downcase-word
    bind --preset \e\/ history-token-search-forward
    bind --preset \e\, __fish_list_current_token
    bind \e\; fzf-file-widget
end
