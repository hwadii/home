set fish_greeting

set -gx EDITOR nvim
set -gx VISUAL nvim

fish_add_path ~/.cargo/bin
fish_add_path ~/.local/bin

set -x LESS " --mouse -Ri"
set -x PAGER less
set -x MANWIDTH 80

set -x XDG_CONFIG_HOME ~/.config
set -x RIPGREP_CONFIG_PATH ~/.config/ripgrep/ripgreprc
set -x PASSWORD_STORE_DIR ~/.local/share/pass/
set -x LANG en_US.utf8
set -x LANGUAGE en_US.utf8
set -x LC_ALL fr_FR.UTF-8
set -x JQ_COLORS "2;37:0;37:0;37:0;37:0;32:1;37:1;37"
set -x CARGO_TARGET_DIR ~/.local/cargo/target

zoxide init fish | source
mise activate fish | source
