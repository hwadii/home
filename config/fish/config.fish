set fish_greeting

set -gx EDITOR emacsclient
set -gx VISUAL emacsclient

fish_add_path ~/.cargo/bin
fish_add_path ~/.local/bin
fish_add_path ~/.dotnet/tools

set -gx BROWSER firefox

set -x LESS " --mouse -Ri"
set -x PAGER less
set -x MANWIDTH 80

set -x XDG_CONFIG_HOME ~/.config
set -x RIPGREP_CONFIG_PATH ~/.config/ripgrep/ripgreprc
set -x PASSWORD_STORE_DIR ~/.local/share/pass/
set -x GOPATH ~/.local/go
set -x LANG en_US.UTF-8
set -x LANGUAGE en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x JQ_COLORS "2;37:0;37:0;37:0;37:0;32:1;37:1;37"

set -x CARGO_TARGET_DIR ~/.local/cargo/target

set -x PRE_COMMIT_COLOR never

set -x COMPOSE_REMOVE_ORPHANS true
set -x COMPOSE_BAKE true

set -x LSP_USE_PLISTS true

set -x HOMEBREW_NO_EMOJI true

zoxide init fish | source
fzf --fish | source
