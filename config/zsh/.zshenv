skip_global_compinit=1
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERMINAL="foot"
export SNAP="/snap"
export CHROME_BIN="chromium"
export GPG_TTY=$(tty)
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_LOCAL_BIN="$HOME/.local/bin"
export XDG_DATA_DIR="$HOME/.local/share"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_MISC_DIR="$HOME/.local/misc"
export TMPDIR="/tmp"
export BROWSER="firefox-dev"
export LESS="--RAW-CONTROL-CHARS --mouse -Ri"
export PAGER="less"
export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/ripgreprc"
export NVM_DIR="$HOME/.config/nvm"
export RVM_DIR="$HOME/.rvm"
export CARGO_HOME="$HOME/.local/cargo"
export GOPATH="$HOME/.local/go"
export RUSTUP_HOME="$HOME/.local/rustup"
export GEM_HOME="$RVM_DIR/gems/default"
export GEM_PATH="$RVM_DIR/gems/default"
export RUBY_YJIT_ENABLE=1
export PYENV_ROOT="$XDG_CONFIG_HOME/pyenv"
export DOTNET_ROOT="$HOME/.local/dotnet"
export PATH="$XDG_LOCAL_BIN:$PATH:$NVM_DIR/versions/node/v16.15.1/bin:$HOME/.rvm/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$XDG_LOCAL_BIN/git-extras:$HOME/.local/texlive/2020/bin/x86_64-linux:$CARGO_HOME/bin:$GOPATH/bin:$GEM_HOME/bin:$RVM_DIR/rubies/default/bin:$PYENV_ROOT/bin:$PYENV_ROOT/versions/3.8.7/bin/:$HOME/.local/dotnet/bin"
export EDITOR=nvim
export VISUAL=nvim
export FZF_DEFAULT_COMMAND='fd --type f --follow --hidden -E".git"'
export FZF_DEFAULT_OPTS="
    --height 40%
    --bind 'tab:down' --bind 'btab:up' --bind 'ctrl-s:toggle'
    --bind 'alt-a:select-all'
    --color=16,hl:3,hl+:6,fg+:-1,pointer:6,bg+:-1
"
export FZF_CTRL_T_OPTS="--select-1 --exit-0"
export FZF_ALT_C_COMMAND="fd --type d --hidden --follow --exclude '.git' --exclude 'node_modules'"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/keyring/ssh"
export PASSWORD_STORE_DIR="$XDG_DATA_DIR/pass"
export NNN_BMS="d:$HOME/dl;b:$HOME/things/books;c:$HOME/code;s:/media/wadii/garbage"
if [ -e /home/wadii/.nix-profile/etc/profile.d/nix.sh ]; then . /home/wadii/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
