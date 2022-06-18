set fish_greeting

zoxide init fish | source

if status --is-interactive; and not set -q SSH_AUTH_SOCK
    eval (ssh-agent -c)
    function kill_ssh_agent_on_exit --on-event fish_exit
        ssh-agent -k >/dev/null
    end
end
