function fish_prompt --description 'Write out the prompt'
    set -l last_status $status

    set -l normal (set_color normal)

    set -l nix_shell_info (set_color brcyan) (
      string match -rq '/nix/store' $PATH
      if test $status -eq 0
        echo -n " <nix>"
      end
    ) $normal

    set -l color_suffix (set_color -o yellow)
    if not test $last_status -eq 0
        set color_suffix (set_color -o brred)
    end

    # Color the prompt differently when we're root
    set -l color_cwd (set_color -o blue)
    set -l suffix "»"
    if fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix "#"
    end
    set -l suffix_status $color_suffix $suffix $normal

    set -l background_jobs ""
    if jobs --quiet
        set background_jobs "!"
    end

    set -l color_status (set_color -o yellow)
    set -l jobs_status $color_status $background_jobs $normal

    set -l private_mode_status (
      if test -n "$fish_private_mode"
        echo -n (set_color -o brred) "[P]"
      else
        echo -n ""
      end
    ) $normal

    set -g __fish_git_prompt_char_untrackedfiles "?"
    set -g __fish_git_prompt_char_dirtystate "*"
    set -g __fish_git_prompt_showuntrackedfiles 1
    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_showupstream informative
    set -g __fish_git_prompt_color_upstream yellow -o
    set -g __fish_git_prompt_color_flags yellow -o
    set -g __fish_git_prompt_color brblue
    set -l vcs_status (fish_vcs_prompt)

    set -l pwd_status $color_cwd (prompt_pwd) $normal

    if string length -q $vcs_status
        echo -ns $pwd_status $jobs_status $private_mode_status $nix_shell_info $vcs_status $suffix_status " "
    else
        echo -ns $pwd_status $jobs_status $private_mode_status $nix_shell_info " " $suffix_status " "
    end
end
