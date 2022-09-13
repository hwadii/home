function fish_prompt --description 'Write out the prompt'
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.

    function vcs
        set -l prompt fish_vcs_prompt
        if test $status -eq 0
            $prompt | string trim
        end
    end

    set -l normal (set_color normal)
    set -q fish_color_status or set -g fish_color_status --background=red white
    set -l bold_flag --bold

    # Color the prompt differently when we're root
    set -l color_cwd (set_color $bold_flag grey)
    set suffix "»"
    if fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix "#"
    end

    set -l background_jobs
    if jobs --quiet
        set background_jobs "!"
    else
        set background_jobs ""
    end

    set -l color_suffix (set_color -o yellow)
    if not test $__fish_last_status -eq 0
        set color_suffix (set_color -o red)
    end

    set -l color_status (set_color -o yellow)
    set -l jobs_status $color_status $background_jobs $normal

    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_char_dirtystate "*"
    set -g __fish_git_prompt_char_untrackedfiles "?"
    set -g __fish_git_prompt_color_flags yellow --bold
    set -g __fish_git_prompt_color_suffix "#A09FDF"

    set -l color_vcs (set_color "#A09FDF")
    set -l vcs_status $color_vcs (vcs) $normal

    set -l pwd_status $color_cwd (prompt_pwd) $normal

    echo -ns $pwd_status $jobs_status " " $vcs_status $prompt_status $color_suffix $suffix $normal " "
end
