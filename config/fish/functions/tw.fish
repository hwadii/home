function tw
    set streams "39daph" "EnglishBen" "English_Ben" "TheGreatReview" "ThePrimeagen" "dmmulroy" "louispilfold" "lpil" \
         "papesan" "teej_dv" "theprimeagen" "tigerbeetle" "tsoding" "untangledco" "lcolonq" "sphaerophoria" "etoiles"
    set selection (string split " " $streams | sort | fzf)
    if test (uname) = "Darwin"
        iina https://twitch.tv/$selection
    else
        mpv --quiet https://twitch.tv/$selection
    end
end
