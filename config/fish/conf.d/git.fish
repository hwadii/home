abbr g git

abbr ga git add
abbr gaa git add --all
abbr gapa git add --patch
abbr gau git add --update
abbr gav git add --verbose
abbr gap git apply
abbr gapt git apply --3way
abbr gai git add --interactive

abbr gb git branch
abbr gba git branch -a
abbr gbd git branch -d
abbr gbD git branch -D
abbr gbl git blame -b -w --color-lines
abbr gbnm git branch --no-merged
abbr gbr git branch --remote
abbr gbs git bisect
abbr gbsb git bisect bad
abbr gbsg git bisect good
abbr gbsr git bisect reset
abbr gbss git bisect start

abbr gc git commit -v
abbr gc! git commit -v --amend
abbr gcn! git commit -v --no-edit --amend
abbr gca git commit -v -a
abbr gca! git commit -v -a --amend
abbr gcan! git commit -v -a --no-edit --amend
abbr gcans! git commit -v -a -s --no-edit --amend
abbr gcam git commit -a -m
abbr gcsm git commit -s -m
abbr gcas git commit -a -s
abbr gcasm git commit -a -s -m
abbr gcb git checkout -b
abbr gcf git config --list


abbr gcl git clone --recurse-submodules
abbr gclean git clean -id
abbr gcm git checkout master
abbr gcd git checkout dev
abbr gcmsg git commit -m
abbr gco git checkout
abbr gcor git checkout --recurse-submodules
abbr gcount git shortlog -sn
abbr gcp git cherry-pick
abbr gcpa git cherry-pick --abort
abbr gcpc git cherry-pick --continue
abbr gcs git commit -S
abbr gcss git commit -S -s
abbr gcssm git commit -S -s -m

abbr gd git diff
abbr gdca git diff --cached
abbr gdcw git diff --cached --word-diff
abbr gdct "git describe --tags (git rev-list --tags --max-count=1)"
abbr gds git diff --staged
abbr gdt git diff-tree --no-commit-id --name-only -r
abbr gdup git diff @{upstream}
abbr gdw git diff --word-diff

abbr gf git fetch
abbr gfo git fetch origin
abbr gfa git fetch --all --prune --jobs=10

abbr gfg "git ls-files | grep"

abbr gg git gui citool
abbr gga git gui citool --amend

abbr ghh git help

abbr gl git pull
abbr ggl git pull
abbr glg git log --stat
abbr glgp git log --stat -p
abbr glgg git log --graph
abbr glgga git log --graph --decorate --all
abbr glgm git log --graph --max-count=10
abbr glo git log --oneline --decorate
abbr glog git log --oneline --decorate --graph
abbr gloga git log --oneline --decorate --graph --all

abbr gm git merge
abbr gmom git merge origin/master
abbr gmtl git mergetool --no-prompt
abbr gmtlvim git mergetool --no-prompt --tool=vimdiff
abbr gmum git merge upstream/master
abbr gma git merge --abort

abbr gp git push
abbr ggp git push
abbr gpd git push --dry-run
abbr gpf git push --force-with-lease --force-if-includes
abbr gpf! git push --force
abbr gpr git pull --rebase
abbr gpu git push upstream
abbr gpv git push -v

abbr gr git remote
abbr gra git remote add
abbr grb git rebase
abbr grba git rebase --abort
abbr grbc git rebase --continue
abbr grbd git rebase dev
abbr grbi git rebase -i
abbr grbm git rebase master
abbr grbom git rebase origin/master
abbr grbo git rebase --onto
abbr grbs git rebase --skip
abbr grev git revert
abbr grh git reset
abbr grhh git reset --hard
abbr grm git rm
abbr grmc git rm --cached
abbr grmv git remote rename
abbr grrm git remote remove
abbr grs git restore
abbr grset git remote set-url
abbr grss git restore --source
abbr grst git restore --staged
abbr grt "cd (git rev-parse --show-toplevel || echo .)"
abbr gru git reset --
abbr grup git remote update
abbr grv git remote -v

abbr gsb git status -sb
abbr gsd git svn dcommit
abbr gsh git show
abbr gsi git submodule init
abbr gsps git show --pretty=short --show-signature
abbr gsr git svn rebase
abbr gss git status -bs
abbr gst git status

abbr gsta git stash push
abbr gstaa git stash apply
abbr gstc git stash clear
abbr gstd git stash drop
abbr gstl git stash list
abbr gstp git stash pop
abbr gsts git stash show --text
abbr gstu gsta --include-untracked
abbr gstall git stash --all
abbr gsu git submodule update
abbr gsw git switch
abbr gswc git switch -c
abbr gswm git switch master
abbr gswd git switch dev

abbr gts git tag -s
abbr gtv "git tag | sort -V"

abbr gunignore git update-index --no-assume-unchanged
abbr gup git pull --rebase
abbr gupv git pull --rebase -v
abbr gupa git pull --rebase --autostash
abbr gupav git pull --rebase --autostash -v

abbr gwch git whatchanged -p --abbrev-commit --pretty=medium

abbr gam git am
abbr gamc git am --continue
abbr gams git am --skip
abbr gama git am --abort
abbr gamscp git am --show-current-patch
