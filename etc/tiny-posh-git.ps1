# Forked subset of https://github.com/dahlbyk/posh-git with a few tweaks.

# Copyright (c) 2010-2018 Keith Dahlby, Keith Hill, and contributors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

### GitUtils.ps1

# Inspired by Mark Embling
# http://www.markembling.info/view/my-ideal-powershell-prompt-with-git-integration

<#
.SYNOPSIS
    Gets the path to the current repository's .git dir.
.DESCRIPTION
    Gets the path to the current repository's .git dir.  Or if the repository
    is a bare repository, the root directory of the bare repository.
.EXAMPLE
    PS C:\GitHub\posh-git\tests> Get-GitDirectory
    Returns C:\GitHub\posh-git\.git
.INPUTS
    None.
.OUTPUTS
    System.String
#>
function Get-GitDirectory {
    $pathInfo = Microsoft.PowerShell.Management\Get-Location
    if (!$pathInfo -or ($pathInfo.Provider.Name -ne 'FileSystem')) {
        $null
    }
    elseif ($Env:GIT_DIR) {
        $Env:GIT_DIR -replace '\\|/', [System.IO.Path]::DirectorySeparatorChar
    }
    else {
        $currentDir = Get-Item -LiteralPath $pathInfo -Force
        while ($currentDir) {
            $gitDirPath = Join-Path $currentDir.FullName .git
            if (Test-Path -LiteralPath $gitDirPath -PathType Container) {
                return $gitDirPath
            }

            # Handle the worktree case where .git is a file
            if (Test-Path -LiteralPath $gitDirPath -PathType Leaf) {
                $gitDirPath = git rev-parse --git-dir 2>$null
                if ($gitDirPath) {
                    return $gitDirPath
                }
            }

            $headPath = Join-Path $currentDir.FullName HEAD
            if (Test-Path -LiteralPath $headPath -PathType Leaf) {
                $refsPath = Join-Path $currentDir.FullName refs
                $objsPath = Join-Path $currentDir.FullName objects
                if ((Test-Path -LiteralPath $refsPath -PathType Container) -and
                    (Test-Path -LiteralPath $objsPath -PathType Container)) {

                    $bareDir = git rev-parse --git-dir 2>$null
                    if ($bareDir -and (Test-Path -LiteralPath $bareDir -PathType Container)) {
                        $resolvedBareDir = (Resolve-Path $bareDir).Path
                        return $resolvedBareDir
                    }
                }
            }

            $currentDir = $currentDir.Parent
        }
    }
}

# NG: Modified to avoid slow shelling out to git as much as possible
function Get-GitBranch($gitDir = $(Get-GitDirectory)) {
    if (!$gitDir) { return }
    $r = ''; $b = ''; $c = ''
    $step = ''; $total = ''
    if (Test-Path $gitDir/rebase-merge) {
        if (Test-Path $gitDir/rebase-merge/interactive) {
            $r = '|REBASE-i'
        }
        else {
            $r = '|REBASE-m'
        }
        $b = "$(Get-Content $gitDir/rebase-merge/head-name)"
        $step = "$(Get-Content $gitDir/rebase-merge/msgnum)"
        $total = "$(Get-Content $gitDir/rebase-merge/end)"
    }
    else {
        if (Test-Path $gitDir/rebase-apply) {
            $step = "$(Get-Content $gitDir/rebase-apply/next)"
            $total = "$(Get-Content $gitDir/rebase-apply/last)"

            if (Test-Path $gitDir/rebase-apply/rebasing) {
                $r = '|REBASE'
            }
            elseif (Test-Path $gitDir/rebase-apply/applying) {
                $r = '|AM'
            }
            else {
                $r = '|AM/REBASE'
            }
        }
        elseif (Test-Path $gitDir/MERGE_HEAD) {
            $r = '|MERGING'
        }
        elseif (Test-Path $gitDir/CHERRY_PICK_HEAD) {
            $r = '|CHERRY-PICKING'
        }
        elseif (Test-Path $gitDir/REVERT_HEAD) {
            $r = '|REVERTING'
        }
        elseif (Test-Path $gitDir/BISECT_LOG) {
            $r = '|BISECTING'
        }

        if ($step -and $total) {
            $r += " $step/$total"
        }

        $b = & {
            $ref = $null
            if (Test-Path $gitDir/HEAD) {
                $ref = Get-Content $gitDir/HEAD 2>$null
            }
            else {
                $ref = git --no-optional-locks rev-parse HEAD 2>$null
            }

            if ($ref -match 'ref: (?<ref>.+)') {
                return $Matches['ref']
            }
            elseif ($ref -and $ref.Length -ge 7) {
                return $ref.Substring(0,7)+'...'
            }
            else {
                return 'unknown'
            }
        }
    }

    "$c$($b -replace 'refs/heads/','')$r"
}

function Get-AliasPattern($cmd) {
    $aliases = @($cmd) + @(Get-Alias | Where-Object { $_.Definition -match "^$cmd(\.exe)?$" } | Foreach-Object Name)
    "($($aliases -join '|'))"
}

### TortoiseGit.ps1

# NG: Modified to not support old versions of tgit
function private:Get-TortoiseGitPath {
    return "C:\Program Files\TortoiseGit\bin\TortoiseGitProc.exe"
}

$Global:TortoiseGitSettings = new-object PSObject -Property @{
    TortoiseGitPath = (Get-TortoiseGitPath)
    TortoiseGitCommands = @{
        "about" = "about";
        "add" = "add";
        "blame" = "blame";
        "cat" = "cat";
        "cleanup" = "cleanup";
        "clean" = "cleanup";
        "commit" = "commit";
        "conflicteditor" = "conflicteditor";
        "createpatch" = "createpatch";
        "patch" = "createpatch";
        "diff" = "diff";
        "export" = "export";
        "help" = "help";
        "ignore" = "ignore";
        "log" = "log";
        "merge" = "merge";
        "pull" = "pull";
        "push" = "push";
        "rebase" = "rebase";
        "refbrowse" = "refbrowse";
        "reflog" = "reflog";
        "remove" = "remove";
        "rm" = "remove";
        "rename" = "rename";
        "mv" = "rename";
        "repocreate" = "repocreate";
        "init" = "repocreate";
        "repostatus" = "repostatus";
        "status" = "repostatus";
        "resolve" = "resolve";
        "revert" = "revert";
        "settings" = "settings";
        "config" = "settings";
        "stash" = "stash";
        "stashapply" = "stashapply";
        "stashsave" = "stashsave";
        "subadd" = "subadd";
        "subsync" = "subsync";
        "subupdate" = "subupdate";
        "switch" = "switch";
        "checkout" = "switch";
        "fetch" = "sync";
        "sync" = "sync";
    }
}

# NG: Modified to take path argument and default it to current git repo
function tgit {
    param (
        [Parameter(Mandatory=$true, Position=0)]
        $command,

        [Parameter(Position=1)]
        $path,

        [Parameter(ValueFromRemainingArguments=$true)]
        $args
    )

    # Replace any aliases with actual TortoiseGit commands
    if ($Global:TortoiseGitSettings.TortoiseGitCommands.ContainsKey($command)) {
        $command = $Global:TortoiseGitSettings.TortoiseGitCommands.Get_Item($command)
    }

    if ($command -eq "help") {
        # Replace the built-in help behaviour with just a list of commands
        $Global:TortoiseGitSettings.TortoiseGitCommands.Values.GetEnumerator() | Sort-Object | Get-Unique
        return
    }

    if (!$path) {
        $path = git rev-parse --show-toplevel
        if ($LASTEXITCODE -ne 0) {
            return
        }
    }

    & $Global:TortoiseGitSettings.TortoiseGitPath /command:$command /path:$path @args
}


## GitParamTabExpansion.ps1

# Variable is used in GitTabExpansion.ps1
$shortGitParams = @{
    add = 'n v f i p e u A N'
    bisect = ''
    blame = 'b L l t S p M C h c f n s e w'
    branch = 'd D l f m M r a v vv q t u'
    checkout = 'q f b B t l m p'
    cherry = 'v'
    'cherry-pick' = 'e x r m n s S X'
    clean = 'd f i n q e x X'
    clone = 'l s q v n o b u c'
    commit = 'a p C c z F m t s n e i o u v q S'
    config = 'f l z e'
    diff = 'p u s U z B M C D l S G O R a b w W'
    difftool = 'd y t x g'
    fetch = 'a f k p n t u q v'
    grep = 'a i I w v h H E G P F n l L O z c p C A B W f e q'
    help = 'a g i m w'
    init = 'q'
    log = 'L n i E F g c c m r t'
    merge = 'e n s X q v S m'
    mergetool = 't y'
    mv = 'f k n v'
    notes = 'f m F C c n s q v'
    prune = 'n v'
    pull = 'q v e n s X r a f k u'
    push = 'n f u q v'
    rebase = 'm s X S q v n C f i p x'
    remote = 'v'
    reset = 'q p'
    restore = 's p W S q m'
    revert = 'e m n S s X'
    rm = 'f n r q'
    shortlog = 'n s e w'
    stash = 'p k u a q'
    status = 's b u z'
    submodule = 'q b f n N'
    switch = 'c C d f m q t'
    tag = 'a s u f d v n l m F'
    whatchanged = 'p'
}

# Variable is used in GitTabExpansion.ps1
$longGitParams = @{
    add = 'dry-run verbose force interactive patch edit update all no-ignore-removal no-all ignore-removal intent-to-add refresh ignore-errors ignore-missing renormalize'
    bisect = 'no-checkout term-old term-new'
    blame = 'root show-stats reverse porcelain line-porcelain incremental encoding= contents date score-debug show-name show-number show-email abbrev'
    branch = 'color no-color list abbrev= no-abbrev column no-column merged no-merged contains set-upstream track no-track set-upstream-to= unset-upstream edit-description delete create-reflog force move all verbose quiet'
    checkout = 'quiet force ours theirs track no-track detach orphan ignore-skip-worktree-bits merge conflict= patch'
    'cherry-pick' = 'edit mainline no-commit signoff gpg-sign ff allow-empty allow-empty-message keep-redundant-commits strategy= strategy-option= continue quit abort'
    clean = 'force interactive dry-run quiet exclude='
    clone = 'local no-hardlinks shared reference quiet verbose progress no-checkout bare mirror origin branch upload-pack template= config depth single-branch no-single-branch recursive recurse-submodules separate-git-dir='
    commit = 'all patch reuse-message reedit-message fixup squash reset-author short branch porcelain long null file author date message template signoff no-verify allow-empty allow-empty-message cleanup= edit no-edit amend no-post-rewrite include only untracked-files verbose quiet dry-run status no-status gpg-sign no-gpg-sign'
    config = 'replace-all add get get-all get-regexp get-urlmatch global system local file blob remove-section rename-section unset unset-all list bool int bool-or-int path null get-colorbool get-color edit includes no-includes'
    describe = 'dirty all tags contains abbrev candidates= exact-match debug long match always first-parent'
    diff = 'cached patch no-patch unified= raw patch-with-raw minimal patience histogram diff-algorithm= stat numstat shortstat dirstat summary patch-with-stat name-only name-status submodule color no-color word-diff word-diff-regex color-words no-renames check full-index binary apprev break-rewrites find-renames find-copies find-copies-harder irreversible-delete diff-filter= pickaxe-all pickaxe-regex relative text ignore-space-at-eol ignore-space-change ignore-all-space ignore-blank-lines inter-hunk-context= function-context exit-code quiet ext-diff no-ext-diff textconv no-textconv ignore-submodules src-prefix dst-prefix no-prefix staged'
    difftool = 'dir-diff no-prompt prompt tool= tool-help no-symlinks symlinks extcmd= gui'
    fetch = 'all append depth= unshallow update-shallow dry-run force keep multiple prune no-tags tags recurse-submodules= no-recurse-submodules submodule-prefix= recurse-submodules-default= update-head-ok upload-pack quiet verbose progress'
    gc = 'aggressive auto prune= no-prune quiet force'
    grep = 'cached no-index untracked no-exclude-standard exclude-standard text textconv no-textconv ignore-case max-depth word-regexp invert-match full-name extended-regexp basic-regexp perl-regexp fixed-strings line-number files-with-matches open-file-in-pager null count color no-color break heading show-function context after-context before-context function-context and or not all-match quiet'
    help = 'all guides info man web'
    init = 'quiet bare template= separate-git-dir= shared='
    log = 'follow no-decorate decorate source use-mailmap full-diff log-size max-count skip since after until before author committer grep-reflog grep all-match regexp-ignore-case basic-regexp extended-regexp fixed-strings perl-regexp remove-empty merges no-merges min-parents max-parents no-min-parents no-max-parents first-parent not all branches tags remote glob= exclude= ignore-missing bisect stdin cherry-mark cherry-pick left-only right-only cherry walk-reflogs merge boundary simplify-by-decoration full-history dense sparse simplify-merges ancestry-path date-order author-date-order topo-order reverse objects objects-edge unpacked no-walk= do-walk pretty format= abbrev-commit no-abbrev-commit oneline encoding= notes no-notes standard-notes no-standard-notes show-signature relative-date date= parents children left-right graph show-linear-break patch stat'
    merge = 'commit no-commit edit no-edit ff no-ff ff-only log no-log stat no-stat squash no-squash strategy strategy-option verify-signatures no-verify-signatures summary no-summary quiet verbose progress no-progress gpg-sign rerere-autoupdate no-rerere-autoupdate abort allow-unrelated-histories'
    mergetool = 'tool= tool-help no-prompt prompt'
    mv = 'force dry-run verbose'
    notes = 'force message file reuse-message reedit-message ref ignore-missing stdin dry-run strategy= commit abort quiet verbose'
    prune = 'dry-run verbose expire'
    pull = 'quiet verbose recurse-submodules= no-recurse-submodules= commit no-commit edit no-edit ff no-ff ff-only log no-log stat no-stat squash no-squash strategy= strategy-option= verify-signatures no-verify-signatures summary no-summary rebase= no-rebase all append depth= unshallow update-shallow force keep no-tags update-head-ok upload-pack progress'
    push = 'all prune mirror dry-run porcelain delete tags follow-tags receive-pack= exec= force-with-lease no-force-with-lease force repo= set-upstream thin no-thin quiet verbose progress recurse-submodules= verify no-verify'
    rebase = 'onto continue abort keep-empty skip edit-todo merge strategy= strategy-option= gpg-sign quiet verbose stat no-stat no-verify verify force-rebase fork-point no-fork-point ignore-whitespace whitespace= committer-date-is-author-date ignore-date interactive preserve-merges exec root autosquash no-autosquash autostash no-autostash no-ff'
    reflog = 'stale-fix expire= expire-unreachable= all updateref rewrite verbose'
    remote = 'verbose'
    reset = 'patch quiet soft mixed hard merge keep'
    restore = 'source= patch worktree staged quiet progress no-progress ours theirs merge conflict= ignore-unmerged ignore-skip-worktree-bits overlay no-overlay'
    revert = 'edit mainline no-edit no-commit gpg-sign signoff strategy= strategy-option continue quit abort'
    rm = 'force dry-run cached ignore-unmatch quiet'
    shortlog = 'numbered summary email format='
    show = 'pretty= format= abbrev-commit no-abbrev-commit oneline encoding= notes no-notes show-notes no-standard-notes standard-notes show-signature'
    stash = 'patch no-keep-index keep-index include-untracked all quiet index'
    status = 'short branch porcelain long untracked-files ignore-submodules ignored column no-column'
    submodule = 'quiet branch force cached files summary-limit remote no-fetch checkout merge rebase init name reference recursive depth'
    switch = 'create force-create detach guess no-guess force discard-changes merge conflict= quiet no-progress track no-track orphan ignore-other-worktrees recurse-submodules no-recurse-submodules'
    tag = 'annotate sign local-user force delete verify list sort column no-column contains points-at message file cleanup'
    whatchanged = 'since'
}

$shortVstsGlobal = 'h o'
$shortVstsParams = @{
    abandon = "i $shortVstsGlobal"
    create = "d i p r s t $shortVstsGlobal"
    complete = "i $shortVstsGlobal"
    list = "i p r s t $shortVstsGlobal"
    reactivate = "i $shortVstsGlobal"
    'set-vote' = "i $shortVstsGlobal"
    show = "i $shortVstsGlobal"
    update = "d i $shortVstsGlobal"
}

$longVstsGlobal = 'debug help output query verbose'
$longVstsParams = @{
    abandon = "id detect instance $longVstsGlobal"
    create = "auto-complete delete-source-branch work-items bypass-policy bypass-policy-reason description detect instance merge-commit-message open project repository reviewers source-branch squash target-branch title $longVstsGlobal"
    complete = "id detect instance $longVstsGlobal"
    list = " $longVstsGlobal"
    reactivate = " $longVstsGlobal"
    'set-vote' = " $longVstsGlobal"
    show = " $longVstsGlobal"
    update = " $longVstsGlobal"
}

# Variable is used in GitTabExpansion.ps1
$gitParamValues = @{
    blame = @{
        encoding = 'utf-8 none'
    }
    branch = @{
        color = 'always never auto'
        abbrev = '7 8 9 10'
    }
    checkout = @{
        conflict = 'merge diff3'
    }
    'cherry-pick' = @{
        strategy = 'resolve recursive octopus ours subtree'
    }
    commit = @{
        'cleanup' = 'strip whitespace verbatim scissors default'
    }
    diff = @{
        unified = '0 1 2 3 4 5'
        'diff-algorithm' = 'default patience minimal histogram myers'
        color = 'always never auto'
        'word-diff' = 'color plain porcelain none'
        abbrev = '7 8 9 10'
        'diff-filter' = 'A C D M R T U X B *'
        'inter-hunk-context' = '0 1 2 3 4 5'
        'ignore-submodules' = 'none untracked dirty all'
    }
    difftool = @{
        tool = 'vimdiff vimdiff2 araxis bc3 codecompare deltawalker diffmerge diffuse ecmerge emerge gvimdiff gvimdiff2 kdiff3 kompare meld opendiff p4merge tkdiff xxdiff'
    }
    fetch = @{
        'recurse-submodules' = 'yes on-demand no'
        'recurse-submodules-default' = 'yes on-demand'
    }
    init = @{
        shared = 'false true umask group all world everybody o'
    }
    log = @{
        decorate = 'short full no'
        'no-walk' = 'sorted unsorted'
        pretty = {
            param($format)
            gitConfigKeys 'pretty' $format 'oneline short medium full fuller email raw'
        }
        format = {
            param($format)
            gitConfigKeys 'pretty' $format 'oneline short medium full fuller email raw'
        }
        encoding = 'UTF-8'
        date = 'relative local default iso rfc short raw'
    }
    merge = @{
        strategy = 'resolve recursive octopus ours subtree'
        log = '1 2 3 4 5 6 7 8 9'
    }
    mergetool = @{
        tool = 'vimdiff vimdiff2 araxis bc3 codecompare deltawalker diffmerge diffuse ecmerge emerge gvimdiff gvimdiff2 kdiff3 kompare meld opendiff p4merge tkdiff xxdiff'
    }
    notes = @{
        strategy = 'manual ours theirs union cat_sort_uniq'
    }
    pull = @{
        strategy = 'resolve recursive octopus ours subtree'
        'recurse-submodules' = 'yes on-demand no'
        'no-recurse-submodules' = 'yes on-demand no'
        rebase = 'false true preserve'
    }
    push = @{
        'recurse-submodules' = 'check on-demand'
    }
    rebase = @{
        strategy = 'resolve recursive octopus ours subtree'
    }
    restore = @{
        conflict = 'merge diff3'
        source = {
            param($ref)
            gitBranches $ref $true
            gitTags $ref
        }
    }
    revert = @{
        strategy = 'resolve recursive octopus ours subtree'
    }
    show = @{
        pretty = {
            param($format)
            gitConfigKeys 'pretty' $format 'oneline short medium full fuller email raw'
        }
        format = {
            param($format)
            gitConfigKeys 'pretty' $format 'oneline short medium full fuller email raw'
        }
        encoding = 'utf-8'
    }
    status = @{
        'untracked-files' = 'no normal all'
        'ignore-submodules' = 'none untracked dirty all'
    }
    switch = @{
        conflict = 'merge diff3'
    }
}

### GitTabExpansion.ps1

# Initial implementation by Jeremy Skinner
# http://www.jeremyskinner.co.uk/2010/03/07/using-git-with-windows-powershell/

$Global:GitTabSettings = New-Object PSObject -Property @{
    AllCommands = $false
    KnownAliases = @{
        '!f() { exec vsts code pr "$@"; }; f' = 'vsts.pr'
    }
    EnableLogging = $false
    LogPath = Join-Path ([System.IO.Path]::GetTempPath()) posh-git_tabexp.log
    RegisteredCommands = ""
}

$subcommands = @{
    bisect = "start bad good skip reset visualize replay log run"
    notes = 'add append copy edit get-ref list merge prune remove show'
    'vsts.pr' = 'create update show list complete abandon reactivate reviewers work-items set-vote policies'
    reflog = "show delete expire"
    remote = "
        add rename remove set-head set-branches
        get-url set-url show prune update
        "
    rerere = "clear forget diff remaining status gc"
    stash = 'push save list show apply clear drop pop create branch'
    submodule = "add status init deinit update summary foreach sync"
    svn = "
        init fetch clone rebase dcommit log find-rev
        set-tree commit-diff info create-ignore propget
        proplist show-ignore show-externals branch tag blame
        migrate mkdirs reset gc
        "
    tfs = "
        list-remote-branches clone quick-clone bootstrap init
        clone fetch pull quick-clone unshelve shelve-list labels
        rcheckin checkin checkintool shelve shelve-delete
        branch
        info cleanup cleanup-workspaces help verify autotag subtree reset-remote checkout
        "
    flow = "init feature bugfix release hotfix support help version"
    worktree = "add list lock move prune remove unlock"
}

$gitflowsubcommands = @{
    init = 'help'
    feature = 'list start finish publish track diff rebase checkout pull help delete'
    bugfix = 'list start finish publish track diff rebase checkout pull help delete'
    release = 'list start finish track publish help delete'
    hotfix = 'list start finish track publish help delete'
    support = 'list start help'
    config = 'list set base'
}

function script:gitCmdOperations($commands, $command, $filter) {
    $commands[$command].Trim() -split '\s+' | Where-Object { $_ -like "$filter*" }
}

$script:someCommands = @('add','am','annotate','archive','bisect','blame','branch','bundle','checkout','cherry',
                         'cherry-pick','citool','clean','clone','commit','config','describe','diff','difftool','fetch',
                         'format-patch','gc','grep','gui','help','init','instaweb','log','merge','mergetool','mv',
                         'notes','prune','pull','push','rebase','reflog','remote','rerere','reset','restore','revert','rm',
                         'shortlog','show','stash','status','submodule','svn','switch','tag','whatchanged', 'worktree')

if ((($PSVersionTable.PSVersion.Major -eq 5) -or $IsWindows) -and ($script:GitVersion -ge [System.Version]'2.16.2')) {
    $script:someCommands += 'update-git-for-windows'
}

$script:gitCommandsWithLongParams = $longGitParams.Keys -join '|'
$script:gitCommandsWithShortParams = $shortGitParams.Keys -join '|'
$script:gitCommandsWithParamValues = $gitParamValues.Keys -join '|'
$script:vstsCommandsWithShortParams = $shortVstsParams.Keys -join '|'
$script:vstsCommandsWithLongParams = $longVstsParams.Keys -join '|'

# The regular expression here is roughly follows this pattern:
#
# <begin anchor><whitespace>*<git>(<whitespace><parameter>)*<whitespace>+<$args><whitespace>*<end anchor>
#
# The delimiters inside the parameter list and between some of the elements are non-newline whitespace characters ([^\S\r\n]).
# In those instances, newlines are only allowed if they preceded by a non-newline whitespace character.
#
# Begin anchor (^|[;`n])
# Whitespace   (\s*)
# Git Command  (?<cmd>$(GetAliasPattern git))
# Parameters   (?<params>(([^\S\r\n]|[^\S\r\n]``\r?\n)+\S+)*)
# $args Anchor (([^\S\r\n]|[^\S\r\n]``\r?\n)+\`$args)
# Whitespace   (\s|``\r?\n)*
# End Anchor   ($|[|;`n])
$script:GitProxyFunctionRegex = "(^|[;`n])(\s*)(?<cmd>$(Get-AliasPattern git))(?<params>(([^\S\r\n]|[^\S\r\n]``\r?\n)+\S+)*)(([^\S\r\n]|[^\S\r\n]``\r?\n)+\`$args)(\s|``\r?\n)*($|[|;`n])"

try {
    if ($null -ne (git help -a 2>&1 | Select-String flow)) {
        $script:someCommands += 'flow'
    }
}
catch {
    Write-Debug "Search for 'flow' in 'git help' output failed with error: $_"
}

filter quoteStringWithSpecialChars {
    if ($_ -and ($_ -match '\s+|#|@|\$|;|,|''|\{|\}|\(|\)')) {
        $str = $_ -replace "'", "''"
        "'$str'"
    }
    else {
        $_
    }
}

function script:gitCommands($filter, $includeAliases) {
    $cmdList = @()
    if (-not $global:GitTabSettings.AllCommands) {
        $cmdList += $someCommands -like "$filter*"
    }
    else {
        $cmdList += git help --all |
            Where-Object { $_ -match '^\s{2,}\S.*' } |
            ForEach-Object { $_.Split(' ', [StringSplitOptions]::RemoveEmptyEntries) } |
            Where-Object { $_ -like "$filter*" }
    }

    if ($includeAliases) {
        $cmdList += gitAliases $filter
    }

    $cmdList | Sort-Object
}

function script:gitRemotes($filter) {
    git remote |
        Where-Object { $_ -like "$filter*" } |
        quoteStringWithSpecialChars
}

function script:gitBranches($filter, $includeHEAD = $false, $prefix = '') {
    if ($filter -match "^(?<from>\S*\.{2,3})(?<to>.*)") {
        $prefix += $matches['from']
        $filter = $matches['to']
    }

    $branches = @(git branch --no-color | ForEach-Object { if (($_ -notmatch "^\* \(HEAD detached .+\)$") -and ($_ -match "^[\*\+]?\s*(?<ref>\S+)(?: -> .+)?")) { $matches['ref'] } }) +
                @(git branch --no-color -r | ForEach-Object { if ($_ -match "^  (?<ref>\S+)(?: -> .+)?") { $matches['ref'] } }) +
                @(if ($includeHEAD) { 'HEAD','FETCH_HEAD','ORIG_HEAD','MERGE_HEAD' })

    $branches |
        Where-Object { $_ -ne '(no branch)' -and $_ -like "$filter*" } |
        ForEach-Object { $prefix + $_ } |
        quoteStringWithSpecialChars
}

function script:gitRemoteUniqueBranches($filter) {
    git branch --no-color -r |
        ForEach-Object { if ($_ -match "^  (?<remote>[^/]+)/(?<branch>\S+)(?! -> .+)?$") { $matches['branch'] } } |
        Group-Object -NoElement |
        Where-Object { $_.Count -eq 1 } |
        Select-Object -ExpandProperty Name |
        Where-Object { $_ -like "$filter*" } |
        quoteStringWithSpecialChars
}

function script:gitConfigKeys($section, $filter, $defaultOptions = '') {
    $completions = @($defaultOptions -split ' ')

    git config --name-only --get-regexp ^$section\..* |
        ForEach-Object { $completions += ($_ -replace "$section\.","") }

    return $completions |
        Where-Object { $_ -like "$filter*" } |
        Sort-Object |
        quoteStringWithSpecialChars
}

function script:gitTags($filter, $prefix = '') {
    git tag |
        Where-Object { $_ -like "$filter*" } |
        ForEach-Object { $prefix + $_ } |
        quoteStringWithSpecialChars
}

function script:gitFeatures($filter, $command) {
    $featurePrefix = git config --local --get "gitflow.prefix.$command"
    $branches = @(git branch --no-color | ForEach-Object { if ($_ -match "^\*?\s*$featurePrefix(?<ref>.*)") { $matches['ref'] } })
    $branches |
        Where-Object { $_ -ne '(no branch)' -and $_ -like "$filter*" } |
        ForEach-Object { $featurePrefix + $_ } |
        quoteStringWithSpecialChars
}

function script:gitRemoteBranches($remote, $ref, $filter, $prefix = '') {
    git branch --no-color -r |
        Where-Object { $_ -like "  $remote/$filter*" } |
        ForEach-Object { $prefix + $ref + ($_ -replace "  $remote/","") } |
        quoteStringWithSpecialChars
}

function script:gitStashes($filter) {
    (git stash list) -replace ':.*','' |
        Where-Object { $_ -like "$filter*" } |
        quoteStringWithSpecialChars
}

function script:gitTfsShelvesets($filter) {
    (git tfs shelve-list) |
        Where-Object { $_ -like "$filter*" } |
        quoteStringWithSpecialChars
}

function script:gitFiles($filter, $files) {
    $files | Sort-Object |
        Where-Object { $_ -like "$filter*" } |
        quoteStringWithSpecialChars
}

function script:gitIndex($GitStatus, $filter) {
    gitFiles $filter $GitStatus.Index
}

function script:gitAddFiles($GitStatus, $filter) {
    gitFiles $filter (@($GitStatus.Working.Unmerged) + @($GitStatus.Working.Modified) + @($GitStatus.Working.Added))
}

function script:gitCheckoutFiles($GitStatus, $filter) {
    gitFiles $filter (@($GitStatus.Working.Unmerged) + @($GitStatus.Working.Modified) + @($GitStatus.Working.Deleted))
}

function script:gitDeleted($GitStatus, $filter) {
    gitFiles $filter $GitStatus.Working.Deleted
}

function script:gitDiffFiles($GitStatus, $filter, $staged) {
    if ($staged) {
        gitFiles $filter $GitStatus.Index.Modified
    }
    else {
        gitFiles $filter (@($GitStatus.Working.Unmerged) + @($GitStatus.Working.Modified) + @($GitStatus.Index.Modified))
    }
}

function script:gitMergeFiles($GitStatus, $filter) {
    gitFiles $filter $GitStatus.Working.Unmerged
}

function script:gitRestoreFiles($GitStatus, $filter, $staged) {
    if ($staged) {
        gitFiles $filter (@($GitStatus.Index.Added) + @($GitStatus.Index.Modified) + @($GitStatus.Index.Deleted))
    }
    else {
        gitFiles $filter (@($GitStatus.Working.Unmerged) + @($GitStatus.Working.Modified) + @($GitStatus.Working.Deleted))
    }
}

function script:gitAliases($filter) {
    git config --get-regexp ^alias\. | ForEach-Object{
        if ($_ -match "^alias\.(?<alias>\S+) .*") {
            $alias = $Matches['alias']
            if ($alias -like "$filter*") {
                $alias
            }
        }
    } | Sort-Object -Unique
}

function script:expandGitAlias($cmd, $rest) {
    $alias = git config "alias.$cmd"

    if ($alias) {
        $known = $Global:GitTabSettings.KnownAliases[$alias]
        if ($known) {
            return "git $known$rest"
        }

        return "git $alias$rest"
    }
    else {
        return "git $cmd$rest"
    }
}

function script:expandLongParams($hash, $cmd, $filter) {
    $hash[$cmd].Trim() -split ' ' |
        Where-Object { $_ -like "$filter*" } |
        Sort-Object |
        ForEach-Object { -join ("--", $_) }
}

function script:expandShortParams($hash, $cmd, $filter) {
    $hash[$cmd].Trim() -split ' ' |
        Where-Object { $_ -like "$filter*" } |
        Sort-Object |
        ForEach-Object { -join ("-", $_) }
}

function script:expandParamValues($cmd, $param, $filter) {
    $paramValues = $gitParamValues[$cmd][$param]

    $completions = if ($paramValues -is [scriptblock]) {
        & $paramValues $filter
    }
    else {
        $paramValues.Trim() -split ' ' | Where-Object { $_ -like "$filter*" } | Sort-Object
    }

    $completions | ForEach-Object { -join ("--", $param, "=", $_) }
}

function Expand-GitCommand($Command) {
    # Parse all Git output as UTF8, including tab completion output - https://github.com/dahlbyk/posh-git/pull/359
    $res = GitTabExpansionInternal $Command $Global:GitStatus
    $res
}

function GitTabExpansionInternal($lastBlock, $GitStatus = $null) {
    $ignoreGitParams = '(?<params>\s+-(?:[aA-zZ0-9]+|-[aA-zZ0-9][aA-zZ0-9-]*)(?:=\S+)?)*'

    if ($lastBlock -match "^$(Get-AliasPattern git) (?<cmd>\S+)(?<args> .*)$") {
        $lastBlock = expandGitAlias $Matches['cmd'] $Matches['args']
    }

    # Handles tgit <command> (tortoisegit)
    if ($lastBlock -match "^$(Get-AliasPattern tgit) (?<cmd>\S*)$") {
        # Need return statement to prevent fall-through.
        return $Global:TortoiseGitSettings.TortoiseGitCommands.Keys.GetEnumerator() | Sort-Object | Where-Object { $_ -like "$($matches['cmd'])*" }
    }

    # Handles gitk
    if ($lastBlock -match "^$(Get-AliasPattern gitk).* (?<ref>\S*)$") {
        return gitBranches $matches['ref'] $true
    }

    switch -regex ($lastBlock -replace "^$(Get-AliasPattern git) ","") {

        # Handles git <cmd> <op>
        "^(?<cmd>$($subcommands.Keys -join '|'))\s+(?<op>\S*)$" {
            gitCmdOperations $subcommands $matches['cmd'] $matches['op']
        }

        # Handles git flow <cmd> <op>
        "^flow (?<cmd>$($gitflowsubcommands.Keys -join '|'))\s+(?<op>\S*)$" {
            gitCmdOperations $gitflowsubcommands $matches['cmd'] $matches['op']
        }

        # Handles git flow <command> <op> <name>
        "^flow (?<command>\S*)\s+(?<op>\S*)\s+(?<name>\S*)$" {
            gitFeatures $matches['name'] $matches['command']
        }

        # Handles git remote (rename|rm|set-head|set-branches|set-url|show|prune) <stash>
        "^remote.* (?:rename|rm|set-head|set-branches|set-url|show|prune).* (?<remote>\S*)$" {
            gitRemotes $matches['remote']
        }

        # Handles git stash (show|apply|drop|pop|branch) <stash>
        "^stash (?:show|apply|drop|pop|branch).* (?<stash>\S*)$" {
            gitStashes $matches['stash']
        }

        # Handles git bisect (bad|good|reset|skip) <ref>
        "^bisect (?:bad|good|reset|skip).* (?<ref>\S*)$" {
            gitBranches $matches['ref'] $true
        }

        # Handles git tfs unshelve <shelveset>
        "^tfs +unshelve.* (?<shelveset>\S*)$" {
            gitTfsShelvesets $matches['shelveset']
        }

        # Handles git branch -d|-D|-m|-M <branch name>
        # Handles git branch <branch name> <start-point>
        "^branch.* (?<branch>\S*)$" {
            gitBranches $matches['branch']
        }

        # Handles git <cmd> (commands & aliases)
        "^(?<cmd>\S*)$" {
            gitCommands $matches['cmd'] $TRUE
        }

        # Handles git help <cmd> (commands only)
        "^help (?<cmd>\S*)$" {
            gitCommands $matches['cmd'] $FALSE
        }

        # Handles git push remote <ref>:<branch>
        # Handles git push remote +<ref>:<branch>
        "^push${ignoreGitParams}\s+(?<remote>[^\s-]\S*).*\s+(?<force>\+?)(?<ref>[^\s\:]*\:)(?<branch>\S*)$" {
            gitRemoteBranches $matches['remote'] $matches['ref'] $matches['branch'] -prefix $matches['force']
        }

        # Handles git push remote <ref>
        # Handles git push remote +<ref>
        # Handles git pull remote <ref>
        "^(?:push|pull)${ignoreGitParams}\s+(?<remote>[^\s-]\S*).*\s+(?<force>\+?)(?<ref>[^\s\:]*)$" {
            gitBranches $matches['ref'] -prefix $matches['force']
            gitTags $matches['ref'] -prefix $matches['force']
        }

        # Handles git pull <remote>
        # Handles git push <remote>
        # Handles git fetch <remote>
        "^(?:push|pull|fetch)${ignoreGitParams}\s+(?<remote>\S*)$" {
            gitRemotes $matches['remote']
        }

        # Handles git reset HEAD <path>
        # Handles git reset HEAD -- <path>
        "^reset.* HEAD(?:\s+--)? (?<path>\S*)$" {
            gitIndex $GitStatus $matches['path']
        }

        # Handles git <cmd> <ref>
        "^commit.*-C\s+(?<ref>\S*)$" {
            gitBranches $matches['ref'] $true
        }

        # Handles git add <path>
        "^add.* (?<files>\S*)$" {
            gitAddFiles $GitStatus $matches['files']
        }

        # Handles git checkout -- <path>
        "^checkout.* -- (?<files>\S*)$" {
            gitCheckoutFiles $GitStatus $matches['files']
        }

        # Handles git restore -s <ref> / --source=<ref> - must come before the next regex case
        "^restore.* (?-i)(-s\s*|(?<source>--source=))(?<ref>\S*)$" {
            gitBranches $matches['ref'] $true $matches['source']
            gitTags $matches['ref']
            break
        }

        # Handles git restore <path>
        "^restore(?:.* (?<staged>(?:(?-i)-S|--staged))|.*) (?<files>\S*)$" {
            gitRestoreFiles $GitStatus $matches['files'] $matches['staged']
        }

        # Handles git rm <path>
        "^rm.* (?<index>\S*)$" {
            gitDeleted $GitStatus $matches['index']
        }

        # Handles git diff/difftool <path>
        "^(?:diff|difftool)(?:.* (?<staged>(?:--cached|--staged))|.*) (?<files>\S*)$" {
            gitDiffFiles $GitStatus $matches['files'] $matches['staged']
        }

        # Handles git merge/mergetool <path>
        "^(?:merge|mergetool).* (?<files>\S*)$" {
            gitMergeFiles $GitStatus $matches['files']
        }

        # Handles git checkout|switch <ref>
        "^(?:checkout|switch).* (?<ref>\S*)$" {
            & {
                gitBranches $matches['ref'] $true
                gitRemoteUniqueBranches $matches['ref']
                gitTags $matches['ref']
                # Return only unique branches (to eliminate duplicates where the branch exists locally and on the remote)
            } | Select-Object -Unique
        }

        # Handles git worktree add <path> <ref>
        "^worktree add.* (?<files>\S+) (?<ref>\S*)$" {
            gitBranches $matches['ref']
        }

        # Handles git <cmd> <ref>
        "^(?:cherry|cherry-pick|diff|difftool|log|merge|rebase|reflog\s+show|reset|revert|show).* (?<ref>\S*)$" {
            gitBranches $matches['ref'] $true
            gitTags $matches['ref']
        }

        # Handles git <cmd> --<param>=<value>
        "^(?<cmd>$gitCommandsWithParamValues).* --(?<param>[^=]+)=(?<value>\S*)$" {
            expandParamValues $matches['cmd'] $matches['param'] $matches['value']
        }

        # Handles git <cmd> --<param>
        "^(?<cmd>$gitCommandsWithLongParams).* --(?<param>\S*)$" {
            expandLongParams $longGitParams $matches['cmd'] $matches['param']
        }

        # Handles git <cmd> -<shortparam>
        "^(?<cmd>$gitCommandsWithShortParams).* -(?<shortparam>\S*)$" {
            expandShortParams $shortGitParams $matches['cmd'] $matches['shortparam']
        }

        # Handles git pr alias
        "vsts\.pr\s+(?<op>\S*)$" {
            gitCmdOperations $subcommands 'vsts.pr' $matches['op']
        }

        # Handles git pr <cmd> --<param>
        "vsts\.pr\s+(?<cmd>$vstsCommandsWithLongParams).*--(?<param>\S*)$"
        {
            expandLongParams $longVstsParams $matches['cmd'] $matches['param']
        }

        # Handles git pr <cmd> -<shortparam>
        "vsts\.pr\s+(?<cmd>$vstsCommandsWithShortParams).*-(?<shortparam>\S*)$"
        {
            expandShortParams $shortVstsParams $matches['cmd'] $matches['shortparam']
        }
    }
}

function Expand-GitProxyFunction($command) {
    # Make sure the incoming command matches: <Command> <Args>, so we can extract the alias/command
    # name and the arguments being passed in.
    if ($command -notmatch '^(?<command>\S+)([^\S\r\n]|[^\S\r\n]`\r?\n)+(?<args>([^\S\r\n]|[^\S\r\n]`\r?\n|\S)*)$') {
        return $command
    }

    # Store arguments for replacement later
    $arguments = $matches['args']

    # Get the command name; if an alias exists, get the actual command name
    $commandName = $matches['command']
    if (Test-Path -Path Alias:\$commandName) {
        $commandName = Get-Item -Path Alias:\$commandName | Select-Object -ExpandProperty 'ResolvedCommandName'
    }

    # Extract definition of git usage
    if (Test-Path -Path Function:\$commandName) {
        $definition = Get-Item -Path Function:\$commandName | Select-Object -ExpandProperty 'Definition'
        if ($definition -match $script:GitProxyFunctionRegex) {
            # Clean up the command by removing extra delimiting whitespace and backtick preceding newlines
            return (("$($matches['cmd'].TrimStart()) $($matches['params']) $arguments") -replace '`\r?\n', ' ' -replace '\s+', ' ')
        }
    }

    return $command
}

function WriteTabExpLog([string] $Message) {
    if (!$global:GitTabSettings.EnableLogging) { return }

    $timestamp = Get-Date -Format HH:mm:ss
    "[$timestamp] $Message" | Out-File -Append $global:GitTabSettings.LogPath
}

if (!$UseLegacyTabExpansion -and ($PSVersionTable.PSVersion.Major -ge 6)) {
    $cmdNames = "git","tgit","gitk"

    # Create regex pattern from $cmdNames: ^(git|git\.exe|tgit|tgit\.exe|gitk|gitk\.exe)$
    $cmdNamesPattern = "^($($cmdNames -join '|'))(\.exe)?$"
    $cmdNames += Get-Alias | Where-Object { $_.Definition -match $cmdNamesPattern } | Foreach-Object Name

    if ($EnableProxyFunctionExpansion) {
        $funcNames += Get-ChildItem -Path Function:\ | Where-Object { $_.Definition -match $script:GitProxyFunctionRegex } | Foreach-Object Name
        $cmdNames += $funcNames

        # Create regex pattern from $funcNames e.g.: ^(Git-Checkout|Git-Switch)$
        $funcNamesPattern = "^($($funcNames -join '|'))$"
        $cmdNames += Get-Alias | Where-Object { $_.Definition -match $funcNamesPattern } | Foreach-Object Name
    }

    $global:GitTabSettings.RegisteredCommands = $cmdNames -join ", "

    Microsoft.PowerShell.Core\Register-ArgumentCompleter -CommandName $cmdNames -Native -ScriptBlock {
        param($wordToComplete, $commandAst, $cursorPosition)

        # The PowerShell completion has a habit of stripping the trailing space when completing:
        # git checkout <tab>
        # The Expand-GitCommand expects this trailing space, so pad with a space if necessary.
        $padLength = $cursorPosition - $commandAst.Extent.StartOffset
        $textToComplete = $commandAst.ToString().PadRight($padLength, ' ').Substring(0, $padLength)
        if ($EnableProxyFunctionExpansion) {
            $textToComplete = Expand-GitProxyFunction($textToComplete)
        }

        WriteTabExpLog "Expand: command: '$($commandAst.Extent.Text)', padded: '$textToComplete', padlen: $padLength"
        Expand-GitCommand $textToComplete
    }
}
else {
    $PowerTab_RegisterTabExpansion = if (Get-Module -Name powertab) { Get-Command Register-TabExpansion -Module powertab -ErrorAction SilentlyContinue }
    if ($PowerTab_RegisterTabExpansion) {
        & $PowerTab_RegisterTabExpansion git -Type Command {
            param($Context, [ref]$TabExpansionHasOutput, [ref]$QuoteSpaces)

            $line = $Context.Line
            $lastBlock = [regex]::Split($line, '[|;]')[-1].TrimStart()
            if ($EnableProxyFunctionExpansion) {
                $lastBlock = Expand-GitProxyFunction($lastBlock)
            }
            $TabExpansionHasOutput.Value = $true
            WriteTabExpLog "PowerTab expand: '$lastBlock'"
            Expand-GitCommand $lastBlock
        }

        return
    }

    function TabExpansion($line, $lastWord) {
        $lastBlock = [regex]::Split($line, '[|;]')[-1].TrimStart()
        if ($EnableProxyFunctionExpansion) {
            $lastBlock = Expand-GitProxyFunction($lastBlock)
        }
        $msg = "Legacy expand: '$lastBlock'"

        switch -regex ($lastBlock) {
            # Execute git tab completion for all git-related commands
            "^$(Get-AliasPattern git) (.*)"  { WriteTabExpLog $msg; Expand-GitCommand $lastBlock }
            "^$(Get-AliasPattern tgit) (.*)" { WriteTabExpLog $msg; Expand-GitCommand $lastBlock }
            "^$(Get-AliasPattern gitk) (.*)" { WriteTabExpLog $msg; Expand-GitCommand $lastBlock }
        }
    }
}
