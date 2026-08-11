---
name: arch-wiki
description: Read the ArchWiki from disk instead of searching the web. Use for any Arch-specific question -- package names, pacman flags, config file syntax, systemd units, mkinitcpio, systemd-boot, Btrfs, dm-crypt, LUKS, snapper, UKIs -- on any host, Arch or not. Faster than a web search, works offline, and returns the page rather than a summary of it.
---

# The ArchWiki, offline

The ArchWiki answers an Arch question better than a search engine does, and the
whole of it is static HTML on disk. Reach for this before the web.

## Reading it

`wiki.py` beside this file is the whole of the mechanics. It needs the standard
library and nothing else, so it works the same on every host.

```sh
python3 .claude/skills/arch-wiki/wiki.py search dm-crypt             # titles
python3 .claude/skills/arch-wiki/wiki.py search luksHeaderBackup -f  # and bodies
python3 .claude/skills/arch-wiki/wiki.py read Dm-crypt/Device_encryption
python3 .claude/skills/arch-wiki/wiki.py status
python3 .claude/skills/arch-wiki/wiki.py fetch
```

- **`search` walks the titles**, ranked: an exact title, then one that starts
  with the words, then the shortest of the rest. That order is why `btrfs` puts
  `Btrfs` above `Btrfs/Troubleshooting` above every page merely mentioning it.
- **`-f` extends it to the article bodies** and prints the matching text. It
  reads 93 MB where a title search walks 2,500 names, so it is the second thing
  to try rather than the first.
- **`read` renders one page to text**, taking the article and leaving the
  navigation, the table of contents and the search box. A title, a path, or any
  case will do: `dm-crypt`, `Dm-crypt` and `dm crypt` all name one page, and a
  subpage is `Dm-crypt/Device_encryption`.
- **`status` changes nothing, including by not downloading.** The question "have
  I got this" has to be answerable without the answer becoming yes as a side
  effect of asking it.
- **`fetch` downloads a copy** even where one is already there, which is the
  manual way to take a newer snapshot.

`search` and `read` fetch a missing copy first, and refresh one that has fallen
more than a week behind the published package. Nothing touches the network once
a copy is here and current.

## Where the pages come from

They come from `arch-wiki-docs`, an `any`-architecture Arch package that is
nothing but HTML.

- **On Arch**, `pacman -S arch-wiki-docs` puts them under
  `/usr/share/doc/arch-wiki/html`, and they stay current with the system. That
  tree wins over a download: it is already there, and fetching a second copy
  would be 93 MB spent to arrive at the same pages.
- **Anywhere else**, `fetch` downloads the package and extracts the English
  pages under `$XDG_CACHE_HOME/arch-wiki` or `~/.cache/arch-wiki`. Around 19 MB
  over the wire, 93 MB on disk, and it needs no root.
- **`ARCH_WIKI`** points at a tree somewhere else and overrides both.

English only. The package carries 60-odd languages and 236 MB, and the other 143
MB answer no question asked here.

Unpacking needs `bsdtar`, which reads `.tar.zst` natively. The standard library
gained zstd in Python 3.14, and this runs on whatever interpreter a host has.

## Citing what it says

Name the page in the text and link the same target as an HTTPS URL with its
anchor. Build the anchor from the heading with spaces as underscores and check
it against the copy on disk rather than guessing. Give the date read, and where
the exact wording matters name the package the pages came from, since that is
what makes a later disagreement auditable. `status` prints it. ArchWiki text is
GFDL 1.3-or-later, so a verbatim quotation stays the wiki's and is set off as a
blockquote.

## Limits

**It is a snapshot.** `status` names the package, whose filename carries the
date it was built -- `arch-wiki-docs-20260702-1-any.pkg.tar.zst` is 2026-07-02.
Keeping the copy current with the published package is not the same as keeping
it current with the wiki, so for a question turning on something recent, check
that date and go to the web.

**The download is not authenticated.** `pacman` checks the package signature
against its keyring; fetching the same file over HTTPS has nothing to check it
against, and `archlinux.org` publishes the size but no digest. So this is an
integrity story rather than an authenticity one. It is static HTML that nothing
executes, and that reasoning does not extend to anything that runs.

**It is the wiki, not upstream.** Go to the web for release notes, a kernel
commit, or a bug tracker.
