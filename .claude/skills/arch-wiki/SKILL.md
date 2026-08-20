---
name: arch-wiki
description: Read the ArchWiki from disk instead of searching the web. Use for any Arch-specific question -- package names, pacman flags, config file syntax, systemd units, mkinitcpio, systemd-boot, Btrfs, dm-crypt, LUKS, snapper, UKIs -- on any host, Arch or not. Faster than a web search, works offline, and returns the page rather than a summary of it.
---

# The ArchWiki, offline

`wiki.py` beside this file is the whole of the mechanics, standard library only:

```sh
python3 .claude/skills/arch-wiki/wiki.py search dm-crypt             # titles
python3 .claude/skills/arch-wiki/wiki.py search luksHeaderBackup -f  # and bodies
python3 .claude/skills/arch-wiki/wiki.py read Dm-crypt/Device_encryption
python3 .claude/skills/arch-wiki/wiki.py status
python3 .claude/skills/arch-wiki/wiki.py fetch
```

- `search` ranks titles: exact, then starting with the words, then shortest. Try
  `-f`, which reads every article body, second.
- `read` renders one page to text; title, path and case are all accepted.
- `status` changes nothing and downloads nothing.
- `fetch` downloads a fresh copy even where one exists.
- `search` and `read` fetch a missing copy and refresh one more than a week
  behind the published package, checking at most once a week.

## Where the pages come from

The `arch-wiki-docs` package.

- On Arch, `pacman -S arch-wiki-docs` puts them under
  `/usr/share/doc/arch-wiki/html`; that tree wins over a download.
- Elsewhere, `fetch` extracts the English pages under
  `$XDG_CACHE_HOME/arch-wiki` or `~/.cache/arch-wiki`, no root. Unpacking needs
  `bsdtar`.
- `ARCH_WIKI` points at a tree elsewhere and overrides both.

## Citing what it says

Name the page in the text and link the same target as an HTTPS URL with its
anchor, built from the heading with spaces as underscores and checked against
the copy on disk. Give the date read, and where the wording matters name the
package `status` prints. Set a verbatim quotation as a blockquote; the text is
GFDL 1.3-or-later.

## Limits

- It is a snapshot; the package filename carries the build date. For anything
  recent, check that date and go to the web.
- The download is unauthenticated; that is tolerable only because the pages are
  static HTML that nothing executes.
- Go to the web for release notes, a kernel commit, or a bug tracker.
