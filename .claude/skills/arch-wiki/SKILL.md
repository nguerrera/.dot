---
name: arch-wiki
description: Read the ArchWiki from disk instead of searching the web. Use for any Arch-specific question -- package names, pacman flags, config file syntax, systemd units, mkinitcpio, systemd-boot, Btrfs, dm-crypt, LUKS, snapper, UKIs -- on any host, Arch or not. Faster than a web search, works offline, and returns the page rather than a summary of it.
---

# The ArchWiki, offline

The whole wiki is static HTML on disk. Reach for it before the web.

## Reading it

`wiki.py` beside this file is the whole of the mechanics. Standard library only,
so it works the same on every host.

```sh
python3 .claude/skills/arch-wiki/wiki.py search dm-crypt             # titles
python3 .claude/skills/arch-wiki/wiki.py search luksHeaderBackup -f  # and bodies
python3 .claude/skills/arch-wiki/wiki.py read Dm-crypt/Device_encryption
python3 .claude/skills/arch-wiki/wiki.py status
python3 .claude/skills/arch-wiki/wiki.py fetch
```

- **`search` walks the titles**, ranked: an exact title, then one starting with
  the words, then the shortest of the rest. That is why `btrfs` puts `Btrfs`
  above `Btrfs/Troubleshooting` above every page merely mentioning it.
- **`-f` extends it to the article bodies** and prints the matching text. It
  reads 93 MB where a title search walks 2,500 names, so try it second.
- **`read` renders one page to text**, taking the article and leaving the
  navigation. A title, a path, or any case will do: `dm-crypt`, `Dm-crypt` and
  `dm crypt` all name one page, and a subpage is `Dm-crypt/Device_encryption`.
- **`status` changes nothing, including by not downloading.** "Have I got this"
  has to be answerable without the answer becoming yes.
- **`fetch` downloads a copy** even where one is there, which is how to take a
  newer snapshot by hand.

`search` and `read` fetch a missing copy first, and refresh one that has fallen
more than a week behind the published package. Asking what is published costs a
HEAD request and the answer is kept for a week, so a current copy is read off
disk and reaches the network at most once every seven days.

## Where the pages come from

`arch-wiki-docs`, an `any`-architecture Arch package that is nothing but HTML.

- **On Arch**, `pacman -S arch-wiki-docs` puts them under
  `/usr/share/doc/arch-wiki/html` and keeps them current with the system. That
  tree wins over a download.
- **Anywhere else**, `fetch` extracts the English pages under
  `$XDG_CACHE_HOME/arch-wiki` or `~/.cache/arch-wiki`. Around 19 MB over the
  wire, 93 MB on disk, no root.
- **`ARCH_WIKI`** points at a tree somewhere else and overrides both.

English only. Unpacking needs `bsdtar`, which reads `.tar.zst` natively; the
standard library gained zstd only in Python 3.14 and this runs on whatever
interpreter a host has.

## Citing what it says

Name the page in the text and link the same target as an HTTPS URL with its
anchor. Build the anchor from the heading with spaces as underscores and check
it against the copy on disk. Give the date read, and where the exact wording
matters name the package the pages came from, which `status` prints. ArchWiki
text is GFDL 1.3-or-later, so set a verbatim quotation off as a blockquote.

## Limits

- **It is a snapshot.** `status` names the package, whose filename carries the
  build date -- `arch-wiki-docs-20260702-1-any.pkg.tar.zst` is 2026-07-02. For a
  question turning on something recent, check that date and go to the web.
- **The download is not authenticated**, `archlinux.org` publishing the size but
  no digest. It is static HTML that nothing executes, and that reasoning does
  not extend to anything that runs.
- **It is the wiki, not upstream.** Go to the web for release notes, a kernel
  commit, or a bug tracker.
