---
name: arch-wiki
description: Read the ArchWiki from disk instead of searching the web. Use for any Arch-specific question -- package names, pacman flags, config file syntax, systemd units, mkinitcpio, systemd-boot, Btrfs, dm-crypt, LUKS, snapper, UKIs -- on any host, Arch or not. Faster than a web search, works offline, and returns the page rather than a summary of it.
---

# The ArchWiki, offline

The ArchWiki answers an Arch question better than a search engine does, and the
whole of it is static HTML on disk. Reach for this before the web.

## Where the pages are

They come from `arch-wiki-docs`, an `any`-architecture Arch package that is
nothing but HTML.

- **On Arch**, `pacman -S arch-wiki-docs` puts them under
  `/usr/share/doc/arch-wiki/html`, and they stay current with the system.
- **Anywhere else**, the package is a public download from
  https://archlinux.org/packages/extra/any/arch-wiki-docs/download/ and the
  pages are the `usr/share/doc/arch-wiki/html/<lang>/` tree inside it. Around 19
  MB for English, and it needs no root.

English is `en/`. The package carries 60 languages and most of its bulk is the
other 59, so take the one directory.

**Find the tree before searching it**, since a host may have either:

```sh
ls -d /usr/share/doc/arch-wiki/html/en ~/.cache/arch-wiki/html/en 2>/dev/null
```

## Searching it

**Titles first.** It is a walk over a couple of thousand filenames and costs
nothing:

```sh
ls "$WIKI" | grep -i dm-crypt
```

A subpage is a directory: `Dm-crypt/Device_encryption.html` under `Dm-crypt/`.

**Then bodies**, as soon as a title search comes back thin. The answer is often
a section of a page named after something else:

```sh
grep -rl luksHeaderBackup "$WIKI"
grep -rn -A3 luksHeaderBackup "$WIKI/Dm-crypt"
```

**Then read the file.** It is HTML, so it carries navigation chrome around the
article. Grep with context, or strip the tags where the noise gets in the way:

```sh
python3 -c 'import html.parser,re,sys
class P(html.parser.HTMLParser):
    def __init__(s):
        super().__init__(); s.out=[]; s.skip=0
    def handle_starttag(s,t,a): s.skip += t in ("script","style")
    def handle_endtag(s,t): s.skip -= t in ("script","style")
    def handle_data(s,d):
        if not s.skip: s.out.append(d)
p=P(); p.feed(open(sys.argv[1]).read())
print(re.sub(r"\n{3,}","\n\n","".join(p.out)))' "$WIKI/Dm-crypt.html"
```

**Where an `arch-wiki` command is on `PATH` it does all of the above more
neatly**, with title and body search as subcommands and pages rendered to text.
Use it if it is there; nothing here depends on it.

## Citing what it says

Name the page in the text and link the same target as an HTTPS URL with its
anchor. Build the anchor from the heading with spaces as underscores and check
it against the copy on disk rather than guessing. Give the date read, and where
the exact wording matters name the package the pages came from, since that is
what makes a later disagreement auditable. ArchWiki text is GFDL 1.3-or-later,
so a verbatim quotation stays the wiki's and is set off as a blockquote.

## Limits

**It is a snapshot.** The package filename carries the date it was built --
`arch-wiki-docs-20260702-1-any.pkg.tar.zst` is 2026-07-02. For a question
turning on something recent, check that date and go to the web.

**A download by URL is not authenticated.** `pacman` checks the package
signature against its keyring; fetching the same file over HTTPS has nothing to
check it against, so this is an integrity story rather than an authenticity one.
It is static HTML that nothing executes, and that reasoning does not extend to
anything that runs.

**It is the wiki, not upstream.** Go to the web for release notes, a kernel
commit, or a bug tracker.
