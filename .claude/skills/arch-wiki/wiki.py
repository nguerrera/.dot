#!/usr/bin/env python3

"""The ArchWiki, on disk, on whatever host this is.

SKILL.md beside this file has when to reach for the wiki and how to cite it.
This is the reading: getting the pages onto the host, keeping that copy current,
searching them, and rendering one.

    python3 .claude/skills/arch-wiki/wiki.py search dm-crypt
    python3 .claude/skills/arch-wiki/wiki.py search luksHeaderBackup -f
    python3 .claude/skills/arch-wiki/wiki.py read Dm-crypt/Device_encryption
    python3 .claude/skills/arch-wiki/wiki.py status
    python3 .claude/skills/arch-wiki/wiki.py fetch

`arch-wiki-docs` is an `any`-architecture package -- static HTML, no
architecture, no scriptlets, nothing built -- so having it on a host with no
`pacman` is a download and an untar. 19 MB over the wire, 93 MB of English
pages on disk, about two seconds.

Reading is the standard library and nothing else. `wikiman` is a picker over a
directory and `w3m` renders the HTML, and both are Arch packages that would put
the portability problem back where it started -- so search is a walk over
filenames and a scan of the article bodies, and rendering is `html.parser`.

THE MIRROR IS UNAUTHENTICATED. pacman would check the package signature against
the shipped keyring; a download of the same file by URL has no keyring to check
it against, and `archlinux.org` publishes the size but no digest. An integrity
story rather than an authenticity one. It is static HTML that nothing executes,
which is the reason to be willing to make it.
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
import unittest
import urllib.request
from collections.abc import Iterable, Mapping
from dataclasses import dataclass
from datetime import date, datetime
from html.parser import HTMLParser
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest import mock

# archlinux.org redirects this to a mirror, so the URL carries no mirror choice
# and no version -- both are answered by following it. The response's final URL
# is the package filename, which is the only version record kept.
PACKAGE_URL = "https://archlinux.org/packages/extra/any/arch-wiki-docs/download/"

# Where the package puts its pages, and so where an Arch host already has them.
INSTALLED = Path("/usr/share/doc/arch-wiki/html")

# English only. The package carries 60-odd languages and 236 MB; en is 93 MB of
# it, and the other 143 MB answer no question asked here.
LANGUAGE = "en"

HTTP_TIMEOUT = 60

# The staleness probe gets its own, much shorter, budget. It is an optimization
# rather than a requirement -- a mirror that cannot answer a HEAD promptly is
# one to stop waiting on, because the copy on disk is already the answer.
PROBE_TIMEOUT = 5

# How far a cached copy may trail the current package before it is refetched,
# and how long an answer about what is published is kept. A week, because a
# week of drift on a snapshot of a wiki changes no answer anybody asks it, and
# the alternative is spending 19 MB on the pages moving underneath.
STALE_DAYS = 7

# The date in a package filename: arch-wiki-docs-20260702-1-any.pkg.tar.zst.
_PACKAGE_DATE = re.compile(r"-(\d{8})-")


class WikiError(Exception):
    """Something the user should read, rather than a stack trace."""


def default_cache(env: Mapping[str, str]) -> Path:
    """Where a downloaded copy lives.

    A cache directory because every byte is a re-download away, and because
    that is the one place a program may write without asking.
    """
    cache = env.get("XDG_CACHE_HOME")
    if cache:
        return Path(cache) / "arch-wiki"
    home = env.get("HOME")
    return (Path(home) if home else Path.home()) / ".cache" / "arch-wiki"


def _cached_root(env: Mapping[str, str]) -> Path:
    """The `html/` directory inside the cache, mirroring the installed tree.

    Same shape as INSTALLED, so everything below takes one kind of path and
    neither knows nor cares which of the two it was handed.
    """
    return default_cache(env) / "html"


def _stamp(env: Mapping[str, str]) -> Path:
    """The package filename a cached copy came from.

    Beside `html/` rather than inside it, so the tree stays a faithful copy of
    what the package holds.
    """
    return default_cache(env) / "package"


def _checked(env: Mapping[str, str]) -> Path:
    """The day this last learned what the current package is.

    A download and a probe that got an answer both write it, being the same
    fact arrived at two ways.
    """
    return default_cache(env) / "checked"


def last_check(env: Mapping[str, str] | None = None) -> date | None:
    """When the copy on disk was last measured against the published one.

    None where nothing has been recorded -- a cache written before this was
    kept -- and where the file holds something this did not write.
    """
    try:
        stored = _checked(os.environ if env is None else env).read_text().strip()
    except OSError:
        return None
    try:
        return date.fromisoformat(stored)
    except ValueError:
        return None


def _record_check(env: Mapping[str, str], day: date) -> None:
    path = _checked(env)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(f"{day.isoformat()}\n")


def populated(root: Path) -> bool:
    return (root / LANGUAGE).is_dir()


def find(env: Mapping[str, str] | None = None) -> Path | None:
    """A readable `html/` root, or None if there is not one yet.

    The installed tree wins. On an Arch host it is already there, already
    current with the rest of the system, and downloading a second copy into a
    cache would be 93 MB spent to arrive at the same pages.
    """
    e = os.environ if env is None else env
    override = e.get("ARCH_WIKI")
    if override:
        root = Path(override)
        if not populated(root):
            raise WikiError(f"ARCH_WIKI={override} has no {LANGUAGE}/ directory in it")
        return root

    for root in (INSTALLED, _cached_root(e)):
        if populated(root):
            return root
    return None


def ensure(env: Mapping[str, str] | None = None, today: date | None = None) -> Path:
    """A readable root, downloading one if this host has not got one or the one
    it has is stale.

    Fetching rather than complaining, because the alternative is every command
    here failing once with an instruction to type a different command -- and
    the thing being asked for is 19 MB and two seconds.
    """
    e = os.environ if env is None else env
    day = today or date.today()
    root = find(e)
    if root is None:
        return fetch(e, day)
    if _worth_probing(e, root, day):
        latest = latest_package()
        if behind(version(e), latest):
            return fetch(e, day)
        if latest is not None:
            _record_check(e, day)
    return root


def _worth_probing(env: Mapping[str, str], root: Path, today: date) -> bool:
    """Whether asking the network what the current package is could change the
    answer.

    The wall clock decides whether to ask at all, and it measures from the
    later of two dates. One is the snapshot's own: upstream cannot have
    published later than today, so a package built inside the window cannot be
    more than STALE_DAYS behind whatever is current, however long ago it was
    downloaded and whatever has been asked since. The
    other is the last check this made, which is what keeps a copy that is
    current but not new from paying a round trip on every command. Upstream
    rebuilds when it rebuilds -- the 20260702 package was still the published
    one on 2026-08-11 -- and by the package date alone every day of a stretch
    like that looks like a reason to ask again.

    SO THE CHECK IS RATE-LIMITED RATHER THAN EXACT. Between two checks the
    published package can move out of reach without this noticing, which puts
    the copy up to a week further behind than the window says before a fetch
    replaces it. That is the trade: a week of drift on a snapshot of a wiki,
    against a request in front of every search on a host that is otherwise
    reading its own disk.

    Only a copy this downloaded is a candidate. An installed tree is pacman's
    and stays current with the system; an ARCH_WIKI tree is the caller's.

    A missing stamp means the copy came from somewhere else or the stamp was
    deleted. An unparseable stamp carries the same problem: `behind` answers
    False without a dated cached name to compare, so probing would spend a
    round trip to learn nothing. Skip it in both cases.
    """
    if root != _cached_root(env):
        return False
    v = version(env)
    if v is None:
        return False
    stamped = package_date(v)
    if stamped is None:
        return False
    checked = last_check(env)
    known = max(stamped, checked) if checked else stamped
    return (today - known).days > STALE_DAYS


def latest_package(timeout: int = PROBE_TIMEOUT) -> str | None:
    """The package filename archlinux.org currently redirects to.

    A HEAD, so the answer costs a round trip rather than 19 MB. None when the
    network cannot answer -- offline is the case this whole module exists for,
    so failing to reach a mirror leaves the copy on disk as the best one there
    is rather than becoming an error.
    """
    request = urllib.request.Request(PACKAGE_URL, method="HEAD")
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            return Path(str(response.url)).name
    except OSError:
        return None


def package_date(name: str) -> date | None:
    """The snapshot date out of a package filename.

    `arch-wiki-docs-20260702-1-any.pkg.tar.zst` is 2026-07-02. None for a name
    carrying no date, which is a name this did not write.
    """
    match = _PACKAGE_DATE.search(name)
    if not match:
        return None
    try:
        return datetime.strptime(match.group(1), "%Y%m%d").date()
    except ValueError:
        return None


def behind(cached: str | None, latest: str | None) -> bool:
    """Whether a cached snapshot trails the current one by more than a week.

    Against the latest package rather than against today, because upstream
    decides the cadence: a month with no new package leaves a month-old copy
    current, and re-downloading it would cost 19 MB to arrive at the same
    pages.

    An unparseable or missing name on either side answers False. Not knowing
    how far behind a copy is falls short of evidence that it is behind.
    """
    ours = package_date(cached) if cached else None
    theirs = package_date(latest) if latest else None
    if ours is None or theirs is None:
        return False
    return (theirs - ours).days > STALE_DAYS


# ------------------------------------------------------------------- fetch


def fetch(env: Mapping[str, str] | None = None, today: date | None = None) -> Path:
    """Download the package and extract the English pages into the cache.

    EXTRACTED ASIDE AND THEN RENAMED. `populated()` is the flag that says a
    copy is here, so it must never name a half-written tree: an interrupted
    untar would otherwise leave one that every later call trusts, and the
    failure arrives as a missing page rather than as the interruption that
    caused it.

    bsdtar rather than the standard library, because `.tar.zst` needs zstd and
    `compression.zstd` arrived in Python 3.14. bsdtar reads zstd natively and
    is one package on every distribution that has it.
    """
    e = os.environ if env is None else env
    if not shutil.which("bsdtar"):
        raise WikiError(
            "bsdtar is needed to unpack the wiki (libarchive on most "
            "distributions, `pacman -S libarchive` on Arch)"
        )

    cache = default_cache(e)
    cache.mkdir(parents=True, exist_ok=True)
    root, partial = _cached_root(e), _cached_root(e).with_name("html.partial")
    package = cache / "package.tar.zst"

    print(f"fetching the ArchWiki from {PACKAGE_URL}", file=sys.stderr)
    try:
        with (
            urllib.request.urlopen(PACKAGE_URL, timeout=HTTP_TIMEOUT) as response,
            package.open("wb") as out,
        ):
            # The URL after redirects is a mirror path ending in the package
            # filename, which is where the version comes from. One request.
            name = Path(str(response.url)).name
            shutil.copyfileobj(response, out)
    except OSError as exc:
        package.unlink(missing_ok=True)
        raise WikiError(f"cannot download {PACKAGE_URL}: {exc}") from exc

    shutil.rmtree(partial, ignore_errors=True)
    partial.mkdir(parents=True)
    try:
        subprocess.run(
            ["bsdtar", "-xf", str(package), "-C", str(partial),
             "--strip-components", "5",
             f"usr/share/doc/arch-wiki/html/{LANGUAGE}/"],
            check=True,
        )  # fmt: skip
        if not populated(partial):
            raise WikiError(f"{name} holds no {LANGUAGE}/ pages")
        shutil.rmtree(root, ignore_errors=True)
        partial.replace(root)
    except BaseException:
        # BaseException so a Ctrl-C mid-extract cleans up too, which is the
        # interruption this is arranged to survive.
        shutil.rmtree(partial, ignore_errors=True)
        raise
    finally:
        package.unlink(missing_ok=True)

    _stamp(e).write_text(f"{name}\n")
    # The download answered the question a probe asks, so it counts as a check.
    # Without this the first search after a fetch probes again, and a fetch of
    # a package that upstream built months ago probes on every command after
    # that until upstream builds another one.
    _record_check(e, today or date.today())
    print(f"{name}: {len(pages(root))} pages in {root}", file=sys.stderr)
    return root


def version(env: Mapping[str, str] | None = None) -> str | None:
    """The package a cached copy came from, or None where it was not us."""
    try:
        return _stamp(os.environ if env is None else env).read_text().strip()
    except OSError:
        return None


# ------------------------------------------------------------------ pages


def pages(root: Path) -> list[Path]:
    """Every article. Recursive: subpages nest, `Bash/Functions` and the
    like."""
    return sorted((root / LANGUAGE).rglob("*.html"))


def title(root: Path, path: Path) -> str:
    """The wiki title a file holds, read from its name.

    Read from the name rather than the `<title>` element, because search ranks
    thousands of these and opening every file to rank it is the difference
    between a command and a wait.
    """
    return path.relative_to(root / LANGUAGE).with_suffix("").as_posix()


def resolve(root: Path, name: str) -> Path:
    """A page from whatever somebody typed: a title, a path, any case.

    `dm-crypt`, `Dm-crypt`, `dm crypt` and the file itself all name one page.
    Case and the underscore/space distinction are how a title differs from
    what a person types, and neither is worth a failed lookup.
    """
    direct = Path(name)
    if direct.is_file():
        return direct

    wanted = _slug(name)
    for path in pages(root):
        if _slug(title(root, path)) == wanted:
            return path
    raise WikiError(f"no page called {name!r} -- `search {name}` to look")


def _slug(text: str) -> str:
    return text.lower().replace(" ", "_").removesuffix(".html")


# ----------------------------------------------------------------- search


@dataclass(frozen=True)
class Hit:
    title: str
    path: Path
    context: str = ""


def search(
    root: Path, terms: Iterable[str], *, full_text: bool = False, limit: int = 40
) -> list[Hit]:
    """Pages matching every term, titles first.

    Two searches rather than one, and the caller picks. A title search is a
    walk over 2,500 names and costs nothing; a full-text search reads 93 MB.
    The first answers most questions, which is why it is the default.
    """
    wanted = [t.lower() for t in terms if t]
    if not wanted:
        return []
    found = _by_title(root, wanted)
    if full_text:
        seen = {hit.path for hit in found}
        found += [h for h in _by_body(root, wanted) if h.path not in seen]
    return found[:limit]


def _by_title(root: Path, wanted: list[str]) -> list[Hit]:
    hits = []
    for path in pages(root):
        name = title(root, path)
        haystack = name.lower().replace("_", " ")
        if all(term in haystack for term in wanted):
            hits.append((_rank(haystack, wanted), name, path))
    return [Hit(name, path) for _, name, path in sorted(hits)]


def _rank(haystack: str, wanted: list[str]) -> tuple[int, int]:
    """Exact title, then a title that starts with it, then shortest.

    Searching `btrfs` should put `Btrfs` above `Btrfs/Troubleshooting` above
    every page whose name merely contains the word. Length breaks the tie
    because a shorter title is the more general page, which is the one
    somebody typing one word is usually after.
    """
    joined = " ".join(wanted)
    if haystack == joined:
        return (0, len(haystack))
    if haystack.startswith(joined):
        return (1, len(haystack))
    return (2, len(haystack))


# Everything before this is skin: navigation, the table of contents, the search
# box. MediaWiki puts the article in one element and marks it, so the body
# search and the renderer both start from the same place.
CONTENT_ID = "mw-content-text"

# The attribute, not the bare id. The sidebar's table of contents opens with
# `id="toc-mw-content-text"`, so cutting the page at the bare string cuts it
# two thousand characters early and every body search then reads the navigation
# as though it were prose -- which shows up as a snippet made of section
# numbers. The renderer compares the parsed attribute and was never exposed to
# this.
_MARKER = f'id="{CONTENT_ID}"'

_TAGS = re.compile(r"<[^>]+>")
_SPACE = re.compile(r"\s+")


def _by_body(root: Path, wanted: list[str]) -> list[Hit]:
    """Full-text, filtered cheaply before anything expensive happens.

    A substring test against the raw article HTML is a C-speed scan, and it is
    allowed to be wrong in one direction only: a term matching an attribute
    rather than prose gets a page through that the strip below then drops. What
    it must not do is miss a page, which is why the cheap test runs on the same
    text the strict one does, markup and all.
    """
    hits = []
    for path in pages(root):
        try:
            article = path.read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        _, marker, article = article.partition(_MARKER)
        if not marker:
            continue
        lowered = article.lower()
        if not all(term in lowered for term in wanted):
            continue

        prose = _SPACE.sub(" ", _TAGS.sub(" ", article))
        where = prose.lower().find(wanted[0])
        if where < 0 or not all(term in prose.lower() for term in wanted):
            continue
        hits.append(Hit(title(root, path), path, _around(prose, where)))
    return hits


def _around(prose: str, where: int, width: int = 120) -> str:
    start = max(0, where - width // 3)
    return ("..." if start else "") + prose[start : start + width].strip() + "..."


# ------------------------------------------------------------------- read


def read(path: Path) -> str:
    """One page as plain text."""
    parser = _Article()
    parser.feed(path.read_text(encoding="utf-8", errors="replace"))
    parser.close()
    return parser.text()


_VOID = frozenset(
    {"area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta",
     "param", "source", "track", "wbr"}
)  # fmt: skip
_DROP = frozenset({"script", "style"})
_BLOCK = frozenset(
    {"p", "div", "li", "tr", "pre", "blockquote", "dt", "dd", "ul", "ol", "dl",
     "table", "section", "figure", "caption", "hr", "br"}
)  # fmt: skip
_HEADING = {f"h{level}": level for level in range(1, 7)}

# Blocks worth a line break on the way OUT as well as in. `li` is the one that
# is not: the next item breaks the line itself and the list's own close tag
# ends the last one, so breaking here too puts a blank line between every pair
# of bullets and doubles the length of a page that is mostly list.
_CLOSES = _BLOCK - {"li"}


class _Article(HTMLParser):
    """MediaWiki HTML to something readable, article only.

    Written against `html.parser` rather than a renderer, so this works on a
    host with nothing installed -- which is the whole reason the module exists.

    Tolerant of unbalanced tags on purpose. A close tag that matches nothing
    open is ignored, and one that matches something further down unwinds what
    is above it, so a page with a stray `</div>` renders instead of ending
    early. There are 2,500 of these and no chance to fix one that will not
    parse.
    """

    def __init__(self) -> None:
        super().__init__(convert_charrefs=True)
        self.title = ""
        self._out: list[str] = []
        self._open: list[str] = []
        self._inside = False
        self._dropping = 0
        self._pre = 0
        self._titling = False

    # -- collecting

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if not self._inside:
            if tag == "title":
                self._titling = True
            elif dict(attrs).get("id") == CONTENT_ID:
                self._inside = True
                self._open = [tag]
            return

        if tag in _VOID:
            if tag == "br":
                self._newline()
            return

        self._open.append(tag)
        if tag in _DROP:
            self._dropping += 1
        if self._dropping:
            return

        if tag in _HEADING:
            self._newline(2)
            self._out.append("#" * _HEADING[tag] + " ")
        elif tag == "pre":
            self._pre += 1
            self._newline(2)
            self._out.append(FENCE + "\n")
        elif tag == "li":
            self._newline()
            self._out.append("- ")
        elif tag in _BLOCK:
            self._newline()

    def handle_endtag(self, tag: str) -> None:
        if self._titling and tag == "title":
            self._titling = False
            return
        if not self._inside or tag in _VOID or tag not in self._open:
            return

        while self._open:
            closed = self._open.pop()
            if closed in _DROP:
                self._dropping = max(0, self._dropping - 1)
            if closed == "pre":
                self._pre = max(0, self._pre - 1)
                if not self._dropping:
                    self._out.append("\n" + FENCE)
            if closed == tag:
                break

        if not self._open:
            self._inside = False
        elif not self._dropping and (tag in _HEADING or tag in _CLOSES):
            self._newline()

    def handle_data(self, data: str) -> None:
        if self._titling:
            self.title += data
        elif self._inside and not self._dropping:
            self._out.append(data if self._pre else _SPACE.sub(" ", data))

    def _newline(self, count: int = 1) -> None:
        self._out.append("\n" * count)

    # -- rendering

    def text(self) -> str:
        name = self.title.removesuffix(" - ArchWiki").strip()
        body = _lines("".join(self._out))
        return f"# {name}\n\n{body}" if name else body


# Fenced so the normalizer below can tell code from prose. Indentation is the
# content of a command, and stripping it turns a transcript into a paragraph.
FENCE = "```"


def _lines(raw: str) -> str:
    """Trim, and let at most one blank line through.

    Block tags nest, so a paragraph inside a div inside a section emits three
    line breaks for one gap; collapsing them here is cheaper than teaching the
    parser which of its ancestors already broke the line.
    """
    out: list[str] = []
    fenced = False
    blank = False
    for line in raw.split("\n"):
        if line.strip() == FENCE:
            fenced = not fenced
            if not fenced:
                # A `<pre>` almost always ends in a newline, which would put a
                # blank line between the last command and the closing fence.
                while out and not out[-1].strip():
                    out.pop()
            out.append(FENCE)
            blank = False
            continue
        line = line.rstrip() if fenced else line.strip()
        if line or fenced:
            out.append(line)
            blank = False
        elif out and not blank:
            out.append("")
            blank = True
    return "\n".join(out).strip() + "\n"


# -------------------------------------------------------------------- cli


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="wiki.py",
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("search", help="pages whose titles match every word")
    p.add_argument("terms", nargs="+")
    p.add_argument(
        "-f",
        "--full-text",
        action="store_true",
        help="also search article bodies, with the matching line",
    )
    p.add_argument("-n", "--limit", type=int, default=40)

    p = sub.add_parser("read", help="one page as plain text")
    p.add_argument("page", help="a title, or a path to a file")

    sub.add_parser("status", help="where the pages are and how many; changes nothing")
    sub.add_parser("fetch", help="download a copy even if one is already here")

    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        return _dispatch(args)
    except WikiError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        return 130
    except BrokenPipeError:  # `read x | head`
        return 0


def _dispatch(args: argparse.Namespace) -> int:
    if args.command == "status":
        return _status()
    if args.command == "fetch":
        fetch()
        return 0

    root = ensure()
    if args.command == "search":
        return _search(root, args)
    print(read(resolve(root, args.page)), end="")
    return 0


def _search(root: Path, args: argparse.Namespace) -> int:
    hits = search(root, args.terms, full_text=args.full_text, limit=args.limit)
    for hit in hits:
        print(f"{hit.title}\t{hit.path}")
        if hit.context:
            print(f"    {hit.context}")
    if not hits:
        how = "" if args.full_text else " -- `-f` also searches article bodies"
        print(f"nothing matches {' '.join(args.terms)}{how}", file=sys.stderr)
        return 1
    return 0


def _status() -> int:
    """Report, and change nothing -- including by not downloading.

    The question "have I got this" must be answerable without the answer
    becoming yes as a side effect of asking it.
    """
    root = find()
    if root is None:
        where = _cached_root(os.environ)
        print(f"no ArchWiki here. `fetch` puts it at {where}")
        return 1
    source = "installed" if root == INSTALLED else (version() or "downloaded")
    print(f"{len(pages(root))} pages in {root}  ({source})")
    return 0


# --------------------------------------------------------------- tests


def _page(root, name, body, title_tag=""):
    """Write one article the way the package lays them out."""
    path = root / LANGUAGE / f"{name}.html"
    path.parent.mkdir(parents=True, exist_ok=True)
    head = f"<title>{title_tag or name} - ArchWiki</title>"
    path.write_text(
        f"<html><head>{head}</head><body><nav>Jump to content</nav>"
        f'<div id="{CONTENT_ID}">{body}</div></body></html>',
        encoding="utf-8",
    )
    return path


class Tree(unittest.TestCase):
    """A wiki root under a temporary directory, torn down after each test."""

    def setUp(self):
        tmp = TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.cache = Path(tmp.name)
        self.env = {"XDG_CACHE_HOME": str(self.cache)}
        self.root = _cached_root(self.env)


class LocatingTest(Tree):
    def test_a_root_with_no_language_directory_is_not_populated(self):
        self.root.mkdir(parents=True)
        self.assertFalse(populated(self.root))

    def test_a_root_with_one_is(self):
        _page(self.root, "Btrfs", "<p>x</p>")
        self.assertTrue(populated(self.root))

    def test_xdg_cache_home_decides_where_a_download_lands(self):
        self.assertEqual(default_cache({"XDG_CACHE_HOME": "/c"}), Path("/c/arch-wiki"))

    def test_home_is_the_fallback(self):
        self.assertEqual(default_cache({"HOME": "/h"}), Path("/h/.cache/arch-wiki"))

    def test_the_cached_root_mirrors_the_installed_shape(self):
        self.assertEqual(_cached_root({"HOME": "/h"}).name, INSTALLED.name)

    def test_nothing_anywhere_is_none_rather_than_an_error(self):
        self.assertIsNone(find(self.env))

    def test_a_cached_copy_is_found(self):
        _page(self.root, "Btrfs", "<p>x</p>")
        self.assertEqual(find(self.env), self.root)

    def test_an_override_wins(self):
        _page(self.root, "Btrfs", "<p>x</p>")
        self.assertEqual(find({"ARCH_WIKI": str(self.root)}), self.root)

    def test_an_override_with_no_pages_is_an_error_rather_than_a_miss(self):
        self.root.mkdir(parents=True)
        with self.assertRaises(WikiError):
            find({"ARCH_WIKI": str(self.root)})

    def test_no_stamp_means_no_version(self):
        self.assertIsNone(version(self.env))

    def test_the_stamp_is_the_version(self):
        _stamp(self.env).parent.mkdir(parents=True, exist_ok=True)
        _stamp(self.env).write_text("arch-wiki-docs-20260702-1-any.pkg.tar.zst\n")
        self.assertEqual(version(self.env), "arch-wiki-docs-20260702-1-any.pkg.tar.zst")


class StalenessTest(Tree):
    """The probe that keeps a downloaded copy current with the published one."""

    NAME = "arch-wiki-docs-20260702-1-any.pkg.tar.zst"

    def stamp(self, name):
        _stamp(self.env).parent.mkdir(parents=True, exist_ok=True)
        _stamp(self.env).write_text(f"{name}\n")

    def test_a_date_comes_out_of_a_package_name(self):
        self.assertEqual(package_date(self.NAME), date(2026, 7, 2))

    def test_a_name_with_no_date_in_it(self):
        self.assertIsNone(package_date("arch-wiki-docs-any.pkg.tar.zst"))

    def test_a_name_carrying_an_impossible_date(self):
        self.assertIsNone(package_date("arch-wiki-docs-20261340-1-any.pkg.tar.zst"))

    def test_a_copy_more_than_a_week_behind_is_behind(self):
        self.assertTrue(behind(self.NAME, "arch-wiki-docs-20260715-1-any.pkg.tar.zst"))

    def test_a_copy_inside_the_window_is_not(self):
        self.assertFalse(behind(self.NAME, "arch-wiki-docs-20260708-1-any.pkg.tar.zst"))

    def test_exactly_the_window_is_not_behind(self):
        self.assertFalse(behind(self.NAME, "arch-wiki-docs-20260709-1-any.pkg.tar.zst"))

    def test_not_knowing_falls_short_of_evidence(self):
        self.assertFalse(behind(None, self.NAME))
        self.assertFalse(behind(self.NAME, None))
        self.assertFalse(behind("nonsense", self.NAME))

    def test_an_installed_tree_is_never_probed(self):
        self.assertFalse(_worth_probing(self.env, INSTALLED, date(2027, 1, 1)))

    def test_a_copy_with_no_stamp_is_not_probed(self):
        self.assertFalse(_worth_probing(self.env, self.root, date(2027, 1, 1)))

    def test_a_copy_with_an_unparseable_stamp_is_not_probed(self):
        self.stamp("nonsense")
        self.assertFalse(_worth_probing(self.env, self.root, date(2027, 1, 1)))

    def test_a_fresh_copy_is_not_probed(self):
        self.stamp(self.NAME)
        self.assertFalse(_worth_probing(self.env, self.root, date(2026, 7, 5)))

    def test_an_old_copy_is(self):
        self.stamp(self.NAME)
        self.assertTrue(_worth_probing(self.env, self.root, date(2026, 8, 1)))

    def test_an_old_package_checked_recently_is_not(self):
        """A package built in July can still be the published one in August,
        and by its own date alone it looks worse every day."""
        self.stamp(self.NAME)
        _record_check(self.env, date(2026, 8, 1))
        self.assertFalse(_worth_probing(self.env, self.root, date(2026, 8, 5)))

    def test_a_check_older_than_the_window_does_not_hold(self):
        self.stamp(self.NAME)
        _record_check(self.env, date(2026, 8, 1))
        self.assertTrue(_worth_probing(self.env, self.root, date(2026, 8, 9)))

    def test_a_check_this_did_not_write_is_ignored(self):
        self.stamp(self.NAME)
        _checked(self.env).write_text("yesterday\n")
        self.assertIsNone(last_check(self.env))
        self.assertTrue(_worth_probing(self.env, self.root, date(2026, 8, 1)))

    def test_the_probe_reads_the_filename_it_is_redirected_to(self):
        response = mock.MagicMock()
        response.url = f"https://mirror.example/{self.NAME}"
        response.__enter__.return_value = response
        with mock.patch("urllib.request.urlopen", return_value=response):
            self.assertEqual(latest_package(), self.NAME)

    def test_a_network_that_cannot_answer_leaves_the_copy_alone(self):
        with mock.patch("urllib.request.urlopen", side_effect=OSError("offline")):
            self.assertIsNone(latest_package())

    def test_a_copy_already_here_and_fresh_is_used_as_it_is(self):
        _page(self.root, "Btrfs", "<p>x</p>")
        self.stamp(self.NAME)
        with mock.patch("urllib.request.urlopen", side_effect=OSError("offline")):
            self.assertEqual(ensure(self.env, date(2026, 7, 5)), self.root)

    def answering(self):
        """A HEAD that reports the package the cached copy already holds."""
        response = mock.MagicMock()
        response.url = f"https://mirror.example/{self.NAME}"
        response.__enter__.return_value = response
        return mock.patch("urllib.request.urlopen", return_value=response)

    def test_a_probe_that_found_nothing_newer_answers_the_next_command(self):
        """The one this is for: several searches in a row cost one request
        between them rather than one apiece."""
        _page(self.root, "Btrfs", "<p>x</p>")
        self.stamp(self.NAME)
        with self.answering() as urlopen:
            ensure(self.env, date(2026, 8, 1))
            self.assertEqual(urlopen.call_count, 1)
            ensure(self.env, date(2026, 8, 1))
            ensure(self.env, date(2026, 8, 5))
            self.assertEqual(urlopen.call_count, 1)
            ensure(self.env, date(2026, 8, 9))
            self.assertEqual(urlopen.call_count, 2)

    def test_a_probe_the_network_could_not_answer_is_not_a_check(self):
        """Learning nothing is not learning the copy is current, and a host
        that was offline for one command may be online for the next."""
        _page(self.root, "Btrfs", "<p>x</p>")
        self.stamp(self.NAME)
        with mock.patch("urllib.request.urlopen", side_effect=OSError()) as urlopen:
            ensure(self.env, date(2026, 8, 1))
            ensure(self.env, date(2026, 8, 1))
            self.assertEqual(urlopen.call_count, 2)
        self.assertIsNone(last_check(self.env))


class TitleTest(Tree):
    def test_a_title_comes_off_the_path(self):
        path = _page(self.root, "Btrfs", "<p>x</p>")
        self.assertEqual(title(self.root, path), "Btrfs")

    def test_a_subpage_keeps_its_parent(self):
        path = _page(self.root, "Dm-crypt/Device_encryption", "<p>x</p>")
        self.assertEqual(title(self.root, path), "Dm-crypt/Device_encryption")

    def test_pages_finds_subpages_too(self):
        _page(self.root, "Dm-crypt", "<p>x</p>")
        _page(self.root, "Dm-crypt/Device_encryption", "<p>x</p>")
        self.assertEqual(len(pages(self.root)), 2)

    def test_case_does_not_stop_a_lookup(self):
        _page(self.root, "Dm-crypt", "<p>x</p>")
        for typed in ("Dm-crypt", "dm-crypt", "DM-CRYPT"):
            self.assertEqual(resolve(self.root, typed).stem, "Dm-crypt")

    def test_an_underscore_and_a_space_name_one_page(self):
        _page(self.root, "Secure_Boot", "<p>x</p>")
        self.assertEqual(resolve(self.root, "secure boot").stem, "Secure_Boot")

    def test_a_page_that_is_not_there(self):
        _page(self.root, "Btrfs", "<p>x</p>")
        with self.assertRaises(WikiError):
            resolve(self.root, "nothing like this")


class RankTest(unittest.TestCase):
    def test_an_exact_title_comes_first(self):
        self.assertLess(_rank("btrfs", ["btrfs"]), _rank("btrfs/x", ["btrfs"]))

    def test_a_prefix_beats_a_mere_mention(self):
        prefix = _rank("btrfs/x", ["btrfs"])
        mention = _rank("installing btrfs", ["btrfs"])
        self.assertLess(prefix, mention)

    def test_the_shorter_title_breaks_a_tie(self):
        self.assertLess(_rank("btrfs/x", ["btrfs"]), _rank("btrfs/xyz", ["btrfs"]))


class SearchTest(Tree):
    def setUp(self):
        super().setUp()
        _page(self.root, "Btrfs", "<p>a filesystem</p>")
        _page(self.root, "Btrfs/Troubleshooting", "<p>when it breaks</p>")
        _page(self.root, "Dm-crypt", "<p>luksHeaderBackup lives here</p>")

    def test_titles_rank_general_before_specific(self):
        found = [hit.title for hit in search(self.root, ["btrfs"])]
        self.assertEqual(found, ["Btrfs", "Btrfs/Troubleshooting"])

    def test_every_term_has_to_match(self):
        self.assertEqual(search(self.root, ["btrfs", "nothing"]), [])

    def test_no_terms_is_no_hits_rather_than_every_page(self):
        self.assertEqual(search(self.root, []), [])
        self.assertEqual(search(self.root, [""]), [])

    def test_a_title_search_does_not_read_bodies(self):
        self.assertEqual(search(self.root, ["luksheaderbackup"]), [])

    def test_full_text_finds_what_a_title_cannot(self):
        found = search(self.root, ["luksheaderbackup"], full_text=True)
        self.assertEqual([hit.title for hit in found], ["Dm-crypt"])

    def test_a_body_hit_carries_its_context(self):
        found = search(self.root, ["luksheaderbackup"], full_text=True)
        self.assertIn("luksHeaderBackup", found[0].context)

    def test_a_page_matching_both_ways_appears_once(self):
        found = search(self.root, ["btrfs"], full_text=True)
        self.assertEqual(len(found), len({hit.path for hit in found}))

    def test_the_limit_binds(self):
        self.assertEqual(len(search(self.root, ["btrfs"], limit=1)), 1)

    def test_navigation_outside_the_article_is_not_searched(self):
        self.assertEqual(search(self.root, ["jump", "content"], full_text=True), [])


class AroundTest(unittest.TestCase):
    def test_a_match_at_the_start_gets_no_leading_ellipsis(self):
        self.assertFalse(_around("luks is here", 0).startswith("..."))

    def test_a_match_further_in_does(self):
        self.assertTrue(_around("x" * 200 + " luks", 201).startswith("..."))


class ReadTest(Tree):
    def render(self, body, name="Page"):
        return read(_page(self.root, name, body))

    def test_the_title_loses_the_site_name(self):
        self.assertTrue(self.render("<p>x</p>", "Btrfs").startswith("# Btrfs\n"))

    def test_navigation_outside_the_article_is_dropped(self):
        self.assertNotIn("Jump to content", self.render("<p>x</p>"))

    def test_script_and_style_content_go(self):
        out = self.render("<p>keep</p><script>var x=1</script><style>p{}</style>")
        self.assertNotIn("var x", out)
        self.assertNotIn("p{}", out)

    def test_a_heading_becomes_hashes(self):
        self.assertIn("## Installation", self.render("<h2>Installation</h2>"))

    def test_a_list_item_becomes_a_bullet(self):
        self.assertIn("- one", self.render("<ul><li>one</li><li>two</li></ul>"))

    def test_a_list_does_not_double_space(self):
        self.assertNotIn("\n\n- two", self.render("<ul><li>one</li><li>two</li></ul>"))

    def test_a_pre_block_is_fenced_and_keeps_its_indent(self):
        out = self.render("<pre>cryptsetup \\\n  --key-size 512</pre>")
        self.assertIn(FENCE, out)
        self.assertIn("  --key-size 512", out)

    def test_character_references_resolve(self):
        self.assertIn("a & b", self.render("<p>a &amp; b</p>"))

    def test_a_close_tag_matching_nothing_open_is_ignored(self):
        self.assertIn("after", self.render("<p>before</p></span><p>after</p>"))

    def test_closing_the_article_element_does_end_it(self):
        self.assertNotIn("after", self.render("<p>before</p></div><p>after</p>"))

    def test_blank_lines_collapse_to_one(self):
        self.assertNotIn("\n\n\n", self.render("<div><div><p>a</p></div></div>"))


class LinesTest(unittest.TestCase):
    def test_prose_is_stripped(self):
        self.assertEqual(_lines("  a  \n\n\n  b  "), "a\n\nb\n")

    def test_a_fence_keeps_indentation(self):
        self.assertIn("  indented", _lines(f"{FENCE}\n  indented\n{FENCE}"))

    def test_a_trailing_blank_inside_a_fence_goes(self):
        self.assertEqual(_lines(f"{FENCE}\ncmd\n\n{FENCE}"), f"{FENCE}\ncmd\n{FENCE}\n")


if __name__ == "__main__":
    sys.exit(main())
