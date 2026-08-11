#!/usr/bin/env python3

"""Print an ArchWiki page as text.

The pages are static HTML, so an article arrives wrapped in navigation chrome
and with its markup in the way of a grep. This drops the tags and leaves the
prose. SKILL.md has where the pages come from and how to search them.

    python .claude/skills/arch-wiki/strip.py "$WIKI/Dm-crypt.html"
"""

import re
import sys
import unittest
from html.parser import HTMLParser
from pathlib import Path

# Their content is source rather than prose, and both tags sit inside the body
# where a plain tag strip would leave the script itself in the output.
SILENT = ("script", "style")


class Text(HTMLParser):
    def __init__(self):
        super().__init__()
        self.parts = []
        self.depth = 0

    def handle_starttag(self, tag, attrs):
        if tag in SILENT:
            self.depth += 1

    def handle_endtag(self, tag):
        if tag in SILENT:
            # Clamped, because a stray closing tag in a page nobody validates
            # would otherwise leave the depth negative and silence the rest of
            # the document.
            self.depth = max(0, self.depth - 1)

    def handle_data(self, data):
        if not self.depth:
            self.parts.append(data)


def text_of(html):
    """The readable text of a page, with runs of blank lines collapsed.

    Character references come back as characters: HTMLParser resolves them
    before handing over the data, so `&amp;` reaches the output as `&`.
    """
    parser = Text()
    parser.feed(html)
    parser.close()
    return re.sub(r"\n{3,}", "\n\n", "".join(parser.parts))


def main(argv):
    if len(argv) != 1:
        print("usage: strip.py <page.html>", file=sys.stderr)
        return 2
    page = Path(argv[0]).read_text(encoding="utf-8", errors="replace")
    print(text_of(page))
    return 0


class TextOfTest(unittest.TestCase):
    def test_tags_go_and_text_stays(self):
        html = "<p>Encrypt <b>the</b> disk</p>"
        self.assertEqual(text_of(html), "Encrypt the disk")

    def test_script_content_is_dropped(self):
        html = "<p>before</p><script>var x = 1;</script><p>after</p>"
        self.assertEqual(text_of(html), "beforeafter")

    def test_style_content_is_dropped(self):
        self.assertEqual(text_of("<style>p{color:red}</style>keep"), "keep")

    def test_a_stray_close_does_not_silence_the_rest(self):
        self.assertEqual(text_of("</script>still here"), "still here")

    def test_character_references_resolve(self):
        html = "<p>cryptsetup &amp; LUKS</p>"
        self.assertEqual(text_of(html), "cryptsetup & LUKS")

    def test_blank_line_runs_collapse(self):
        self.assertEqual(text_of("a\n\n\n\n\nb"), "a\n\nb")

    def test_one_blank_line_survives(self):
        self.assertEqual(text_of("a\n\nb"), "a\n\nb")

    def test_an_unclosed_script_silences_what_follows(self):
        self.assertEqual(text_of("keep<script>var x"), "keep")


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
