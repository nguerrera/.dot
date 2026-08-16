# Contributing

`tools/check` is the gate a change has to pass, and it needs nothing installed.
It reads every tracked `.md` file for a line over 80 columns, a non-ASCII
character, and a relative link that does not resolve, and every tracked file of
any kind for one the ignore rules say should not be tracked at all. CI runs it
on every push and every pull request.

## Rough edges

- It reads the index, so a new file is covered from the moment it is staged and
  not before. `git add` it first.
- A fenced block keeps whatever width it came with, since a command or a
  transcript is not prose to rewrap. The non-ASCII check still reaches inside
  one.
- YAML frontmatter is exempt from the width for the same reason: it is metadata
  a program parses, and a skill declares its description there as one
  deliberate line. The non-ASCII check reaches it too.
- A line is left alone when its indent plus its longest word already passes 80.
  That is the bare URL and the deep path, which no wrap could have saved.
- A link is checked as far as its path. `README.md#no-such-heading` passes, so a
  heading that was renamed is the reader's to catch.
- A tracked file missing from the worktree is reported by name, which is what a
  half-finished rename leaves behind.
- The ignore check asks what the rules say about a path rather than what the
  index does, so a file tracked before a rule started covering it is reported
  too. `git rm --cached` is the fix, and `.gitignore` stays the one statement of
  what is versioned.
- It reads git's global ignore as well as this repository's, so a machine
  carrying extra rules gets a stricter run than CI. The rules worth relying on
  are the ones in `.gitignore` here.
