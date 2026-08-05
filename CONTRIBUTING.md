# Contributing

`tools/check` is the gate a change has to pass, and it needs nothing installed.
It reads every tracked `.md` file for a line over 80 columns, a non-ASCII
character, and a relative link that does not resolve. CI runs it on every push
and every pull request.

## Rough edges

- It reads the index, so a new file is covered from the moment it is staged and
  not before. `git add` it first.
- A fenced block keeps whatever width it came with, since a command or a
  transcript is not prose to rewrap. The non-ASCII check still reaches inside
  one.
- A line is left alone when its indent plus its longest word already passes 80.
  That is the bare URL and the deep path, which no wrap could have saved.
- A link is checked as far as its path. `README.md#no-such-heading` passes, so a
  heading that was renamed is the reader's to catch.
- A tracked file missing from the worktree is reported by name, which is what a
  half-finished rename leaves behind.
