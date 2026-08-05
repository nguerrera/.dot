# Contributing

`tools/check` is the gate a change has to pass, and it needs nothing installed.
It reads every tracked `.md` file for a line over 80 columns, a non-ASCII
character, and a relative link that does not resolve. CI runs it on every push
and every pull request.
