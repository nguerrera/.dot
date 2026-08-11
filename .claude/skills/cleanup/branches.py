#!/usr/bin/env python3

"""Sort swept branches by what their pull requests did, and print the report.

SKILL.md has why ancestry cannot answer this and what each verdict costs. This
is the sort itself: rows in, one report line each out, and the deletions named
rather than made.

    git branch --format='%(refname:short) %(objectname:short)' > local.txt
    git ls-remote --heads origin 'refs/heads/agent/*' > remote.txt
    gh pr list --state all --limit 200 \
      --json number,state,mergedAt,headRefName > pulls.json
    python .claude/skills/cleanup/branches.py local.txt remote.txt pulls.json

The three commands go in as they came out, so nothing between them has to be
converted by hand.
"""

import json
import sys
import unittest
from pathlib import Path

# A verdict decides whether the branch is deleted and what the line says about
# it. UNCLEAR is what more than one pull request disagreeing looks like, and it
# is kept and reported rather than resolved here.
MERGED, CLOSED, OPEN = "merged", "closed", "open"
NONE, UNCLEAR = "none", "unclear"

DELETE = {MERGED, CLOSED}

# Which sides a branch was on. The column is padded to the widest of the three
# whatever a given run turns up, so two runs line up against each other.
SIDES = ("local", "remote", "both")
SIDE_WIDTH = max(len(side) for side in SIDES)

# The branch the sweep runs from and never a candidate. Listing it would put it
# in the report as a branch with no pull request.
TRUNK = "main"

REF = "refs/heads/"


def parse_branches(text):
    """Local branches as name -> hash, the trunk left out.

    From `git branch --format='%(refname:short) %(objectname:short)'`. The
    format is asked for rather than taken from the default output, which marks
    the current branch with an asterisk in the same column as a name.
    """
    found = {}
    for line in text.splitlines():
        fields = line.split()
        if len(fields) == 2 and fields[0] != TRUNK:
            found[fields[0]] = fields[1]
    return found


def parse_refs(text):
    """Remote branches as name -> hash.

    From `git ls-remote --heads origin`, whose lines are the hash, a tab, and
    the full ref. The prefix comes off so both sides key on the same name.
    """
    found = {}
    for line in text.splitlines():
        fields = line.split("\t")
        if len(fields) == 2 and fields[1].startswith(REF):
            found[fields[1].removeprefix(REF)] = fields[0]
    return found


def parse_pulls(rows):
    """Pull request rows grouped by the branch each is on.

    From one `gh pr list --state all --json ...,headRefName` rather than a call
    per branch, which asks GitHub the same question once however many branches
    the sweep turned up.
    """
    found = {}
    for row in rows:
        found.setdefault(row["headRefName"], []).append(row)
    return found


def verdict(pulls):
    """What a branch's pull request rows say happened to it.

    `mergedAt` set is the whole of the merge check and `state` separates a
    closed pull request from an open one, per SKILL.md. No rows at all is the
    branch that never had one.
    """
    if not pulls:
        return NONE
    states = set()
    for pull in pulls:
        if pull.get("mergedAt"):
            states.add(MERGED)
        elif pull.get("state") == "CLOSED":
            states.add(CLOSED)
        else:
            states.add(OPEN)
    if len(states) > 1:
        return UNCLEAR
    return states.pop()


def sides(branch, local, remote):
    """Which sides carry a branch. A branch on one side only is ordinary."""
    here, there = branch in local, branch in remote
    if here and there:
        return "both"
    return "local" if here else "remote"


def note(pulls, found, where):
    """The last column: what happened, and the number that says so."""
    numbers = " ".join(f"#{pull['number']}" for pull in pulls)
    if found == MERGED:
        return f"merged as {numbers}"
    if found == CLOSED:
        return f"{numbers} closed unmerged"
    if found == OPEN:
        return f"{numbers} open, work in flight"
    if found == UNCLEAR:
        return f"{numbers} disagree, left alone"
    if where == "local":
        return "no pull request, never pushed"
    return "no pull request"


def decide(branch, local, remote, pulls):
    """One branch's whole row: what to do with it and what to say about it."""
    found = verdict(pulls)
    where = sides(branch, local, remote)
    return {
        "branch": branch,
        "hash": local.get(branch) or remote.get(branch, ""),
        "action": "deleted" if found in DELETE else "kept",
        "sides": where,
        "note": note(pulls, found, where),
    }


def report(local, remote, pulls):
    """One line per ref and nothing else, branches in name order.

    The hash of everything deleted is printed, which is what makes the
    transcript a recovery record alongside the pull request.
    """
    names = sorted(set(local) | set(remote))
    rows = [decide(name, local, remote, pulls.get(name, [])) for name in names]
    if not rows:
        return []
    branch_width = max(len(row["branch"]) for row in rows)
    return [
        f"{row['action']:<7}  {row['branch']:<{branch_width}}  {row['hash']}  "
        f"{row['sides']:<{SIDE_WIDTH}}  {row['note']}"
        for row in rows
    ]


def read(path):
    return Path(path).read_text(encoding="utf-8")


def main(argv):
    if len(argv) != 3:
        sys.stderr.write("usage: branches.py <local> <remote> <pulls.json>\n")
        return 2
    local = parse_branches(read(argv[0]))
    remote = parse_refs(read(argv[1]))
    pulls = parse_pulls(json.loads(read(argv[2])))
    for line in report(local, remote, pulls):
        print(line)
    return 0


def pull(number, state="OPEN", merged=None, branch="b"):
    return {
        "number": number,
        "state": state,
        "mergedAt": merged,
        "headRefName": branch,
    }


class ParseBranchesTest(unittest.TestCase):
    def test_name_and_hash(self):
        found = parse_branches("agent/a/x f089d6a")
        self.assertEqual(found, {"agent/a/x": "f089d6a"})

    def test_the_trunk_is_not_a_candidate(self):
        found = parse_branches("main 1111111\nagent/a/x 2222222")
        self.assertEqual(found, {"agent/a/x": "2222222"})

    def test_nothing_but_the_trunk_is_no_candidates(self):
        self.assertEqual(parse_branches("main 1111111"), {})

    def test_blank_input(self):
        self.assertEqual(parse_branches(""), {})


class ParseRefsTest(unittest.TestCase):
    def test_the_prefix_comes_off(self):
        line = "8f21a04\trefs/heads/agent/a/x"
        self.assertEqual(parse_refs(line), {"agent/a/x": "8f21a04"})

    def test_a_tag_ref_is_not_a_branch(self):
        self.assertEqual(parse_refs("8f21a04\trefs/tags/v1"), {})

    def test_blank_input(self):
        self.assertEqual(parse_refs(""), {})


class ParsePullsTest(unittest.TestCase):
    def test_rows_group_by_branch(self):
        rows = [pull(1, branch="a"), pull(2, branch="b")]
        self.assertEqual(sorted(parse_pulls(rows)), ["a", "b"])

    def test_a_reused_branch_keeps_both_rows(self):
        rows = [pull(1, branch="a"), pull(2, branch="a")]
        self.assertEqual(len(parse_pulls(rows)["a"]), 2)

    def test_no_pull_requests(self):
        self.assertEqual(parse_pulls([]), {})


class VerdictTest(unittest.TestCase):
    def test_no_rows_is_no_pull_request(self):
        self.assertEqual(verdict([]), NONE)

    def test_merged_at_set_is_the_merge_check(self):
        self.assertEqual(verdict([pull(2, "MERGED", "2026-08-02")]), MERGED)

    def test_closed_without_a_merge(self):
        self.assertEqual(verdict([pull(11, "CLOSED")]), CLOSED)

    def test_open(self):
        self.assertEqual(verdict([pull(9, "OPEN")]), OPEN)

    def test_rows_that_disagree_are_not_resolved_here(self):
        rows = [pull(2, "MERGED", "2026-08-02"), pull(9, "OPEN")]
        self.assertEqual(verdict(rows), UNCLEAR)


class SidesTest(unittest.TestCase):
    def test_both(self):
        self.assertEqual(sides("b", {"b": "a1"}, {"b": "a1"}), "both")

    def test_local_only(self):
        self.assertEqual(sides("b", {"b": "a1"}, {}), "local")

    def test_remote_only(self):
        self.assertEqual(sides("b", {}, {"b": "a1"}), "remote")


class DecideTest(unittest.TestCase):
    def test_a_merged_branch_is_deleted(self):
        pulls = [pull(2, "MERGED", "2026-08-02")]
        row = decide("b", {"b": "f089d6a"}, {}, pulls)
        self.assertEqual(row["action"], "deleted")
        self.assertEqual(row["note"], "merged as #2")

    def test_a_closed_branch_is_deleted(self):
        both = {"b": "8f21a04"}
        row = decide("b", both, both, [pull(11, "CLOSED")])
        self.assertEqual(row["action"], "deleted")
        self.assertEqual(row["note"], "#11 closed unmerged")

    def test_an_open_branch_is_kept(self):
        row = decide("b", {"b": "b31f0c4"}, {}, [pull(9, "OPEN")])
        self.assertEqual(row["action"], "kept")

    def test_a_branch_with_no_pull_request_is_kept(self):
        row = decide("b", {"b": "b31f0c4"}, {}, [])
        self.assertEqual(row["action"], "kept")
        self.assertEqual(row["note"], "no pull request, never pushed")

    def test_never_pushed_is_only_said_of_a_local_branch(self):
        row = decide("b", {}, {"b": "b31f0c4"}, [])
        self.assertEqual(row["note"], "no pull request")

    def test_a_remote_only_branch_still_gets_its_hash(self):
        row = decide("b", {}, {"b": "8f21a04"}, [])
        self.assertEqual(row["hash"], "8f21a04")


class ReportTest(unittest.TestCase):
    def test_nothing_swept_is_no_lines(self):
        self.assertEqual(report({}, {}, {}), [])

    def test_one_line_per_ref(self):
        local = {"agent/a/one": "f089d6a", "agent/b/two": "8f21a04"}
        self.assertEqual(len(report(local, {}, {})), 2)

    def test_branches_come_in_name_order(self):
        local = {"agent/z/last": "1111111", "agent/a/first": "2222222"}
        lines = report(local, {}, {})
        self.assertIn("agent/a/first", lines[0])

    def test_a_line_carries_action_branch_hash_sides_and_note(self):
        local = {"agent/a/one": "f089d6a"}
        pulls = {"agent/a/one": [pull(2, "MERGED", "2026-08-02")]}
        self.assertEqual(
            report(local, {}, pulls)[0],
            "deleted  agent/a/one  f089d6a  local   merged as #2",
        )

    def test_the_sides_column_pads_for_remote_even_with_none_present(self):
        local = {"agent/a/one": "f089d6a"}
        self.assertIn("local   no pull request", report(local, {}, {})[0])

    def test_the_branch_column_pads_to_the_widest(self):
        local = {"agent/a/one": "f089d6a", "agent/a/longer-one": "8f21a04"}
        lines = report(local, {}, {})
        self.assertEqual(lines[0].index("8f21a04"), lines[1].index("f089d6a"))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
