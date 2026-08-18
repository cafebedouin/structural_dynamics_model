#!/usr/bin/env python3
"""doc_pattern_check — make the build-discipline taxonomy's canonicity a CHECKED FACT.

Two always-read documents publish a numbered defect taxonomy. They DISAGREED at indices
3 and 4 from `220739b8` (2026-05-30) until OQ-278 ruled on 2026-08-17 — undetected for
151 commits touching either file:

    CLAUDE.md                        -> "## Build Discipline" numbered list
    docs/technical/build_discipline.md -> "## Pattern N" headings (+ a spine table)

That was Pattern 2 (one-canonical-thing-became-two) instantiated on the pattern list
itself: no queryable fact of canonicity, and BOTH COPIES PARSE — each read as a
complete, coherent six. This checker is the queryable fact, and it is what keeps the
post-ruling agreement from silently decaying back. OQ-278.

WHY THE COUNTS COULD NEVER DETECT IT
    7af6b945  2026-05-29   CLAUDE=3  BD=2   <- unequal for one day
    220739b8  2026-05-30   CLAUDE=4  BD=4   <- counts CONVERGE; contents DIVERGE at 3 and 4
    aaba00e0  2026-05-31   5  5    f8f9eb6b  2026-06-10   6  6    HEAD  6  6
The counts converged at the exact commit where the contents diverged, and matched at
every append after. Anything counting members reads green forever. Hence: compare NAMES
at each index, never cardinalities.

THE MANIFEST HOLDS LOCATIONS AND STATES — NEVER SLUGS
Storing the authored pattern names here would make this file a THIRD authored copy of
the taxonomy, hand-synced at every renumbering, in a checker whose whole subject is that
exact failure. `spec_enum_check.py:3-5` opens by recording that manual spec/code sync
"has failed structurally twice." So SITES holds file paths and extraction regexes;
DECLARED_COLLISIONS holds index -> (owning OQ, state); agreement is COMPUTED from the
documents themselves.

The `state` field is load-bearing WHENEVER an entry exists. It carried the difference
between index 4 (an unruled collision with two live claimants) and index 3 (a *ruled*
one, CLAUDE.md vacated 2026-08-11, with an unrepaired second site still publishing
`bound-probe`) — without it, an eventual "allowlisted collision disappeared" red is
uninterpretable without opening the OQ. Both entries retired on 2026-08-17 when the
ruling landed in the documents; the mechanism stays for the next one.

WHAT GOES RED
    MISSING INDEX        an index one document publishes and the other does not
    DIVERGENT            an index where the two disagree and the disagreement is NOT declared
                         (a NEW fork — the thing that went unnoticed for 151 commits)
    UNDECLARED RESOLUTION a declared collision where the documents now AGREE: a ruling
                         landed in the documents without updating this allowlist. Same
                         stale-entry report `prolog/axis_boundary_allowlist.txt` makes.
    UNKNOWN COLLISION    the allowlist names an index no document publishes (points at nothing)
    SPINE LAG            build_discipline.md's own spine table publishes a different INDEX
                         SET than its own headings, beyond the declared lag

SCOPE OF THE VERDICT LINE — stated because an unstated selection rule is the defect this
taxonomy is about. NAMES are compared between CLAUDE.md's list and build_discipline.md's
HEADINGS. build_discipline.md's spine table (`## The spine:` -> its table) is a SECOND
encoding of index->name in the same file, and it is checked for INDEX-SET agreement only,
NOT names — it deliberately uses short forms ("Silent fork" for
"One-canonical-thing-became-two", "Bound-probe bypasses cut" for
"...bypasses clause-order"), so a name comparison there would fire on abbreviation rather
than on divergence. Checking its index set is what caught the live defect: the spine
table published 1-5 and said "The five patterns" while the headings published 1-6.

STILL UNGUARDED (enumerated rather than left silent — every one is an index->pattern
encoding this checker does not read):
    CLAUDE.md:158                    cardinality claim ("seven members at eight indices")
    README.md:170                    cardinality claim ("the seven defect patterns")
    docs/amnesiac_institution*.md    6 paper versions, each publishing the list
    docs/design/design_discipline.md :464 :600 :710 index citations
    KNOWN_STATE.md                   ~10 index citations
    audits/2026-08-10_oq277_rq2_crosscoding/PREREGISTRATION.md:350,356
                                     md5-FROZEN and gate-enforced (`scripts/gate.sh:45`).
                                     THIS CHECKER MUST NEVER WRITE IT. It is not read here
                                     either: it is a point-in-time record of what an
                                     out-of-harness coder was shown, so it is *correct*
                                     while expressing a numbering the documents may leave.
    228 machine-generated JSON payloads under the oq277 audit — point-in-time

Usage:
    python3 python/doc_pattern_check.py --check         # gate mode; default. Selftest rides.
    python3 python/doc_pattern_check.py --list          # dump every extraction + the manifest
    python3 python/doc_pattern_check.py --pairwise [REV] # manifest-free agreement, optionally
                                                        # at a git rev (the discrimination record)
    python3 python/doc_pattern_check.py --selftest      # controls only
"""

import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CLAUDE_MD = "CLAUDE.md"
BUILD_DISC = "docs/technical/build_discipline.md"

# --- the manifest: LOCATIONS and STATES, never slugs -------------------------

SITES = {
    # key -> (repo-relative path, kind). Extraction regexes live with their extractors.
    "claude_md_list": (CLAUDE_MD, "numbered bold list under '## Build Discipline'"),
    "bd_headings": (BUILD_DISC, "'## Pattern N' section headings"),
    "bd_spine_table": (BUILD_DISC, "the spine table under '## The spine:' (index set only)"),
}

DECLARED_COLLISIONS = {
    # index -> (owning OQ, state).  state is NOT decoration: see the header.
    #
    # EMPTY SINCE 2026-08-17 — OQ-278 RULED, and the entries were retired in the same change that
    # landed the ruling in the documents. Both former entries are gone for DIFFERENT reasons and
    # neither is "we stopped caring": index 4 was ruled to `fabricated-default` (R2 = C2) so the
    # documents now AGREE, and index 3 became a grave in both (`destructive-replace` demoted here,
    # `bound-probe` renumbered to 7 there) so it agrees as `<vacated>`. Leaving either entry in
    # place would fire UNDECLARED RESOLUTION — which is this checker working, not a nuisance.
    #
    # An empty allowlist is the strong state, not a lapsed one: every index is now compared on
    # names with nothing exempted, so ANY divergence is a NEW fork and goes red immediately.
}

DECLARED_SPINE_LAG = {
    # indices the spine table is KNOWN to omit, and why the repair is held.
    #
    # EMPTY SINCE 2026-08-17. The lag was `[6]`, held deliberately until OQ-278's ruling because
    # editing the spine before it would have destroyed the evidence of what each document
    # published when. The ruling landed, the spine table now publishes every index its own
    # headings publish (1-8, including the vacated 3 as an explicit empty row), and the
    # declaration retired with it. The mechanism stays: a new omission goes red with no allowlist
    # to hide in.
    "missing_from_spine": [],
    "owning_oq": "OQ-278",
    "reason": "no lag declared — the spine table and the headings publish the same index set.",
}

VACATED = "<vacated>"

# --- extraction --------------------------------------------------------------

# CLAUDE.md items appear in TWO authored forms and both must parse: bolded (every item
# since 220739b8's successors) and UNBOLDED (item 3 as authored at 7af6b945/220739b8 —
# required for --pairwise to reach the commit that created the collision).
# DOTALL is load-bearing on the bolded form: items 3 and 6 hard-wrap INSIDE the bold run,
# so the closing `**` sits on the next line. Without it those two silently extract to
# nothing and the check reads green on four of six indices — the checker's own Pattern 5.
CM_BOLD_RE = re.compile(r"^\*\*(\d+)\.\s*(.+?)\.?\*\*", re.MULTILINE | re.DOTALL)
CM_PLAIN_RE = re.compile(r"^(\d+)\.\s+(.+?)\.\s", re.MULTILINE)
BD_HEADING_RE = re.compile(r"^## Pattern (\d+)\s*[—:-]\s*(.+)$", re.MULTILINE)
SPINE_ROW_RE = re.compile(r"^\|\s*(\d+)\s*\|", re.MULTILINE)


def normalize(raw):
    """Head phrase before the first '(' or em-dash, lowercased, non-alnum -> '-'.

    Parentheticals differ even for members the two documents SHARE ("...vs absent" here,
    "...vs absent conflation" there), so a whole-string compare would fire on gloss drift
    rather than on divergence.
    """
    if "VACATED" in raw:
        return VACATED
    head = re.split(r"[(—]", raw, maxsplit=1)[0]
    slug = re.sub(r"[^a-z0-9]+", "-", head.lower()).strip("-")
    return slug


def _section(text, start_re, stop_re):
    m = re.search(start_re, text, re.MULTILINE)
    if not m:
        raise RuntimeError(f"section anchor not found: {start_re!r}")
    rest = text[m.end():]
    stop = re.search(stop_re, rest, re.MULTILINE)
    return rest[: stop.start()] if stop else rest


def claude_md_patterns(text):
    """index -> slug, from the numbered list scoped to '## Build Discipline'."""
    body = _section(text, r"^## Build Discipline\b", r"^## ")
    out = {}
    for rx in (CM_BOLD_RE, CM_PLAIN_RE):
        for idx, raw in rx.findall(body):
            out.setdefault(int(idx), normalize(raw))
    if not out:
        raise RuntimeError("CLAUDE.md: extracted ZERO numbered patterns — a regex over prose "
                           "that matches nothing is the absence-satisfies-the-gate shape")
    return out


def bd_heading_patterns(text):
    """index -> slug, from '## Pattern N' headings."""
    out = {int(i): normalize(raw) for i, raw in BD_HEADING_RE.findall(text)}
    if not out:
        raise RuntimeError("build_discipline.md: extracted ZERO pattern headings")
    return out


def bd_spine_indices(text):
    """The index SET published by the spine table (names deliberately not compared)."""
    body = _section(text, r"^## The spine:", r"^## ")
    return {int(i) for i in SPINE_ROW_RE.findall(body)}


def bd_spine_rows(text):
    """index -> short name, for --list only. NEVER compared (see the header's scope note)."""
    body = _section(text, r"^## The spine:", r"^## ")
    rows = re.findall(r"^\|\s*(\d+)\s*\|\s*([^|]+?)\s*\|", body, re.MULTILINE)
    return {int(i): name for i, name in rows}


# --- the check ---------------------------------------------------------------

def run_check(cm_text, bd_text, collisions=None, spine_lag=None):
    """Return a list of error strings (empty = green)."""
    collisions = DECLARED_COLLISIONS if collisions is None else collisions
    spine_lag = DECLARED_SPINE_LAG if spine_lag is None else spine_lag
    errors = []

    cm = claude_md_patterns(cm_text)
    bd = bd_heading_patterns(bd_text)

    for idx in sorted(set(cm) | set(bd)):
        if idx not in bd:
            errors.append(f"MISSING INDEX: {idx} published by {CLAUDE_MD} "
                          f"('{cm[idx]}') but absent from {BUILD_DISC}")
            continue
        if idx not in cm:
            errors.append(f"MISSING INDEX: {idx} published by {BUILD_DISC} "
                          f"('{bd[idx]}') but absent from {CLAUDE_MD}")
            continue
        agree = cm[idx] == bd[idx]
        declared = idx in collisions
        if not agree and not declared:
            errors.append(f"DIVERGENT: index {idx} — {CLAUDE_MD}='{cm[idx]}' "
                          f"{BUILD_DISC}='{bd[idx]}' (not a declared collision)")
        elif agree and declared:
            oq, state = collisions[idx]
            errors.append(f"UNDECLARED RESOLUTION: index {idx} is allowlisted as a collision "
                          f"({oq}, state={state}) but both documents now agree on '{cm[idx]}' — "
                          f"the ruling landed in the documents without updating this allowlist")

    published = set(cm) | set(bd)
    for idx, (oq, state) in sorted(collisions.items()):
        if idx not in published:
            errors.append(f"UNKNOWN COLLISION: allowlist names index {idx} ({oq}, state={state}) "
                          f"which neither document publishes")

    # Second encoding inside build_discipline.md: index set only (see the scope note).
    spine = bd_spine_indices(bd_text)
    expected_missing = set(spine_lag["missing_from_spine"])
    missing = set(bd) - spine - expected_missing
    extra = spine - set(bd)
    if missing:
        errors.append(f"SPINE LAG: build_discipline.md's spine table omits index/indices "
                      f"{sorted(missing)} that its own headings publish (beyond the declared lag "
                      f"{sorted(expected_missing)}, {spine_lag['owning_oq']})")
    if extra:
        errors.append(f"SPINE LAG: build_discipline.md's spine table publishes index/indices "
                      f"{sorted(extra)} that its own headings do not")
    resolved_lag = expected_missing & spine
    if resolved_lag:
        errors.append(f"UNDECLARED RESOLUTION: spine table now publishes {sorted(resolved_lag)}, "
                      f"declared missing by {spine_lag['owning_oq']} — the lag was repaired "
                      f"without updating this declaration")
    return errors


# --- pairwise: manifest-free, optionally at a git rev ------------------------

def _read_at(rev, relpath):
    if rev is None:
        return (REPO / relpath).read_text(encoding="utf-8")
    return subprocess.run(["git", "show", f"{rev}:{relpath}"], cwd=REPO,
                          capture_output=True, text=True, check=True).stdout


def pairwise(rev=None):
    """Agreement over SHARED indices with no manifest. Returns (shared, disagree, cm, bd).

    Required for the discrimination record: `--check` cannot be run at a historical commit,
    because the manifest pins TODAY's indices and any older commit would go red for
    MISSING INDEX rather than for the collision. This mode discriminates.
    """
    cm = claude_md_patterns(_read_at(rev, CLAUDE_MD))
    bd = bd_heading_patterns(_read_at(rev, BUILD_DISC))
    shared = sorted(set(cm) & set(bd))
    disagree = [i for i in shared if cm[i] != bd[i]]
    return shared, disagree, cm, bd


# --- selftest: mutation IN MEMORY ONLY, never on disk ------------------------

def selftest():
    """Seven controls: six violation shapes forced red, plus the unmutated pair asserted green.

    THE ALLOWLISTS ARE EMPTY SINCE THE OQ-278 RULING, so two shapes can no longer be forced by
    mutating a DOCUMENT — there is no declared entry left for a document edit to contradict.
    They are forced by passing a synthetic manifest instead (controls 3 and 6). Dropping them
    when the allowlists emptied would have retired two controls for a reason that has nothing
    to do with whether the code paths still work: an unexercised branch that reads green is the
    orphan shape this file's own taxonomy names.
    """
    failures = []
    cm_text = (REPO / CLAUDE_MD).read_text(encoding="utf-8")
    bd_text = (REPO / BUILD_DISC).read_text(encoding="utf-8")

    def want(tag, errs, note):
        if not any(e.startswith(tag) for e in errs):
            failures.append(f"selftest FAILED: {note} did not produce {tag} (got: {errs or 'GREEN'})")

    # (0) NEGATIVE CONTROL — the unmutated pair must be GREEN. Without this the five
    #     positives only show the instrument CAN fire, never that its firing informs.
    base = run_check(cm_text, bd_text)
    if base:
        failures.append(f"selftest FAILED: negative control — unmutated pair is not green: {base}")

    # (1) MISSING INDEX — drop a heading build_discipline.md publishes.
    mutated = bd_text.replace("## Pattern 6 —", "## Retired 6 —", 1)
    want("MISSING INDEX", run_check(cm_text, mutated), "dropped Pattern 6 heading")

    # (2) DIVERGENT — a NEW fork at a currently-agreeing index. This is the 151-commit defect.
    mutated = bd_text.replace("## Pattern 1 — Produced-but-not-consumed",
                              "## Pattern 1 — Dangling-wire syndrome", 1)
    want("DIVERGENT", run_check(cm_text, mutated), "renamed Pattern 1 in one document only")

    # (3) UNDECLARED RESOLUTION — an index the manifest calls a collision on which the documents
    #     AGREE. Forced with a synthetic manifest: the real one is empty post-ruling, so the
    #     stale-entry report has no live entry to demonstrate itself on.
    want("UNDECLARED RESOLUTION",
         run_check(cm_text, bd_text, collisions={4: ("OQ-TEST", "synthetic")}),
         "allowlisting an index on which both documents agree")

    # (4) UNKNOWN COLLISION — an allowlist entry pointing at nothing.
    bogus = {**DECLARED_COLLISIONS, 99: ("OQ-999", "stale")}
    want("UNKNOWN COLLISION", run_check(cm_text, bd_text, collisions=bogus),
         "allowlist naming an unpublished index")

    # (5) SPINE LAG — the second encoding inside build_discipline.md drifts from its own headings.
    mutated = bd_text.replace("| 4 | Fabricated default |", "| 9 | Fabricated default |", 1)
    want("SPINE LAG", run_check(cm_text, mutated), "spine table row renumbered away from headings")

    # (6) UNDECLARED RESOLUTION on the SPINE lag — a declared omission the table now publishes.
    #     Same synthetic-manifest reason as (3): `missing_from_spine` is empty post-ruling.
    want("UNDECLARED RESOLUTION",
         run_check(cm_text, bd_text,
                   spine_lag={"missing_from_spine": [6], "owning_oq": "OQ-TEST",
                              "reason": "synthetic control"}),
         "declaring a spine omission the table in fact publishes")

    return failures


# --- main --------------------------------------------------------------------

def _print_list():
    cm_text = (REPO / CLAUDE_MD).read_text(encoding="utf-8")
    bd_text = (REPO / BUILD_DISC).read_text(encoding="utf-8")
    cm, bd = claude_md_patterns(cm_text), bd_heading_patterns(bd_text)
    spine_rows, spine_idx = bd_spine_rows(bd_text), bd_spine_indices(bd_text)
    print("SITES (locations, not slugs):")
    for key, (path, kind) in SITES.items():
        print(f"  {key:18s} {path} — {kind}")
    print("\nDECLARED_COLLISIONS (index -> owning OQ, state):")
    for idx, (oq, state) in sorted(DECLARED_COLLISIONS.items()):
        print(f"  {idx}  {oq}  {state}")
    print(f"\nDECLARED_SPINE_LAG: missing {DECLARED_SPINE_LAG['missing_from_spine']} "
          f"({DECLARED_SPINE_LAG['owning_oq']})")
    print(f"\n{'idx':>3}  {'CLAUDE.md':<34} {'build_discipline.md headings':<34} agree")
    for idx in sorted(set(cm) | set(bd)):
        a, b = cm.get(idx, "—ABSENT—"), bd.get(idx, "—ABSENT—")
        mark = "=" if a == b else ("DECLARED" if idx in DECLARED_COLLISIONS else "**FORK**")
        print(f"{idx:>3}  {a:<34} {b:<34} {mark}")
    print(f"\nspine table (index set only, names NOT compared): {sorted(spine_idx)}")
    for idx in sorted(spine_rows):
        print(f"  {idx}  {spine_rows[idx]}")


def _print_pairwise(rev):
    shared, disagree, cm, bd = pairwise(rev)
    label = rev or "working tree"
    print(f"{label}  shared={shared}  DISAGREE={disagree}  "
          f"<- {'FIRES' if disagree else 'DECLINES'}")
    for idx in disagree:
        print(f"    idx {idx}: CM='{cm[idx]}'  BD='{bd[idx]}'")
    return 0


def main(argv):
    if "--list" in argv:
        _print_list()
        return 0

    if "--pairwise" in argv:
        i = argv.index("--pairwise")
        rev = argv[i + 1] if len(argv) > i + 1 and not argv[i + 1].startswith("-") else None
        return _print_pairwise(rev)

    selftest_only = "--selftest" in argv
    if not (selftest_only or "--check" in argv):
        print(__doc__)
        return 2

    for rel in (CLAUDE_MD, BUILD_DISC):
        if not (REPO / rel).exists():
            print(f"doc_pattern_check: RED — document not found: {rel}")
            return 1

    # Positive controls ride EVERY run: an unwitnessed checker is a claim.
    st = selftest()
    if st:
        for f in st:
            print(f"  {f}")
        print("doc_pattern_check: RED (selftest)")
        return 1
    if selftest_only:
        print("doc_pattern_check: selftest 7/7 (6 violation shapes red-capable + negative control)")
        return 0

    errors = run_check((REPO / CLAUDE_MD).read_text(encoding="utf-8"),
                       (REPO / BUILD_DISC).read_text(encoding="utf-8"))
    if errors:
        for e in errors:
            print(f"  {e}")
        print(f"doc_pattern_check: RED — {len(errors)} problem(s)")
        return 1
    cm = claude_md_patterns((REPO / CLAUDE_MD).read_text(encoding="utf-8"))
    n_decl = len(DECLARED_COLLISIONS)
    decl = (f"{n_decl} declared collision(s) at {sorted(DECLARED_COLLISIONS)}"
            if n_decl else "0 declared collisions — every index compared, nothing exempted "
                           "(OQ-278 ruled 2026-08-17)")
    print(f"doc_pattern_check: GREEN — {len(cm)} indices, {decl}; names checked CLAUDE.md vs BD "
          f"headings, spine table index-set only; selftest 7/7")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
