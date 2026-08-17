#!/usr/bin/env python3
"""bound_selector_check.py — BD-P3 made mechanical.

A call to a CUT-ORDERED dispatch predicate with the *selecting* argument BOUND to a
literal atom does not ask "what does the engine assign?" — earlier clauses fail to
UNIFY on the wrong atom, so their cuts never execute and the query falls through to a
later clause body. It answers "satisfies that clause body in isolation."

The bound form is OVER-PERMISSIVE. Consequences worth keeping straight:
  * a bound-arg ZERO is conservative and trustworthy (if the permissive form cannot
    fire, the real cascade certainly cannot);
  * a bound-arg NONZERO is an artifact until checked.

Rule (docs/technical/build_discipline.md -> *Bound-probe bypasses clause-order*): query
UNBOUND and post-filter by equality, or use once/1 for a census.

WHY THIS EXISTS AS A GATE ROW RATHER THAN A DOCUMENTED RULE. BD-P3 was written
2026-05-30 with `constraint_signature(C, <bound-atom>)` as its worked example, AND the
author of signature_detection.pl annotated the correct form inline at :1771 and :1790.
Documentation routing therefore got the strongest treatment available to it and still
left four bound-arg call sites, two of them on an atom that actually fires. Provenance:
KNOWN_STATE 2026-08-17; OQ-278 evidence; OQ-296.

DISCRIMINATION RECORD — ANCHORED TO CONTENT, NOT TO A COMMIT (operator, 2026-08-17).
A SHA-only record dangles if the anchoring commit is amended or rebased, and one half of
this pair was another writer's commit. So the record IS the output text below; the SHAs
are convenience pointers.

  FIRES on exactly these five sites (pre-repair):
    prolog/diagnostic_summary.pl:424        selector `natural_law`
    prolog/diagnostic_summary.pl:450        selector `false_ci_rope`
    prolog/routing_sink.pl:120              selector `natural_law`
    prolog/tests/test_reading_totality.pl:139  selector `unknown`
    python/fcr_ablation.py:75               selector `false_ci_rope`
    -> "bound_selector_check: RED — 5 bound-selector call site(s)"
  DECLINES once all five carry `once(...(C, Sig)), Sig == <atom>`:
    -> "bound_selector_check: GREEN — ... 0 exemption(s) ..."

  To re-verify without trusting a SHA: revert any one of the five lines to the bound
  form and the row goes RED naming that line. (Convenience pointers: RED at dcde9591,
  GREEN at the repair commit.)

CARVE-OUTS ARE SHAPE-KEYED WHERE IT MATTERS. Clause-head and comment carve-outs are
shape tests. The self carve-out is SENTINEL-BOUNDED, not file-keyed: a bound call added
anywhere in this file outside the marked region is still caught (verified 2026-08-17 by
planting one — it fired at the planted line). Path exclusions ARE file-keyed, carry
reason strings, and are PRINTED in the green line so none of them is silent.

Usage:
    python3 python/bound_selector_check.py --check      # selftest, then live sweep
    python3 python/bound_selector_check.py --selftest   # fixtures only
    python3 python/bound_selector_check.py --list       # every call site, classified
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent

# ---------------------------------------------------------------------------
# Registry of cut-ordered dispatch predicates.
#
# ADD A PREDICATE HERE when it (a) dispatches by clause order and (b) commits with a
# cut. Registration is OPT-IN, which is itself a silent-escape shape — the same one
# reading_registry and spec_enum_check carry. A predicate absent from this table is
# NOT checked and NOT reported as safe.
# ---------------------------------------------------------------------------
CUT_ORDERED = {
    # name: (arity, 1-based selector position)
    "constraint_signature": (2, 2),
}

# ---------------------------------------------------------------------------
# SCAN SCOPE — repo-wide by default, NOT a directory allowlist.
#
# An allowlist of scanned dirs is itself fail-open: a new directory is invisible
# rather than checked. (The first version of this checker listed ["prolog", "python"]
# and would have missed a bound selector inside a `swipl -g` in scripts/.) So: walk
# everything, and every exclusion is DECLARED WITH A REASON below.
# ---------------------------------------------------------------------------
SCAN_SUFFIXES = {".pl", ".py", ".sh"}

# Path-component exclusions. Same discipline as EXEMPT: a reason per entry, never a
# bare list — a bare list decays into "places someone stopped looking." These are
# FILE-KEYED and therefore carry the fail-open risk that shape-keyed carve-outs do
# not: if an excluded region ever acquires a live call site, it is invisible forever.
# That is the trade, stated: each reason must say why the region cannot hold one.
SKIP_PARTS: dict[str, str] = {
    ".git": "VCS internals — not source.",
    ".claude": "MACHINE-LOCAL, and STRUCTURALLY SAFE — not an acknowledged hole. "
               "`.claude/*` is gitignored apart from settings.json; `.claude/worktrees/` "
               "holds worktree checkouts. A merge FROM a worktree writes to ENGINE paths "
               "(prolog/, python/, ...), which are scanned — so merged content lands in "
               "scope and this row catches it there. The exclusion can therefore never "
               "hide a live call site; it only skips duplicate copies. Do not try to "
               "'close' it by scanning worktrees: that would double-report every file. "
               "(2026-08-17: the first repo-wide sweep flagged "
               "`.claude/worktrees/oq-48-recalibration/` with pre-repair copies of the "
               "four engine sites. Checked, not assumed — that branch is FULLY MERGED "
               "into main with 0 commits ahead and last activity 2026-06-18, i.e. a stale "
               "checkout of main's own history, not divergent work. No coordination "
               "exposure, nothing to merge, no message owed. Pruning it is the operator's "
               "call.)",
    "testsets": "CORPUS DATA. Story files author facts; they do not call the engine. "
                "A .pl here is a data pack, not a call site.",
    "archives": "ARCHIVED corpora and point-in-time audit probes. Per audits/README.md, "
                "point-in-time documents are NOT retro-edited, so a historical probe's "
                "bound query is a record of what was run, not a live call site.",
    "audits": "Point-in-time evidence and archived probes — same rule as `archives`. "
              "A probe here is a record. Live probes belong in prolog/ or python/.",
    "outputs": "Generated artifacts, gitignored. Not source.",
    "node_modules": "Vendored third-party code.",
}

# Sentinel-bounded regions inside THIS file where the violating shape is QUOTED by
# necessity (module docstring example + the selftest fixtures). Bounded by markers
# rather than skipping the whole file, so a genuine new call site added ANYWHERE ELSE
# in this checker is still caught — the file-keyed version of this carve-out would
# have made the checker permanently blind to itself.
SELF_QUOTE_BEGIN = "BSC-QUOTED-SHAPES-BEGIN"
SELF_QUOTE_END = "BSC-QUOTED-SHAPES-END"

# ---------------------------------------------------------------------------
# Exemptions. Each entry REQUIRES a reason string — a bare path list decays into
# "sites someone silenced" (operator, 2026-08-17). Key is "path:line_content_anchor".
# ---------------------------------------------------------------------------
EXEMPT: dict[str, str] = {
    # (empty at introduction — the four known sites are being REPAIRED, not exempted.
    #  An exemption is for a site where the bound form is provably correct, e.g. a
    #  predicate whose registry entry says the selector is the FIRST clause.)
}

# A bound selector = a lowercase atom (Prolog) or a quoted atom (Python-embedded
# Prolog). An UNBOUND selector is a Capitalised var or `_`.
_BOUND_ATOM = r"[a-z][A-Za-z0-9_]*"


def _pattern(name: str) -> re.Pattern:
    return re.compile(rf"\b{name}\s*\(\s*[^(),]+?\s*,\s*({_BOUND_ATOM})\s*\)")


def _is_comment(line: str, path: Path) -> bool:
    s = line.lstrip()
    if path.suffix == ".pl":
        return s.startswith("%") or s.startswith("*")
    return s.startswith("#")


def _is_clause_head(line: str, name: str, m: re.Match) -> bool:
    """A DEFINITION, not a call site.

    `constraint_signature(C, <bound-atom>) :- ...` is the cut-ordered predicate being
    DEFINED — bound atoms in clause heads are how the dispatch is written and are the
    thing the rule protects, not a violation of it. Head position = the match begins the
    line (modulo indentation) and the term is followed by `:-` or `.`.
    """
    if line[:m.start()].strip():
        return False
    return re.match(r"\s*\)?\s*(:-|\.)", line[m.end():]) is not None


def _quoted_shape_lines(text: str) -> set[int]:
    """Line numbers inside sentinel-bounded 'this file quotes the shape' regions."""
    inside, out = False, set()
    for i, line in enumerate(text.splitlines(), start=1):
        if SELF_QUOTE_BEGIN in line:
            inside = True
        if inside:
            out.add(i)
        if SELF_QUOTE_END in line:
            inside = False
    return out


def scan_text(text: str, path: Path,
              skip_lines: set[int] | None = None) -> list[tuple[int, str, str]]:
    """Return [(lineno, selector_atom, line)] for bound-selector CALL sites."""
    skip_lines = skip_lines or set()
    hits = []
    for name in CUT_ORDERED:
        pat = _pattern(name)
        for i, line in enumerate(text.splitlines(), start=1):
            if i in skip_lines:
                continue
            if _is_comment(line, path):
                continue
            m = pat.search(line)
            if not m:
                continue
            if _is_clause_head(line, name, m):
                continue
            hits.append((i, m.group(1), line.strip()))
    return hits


def iter_files():
    for p in sorted(REPO.rglob("*")):
        if p.suffix not in SCAN_SUFFIXES or not p.is_file():
            continue
        if any(part in SKIP_PARTS for part in p.parts):
            continue
        yield p


def live_sweep_hits(p: Path) -> list[tuple[int, str, str]]:
    """Scan one file, applying the sentinel-bounded self carve-out where relevant."""
    text = p.read_text(encoding="utf-8", errors="replace")
    skip = _quoted_shape_lines(text) if p.resolve() == Path(__file__).resolve() else None
    return scan_text(text, p, skip)


def live_sweep() -> tuple[list[str], int]:
    problems, scanned = [], 0
    for p in iter_files():
        scanned += 1
        try:
            hits = live_sweep_hits(p)
        except OSError as e:  # fail closed: unreadable file is not "clean"
            problems.append(f"UNREADABLE {p}: {e}")
            continue
        rel = p.relative_to(REPO)
        for lineno, atom, line in hits:
            key = f"{rel}:{atom}"
            if key in EXEMPT:
                continue
            problems.append(
                f"{rel}:{lineno}: bound selector `{atom}` on a cut-ordered "
                f"predicate — query unbound + post-filter (BD-P3). | {line}"
            )
    if scanned == 0:
        # An empty sweep is a broken sweep, not a clean one (Pattern 5).
        raise SystemExit("bound_selector_check: RED — scanned 0 files")
    return problems, scanned


# ---------------------------------------------------------------------------
# Selftest fixtures: violation shapes must be flagged, conforming shapes must not.
# ---------------------------------------------------------------------------
_PL = Path("fixture.pl")
_PY = Path("fixture.py")

# BSC-QUOTED-SHAPES-BEGIN — fixtures QUOTE the violating shape by necessity. Skipped by
# LINE RANGE, not by filename: a real call site added anywhere else in this file is still
# caught. A file-keyed self-skip would have made this checker permanently blind to itself.
FIXTURES = [
    # (label, text, path, expect_hit)
    ("prolog bound atom",
     "foo :- signature_detection:constraint_signature(C, natural_law), !.", _PL, True),
    ("prolog bound inside catch",
     "bar :- catch(signature_detection:constraint_signature(C, false_ci_rope), _, fail).",
     _PL, True),
    ("python-embedded bound",
     "q = '( signature_detection:constraint_signature(C, false_ci_rope)'", _PY, True),
    ("CONFORMING unbound + post-filter",
     "baz :- constraint_signature(C, Sig), Sig == false_ci_rope.", _PL, False),
    ("CONFORMING unbound var",
     "qux :- catch(signature_detection:constraint_signature(C, Sig), _, fail).", _PL, False),
    ("CONFORMING anonymous var",
     "quux :- constraint_signature(C, _).", _PL, False),
    ("NEGATIVE CONTROL comment line is not a call site",
     "%   - detector : constraint_signature(C, natural_law) — a SOCKETED router input",
     _PL, False),
    ("NEGATIVE CONTROL unregistered predicate untouched",
     "zap :- some_other_pred(C, natural_law).", _PL, False),
    ("NEGATIVE CONTROL clause HEAD is a definition, not a call",
     "constraint_signature(C, false_ci_rope) :-\n    false_ci_rope(C, _), !.", _PL, False),
    ("NEGATIVE CONTROL indented clause head",
     "    constraint_signature(C, natural_law) :-", _PL, False),
    ("still flags a bound call INSIDE the defining file",
     "helper(C) :- once(constraint_signature(C, natural_law)).", _PL, True),
]
# BSC-QUOTED-SHAPES-END


# Sentinels bound a REGION, not its CONTENTS — the plant-and-restore check proves the
# boundary holds today, but nothing stops the region growing until it is a de-facto
# file-keyed skip (operator, 2026-08-17). So cap it. Currently 2 + 31 = 33 lines.
MAX_SELF_QUOTE_LINES = 45


def selftest() -> list[str]:
    fails = []
    # The carve-out must stay small enough to remain a carve-out.
    own = Path(__file__).read_text(encoding="utf-8")
    n_quoted = len(_quoted_shape_lines(own))
    if n_quoted > MAX_SELF_QUOTE_LINES:
        fails.append(
            f"SELFTEST self-quote region has grown to {n_quoted} lines "
            f"(cap {MAX_SELF_QUOTE_LINES}) — a bounded carve-out is turning into a "
            f"file-keyed skip. Shrink it or de-literalise the quoted shapes.")
    if n_quoted == 0:
        fails.append(
            "SELFTEST self-quote sentinels missing — the fixtures would self-fire, and "
            "a maintainer's likely 'fix' is a file-keyed skip. Restore the markers.")
    for label, text, path, expect in FIXTURES:
        got = bool(scan_text(text, path))
        if got != expect:
            fails.append(f"SELFTEST {label}: expected hit={expect}, got hit={got}")
    return fails


def main(argv: list[str]) -> int:
    selftest_only = "--selftest" in argv
    listing = "--list" in argv

    st = selftest()
    if st:
        for f in st:
            print(f"  {f}")
        print("bound_selector_check: RED (selftest)")
        return 1
    n_hit = sum(1 for f in FIXTURES if f[3])
    if selftest_only:
        print(f"bound_selector_check: selftest {len(FIXTURES)}/{len(FIXTURES)} "
              f"({n_hit} violation shapes red-capable + "
              f"{len(FIXTURES) - n_hit} negative controls)")
        return 0

    if listing:
        for p in iter_files():
            for lineno, atom, line in live_sweep_hits(p):
                rel = p.relative_to(REPO)
                mark = "EXEMPT" if f"{rel}:{atom}" in EXEMPT else "VIOLATION"
                print(f"{mark:10} {rel}:{lineno}  selector={atom}")
        return 0

    problems, scanned = live_sweep()
    if problems:
        for e in problems:
            print(f"  {e}")
        print(f"bound_selector_check: RED — {len(problems)} bound-selector call site(s)")
        return 1
    print(f"bound_selector_check: GREEN — {scanned} files, {len(CUT_ORDERED)} cut-ordered "
          f"predicate(s) registered, {len(EXEMPT)} exemption(s), "
          f"{len(SKIP_PARTS)} declared path exclusion(s) "
          f"({', '.join(sorted(SKIP_PARTS))}), selftest {len(FIXTURES)}/{len(FIXTURES)}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
