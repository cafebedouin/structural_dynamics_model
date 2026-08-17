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

Rule (BD-P3, docs/technical/build_discipline.md): query UNBOUND and post-filter by
equality — `constraint_signature(C, Sig), Sig == false_ci_rope` — or use once/1 for a
census.

WHY THIS EXISTS AS A GATE ROW RATHER THAN A DOCUMENTED RULE. BD-P3 was written
2026-05-30 with `constraint_signature(C, natural_law)` as its worked example, AND the
author of signature_detection.pl annotated the correct form inline at :1771 and :1790.
Documentation routing therefore got the strongest treatment available to it and still
left four bound-arg call sites, two of them on an atom that actually fires. Provenance:
KNOWN_STATE 2026-08-17; OQ-278 evidence; OQ-296.

DISCRIMINATION RECORD (naturally-arising pair, per build_discipline → "when a defect is
found, its before-commit is a free negative control"): this checker FIRES on the four
sites present at its introducing commit's parent, and DECLINES after they are repaired.
Both SHAs are recorded in KNOWN_STATE 2026-08-17. That is a stronger record than the
selftest fixtures below, which show only that authored violations are rejected.

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

# Scanned trees. testsets*/archives are DATA, not call sites.
SCAN_DIRS = ["prolog", "python"]
SCAN_SUFFIXES = {".pl", ".py"}
SKIP_PARTS = ("testsets", "archives")

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

    `constraint_signature(C, natural_law) :- ...` is the cut-ordered predicate being
    DEFINED — bound atoms in clause heads are how the dispatch is written and are the
    thing the rule protects, not a violation of it. Head position = the match begins the
    line (modulo indentation) and the term is followed by `:-` or `.`.
    """
    if line[:m.start()].strip():
        return False
    return re.match(r"\s*\)?\s*(:-|\.)", line[m.end():]) is not None


def scan_text(text: str, path: Path) -> list[tuple[int, str, str]]:
    """Return [(lineno, selector_atom, line)] for bound-selector CALL sites."""
    hits = []
    for name in CUT_ORDERED:
        pat = _pattern(name)
        for i, line in enumerate(text.splitlines(), start=1):
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
    for d in SCAN_DIRS:
        root = REPO / d
        if not root.is_dir():
            raise SystemExit(f"bound_selector_check: RED — scan dir missing: {root}")
        for p in sorted(root.rglob("*")):
            if p.suffix not in SCAN_SUFFIXES:
                continue
            if any(part in SKIP_PARTS for part in p.parts):
                continue
            # NO SELF-FIRE: this checker's own docstring and fixtures QUOTE the
            # violating shape by necessity — they are records, not call sites. Same
            # carve-out claim_cite_check makes, and it is asserted by a selftest row
            # rather than assumed.
            if p.resolve() == Path(__file__).resolve():
                continue
            yield p


def live_sweep() -> list[str]:
    problems, scanned = [], 0
    for p in iter_files():
        scanned += 1
        try:
            text = p.read_text(encoding="utf-8", errors="replace")
        except OSError as e:  # fail closed: unreadable file is not "clean"
            problems.append(f"UNREADABLE {p}: {e}")
            continue
        rel = p.relative_to(REPO)
        for lineno, atom, line in scan_text(text, p):
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
    return problems


# ---------------------------------------------------------------------------
# Selftest fixtures: violation shapes must be flagged, conforming shapes must not.
# ---------------------------------------------------------------------------
_PL = Path("fixture.pl")
_PY = Path("fixture.py")

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


def selftest() -> list[str]:
    fails = []
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
            for lineno, atom, line in scan_text(
                    p.read_text(encoding="utf-8", errors="replace"), p):
                rel = p.relative_to(REPO)
                mark = "EXEMPT" if f"{rel}:{atom}" in EXEMPT else "VIOLATION"
                print(f"{mark:10} {rel}:{lineno}  selector={atom}")
        return 0

    problems = live_sweep()
    if problems:
        for e in problems:
            print(f"  {e}")
        print(f"bound_selector_check: RED — {len(problems)} bound-selector call site(s)")
        return 1
    print(f"bound_selector_check: GREEN — {len(CUT_ORDERED)} cut-ordered predicate(s) "
          f"registered, {len(EXEMPT)} exemption(s), "
          f"selftest {len(FIXTURES)}/{len(FIXTURES)}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
