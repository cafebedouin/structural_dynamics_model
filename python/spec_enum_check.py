#!/usr/bin/env python3
"""Spec-vs-code enumeration tripwire for the Commitment Systems spec (v6+).

Manual spec/code sync has failed structurally twice (two pattern atoms at v5.2;
six whole subsystems by v6) — this makes the sync a GATE CHECK in the
`terminal_set_pinned` family, not a write-time discipline.

The spec (docs/commitment_systems/commitment_systems_sketch_v6.md) marks each
machine-checkable enumeration with a fenced sentinel block:

    <!-- spec-enum: cs_terminals -->
    ```
    axiom_foreclosure
    extinction
    ...
    ```
    <!-- /spec-enum -->

This checker holds a MANIFEST of required enum names. For each it (a) fails
loud if the sentinel block is ABSENT from the spec — a rewritten section that
drops its sentinel must go RED, never silently green on nothing-to-diff;
(b) extracts the block's atoms/rows and diffs them against the code pin,
exiting non-zero naming every divergent or missing enum.

Code pins (the same pins the plunit tripwire tests use where one exists):
  cs_terminals / cs_gap_directions  tests/test_cs_drift_engine.pl Pinned* lists
                                    (terminal_set_pinned keeps test == engine)
  cs_gap_magnitudes                 tests/test_cs_drift_engine.pl grid vocabulary
  cs_patterns                       cs_corpus_analysis.pl all_cs_patterns/1
  cs_verdicts                       cs_pattern_detection.pl cs_verdict/2 clause heads
  cs_obstruction_statuses           cs_kernel_registry.pl `Status = <atom>` assignments
  cs_trifurcation_types             cs_trifurcation.pl trif_dispatch/4 literals
  cs_attractor_table                cs_drift_engine.pl cs_terminal_attractor/4 clause
                                    heads + guards, normalized (see normalize below)

The attractor-table rows are normalized `dir | mag | ack -> terminal`, with
variables rendered `*` and a `Var \\= atom` guard rendered `*!=atom`. 16 clause
rows cover the 42 (Direction x Magnitude x Acknowledged) combos; disjointness
and coverage are proven by tests/test_cs_drift_engine.pl, not here — this check
only pins the table CONTENT so the spec's Appendix A cannot silently drift.

The selftest (positive controls) rides EVERY --check run, covering all three
failure shapes on mutated copies of the real spec: add-atom, remove-atom, and
deleted-sentinel (the likeliest real failure). A checker never seen red is
unwitnessed; one never seen red on the absent-block shape is unwitnessed where
it matters most.

Usage:  python3 python/spec_enum_check.py --check     (gate mode; default)
        python3 python/spec_enum_check.py --list      (dump code-side enums)
"""

import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
SPEC = REPO / "docs/commitment_systems/commitment_systems_sketch_v6.md"
PROLOG = REPO / "prolog"

TEST_DRIFT = PROLOG / "tests/test_cs_drift_engine.pl"
DRIFT_ENGINE = PROLOG / "cs_drift_engine.pl"
CORPUS_ANALYSIS = PROLOG / "cs_corpus_analysis.pl"
PATTERN_DETECTION = PROLOG / "cs_pattern_detection.pl"
KERNEL_REGISTRY = PROLOG / "cs_kernel_registry.pl"
TRIFURCATION = PROLOG / "cs_trifurcation.pl"


def _strip_comments(text):
    """Remove Prolog line comments (naive: no % inside quoted atoms in these files)."""
    return "\n".join(line.split("%", 1)[0] for line in text.splitlines())


def _read(path):
    return path.read_text(encoding="utf-8")


def _atoms_in_list(source, anchor_re, what):
    """Extract atoms from a Prolog list following anchor_re (list may span lines)."""
    m = re.search(anchor_re + r"\s*\[([^\]]*)\]", source, re.DOTALL)
    if not m:
        raise RuntimeError(f"code pin not found: {what}")
    atoms = [a.strip() for a in m.group(1).replace("\n", " ").split(",")]
    atoms = [a for a in atoms if a]
    bad = [a for a in atoms if not re.fullmatch(r"[a-z][a-zA-Z0-9_]*", a)]
    if bad:
        raise RuntimeError(f"unparseable atoms in {what}: {bad}")
    return sorted(set(atoms))


# --- code-side extractors ---------------------------------------------------

def code_cs_terminals():
    return _atoms_in_list(_strip_comments(_read(TEST_DRIFT)),
                          r"PinnedTerminals\s*=", "PinnedTerminals")


def code_cs_gap_directions():
    return _atoms_in_list(_strip_comments(_read(TEST_DRIFT)),
                          r"PinnedDirections\s*=", "PinnedDirections")


def code_cs_gap_magnitudes():
    return _atoms_in_list(_strip_comments(_read(TEST_DRIFT)),
                          r"member\(M,\s*", "magnitude grid member(M, [...])")


def code_cs_patterns():
    return _atoms_in_list(_strip_comments(_read(CORPUS_ANALYSIS)),
                          r"all_cs_patterns\(", "all_cs_patterns/1")


def code_cs_verdicts():
    src = _strip_comments(_read(PATTERN_DETECTION))
    atoms = re.findall(r"^cs_verdict\(\s*\w+\s*,\s*([a-z][a-zA-Z0-9_]*)\s*\)",
                       src, re.MULTILINE)
    if not atoms:
        raise RuntimeError("code pin not found: cs_verdict/2 clause heads")
    return sorted(set(atoms))


def code_cs_obstruction_statuses():
    src = _strip_comments(_read(KERNEL_REGISTRY))
    atoms = re.findall(r"Status\s*=\s*([a-z][a-zA-Z0-9_]*)", src)
    if not atoms:
        raise RuntimeError("code pin not found: Status = <atom> in cs_kernel_registry")
    return sorted(set(atoms))


def code_cs_trifurcation_types():
    src = _strip_comments(_read(TRIFURCATION))
    heads = re.findall(r"trif_dispatch\(\s*[^,]+,\s*[a-z][a-zA-Z0-9_]*\s*,"
                       r"\s*([a-zA-Z_][a-zA-Z0-9_]*)", src)
    literals = [h for h in heads if re.fullmatch(r"[a-z][a-zA-Z0-9_]*", h)]
    literals += re.findall(r"Type\s*=\s*([a-z][a-zA-Z0-9_]*)", src)
    if not literals:
        raise RuntimeError("code pin not found: trif_dispatch/4 type literals")
    return sorted(set(literals))


def code_cs_attractor_table():
    """Normalize cs_terminal_attractor/4 clause heads (+ single \\= guard) to rows."""
    src = _strip_comments(_read(DRIFT_ENGINE))
    # A clause is head(...) optionally followed by `:- Guard` up to the final period.
    clause_re = re.compile(
        r"^cs_terminal_attractor\(\s*([^,]+?)\s*,\s*([^,]+?)\s*,\s*([^,]+?)\s*,"
        r"\s*([^)]+?)\s*\)\s*(\.|:-\s*(.*?)\.)", re.MULTILINE | re.DOTALL)
    rows = []
    for m in clause_re.finditer(src):
        d, mag, ack, term = m.group(1), m.group(2), m.group(3), m.group(4)
        guards = {}
        if m.group(6):
            for gv, ga in re.findall(r"([A-Z][a-zA-Z0-9_]*)\s*\\=\s*([a-z][a-zA-Z0-9_]*)",
                                     m.group(6)):
                guards[gv] = ga
        def norm(arg):
            if re.fullmatch(r"[a-z][a-zA-Z0-9_]*", arg):
                return arg
            if arg in guards:
                return f"*!={guards[arg]}"
            return "*"
        if not re.fullmatch(r"[a-z][a-zA-Z0-9_]*", term):
            raise RuntimeError(f"non-atom terminal in attractor clause: {term}")
        rows.append(f"{norm(d)} | {norm(mag)} | {norm(ack)} -> {term}")
    if not rows:
        raise RuntimeError("code pin not found: cs_terminal_attractor/4 clauses")
    return sorted(rows)


MANIFEST = {
    "cs_terminals": code_cs_terminals,
    "cs_gap_directions": code_cs_gap_directions,
    "cs_gap_magnitudes": code_cs_gap_magnitudes,
    "cs_patterns": code_cs_patterns,
    "cs_verdicts": code_cs_verdicts,
    "cs_obstruction_statuses": code_cs_obstruction_statuses,
    "cs_trifurcation_types": code_cs_trifurcation_types,
    "cs_attractor_table": code_cs_attractor_table,
}


# --- spec-side extraction ---------------------------------------------------

BLOCK_RE = re.compile(
    r"<!--\s*spec-enum:\s*([A-Za-z0-9_]+)\s*-->\s*```[^\n]*\n(.*?)```\s*"
    r"<!--\s*/spec-enum\s*-->", re.DOTALL)


def spec_blocks(text):
    """name -> sorted list of non-empty stripped lines inside the fenced block."""
    blocks = {}
    for name, body in BLOCK_RE.findall(text):
        lines = [ln.strip() for ln in body.splitlines()]
        blocks[name] = sorted(ln for ln in lines if ln)
    return blocks


def run_check(spec_text):
    """Return list of error strings (empty = green)."""
    errors = []
    blocks = spec_blocks(spec_text)
    for name, extractor in MANIFEST.items():
        if name not in blocks:
            errors.append(f"MISSING SENTINEL: spec-enum block '{name}' absent from spec")
            continue
        code_side = extractor()
        spec_side = blocks[name]
        missing = [a for a in code_side if a not in spec_side]
        extra = [a for a in spec_side if a not in code_side]
        if missing or extra:
            detail = []
            if missing:
                detail.append(f"in code, not in spec: {missing}")
            if extra:
                detail.append(f"in spec, not in code: {extra}")
            errors.append(f"DIVERGENT: '{name}' — " + "; ".join(detail))
    for name in blocks:
        if name not in MANIFEST:
            errors.append(f"UNKNOWN ENUM: spec-enum block '{name}' not in checker manifest")
    return errors


# --- selftest: positive controls on mutated copies of the real spec ---------

def selftest(spec_text):
    """Three failure shapes, each asserted RED on a mutated copy. Returns errors."""
    failures = []
    target = "cs_terminals"

    def mutate_block(text, fn):
        m = re.search(r"(<!--\s*spec-enum:\s*" + target + r"\s*-->\s*```[^\n]*\n)(.*?)(```)",
                      text, re.DOTALL)
        if not m:
            raise RuntimeError(f"selftest: block '{target}' not found in spec")
        return text[:m.start(2)] + fn(m.group(2)) + text[m.end(2):]

    # (1) add-atom
    mutated = mutate_block(spec_text, lambda body: body + "sealed_closure\n")
    if not any("DIVERGENT" in e and target in e for e in run_check(mutated)):
        failures.append("selftest FAILED: add-atom mutation did not turn the check red")

    # (2) remove-atom
    mutated = mutate_block(
        spec_text,
        lambda body: "\n".join(body.splitlines()[1:]) + "\n")
    if not any("DIVERGENT" in e and target in e for e in run_check(mutated)):
        failures.append("selftest FAILED: remove-atom mutation did not turn the check red")

    # (3) deleted-sentinel (the likeliest real failure: a rewritten section drops
    # its block entirely — must go RED on absence, not green on nothing-to-diff)
    mutated = re.sub(r"<!--\s*spec-enum:\s*" + target + r"\s*-->.*?<!--\s*/spec-enum\s*-->",
                     "", spec_text, flags=re.DOTALL)
    if not any("MISSING SENTINEL" in e and target in e for e in run_check(mutated)):
        failures.append("selftest FAILED: deleted-sentinel mutation did not turn the check red")

    return failures


def main(argv):
    if "--list" in argv:
        for name, extractor in MANIFEST.items():
            print(f"[{name}]")
            for item in extractor():
                print(f"  {item}")
        return 0

    if not SPEC.exists():
        print(f"spec_enum_check: RED — spec not found: {SPEC}")
        return 1
    spec_text = _read(SPEC)

    # Positive controls ride every run (an unwitnessed checker is a claim).
    # If the REAL spec already lacks the selftest's target block, the mutate
    # helper cannot run — skip to the main check, which reports the missing
    # sentinel cleanly (the selftest is inapplicable, not passed).
    try:
        st = selftest(spec_text)
    except RuntimeError as e:
        print(f"  selftest inapplicable ({e}); falling through to main check")
        st = []
    if st:
        for f in st:
            print(f"  {f}")
        print("spec_enum_check: RED (selftest)")
        return 1

    errors = run_check(spec_text)
    if errors:
        for e in errors:
            print(f"  {e}")
        print(f"spec_enum_check: RED — {len(errors)} enum(s) divergent/missing")
        return 1
    print(f"spec_enum_check: GREEN — {len(MANIFEST)} enums in sync (selftest 3/3 red-capable)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
