#!/usr/bin/env python3
"""Gate-context drift guard for agent/narrative_transform/logic_symbolic.md §IV.

WHY THIS EXISTS
---------------
`logic_symbolic.md §IV` is a hand-mirror of the classification gate thresholds that
live authoritatively in `prolog/config.pl` (`param/2` facts), consumed by the
narrative pipeline at Stage 0 (per-character classification) and Stage 1
(formalization). A hand-copied threshold table with no checked link to its source is
build-discipline Pattern 2 (the silent fork). This guard is the checked link.

WHAT IT CHECKS (value-in-gate-context, not mere presence)
---------------------------------------------------------
1. DERIVED CHECKLIST (not a hand-list): it parses the `config:param(Name, _)` calls
   that `classify_from_metrics/6` actually references in `drl_core.pl`, reads their
   values from `config.pl`, and requires each numeric value to appear on a §IV line
   that ALSO names the param (the `(param_name)` annotation is the gate context).
   Adding a new inline gate param to `classify_from_metrics/6` auto-extends coverage;
   if the doc has not been updated to annotate it, this guard fails loudly.

2. A right-number-on-wrong-gate edit (e.g. "Snare χ ≤ 0.45") fails: 0.45 is a real
   value elsewhere so a bare presence-grep stays green, but 0.45 is not the value on
   the `snare_chi_floor` line, so this guard goes RED.

STATED SCOPE — what this guard CANNOT check, by design not blind spot
---------------------------------------------------------------------
* STRUCTURAL EXCLUSION/PREDICATE GATES carry no numeric threshold and are therefore
  numeric-unguardable here: `snare_immutability_check`, `scaffold_temporality_check`,
  `requires_active_enforcement`, `natural_law_without_beneficiary`,
  `constraint_captured`, `coordination_dead`. Their bodies were read (this session):
  each is a flag / stakeholder / immutability check with NO `config:param` numeric
  threshold one level down, so inline-param derivation is COMPLETE. If a future
  refactor pushes a numeric threshold into one of these predicates, the derivation
  must recurse one level — this file must be updated then (search: RECURSE-IF-REFACTORED).
* THE SCAFFOLD THEATER CEILING is a HARDCODED LITERAL `TR > 0.70` in
  `drl_core.pl` (the scaffold clause), NOT a `config:param`. It cannot be read from
  `config.pl`, so it is outside the derived checklist. It is annotated as a literal in
  §IV. If it is ever promoted to a param, it joins the derived set automatically.

This guard is NOT wired into any pipeline gate (operator say-so required).
Run manually:  python3 python/check_logic_symbolic_drift.py [--verbose]
Exit 0 = mirror consistent; exit 1 = drift (details printed).
"""

import math
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CONFIG_PL = REPO / "prolog" / "config.pl"
DRL_CORE_PL = REPO / "prolog" / "drl_core.pl"
LOGIC_MD = REPO / "agent" / "narrative_transform" / "logic_symbolic.md"

# Structural / non-numeric predicates the derived set is expected to name but which
# carry no config:param numeric threshold. Documented here so the boundary is a stated
# fact (see module docstring). Not consulted for pass/fail — informational only.
STRUCTURAL_GATES = [
    "snare_immutability_check",
    "scaffold_temporality_check",
    "requires_active_enforcement",
    "natural_law_without_beneficiary",
    "constraint_captured",
    "coordination_dead",
]


def extract_classify_span(drl_core_text: str) -> str:
    """Return the source span of the classify_from_metrics/6 clauses.

    From the first `classify_from_metrics(` head to the terminal unknown clause.
    Fails loudly if the anchors are not found (the file was restructured).
    """
    start = drl_core_text.find("classify_from_metrics(")
    if start == -1:
        raise SystemExit("FATAL: classify_from_metrics/6 not found in drl_core.pl")
    # Terminal clause is the catch-all unknown; anchor on it so we include every clause.
    end_anchor = "classify_from_metrics(_C, _BaseEps, _Chi, _Supp, _Context, unknown)."
    end = drl_core_text.find(end_anchor, start)
    if end == -1:
        raise SystemExit(
            "FATAL: terminal classify_from_metrics unknown clause not found — "
            "the clause set was restructured; re-anchor this guard."
        )
    return drl_core_text[start : end + len(end_anchor)]


def referenced_params(span: str) -> list[str]:
    """The config:param names referenced inside classify_from_metrics/6 (deduped, ordered)."""
    names = re.findall(r"config:param\(\s*([a-z_]+)\s*,", span)
    seen = []
    for n in names:
        if n not in seen:
            seen.append(n)
    return seen


def config_value(config_text: str, name: str):
    """Return the raw numeric token for param(name, Value) in config.pl, or None if non-numeric/absent.

    Returns a tuple (float_value, raw_token) for numeric params; None otherwise.
    """
    m = re.search(rf"^param\(\s*{re.escape(name)}\s*,\s*([^)]+?)\s*\)\.", config_text, re.MULTILINE)
    if not m:
        return None
    raw = m.group(1).strip()
    try:
        return (float(raw), raw)
    except ValueError:
        return None  # non-numeric (e.g. theater_metric_name -> theater_ratio)


def line_numbers_on_annotated_lines(md_lines, name, value):
    """Lines that name `name` AND carry a numeric token float-close to `value`.

    Returns (annotated_line_idxs, matched). annotated_line_idxs = lines naming the
    param; matched = subset where the value is also co-located (the gate-context check).
    """
    annotated = [i for i, ln in enumerate(md_lines) if name in ln]
    matched = []
    for i in annotated:
        toks = re.findall(r"\d+\.\d+|\d+", md_lines[i])
        for t in toks:
            try:
                if math.isclose(float(t), value, rel_tol=0, abs_tol=1e-9):
                    matched.append(i)
                    break
            except ValueError:
                continue
    return annotated, matched


def main() -> int:
    verbose = "--verbose" in sys.argv

    config_text = CONFIG_PL.read_text(encoding="utf-8")
    drl_core_text = DRL_CORE_PL.read_text(encoding="utf-8")
    md_text = LOGIC_MD.read_text(encoding="utf-8")
    md_lines = md_text.splitlines()

    span = extract_classify_span(drl_core_text)
    params = referenced_params(span)

    numeric = []      # (name, float_value, raw_token)
    non_numeric = []  # names skipped (e.g. theater_metric_name)
    for name in params:
        cv = config_value(config_text, name)
        if cv is None:
            non_numeric.append(name)
        else:
            fval, raw = cv
            numeric.append((name, fval, raw))

    failures = []
    passes = []
    for name, fval, raw in numeric:
        annotated, matched = line_numbers_on_annotated_lines(md_lines, name, fval)
        if not annotated:
            failures.append(
                f"  MISSING ANNOTATION: `{name}` (config value {raw}) is referenced by "
                f"classify_from_metrics/6 but appears NOWHERE in {LOGIC_MD.name}. "
                f"Annotate it on its §IV gate line."
            )
        elif not matched:
            found_vals = sorted(
                {t for i in annotated for t in re.findall(r"\d+\.\d+|\d+", md_lines[i])}
            )
            failures.append(
                f"  WRONG-GATE / DRIFT: `{name}` = {raw} in config.pl, but the §IV "
                f"line(s) naming `{name}` carry {found_vals} (lines "
                f"{[i + 1 for i in annotated]}), not {raw}."
            )
        else:
            passes.append((name, raw, [i + 1 for i in matched]))

    # ---- report ----
    print(f"logic_symbolic.md §IV drift guard  ({LOGIC_MD.relative_to(REPO)})")
    print(f"  source of truth : {CONFIG_PL.relative_to(REPO)} (verified vs "
          f"{DRL_CORE_PL.relative_to(REPO)}:classify_from_metrics/6)")
    print(f"  derived checklist: {len(numeric)} numeric gate params referenced by "
          f"classify_from_metrics/6")
    if verbose:
        for name, raw, lns in passes:
            print(f"    PASS  {name} = {raw}  (§IV line {lns})")
    if non_numeric:
        print(f"  non-numeric params skipped: {', '.join(non_numeric)}")
    print("  stated scope (numeric-unguardable BY DESIGN):")
    print(f"    - structural gates (no config:param threshold): {', '.join(STRUCTURAL_GATES)}")
    print("    - scaffold theater ceiling: HARDCODED literal `TR > 0.70` in drl_core.pl "
          "(not a config:param)")

    if failures:
        print(f"\n  RESULT: RED — {len(failures)} drift(s):")
        for f in failures:
            print(f)
        return 1

    print(f"\n  RESULT: GREEN — all {len(numeric)} numeric gate values mirror config.pl "
          "in their §IV gate context.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
