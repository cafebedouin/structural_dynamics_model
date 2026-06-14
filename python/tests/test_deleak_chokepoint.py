"""
De-leak chokepoint guard — threshold-coupled lint never reaches an authoring prompt.

Witness for: ISSUES.md OQ-116 (MOUNTAIN_METRIC_CONFLICT resolution) and OQ-127
(SCAFFOLD_DANGER_ZONE refile). Part D of the OQ-116 close plan.

Why this test is non-circular: the first-draft guard only confirmed "whatever is in
THRESHOLD_COUPLED_LINT gets removed" — circular. This test drives REAL rule output:
it constructs stories that actually FIRE MOUNTAIN_METRIC_CONFLICT and
SCAFFOLD_DANGER_ZONE through lint_file, asserts the literal codes are PRESENT in the
lint, then passes that lint through the production prompt builder
(regenerate_stories.build_user_prompt — the assembled interface, not the strip helper
in isolation) and asserts the codes are ABSENT. A non-coupled code (MISSING_MODULE) is
the negative control: it must SURVIVE into the prompt, proving the strip is selective,
not a blanket suppressor.

The census check (test_lint_to_prompt_census) is itself an empty-result claim, so it
carries its own positive control: it plants a temp module that joins the
{lint_file ∧ build_prompt} set, confirms the census flags the newcomer, then removes it
(build_discipline: every diagnostic needs a positive control).
"""

import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))

from linter import lint_file, build_author_feedback, THRESHOLD_COUPLED_LINT  # noqa: E402
import regenerate_stories  # noqa: E402

TESTSETS_DIR = ROOT / "prolog" / "testsets"

# Fixtures live inside prolog/testsets/ so lint_file can resolve config.pl via
# dirname(dirname(filepath)) (its documented path-resolution contract).
_MMC_FIXTURE = """:- module(tmp_deleak_mmc, []).
narrative_ontology:interval(c, t0, t1).
constraint_classification(c, rope, context(low, short, open, local)).
constraint_classification(c, snare, context(high, long, closed, national)).
constraint_claim(c, mountain).
base_extractiveness(c, 0.68).
"""

_SDZ_FIXTURE = """:- module(tmp_deleak_sdz, []).
narrative_ontology:interval(c, t0, t1).
constraint_classification(c, rope, context(low, short, open, local)).
constraint_classification(c, snare, context(high, long, closed, national)).
base_extractiveness(c, 0.20).
constraint_beneficiary(c, some_agent).
"""

# No `:- module(...)` line => MISSING_MODULE (a NON-coupled code) fires.
_MISSING_MODULE_FIXTURE = """narrative_ontology:interval(c, t0, t1).
constraint_classification(c, rope, context(low, short, open, local)).
base_extractiveness(c, 0.20).
"""


def _lint_fixture(name, content):
    tmp = TESTSETS_DIR / f".tmp_deleak_{name}.pl"
    tmp.write_text(content, encoding="utf-8")
    try:
        return lint_file(str(tmp))
    finally:
        tmp.unlink(missing_ok=True)


def _code_in(code, errors):
    return any(e.startswith(code) for e in errors)


def test_mmc_present_in_lint_absent_in_prompt():
    lint = _lint_fixture("mmc", _MMC_FIXTURE)
    assert _code_in("MOUNTAIN_METRIC_CONFLICT", lint), \
        f"fixture failed to fire MOUNTAIN_METRIC_CONFLICT; lint={lint}"
    prompt = regenerate_stories.build_user_prompt("tmp.pl", "orig", lint)
    assert "MOUNTAIN_METRIC_CONFLICT" not in prompt, \
        "MOUNTAIN_METRIC_CONFLICT leaked into the built author prompt"


def test_sdz_present_in_lint_absent_in_prompt():
    lint = _lint_fixture("sdz", _SDZ_FIXTURE)
    assert _code_in("SCAFFOLD_DANGER_ZONE", lint), \
        f"fixture failed to fire SCAFFOLD_DANGER_ZONE; lint={lint}"
    prompt = regenerate_stories.build_user_prompt("tmp.pl", "orig", lint)
    assert "SCAFFOLD_DANGER_ZONE" not in prompt, \
        "SCAFFOLD_DANGER_ZONE leaked into the built author prompt"


def test_noncoupled_code_survives_into_prompt():
    """Negative control: the strip is selective, not a blanket suppressor."""
    lint = _lint_fixture("missing_module", _MISSING_MODULE_FIXTURE)
    assert _code_in("MISSING_MODULE", lint), \
        f"fixture failed to fire MISSING_MODULE; lint={lint}"
    assert not any(
        e.startswith(c) for c in THRESHOLD_COUPLED_LINT for e in [*lint]), \
        "negative-control fixture unexpectedly fired a threshold-coupled code"
    prompt = regenerate_stories.build_user_prompt("tmp.pl", "orig", lint)
    assert "MISSING_MODULE" in prompt, \
        "MISSING_MODULE (non-coupled) was wrongly stripped from the prompt"


# --- Census tripwire: who builds an authoring prompt AND touches lint ----------
# Recorded allowlist of files that call lint_file AND construct a generation
# prompt, each with its audited status (OQ-116 Part C, 2026-06-14):
#   regenerate_stories.py  — LIVE lint->prompt path; routes through build_author_feedback.
#   cohort_zero_regen.py   — latent (feeds validate_json errors, not lint); hardened anyway.
#   story_generator_base.py— latent (lint is print-only; retry feedback = validate_json).
#   perspective_experiment.py — latent (lint stored in result, not fed to prompt).
# A NEW entrant to this set must be triaged: if it feeds lint into a prompt it MUST
# route through build_author_feedback (de-leak); if not, add it here with its status.
_LINT_AND_PROMPT_ALLOWLIST = {
    "regenerate_stories.py",
    "cohort_zero_regen.py",
    "story_generator_base.py",
    "perspective_experiment.py",
}

_SCAN_DIRS = ["python", "agent"]
_PROMPT_TOKENS = ("build_prompt(", "build_user_prompt(")


def _census_lint_and_prompt_files():
    found = set()
    for d in _SCAN_DIRS:
        base = ROOT / d
        for path in base.rglob("*.py"):
            lowered = str(path).lower()
            if "archive" in lowered or os.sep + "tests" + os.sep in lowered:
                continue
            text = path.read_text(encoding="utf-8", errors="replace")
            if "lint_file(" in text and any(tok in text for tok in _PROMPT_TOKENS):
                found.add(path.name)
    return found


def test_lint_to_prompt_census_matches_allowlist():
    found = _census_lint_and_prompt_files()
    new = found - _LINT_AND_PROMPT_ALLOWLIST
    assert not new, (
        f"New module(s) call lint_file AND build a generation prompt: {sorted(new)}. "
        "Triage each: if it feeds lint into the prompt it MUST route through "
        "linter.build_author_feedback (de-leak / OQ-116); then add it to "
        "_LINT_AND_PROMPT_ALLOWLIST with its status.")

    # Positive control: plant a violator in a scanned dir, confirm the census
    # flags it, then remove it. Without this the census could be byte-identical
    # to one that never looks.
    plant = ROOT / "agent" / "_tmp_deleak_census_plant.py"
    plant.write_text(
        "from linter import lint_file\n"
        "def go():\n"
        "    errs = lint_file('x.pl')\n"
        "    return build_prompt('desc' + str(errs))\n",
        encoding="utf-8")
    try:
        flagged = _census_lint_and_prompt_files()
        assert "_tmp_deleak_census_plant.py" in flagged, \
            "census positive control FAILED — planted lint->prompt file not detected"
    finally:
        plant.unlink(missing_ok=True)


if __name__ == "__main__":
    failures = 0
    for name, fn in sorted(globals().items()):
        if name.startswith("test_") and callable(fn):
            try:
                fn()
                print(f"PASS {name}")
            except AssertionError as e:
                failures += 1
                print(f"FAIL {name}: {e}")
    sys.exit(1 if failures else 0)
