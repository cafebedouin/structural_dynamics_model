#!/usr/bin/env python3
"""controls.py — positive/negative controls for audit_citation_status.py (OQ-104).

One control per grammar class (the plan's mandatory-before-done set). Each asserts the
EXPECTED (status, normalized) for a fixture token; a checker byte-identical because it
"stopped looking" fails these. Run: python3 controls.py  (exit 0 = all pass).

Idempotence and rot-sensitivity are whole-run controls; see controls_run.sh.
"""
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "python"))
import audit_citation_status as A  # noqa: E402

HEADS = A.repo_top_heads()
FILES, DIRS = A.tracked_sets()
AUDIT_DIR = "audits/2026-06-18_oq104_citation_checker"

# Control-4 fixture: an existing-but-UNTRACKED file in this audit dir, created here so
# the control is reproducible on a fresh clone (never git-add evidence/ — it must stay
# untracked for the writeup-relative-untracked control to be meaningful).
_fixture = REPO / AUDIT_DIR / "evidence" / "summary.json"
_fixture.parent.mkdir(parents=True, exist_ok=True)
if not _fixture.exists():
    _fixture.write_text('{"control":"writeup-relative untracked fixture"}\n')


def classify(token, writeup_dir=AUDIT_DIR):
    return A.classify(token, writeup_dir, HEADS, FILES, DIRS)


def expand_classify(raw, writeup_dir=AUDIT_DIR):
    """Mirror the scan() loop: brace-expand then classify each member."""
    return [classify(t, writeup_dir) for t in A.expand_braces(raw)]


CONTROLS = []


def check(name, got, want):
    ok = got == want
    CONTROLS.append((name, ok, got, want))


# 1. en-dash anchor strips and SURVIVES to the tracked file (positive control).
check("en-dash anchor -> tracked",
      classify("prolog/arakelov_height.pl:100–111"),
      (A.PASS_TRACKED, "prolog/arakelov_height.pl"))
# 1b. ASCII-hyphen and comma-list anchors also strip.
check("hyphen anchor -> tracked",
      classify("prolog/arakelov_height.pl:100-111"),
      (A.PASS_TRACKED, "prolog/arakelov_height.pl"))
check("comma line-list anchor -> tracked",
      classify("python/run_pipeline.py:171,207"),
      (A.PASS_TRACKED, "python/run_pipeline.py"))
check("symbol anchor -> tracked",
      classify("python/generate_constraint_pl.py:_grid_alignment_errors"),
      (A.PASS_TRACKED, "python/generate_constraint_pl.py"))

# 2. glob-star -> grammar-ambiguous (pattern, not a hard per-file citation); no crash.
check("glob-star -> grammar-ambiguous",
      classify("testsets/*.pl"),
      (A.AMBIG, "testsets/*.pl"))

# 3. brace EXPANDS to two paths, each checked (false-negative guard). arakelov.pl is
#    tracked; arakelov.json is not -> the two members get DIFFERENT verdicts.
brace = expand_classify("prolog/arakelov_height.{pl,json}")
check("brace expands to 2 members", len(brace), 2)
check("brace member .pl -> tracked", brace[0], (A.PASS_TRACKED, "prolog/arakelov_height.pl"))
check("brace member .json -> not pass",
      brace[1][0] in (A.UNTRACKED_FROZEN, A.UNTRACKED_REGEN, A.MISSING), True)

# 4. DISCRIMINATOR-ISOLATION MATCHED PAIR (the redesign's core claim: startswith("outputs/")
#    decides). Hold fixture content CONSTANT, vary only the location — so the test reads
#    "the prefix is the deciding variable," not "two unrelated fixtures both pass."
#    frozen arm: writeup-relative evidence/summary.json inside an audit dir (NOT under
#    outputs/) -> untracked-frozen-evidence -> GATING (gate RED on recurrence).
check("matched-pair FROZEN arm -> untracked-frozen-evidence (gating)",
      classify("evidence/summary.json"),
      (A.UNTRACKED_FROZEN, f"{AUDIT_DIR}/evidence/summary.json"))
#    regenerable arm: IDENTICAL content placed under top-level outputs/, cited untracked ->
#    untracked-regenerable -> non-gating (gate GREEN).
_regen_fixture = REPO / "outputs" / "_oq104_control_fixture.json"
_regen_fixture.parent.mkdir(parents=True, exist_ok=True)
if not _regen_fixture.exists():
    _regen_fixture.write_text('{"control":"writeup-relative untracked fixture"}\n')
check("matched-pair REGENERABLE arm -> untracked-regenerable (non-gating)",
      classify("outputs/_oq104_control_fixture.json"),
      (A.UNTRACKED_REGEN, "outputs/_oq104_control_fixture.json"))
# 4a. post-normalization (5a): a dotted './outputs/...' spelling collapses to 'outputs/...'
#     BEFORE the prefix test, so it must land regenerable, not a spurious frozen RED.
check("dotted ./outputs/ collapses -> untracked-regenerable",
      classify("./outputs/_oq104_control_fixture.json"),
      (A.UNTRACKED_REGEN, "outputs/_oq104_control_fixture.json"))

# 5. home-dir -> allowlist (by-design external), NOT missing-flagged.
check("home-dir -> allowlist",
      classify("~/.claude/plans/x.md"),
      (A.PASS_ALLOWLIST, "~/.claude/plans/x.md"))

# 6. boundary: real repo paths survive; arity / field-list debris drops.
check("tracked testset survives",
      classify("prolog/validation_suite.pl"),
      (A.PASS_TRACKED, "prolog/validation_suite.pl"))
check("arity drops (None)", classify("assess_scaffold_need/2-3"), None)
check("mod:pred/N arity drops (None)", classify("diagnostic_summary:verdict_join/3"), None)
check("field-list drops (None)", classify("accessibility_collapse/stakes_inflation"), None)
check("bracket template drops (None)", classify("prolog/[core]"), None)
check("whitespace command -> grammar-ambiguous",
      classify("git log -1 -- prolog/config.pl")[0], A.AMBIG)
check("leading ../ -> grammar-ambiguous",
      classify("../outputs/pipeline_output.json")[0], A.AMBIG)

# 7. absolute-inside-repo normalizes to repo-relative and is FLAGGED (not pass). Because
#    it normalizes UNDER outputs/, it lands untracked-regenerable — proving the prefix test
#    runs POST-normalization (the abs '<REPO>/outputs/...' spelling reaches it as 'outputs/...').
abs_in = classify(f"{REPO}/outputs/pipeline_output.json")
check("abs-in-repo normalizes to outputs/pipeline_output.json",
      abs_in[1], "outputs/pipeline_output.json")
check("abs-in-repo is flagged regenerable (post-normalization, not pass)",
      abs_in[0], A.UNTRACKED_REGEN)
# 7b. absolute OUTSIDE repo -> allowlist.
check("abs-outside-repo -> allowlist",
      classify("/etc/passwd")[0], A.PASS_ALLOWLIST)
check("/tmp -> allowlist", classify("/tmp/recon.py")[0], A.PASS_ALLOWLIST)

# 8. rot-sensitivity (in-memory twin of controls_run.sh): hold existence constant, drop
#    the file from the tracked set -> a previously-PASS cited file must FLIP to flagged.
#    A checker that "stopped looking" would still return pass here. rot_file is NOT under
#    outputs/, so the flip lands in the GATING frozen-evidence class (pass -> RED, not -> WARN).
rot_file = "prolog/arakelov_height.pl"
before = A.classify(rot_file, AUDIT_DIR, HEADS, FILES, DIRS)
files_minus = set(FILES); files_minus.discard(rot_file)
dirs_minus = set(DIRS)  # parent dirs stay; only the exact file untracked
after = A.classify(rot_file, AUDIT_DIR, HEADS, files_minus, dirs_minus)
check("rot: tracked cited file PASSES", before, (A.PASS_TRACKED, rot_file))
check("rot: same file untracked-on-disk FLIPS to untracked-frozen-evidence (gating)",
      after, (A.UNTRACKED_FROZEN, rot_file))


def main():
    width = max(len(n) for n, *_ in CONTROLS)
    failed = 0
    for name, ok, got, want in CONTROLS:
        tag = "PASS" if ok else "FAIL"
        if not ok:
            failed += 1
        line = f"[{tag}] {name.ljust(width)}"
        if not ok:
            line += f"   got={got!r} want={want!r}"
        print(line)
    print(f"--- {len(CONTROLS) - failed}/{len(CONTROLS)} controls passed")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
