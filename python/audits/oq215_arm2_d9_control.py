#!/usr/bin/env python3
"""OQ-215 arm 2 — D9 discrimination control (evaluation-only, no rewrites).

Drives stages 9 and 10 directly over two finished stories, both under the
SAME world-independent contract (symmetric arms — world-specific contract
text must not confound the discrimination):

  A. "assessment"  — the pre-rewrite invariant-dropped story
     (stories/the-empty-pan_rev1.md, run the_empty_pan_1783474314)
     PRE-REGISTERED EXPECTATION: falsifier LOST -> ROUTE: STRATEGY;
     D9 <= 2 -> Cannot-PUBLISH override. This is the NEGATIVE control:
     a battery that only ever passes things has not been shown to
     discriminate.
  B. "fortyhertz"  — the post-rewrite counting-defect story whose
     invariant nevertheless survived
     (stories/the_empty_pan_rev1.md, run the_empty_pan_1783821245)
     PRE-REGISTERED EXPECTATION: falsifier HOLDS; D9 >= 3, no override.

Stories are fed manifest-stripped (blind stage 9 must not see edit
history) and trailer-stripped (the save trailer names the source title).

Stage 10 runs in CRAFT mode (no stage_1_anon, no stage_6) — D9 is
governed by contract availability, not mode, per stage10.md.

Writes raw outputs + summary into audits/2026-07-12_oq215_arm2_d9_control/.
"""
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "agent"))

import uke_narrative_orchestrator as uko  # noqa: E402

AUDIT_DIR = REPO / "audits" / "2026-07-12_oq215_arm2_d9_control"
CONTRACT = (AUDIT_DIR / "contract_world_independent.md").read_text(encoding="utf-8")

CASES = {
    "assessment": REPO / "agent/narrative_transform/stories/the-empty-pan_rev1.md",
    "fortyhertz": REPO / "agent/narrative_transform/stories/the_empty_pan_rev1.md",
}
EXPECT = {
    "assessment": "falsifier LOST, ROUTE: STRATEGY, D9 <= 2 (override)",
    "fortyhertz": "falsifier HOLDS, D9 >= 3, no override",
}


def clean_story(text: str) -> str:
    """Strip edit manifest (blindness) and the source-naming save trailer."""
    story, _ = uko._split_edit_manifest(text)
    lines = story.rstrip().splitlines()
    while lines and (
        lines[-1].strip().startswith("*Original:")
        or lines[-1].strip() in ("---", "")
    ):
        lines.pop()
    return "\n".join(lines) + "\n"


def extract_d9(stage_10_text: str) -> str:
    m = re.search(r'^.*D9[^\n]*$', stage_10_text, flags=re.MULTILINE)
    return m.group(0).strip() if m else "D9 LINE NOT FOUND"


def main() -> int:
    summary = []
    for label, path in CASES.items():
        outdir = AUDIT_DIR / f"run_{label}"
        outdir.mkdir(parents=True, exist_ok=True)
        orch = uko.UKEOrchestrator(
            mode="narrative", skip_engine=True, output_dir=outdir)

        story = clean_story(path.read_text(encoding="utf-8"))
        assert "EDIT MANIFEST" not in story, f"{label}: manifest not stripped"
        assert "*Original:" not in story, f"{label}: trailer not stripped"
        (outdir / "story_under_test.md").write_text(story, encoding="utf-8")

        outputs = {"stage_8": story, "invariant_contract": CONTRACT}
        for stage in ("stage_9", "stage_10"):
            step = orch._run_stage_generic(stage, outputs, "")
            if step.status != "success":
                print(f"FATAL {label} {stage}: {step.error}")
                return 1
            outputs[stage] = step.data
            (outdir / f"{stage}_output.md").write_text(
                step.data, encoding="utf-8")

        route = uko.UKEOrchestrator._parse_review_route(
            uko.StepResult(step="stage_9", status="success",
                           data=outputs["stage_9"]))
        d9 = extract_d9(outputs["stage_10"])
        overrides = "OVERRIDE" if re.search(
            r'D9[^\n]*(?:override|Cannot PUBLISH)', outputs["stage_10"],
            flags=re.IGNORECASE) else ""
        summary.append(
            f"{label}: ROUTE={route} | {d9} {overrides}\n"
            f"  expected: {EXPECT[label]}")

    report = "\n".join(summary) + "\n"
    (AUDIT_DIR / "SUMMARY.txt").write_text(report, encoding="utf-8")
    print(report)
    return 0


if __name__ == "__main__":
    sys.exit(main())
