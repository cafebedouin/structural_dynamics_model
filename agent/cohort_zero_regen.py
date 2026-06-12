#!/usr/bin/env python3
"""Cohort-zero regen driver (OQ-109 Phase C, cohort ruling 2026-06-12).

Seeds: prolog/archives/datasets/kernel_v2_test2/json/ (60 stories; renamed from pre_cohort_zero_2026-06-12 per operator ruling).
Each archived story is re-authored FROM SCRATCH under the live prompt/schema/example,
conditioned ONLY on the DECLARED SEED SPEC (SIGMA_SEAT_PREDICTION.md — the prediction
commit holds this driver to it; deviation = halt):
  (1) topic/title material (human_readable, topic_domain),
  (2) a one-paragraph situation summary from the archived commentary.narrative_context,
  (3) NOTHING else — no metrics, types, stakeholder lists, six-questions content, omegas.

Identity is authored FORWARD (plumbing, not recovered backward): the driver OVERRIDES the
model's header.constraint_id with the deterministic cohort id ({seed}_c0, replicate extras
{seed}_c0_d{k}) and stamps provenance (incl. seeded_from + draw), exactly like the other
mechanical stamps. Each draw is one sample from the generation distribution.

REPLICATE_SET stories get 3 draws (the OQ-109 replicate probe); draw 1 enters the corpus
staging dir, draws 2-3 go to the probe dir only. Outputs are STAGED
(json_cohort0/ + testsets_cohort0/ + audits/2026-06-12_cohort_zero/replicates/); the
corpus swap is a separate witnessed commit.

Usage:
  python3 agent/cohort_zero_regen.py --dry-run            # assemble 2 prompts, est. cost, no API
  python3 agent/cohort_zero_regen.py                      # full run
  python3 agent/cohort_zero_regen.py --only id1,id2       # subset
"""
import argparse
import json
import os
import subprocess
import sys
from datetime import date
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT / "agent"))
sys.path.insert(0, str(REPO_ROOT / "python"))

from story_generator_base import (  # noqa: E402
    build_prompt, process_response, _SYSTEM_INSTRUCTION, EXAMPLE_PATH,
)
import llm_call  # noqa: E402
from generate_constraint_pl import validate_json, generate_pl  # noqa: E402
from linter import lint_file  # noqa: E402

ARCHIVE = REPO_ROOT / "prolog/archives/datasets/kernel_v2_test2/json"
STAGE_JSON = REPO_ROOT / "json_cohort0"
STAGE_PL = REPO_ROOT / "testsets_cohort0"
REPLICATE_DIR = REPO_ROOT / "audits/2026-06-12_cohort_zero/replicates"

MODEL = "claude-sonnet-4-5-20250929"   # the architect model (c-orchestrator MODELS)
TEMPERATURE = float(os.environ.get("DR_TEMPERATURE", "0.2"))
REPLICATE_SET = {
    "scale_ceiling",                          # NL-profile / old-guard-protected
    "adjunctification_of_university_teaching",  # extractive, R5 zombie-flagged
    "organization_floor",                     # OQ-114 named-pair member
    "garment_supplychain_audit_theater",      # supp-driven residual host
}


def _git_commit_of(path):
    return subprocess.run(["git", "log", "-1", "--format=%H", "--", path],
                          capture_output=True, text=True, cwd=REPO_ROOT).stdout.strip()


def seed_material(archived: dict) -> tuple[str, str, str]:
    """Extract EXACTLY the declared seed spec. Guard: touches only the three fields."""
    bp = archived.get("base_properties", {})
    title = bp.get("human_readable", "")
    domain = bp.get("topic_domain", "")
    ctx = (archived.get("commentary", {}) or {}).get("narrative_context", "")
    summary = ctx.split("\n\n")[0][:700]
    if not title or not summary:
        raise RuntimeError("seed material incomplete — halt, do not improvise a seed")
    return title, domain, summary


def source_desc(title, domain, summary) -> str:
    return (
        f"Topic: {title} (domain: {domain}).\n\n"
        f"Situation material (from a prior corpus; RE-AUTHOR FROM SCRATCH under the "
        f"current format — do not attempt to reconstruct any prior story's fields):\n"
        f"{summary}\n"
    )


def stamps(seed_id: str, draw: int) -> dict:
    return {
        "prompt_commit": _git_commit_of("prompts/constraint_story_generation_prompt_json.md"),
        "schema_commit": _git_commit_of("schemas/constraint_story_schema.json"),
        "generated_date": date.today().isoformat(),
        "source_essay": "cohort_zero_regen",
        "one_shot_example": str(EXAMPLE_PATH.relative_to(REPO_ROOT)),
        "model": MODEL,
        "sampling_params": f"temperature={TEMPERATURE}",
        "seeded_from": seed_id,
        "draw": draw,
    }


def generate_one(seed_id, title, domain, summary, draw):
    prompt = build_prompt(source_desc(title, domain, summary))
    text, tin, tout = llm_call.call(
        prompt, MODEL, system=_SYSTEM_INSTRUCTION,
        temperature=TEMPERATURE, max_tokens=16384)
    story, errors = process_response(text)
    if story is None or errors:
        feedback = ("\nYour previous attempt had these validation errors:\n"
                    + "".join(f"  - {e}\n" for e in (errors or ["unparseable output"]))
                    + "Fix these while keeping the rest correct.\n")
        text, tin2, tout2 = llm_call.call(
            build_prompt(source_desc(title, domain, summary) + feedback), MODEL,
            system=_SYSTEM_INSTRUCTION, temperature=TEMPERATURE, max_tokens=16384)
        tin, tout = tin + tin2, tout + tout2
        story, errors = process_response(text)
    if story is None:
        return None, errors or ["unparseable"], tin, tout

    # mechanical stamps: identity forward, provenance, seed link
    cid = f"{seed_id}_c0" if draw == 1 else f"{seed_id}_c0_d{draw}"
    story.setdefault("header", {})["constraint_id"] = cid
    story["provenance"] = stamps(seed_id, draw)
    errors = validate_json(story)
    return story, errors, tin, tout


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--only", default="")
    ap.add_argument("--replicates", default="",
                    help="comma-separated override of REPLICATE_SET (pilot mode)")
    args = ap.parse_args()
    global REPLICATE_SET
    if args.replicates:
        REPLICATE_SET = set(args.replicates.split(","))

    seeds = sorted(p.stem for p in ARCHIVE.glob("*.json"))
    if args.only:
        seeds = [s for s in seeds if s in set(args.only.split(","))]
    plan = [(s, 3 if s in REPLICATE_SET else 1) for s in seeds]
    n_calls = sum(n for _, n in plan)

    if args.dry_run:
        print(f"plan: {len(seeds)} seeds, {n_calls} generations "
              f"(replicates: {sorted(REPLICATE_SET & set(seeds))})")
        for s in seeds[:2]:
            archived = json.load(open(ARCHIVE / f"{s}.json"))
            t, d, m = seed_material(archived)
            prompt = build_prompt(source_desc(t, d, m))
            print(f"  {s}: prompt ~{len(prompt)//4} tokens; seed fields ONLY "
                  f"(title={t[:40]!r}, domain={d!r}, summary {len(m)} chars)")
        est_in = n_calls * (len(prompt) // 4)
        est_out = n_calls * 4000
        print(f"cost est. (sonnet $3/M in, $15/M out): "
              f"${est_in/1e6*3 + est_out/1e6*15:.2f} "
              f"({est_in/1e6:.1f}M in, {est_out/1e6:.2f}M out, {n_calls} calls)")
        return

    STAGE_JSON.mkdir(exist_ok=True)
    STAGE_PL.mkdir(exist_ok=True)
    REPLICATE_DIR.mkdir(parents=True, exist_ok=True)
    log = open(REPO_ROOT / "audits/2026-06-12_cohort_zero/regen_run.log", "a")
    failures = []
    for seed_id, n_draws in plan:
        archived = json.load(open(ARCHIVE / f"{seed_id}.json"))
        t, d, m = seed_material(archived)
        for draw in range(1, n_draws + 1):
            story, errors, tin, tout = generate_one(seed_id, t, d, m, draw)
            tag = f"{seed_id} draw {draw}"
            if story is None or errors:
                failures.append((tag, errors))
                print(f"FAIL {tag}: {errors}", file=log, flush=True)
                continue
            cid = story["header"]["constraint_id"]
            if draw == 1:
                json.dump(story, open(STAGE_JSON / f"{cid}.json", "w"), indent=2,
                          ensure_ascii=False)
                pl = generate_pl(story)
                (STAGE_PL / f"{cid}.pl").write_text(pl)
            else:
                json.dump(story, open(REPLICATE_DIR / f"{cid}.json", "w"), indent=2,
                          ensure_ascii=False)
            print(f"OK {tag} -> {cid} (in={tin} out={tout})", file=log, flush=True)
            print(f"OK {tag} -> {cid}")
    print(f"done: {n_calls - len(failures)}/{n_calls} ok; failures: {failures or 'none'}")
    if failures:
        sys.exit(1)


if __name__ == "__main__":
    main()
