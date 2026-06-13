#!/usr/bin/env python3
"""Replicate-probe batch runner (OQ-109 Phase C σ/seat falsifier; spend authorized 2026-06-12).

Generates N seeds × K draws via the Anthropic Batch API to populate the replicate stability table
(cohort_stability.py) and unlock the σ/seat partition test (cohort_sigma_seat_eval.py).

SEED SPEC PRESERVED (frozen prediction at 5f2a626c applies): each draw is re-authored FROM SCRATCH
conditioned ONLY on the declared seed spec — (1) title (human_readable), (2) domain (topic_domain),
(3) one-paragraph summary — via cohort_zero_regen.source_desc, the exact witnessed wording. Only the
seed SOURCE changes (prolog/kernel_seeds.json instead of the archive); the seed SPEC is unchanged, so
SIGMA_SEAT_PREDICTION.md's seed-supplied bucket {human_readable, topic_domain, narrative_context
echo} is identical. REGIME HOMOGENEITY (refinement #3): every draw uses the same live spec
(model=claude-sonnet-4-5-20250929, temperature=0.2) regardless of seed origin.

Draws are PROBE ARTIFACTS (replicate dir only) — none join the live corpus.

Usage:
  python3 agent/cohort_replicate_batch.py --dry-run         # plan + cost, no API
  python3 agent/cohort_replicate_batch.py                   # submit batch, poll, write draws
  python3 agent/cohort_replicate_batch.py --ids a,b --draws 3
"""
import argparse
import json
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "agent"))
sys.path.insert(0, str(REPO / "python"))

from story_generator_base import build_prompt_parts, _SYSTEM_INSTRUCTION  # noqa: E402
from generate_constraint_pl import validate_json  # noqa: E402
import cohort_zero_regen as czr  # noqa: E402  (reuse source_desc + stamps + MODEL/TEMP)
import generate_kernel_corpus as gkc  # noqa: E402  (reuse poll_batch + client)

SEEDS_FILE = REPO / "prolog" / "kernel_seeds.json"
REPLICATE_DIR = REPO / "audits/2026-06-12_cohort_zero/replicates"
LOG = REPO / "audits/2026-06-12_cohort_zero/replicate_batch_run.log"

# Five contested kernels spanning seat-side prediction fields + domains (rationale in WRITEUP).
DEFAULT_IDS = [
    "qwerty_path_naturalization",      # naturalization -> claimed_type mountain/snare
    "free_market_naturalization",      # beneficiary-vs-lapsed -> claimed_type
    "total_war_unthinkability",        # removal-from-reachable -> disappearance_verdict
    "printing_press_reformation",      # tech-inevitability vs beneficiary (kernel_v1 drift topic)
    "zero_as_number",                  # became-thinkable vs first-held -> founding_problem_status
]
MAX_TOKENS = 16384


def load_seeds(ids):
    seeds = {s["kernel_id"]: s for s in json.load(open(SEEDS_FILE))}
    missing = [i for i in ids if i not in seeds]
    if missing:
        raise SystemExit(f"seed ids not in {SEEDS_FILE.name}: {missing}")
    return [seeds[i] for i in ids]


def seed_fields(seed):
    title = seed.get("human_readable", "")
    domain = seed.get("topic_domain", "")
    summary = (seed.get("summary", "") or "")[:700]
    if not title or not summary:
        raise SystemExit(f"seed {seed.get('kernel_id')} missing title/summary — halt")
    return title, domain, summary


def build_requests(seeds, draws):
    system = [{"type": "text", "text": _SYSTEM_INSTRUCTION,
               "cache_control": {"type": "ephemeral"}}]
    reqs = []
    for seed in seeds:
        title, domain, summary = seed_fields(seed)
        static_prefix, dynamic_tail = build_prompt_parts(czr.source_desc(title, domain, summary))
        for draw in range(1, draws + 1):
            reqs.append({
                "custom_id": f"{seed['kernel_id']}_d{draw}"[:64],
                "params": {
                    "model": czr.MODEL,
                    "max_tokens": MAX_TOKENS,
                    "temperature": czr.TEMPERATURE,
                    "system": system,
                    "messages": [{"role": "user", "content": [
                        {"type": "text", "text": static_prefix,
                         "cache_control": {"type": "ephemeral"}},
                        {"type": "text", "text": dynamic_tail},
                    ]}],
                },
            })
    return reqs


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ids", default=",".join(DEFAULT_IDS))
    ap.add_argument("--draws", type=int, default=3)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--poll-interval", type=int, default=30)
    args = ap.parse_args()

    ids = args.ids.split(",")
    seeds = load_seeds(ids)
    reqs = build_requests(seeds, args.draws)
    n = len(reqs)

    if args.dry_run:
        # token estimate from the assembled first request
        sp, dt = build_prompt_parts(czr.source_desc(*seed_fields(seeds[0])))
        in_tok = (len(_SYSTEM_INSTRUCTION) + len(sp) + len(dt)) // 4
        est_in, est_out = n * in_tok, n * 5000
        sync = est_in / 1e6 * 3 + est_out / 1e6 * 15
        print(f"plan: {len(seeds)} seeds x {args.draws} draws = {n} requests")
        for s in seeds:
            print(f"  {s['kernel_id']}  ({s['topic_domain']})  {s['human_readable']}")
        print(f"per-request ~{in_tok} tok in, ~5000 out | model {czr.MODEL} temp {czr.TEMPERATURE}")
        print(f"cost est (sonnet $3/M in $15/M out): SYNC ${sync:.2f}; "
              f"BATCH ~${sync*0.5:.2f} (50% off, uncached upper bound — "
              f"shared static prefix caches across the {n} requests, real cost lower)")
        return

    REPLICATE_DIR.mkdir(parents=True, exist_ok=True)
    client = gkc.get_client()
    print(f"submitting batch of {n} requests...")
    batch = client.messages.batches.create(requests=reqs)
    print(f"batch created: {batch.id}")
    batch = gkc.poll_batch(client, batch.id, args.poll_interval)

    log = open(LOG, "a")
    print(f"\n=== replicate batch {batch.id} ===", file=log, flush=True)
    ok = fail = 0
    for result in client.messages.batches.results(batch.id):
        cid = result.custom_id
        kid, draw = cid.rsplit("_d", 1)
        draw = int(draw)
        if result.result.type != "succeeded":
            fail += 1
            print(f"FAIL {cid}: result.type={result.result.type}", file=log, flush=True)
            continue
        text = "".join(b.text for b in result.result.message.content if b.type == "text")
        story = None
        try:
            from story_generator_base import process_response
            story, errors = process_response(text)
        except Exception as e:  # noqa: BLE001
            errors = [f"process_response raised: {e}"]
        if story is None or errors:
            fail += 1
            print(f"FAIL {cid}: {errors}", file=log, flush=True)
            continue
        story.setdefault("header", {})["constraint_id"] = cid
        story["provenance"] = czr.stamps(kid, draw)   # seeded_from=kid, draw
        verrors = validate_json(story)
        if verrors:
            fail += 1
            print(f"FAIL(validate) {cid}: {verrors}", file=log, flush=True)
            continue
        json.dump(story, open(REPLICATE_DIR / f"{cid}.json", "w"), indent=2, ensure_ascii=False)
        ok += 1
        print(f"OK {cid}", file=log, flush=True)
        print(f"OK {cid}")
    print(f"\ndone: {ok}/{n} ok, {fail} failed. draws in {REPLICATE_DIR}")
    if fail:
        sys.exit(1)


if __name__ == "__main__":
    main()
