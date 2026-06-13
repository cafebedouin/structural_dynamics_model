#!/usr/bin/env python3
"""OQ-117 Spend A / Spend B driver — reuses the canonical building blocks of
agent/cohort_replicate_batch.py (NOT a fork: imports source_desc / build_prompt_parts /
batch client; only the seed set, the fed-hypothesis injection, and the output dir differ).

Spend A (--arm A): non-naturalization contested kernels, WITHHELD (no hypothesis) — kills the
  selection confound on the within-arm proxy's "mountain 15/15" (idiom vs correct-read).
Spend B (--arm B): the five sigma/seat kernels, hypothesis FED = mountain (the claim the withheld
  arm landed on) — tests whether feeding the claim manufactures metric concordance (epsilon pull).

Predictions + binning are FROZEN in PRE_REGISTRATION.md and committed BEFORE any draw exists.
Draws are PROBE ARTIFACTS (audit dir only) — none join the live corpus.

Usage:
  python3 oq117_spend_driver.py --arm A --dry-run
  python3 oq117_spend_driver.py --arm A
  python3 oq117_spend_driver.py --arm B
"""
import argparse
import json
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "agent"))
sys.path.insert(0, str(REPO / "python"))

from story_generator_base import build_prompt_parts, _SYSTEM_INSTRUCTION, process_response  # noqa: E402
from generate_constraint_pl import validate_json  # noqa: E402
import cohort_zero_regen as czr  # noqa: E402
import generate_kernel_corpus as gkc  # noqa: E402

SEEDS_FILE = REPO / "prolog" / "kernel_seeds.json"
AUDIT = Path(__file__).resolve().parent
MAX_TOKENS = 16384

# Spend A: five non-naturalization contested kernels whose HONEST claim is NOT mountain
# (impositions / coerced reversal / transition / decline). Pre-registered per-kernel in PRE_REGISTRATION.md.
ARM_A_IDS = [
    "turkish_alphabet_reform_1928",     # top-down imposition, zero prior practitioners -> snare/scaffold
    "meiji_calendar_dress_imposition",  # forced top-down imposition -> snare/scaffold
    "mormon_polygamy_manifesto_1890",   # reversal under federal coercion -> snare/piton
    "gold_to_fiat_transition",          # transition to a constructed convention -> scaffold/rope
    "dueling_decline",                  # practice declining to unthinkability -> piton/scaffold
]
# Spend B: the five sigma/seat kernels (same as the withheld replicate arm), hypothesis fed.
ARM_B_IDS = [
    "qwerty_path_naturalization", "free_market_naturalization",
    "total_war_unthinkability", "printing_press_reformation", "zero_as_number",
]
FED_HYPOTHESIS = "mountain"  # the claim the withheld arm landed on; isolates the feeding effect


def seed_fields(seed):
    title = seed.get("human_readable", "")
    domain = seed.get("topic_domain", "")
    summary = (seed.get("summary", "") or "")[:700]
    if not title or not summary:
        raise SystemExit(f"seed {seed.get('kernel_id')} missing title/summary — halt")
    return title, domain, summary


def fed_source(title, domain, summary):
    """Withheld source_desc + an explicit fed-claim instruction (mirrors production's
    'Hypothesis type: X' channel that OQ-117 interrogates)."""
    base = czr.source_desc(title, domain, summary)
    return base + (
        f"\nDeclared claim type for this constraint (author base_properties.claimed_type "
        f"AS this type): {FED_HYPOTHESIS}\n"
    )


def build_requests(seeds, draws, arm):
    system = [{"type": "text", "text": _SYSTEM_INSTRUCTION, "cache_control": {"type": "ephemeral"}}]
    reqs = []
    for seed in seeds:
        title, domain, summary = seed_fields(seed)
        src = fed_source(title, domain, summary) if arm == "B" else czr.source_desc(title, domain, summary)
        static_prefix, dynamic_tail = build_prompt_parts(src)
        for draw in range(1, draws + 1):
            reqs.append({
                "custom_id": f"{seed['kernel_id']}__{arm}__d{draw}"[:64],
                "params": {
                    "model": czr.MODEL, "max_tokens": MAX_TOKENS, "temperature": czr.TEMPERATURE,
                    "system": system,
                    "messages": [{"role": "user", "content": [
                        {"type": "text", "text": static_prefix, "cache_control": {"type": "ephemeral"}},
                        {"type": "text", "text": dynamic_tail},
                    ]}],
                },
            })
    return reqs


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--arm", choices=["A", "B"], required=True)
    ap.add_argument("--draws", type=int, default=3)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--poll-interval", type=int, default=30)
    args = ap.parse_args()

    ids = ARM_A_IDS if args.arm == "A" else ARM_B_IDS
    out_dir = AUDIT / ("nonnat_withheld" if args.arm == "A" else "fed_arm")
    seeds_all = {s["kernel_id"]: s for s in json.load(open(SEEDS_FILE))}
    missing = [i for i in ids if i not in seeds_all]
    if missing:
        raise SystemExit(f"seed ids not in {SEEDS_FILE.name}: {missing}")
    seeds = [seeds_all[i] for i in ids]
    reqs = build_requests(seeds, args.draws, args.arm)
    n = len(reqs)

    if args.dry_run:
        title, domain, summary = seed_fields(seeds[0])
        src = fed_source(title, domain, summary) if args.arm == "B" else czr.source_desc(title, domain, summary)
        sp, dt = build_prompt_parts(src)
        in_tok = (len(_SYSTEM_INSTRUCTION) + len(sp) + len(dt)) // 4
        sync = n * in_tok / 1e6 * 3 + n * 5000 / 1e6 * 15
        print(f"ARM {args.arm}: {len(seeds)} seeds x {args.draws} draws = {n} requests -> {out_dir.name}/")
        for s in seeds:
            print(f"  {s['kernel_id']}  ({s['topic_domain']})")
        if args.arm == "B":
            print(f"  FED hypothesis = {FED_HYPOTHESIS} (appended to source_desc)")
        print(f"per-request ~{in_tok} tok in, ~5000 out | model {czr.MODEL} temp {czr.TEMPERATURE}")
        print(f"cost est: SYNC ${sync:.2f}; BATCH ~${sync*0.5:.2f} (50% off, uncached upper bound; "
              f"shared prefix caches across the {n} requests -> real lower)")
        return

    out_dir.mkdir(parents=True, exist_ok=True)
    client = gkc.get_client()
    print(f"submitting ARM {args.arm} batch of {n} requests...")
    batch = client.messages.batches.create(requests=reqs)
    print(f"batch created: {batch.id}")
    batch = gkc.poll_batch(client, batch.id, args.poll_interval)

    log = open(AUDIT / "spend_batch_run.log", "a")
    print(f"\n=== ARM {args.arm} batch {batch.id} ===", file=log, flush=True)
    ok = fail = 0
    for result in client.messages.batches.results(batch.id):
        cid = result.custom_id
        kid, arm, drawtok = cid.split("__")
        draw = int(drawtok[1:])
        if result.result.type != "succeeded":
            fail += 1
            print(f"FAIL {cid}: result.type={result.result.type}", file=log, flush=True)
            continue
        text = "".join(b.text for b in result.result.message.content if b.type == "text")
        try:
            story, errors = process_response(text)
        except Exception as e:  # noqa: BLE001
            story, errors = None, [f"process_response raised: {e}"]
        if story is None or errors:
            fail += 1
            print(f"FAIL {cid}: {errors}", file=log, flush=True)
            continue
        story.setdefault("header", {})["constraint_id"] = cid
        story["provenance"] = czr.stamps(kid, draw)
        story["provenance"]["oq117_arm"] = arm
        if arm == "B":
            story["provenance"]["fed_hypothesis"] = FED_HYPOTHESIS
        verrors = validate_json(story)
        if verrors:
            # record the failure but keep the raw story for inspection (fed-arm lint conflicts are a finding)
            fail += 1
            print(f"FAIL(validate) {cid}: {verrors}", file=log, flush=True)
            json.dump(story, open(out_dir / f"{cid}.INVALID.json", "w"), indent=2, ensure_ascii=False)
            continue
        json.dump(story, open(out_dir / f"{cid}.json", "w"), indent=2, ensure_ascii=False)
        ok += 1
        print(f"OK {cid}", file=log, flush=True)
        print(f"OK {cid}")
    print(f"\nARM {args.arm} done: {ok}/{n} ok, {fail} failed. draws in {out_dir}")


if __name__ == "__main__":
    main()
