#!/usr/bin/env python3
"""Sonnet third-model twin of generate_kernel_corpus.run_no_scope (the kernel-aware no-scope path).

Same process as the Haiku run, only the model + destinations differ — this makes a THIRD
matched twin (haiku / flash / sonnet) over the SAME 960-intersection seed pool, so the
model-divergence OQs (OQ-124 signature lean, OQ-149 Field C override asymmetry, OQ-123 B4
(b)/(c2) split, OQ-211 (d)(e) cross-corpus replication) get a third data point.

  - SAME prompt: reuses build_cached_messages(seed) verbatim (kernel context, reading_relations
    instructions, raw schema/example) — byte-identical to what Haiku/Flash saw.
  - SAME post-processing: reuses generate_kernel_corpus.process_batch_results UNCHANGED
    (provenance stamp from result.message.model, validate -> repair -> re-stamp -> generate_pl
    -> lint -> write -> ladder). The provenance block records the TRUE Sonnet model id the API
    returns (read from result.result.message.model), so classify_corpus's single-model
    fingerprint (expected_model='claude-sonnet-5') passes on the resulting directory.
  - DIFFERENT destinations: prolog/testsets_sonnet/ + json_sonnet/ + a SEPARATE ladder
    (beta_processed_sonnet.txt), so the Sonnet set pairs with the Haiku/Flash sets by filename
    and does not clobber them. The uniqueness registry is the SONNET dir only (NOT the main
    testsets/), so cids stay == seed cids and the three sets share filenames — the documented
    twin recipe (docs/technical/bulk_corpus_generation.md §6).

Twin-parity note: the Haiku and Flash runs both ran WITHOUT extended thinking, so this sets
thinking={"type":"disabled"} (Sonnet 5 runs adaptive thinking by default, which would add
thinking tokens and break output-length parity). temperature is omitted — Sonnet 5 rejects a
non-default temperature/top_p/top_k with a 400.

Usage:
  python3 -m agent.run_no_scope_sonnet --seeds prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json --estimate
  python3 -m agent.run_no_scope_sonnet --seeds <chunk.json> [--n N] [--model claude-sonnet-5]
"""
import argparse
import json
import time
from pathlib import Path

from agent.story_generator_base import (  # noqa: E402
    _SYSTEM_INSTRUCTION, load_processed_log,
)
from agent.generate_kernel_corpus import (  # noqa: E402
    build_cached_messages, process_batch_results, poll_batch, unique_constraint_id,
    get_client, REPO_ROOT, TESTSETS_DIR,
)

DEFAULT_MODEL = "claude-sonnet-5"   # bare id; API stamps the resolved id, prefix-matches the fingerprint
POLL_INTERVAL = 30
MAX_OUTPUT_TOKENS = 16384           # matches the Haiku/Flash runs (build_batch_requests)

# Sonnet destinations (pair-by-filename with the Haiku/Flash sets; separate ladder + json)
SONNET_TESTSETS = REPO_ROOT / "prolog" / "testsets_sonnet"
SONNET_JSON = REPO_ROOT / "json_sonnet"
SONNET_LADDER = REPO_ROOT / "prolog" / "beta_processed_sonnet.txt"
OUT_DIR = REPO_ROOT / "outputs" / "no_scope_runs_sonnet"


def build_sonnet_batch_requests(gen_seeds, model):
    """Like generate_kernel_corpus.build_indexed_batch_requests but with the passed model
    and thinking DISABLED (twin parity). Short index custom_ids (g0..) map to real cids so
    the constraint_id (module name / filename) can exceed the 64-char batch custom_id cap.
    Returns (requests, id_map: custom_id -> constraint_id)."""
    system = [{"type": "text", "text": _SYSTEM_INSTRUCTION, "cache_control": {"type": "ephemeral"}}]
    reqs, id_map = [], {}
    for i, s in enumerate(gen_seeds):
        cidk = f"g{i}"
        id_map[cidk] = s["constraint_id"]
        reqs.append({
            "custom_id": cidk,
            "params": {
                "model": model,
                "max_tokens": MAX_OUTPUT_TOKENS,
                "thinking": {"type": "disabled"},  # twin parity: Haiku/Flash ran thinking-off
                "system": system,
                "messages": build_cached_messages(s),
            },
        })
    return reqs, id_map


def estimate(client, seeds, model):
    """count_tokens only (no generation, no spend). Prints a scenario table.

    Rates per 1M tokens. Sonnet 5 intro promo (through 2026-08-31): in $2 / out $10;
    standard after: in $3 / out $15. Batch API is -50%; cache reads ~0.1x input.
    Output proxy is the Haiku-measured mean (~10.8k/story) — Sonnet length may differ."""
    IN, OUT, CACHED_IN, BATCH = 2.00, 10.00, 0.20, 0.50   # intro promo (today < 2026-08-31)
    system = [{"type": "text", "text": _SYSTEM_INSTRUCTION}]
    sample = next((s for s in seeds if s.get("kernel_id")), seeds[0])
    msgs = build_cached_messages(sample)
    parts = msgs[0]["content"]
    in_full = client.messages.count_tokens(model=model, system=system, messages=msgs).input_tokens
    task_only = [{"role": "user", "content": [{"type": "text", "text": parts[1]["text"]}]}]
    in_task = client.messages.count_tokens(model=model, messages=task_only).input_tokens
    n = len(seeds)
    out_per = 10813  # measured Haiku mean output/story (proxy; thinking disabled)
    out_tok = n * out_per
    print(f"\n=== PRICING ESTIMATE — {n} seeds, {model} (intro promo rates thru 2026-08-31) ===")
    print(f"  per-request input (no cache): {in_full:,} tok | task-only (cached prefix): ~{in_task:,} tok")
    print(f"  output proxy: {out_per:,}/story (Haiku-measured; Sonnet may differ) -> {out_tok:,} tok\n")
    print(f"  {'scenario':40s} {'in$':>8s} {'out$':>8s} {'TOTAL':>9s}")
    for label, in_cost, batch in [
        ("no cache, interactive", n * in_full / 1e6 * IN, 1.0),
        ("no cache, BATCH (-50%)", n * in_full / 1e6 * IN, BATCH),
        ("cached prefix, interactive", (n * in_task / 1e6 * IN) + (n * in_full / 1e6 * CACHED_IN), 1.0),
        ("cached prefix, BATCH (-50%)", (n * in_task / 1e6 * IN) + (n * in_full / 1e6 * CACHED_IN), BATCH),
    ]:
        oc = out_tok / 1e6 * OUT
        print(f"  {label:40s} {in_cost*batch:8.2f} {oc*batch:8.2f} {(in_cost+oc)*batch:9.2f}")
    print("\n  Script default = cached prefix + BATCH. Standard (post-promo) rates are 1.5x these")
    print("  ($3/$15 vs $2/$10). Output dominates; thinking disabled so no thinking-token surcharge.")


def run(args):
    client = get_client()
    seeds = json.loads(Path(args.seeds).read_text(encoding="utf-8"))
    for s in seeds:
        if "constraint_id" not in s:
            if s.get("kernel_id") and s.get("reading_id"):
                s["constraint_id"] = f"{s['kernel_id']}__{s['reading_id']}"
            elif s.get("kernel_id"):
                s["constraint_id"] = s["kernel_id"]

    if args.estimate:
        estimate(client, seeds, args.model)
        return

    SONNET_TESTSETS.mkdir(parents=True, exist_ok=True)
    SONNET_JSON.mkdir(parents=True, exist_ok=True)
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    processed = load_processed_log(SONNET_LADDER)
    pending = [s for s in seeds if s["constraint_id"] not in processed]
    n = args.n if (args.n and args.n > 0) else len(pending)
    batch_seeds = pending[:n]
    if not batch_seeds:
        print("No unprocessed seeds (sonnet ladder).")
        return

    # Registry = SONNET dir + sonnet ladder ONLY — never the main testsets/, so cids stay
    # == seed cids and the three sets pair by filename (twin recipe §6).
    registry = {p.stem for p in SONNET_TESTSETS.glob("*.pl")} | set(processed)
    final_seeds = []
    for s in batch_seeds:
        s["constraint_id"] = unique_constraint_id(s["constraint_id"], registry)
        registry.add(s["constraint_id"])
        final_seeds.append(s)

    token_acc = {"input_tokens": 0, "output_tokens": 0}
    remaining = final_seeds
    for attempt in range(1, 4):
        gen_by_id = {s["constraint_id"]: s for s in remaining}
        reqs, id_map = build_sonnet_batch_requests(remaining, args.model)
        print(f"\n[attempt {attempt}/3] submitting {len(reqs)} Sonnet requests ({args.model})...")
        batch = client.messages.batches.create(requests=reqs)
        print(f"  batch {batch.id}")
        poll_batch(client, batch.id, args.poll_interval)
        process_batch_results(
            client, batch.id, SONNET_JSON, SONNET_TESTSETS, SONNET_LADDER,
            gen_seeds_by_id=gen_by_id, rejections_path=OUT_DIR / "rejections.json",
            overwrite=True, id_map=id_map, token_acc=token_acc,
            provenance_source="no_scope_rebuild_sonnet",
            sampling_params=f"max_tokens={MAX_OUTPUT_TOKENS},thinking=disabled,temperature=api_default")
        done = load_processed_log(SONNET_LADDER)
        remaining = [s for s in remaining if s["constraint_id"] not in done]
        if not remaining:
            break
        print(f"  {len(remaining)} still failing after attempt {attempt}")

    if remaining:
        (OUT_DIR / "failures.json").write_text(
            json.dumps(remaining, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"\nFAILURES: {len(remaining)} after 3 attempts -> {OUT_DIR / 'failures.json'}")
    succeeded = len(final_seeds) - len(remaining)
    print(f"\nSonnet no-scope run complete: {succeeded}/{len(final_seeds)} into "
          f"{SONNET_TESTSETS.relative_to(REPO_ROOT)} (ladder: {SONNET_LADDER.name}).")
    it, ot = token_acc["input_tokens"], token_acc["output_tokens"]
    # Sonnet 5 batch, intro promo ($1/$5 per MTok after -50%); standard is $1.50/$7.50.
    print(f"  token_acc: input={it:,} output={ot:,} "
          f"-> ~${it/1e6*1.00 + ot/1e6*5.00:.4f} (sonnet-5 batch intro $1/$5; standard $1.50/$7.50)")


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seeds", required=True)
    ap.add_argument("--n", type=int, default=0, help="next N unprocessed (0=all)")
    ap.add_argument("--model", default=DEFAULT_MODEL)
    ap.add_argument("--poll-interval", type=int, default=POLL_INTERVAL)
    ap.add_argument("--estimate", action="store_true", help="count tokens + price; no generation")
    run(ap.parse_args())


if __name__ == "__main__":
    main()
