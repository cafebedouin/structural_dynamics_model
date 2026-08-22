#!/usr/bin/env python3
"""Gemini equivalent of generate_kernel_corpus.run_no_scope (the kernel-aware no-scope path).

Same process as the Haiku run, only the model/provider differs:
  - SAME prompt: reuses build_cached_messages(seed) verbatim (kernel context, reading_relations
    instructions, raw schema/example) — byte-identical to what Haiku saw.
  - SAME post-processing: reuses generate_kernel_corpus.process_batch_results UNCHANGED
    (provenance stamp from result.message.model, validate -> repair -> re-stamp -> generate_pl
    -> lint -> write -> ladder) via a thin Anthropic-result-shaped adapter around the Gemini
    batch responses. The provenance block therefore records the Gemini model id, not Haiku's.
  - DIFFERENT destinations: prolog/testsets_flash/ + json_flash/ + a SEPARATE ladder
    (beta_processed_flash.txt), so the Flash set pairs with the Haiku set by filename and does
    not clobber it. The uniqueness registry is the FLASH dir only (NOT the Haiku testsets/),
    so cids stay == seed cids and the two sets share filenames.

Provider mechanics (google.genai batch + context caching) follow agent/generate_json.py;
the OLD build_prompt there is deliberately NOT used.

Usage:
  python3 -m agent.run_no_scope_gemini --seeds <chunk.json> [--n N] [--estimate] [--no-cache]
"""
import argparse
import json
import os
import time
from pathlib import Path

os.environ.setdefault("GOOGLE_API_KEY", os.environ.get("GEMINI_API_KEY", ""))
import google.genai as genai  # noqa: E402

from agent.story_generator_base import (  # noqa: E402
    _SYSTEM_INSTRUCTION, _load_context_file, load_processed_log,
    PROMPT_PATH, SCHEMA_PATH, EXAMPLE_PATH,
)
from agent.generate_kernel_corpus import (  # noqa: E402
    build_cached_messages, process_batch_results, unique_constraint_id,
    REPO_ROOT, TESTSETS_DIR,
)

DEFAULT_MODEL = "gemini-2.5-flash"
POLL_INTERVAL = 20
MAX_OUTPUT_TOKENS = 16384

# Flash destinations (pair-by-filename with the Haiku set; separate ladder + json)
FLASH_TESTSETS = REPO_ROOT / "prolog" / "testsets_flash"
FLASH_JSON = REPO_ROOT / "json_flash"
FLASH_LADDER = REPO_ROOT / "prolog" / "beta_processed_flash.txt"
OUT_DIR = REPO_ROOT / "outputs" / "no_scope_runs_flash"
PROVENANCE_SOURCE = "no_scope_rebuild_gemini"


def apply_leg_suffix(suffix):
    """--leg-suffix S rebinds every destination to a SIBLING leg (testsets_flash<S>/,
    json_flash<S>/, beta_processed_flash<S>.txt, outputs/no_scope_runs_flash<S>/) and tags
    provenance_source no_scope_rebuild_gemini<S>. Used for a same-model REDRAW leg or a
    regime-contrast leg (--thinking-budget) that must pair with testsets_flash/ by filename
    while never touching it. The registry is the sibling dir only (runbook §6)."""
    global FLASH_TESTSETS, FLASH_JSON, FLASH_LADDER, OUT_DIR, PROVENANCE_SOURCE
    if not suffix:
        return
    FLASH_TESTSETS = REPO_ROOT / "prolog" / f"testsets_flash{suffix}"
    FLASH_JSON = REPO_ROOT / f"json_flash{suffix}"
    FLASH_LADDER = REPO_ROOT / "prolog" / f"beta_processed_flash{suffix}.txt"
    OUT_DIR = REPO_ROOT / "outputs" / f"no_scope_runs_flash{suffix}"
    PROVENANCE_SOURCE = f"no_scope_rebuild_gemini{suffix}"

TERMINAL = {"JOB_STATE_SUCCEEDED", "JOB_STATE_FAILED", "JOB_STATE_CANCELLED",
            "JOB_STATE_EXPIRED", "JOB_STATE_PARTIALLY_SUCCEEDED"}


# --------------------------------------------------------------------------
# Anthropic-result-shaped adapter — lets process_batch_results run UNCHANGED.
# process_batch_results only ever touches: result.custom_id, result.result.type,
# result.result.message.{model,usage.input_tokens,usage.output_tokens,content[].{type,text}}.
# --------------------------------------------------------------------------
class _Usage:
    def __init__(self, i, o):
        self.input_tokens, self.output_tokens = i, o


class _Block:
    def __init__(self, text):
        self.type, self.text = "text", text


class _Msg:
    def __init__(self, text, model, usage):
        self.content, self.model, self.usage = [_Block(text)], model, usage


class _Inner:
    def __init__(self, typ, msg):
        self.type, self.message = typ, msg


class _Result:
    def __init__(self, custom_id, typ, msg):
        self.custom_id, self.result = custom_id, _Inner(typ, msg)


class _Batches:
    def __init__(self, results):
        self._results = results

    def results(self, _batch_id):
        return iter(self._results)


class _Messages:
    def __init__(self, results):
        self.batches = _Batches(results)


class _ShimClient:
    """Quacks like anthropic.Anthropic for process_batch_results' single call site."""
    def __init__(self, results):
        self.messages = _Messages(results)


# --------------------------------------------------------------------------
# Gemini request build + cache (reuses the EXACT kernel prompt from build_cached_messages)
# --------------------------------------------------------------------------
def _static_prefix():
    return (
        f"=== GENERATION PROMPT ===\n{_load_context_file(str(PROMPT_PATH))}\n\n"
        f"=== JSON SCHEMA ===\n{_load_context_file(str(SCHEMA_PATH))}\n\n"
        f"=== EXAMPLE JSON ===\n{_load_context_file(str(EXAMPLE_PATH))}"
    )


def create_cache(client, model):
    from google.genai import types
    try:
        cache = client.caches.create(
            model=model,
            config=types.CreateCachedContentConfig(
                system_instruction=_SYSTEM_INSTRUCTION,
                contents=[{"role": "user", "parts": [{"text": _static_prefix()}]}],
                ttl="3600s",
                display_name="constraint-story-prompt-gemini",
            ),
        )
        print(f"  cache created: {cache.name}")
        return cache.name
    except Exception as e:
        print(f"  caching unavailable ({e}); inlining full prefix per request")
        return None


# Thinking budget for THIS run. 0 = disabled (the original flash leg: Haiku ran without
# extended thinking, so output tokens == story length). A positive budget (--thinking-budget N)
# turns thinking ON for a regime-contrast leg; Gemini counts thinking tokens inside
# max_output_tokens, so the cap is widened by the budget. Stamped into provenance either way.
THINKING_BUDGET = 0


def _gen_config(cache_name):
    cfg = {"max_output_tokens": MAX_OUTPUT_TOKENS + max(THINKING_BUDGET, 0), "temperature": 0.1,
           "thinking_config": {"thinking_budget": THINKING_BUDGET}}
    if cache_name:
        cfg["cached_content"] = cache_name
    else:
        cfg["system_instruction"] = _SYSTEM_INSTRUCTION
    return cfg


def build_gemini_requests(seeds, cache_name):
    """Reuse build_cached_messages -> split into (static prefix, per-seed task). When cached,
    only the task is sent; otherwise prefix+task inline. Short index keys (g0..) map to cids."""
    static = _static_prefix()
    reqs, id_map = [], {}
    cfg = _gen_config(cache_name)
    for i, s in enumerate(seeds):
        key = f"g{i}"
        id_map[key] = s["constraint_id"]
        parts = build_cached_messages(s)[0]["content"]
        task_txt = parts[1]["text"]
        user_text = task_txt if cache_name else (static + "\n\n" + task_txt)
        reqs.append({
            "contents": [{"role": "user", "parts": [{"text": user_text}]}],
            "metadata": {"key": key},
            "config": cfg,
        })
    return reqs, id_map


def poll(client, name, interval):
    while True:
        b = client.batches.get(name=name)
        state = b.state.name if b.state else "UNKNOWN"
        print(f"  batch {state}")
        if state in TERMINAL:
            return b
        time.sleep(interval)


def _wrap_results(batch, id_map, model):
    """Map Gemini inlined_responses -> Anthropic-shaped _Result list (order-stable; metadata
    key when present, else positional)."""
    out = []
    responses = (batch.dest.inlined_responses if batch.dest else None) or []
    keys = list(id_map.keys())
    for i, resp in enumerate(responses):
        key = ((resp.metadata or {}).get("key") if getattr(resp, "metadata", None) else None) \
            or (keys[i] if i < len(keys) else f"g{i}")
        if getattr(resp, "error", None):
            # Print the batch-row error: 2026-08-21 two runs had EVERY row of attempt 2 errored
            # (272/272 and 451/451) with no text recorded, so the cause was unrecoverable.
            print(f"  [{id_map.get(key, key)}] batch row error: {str(resp.error)[:200]}")
            out.append(_Result(key, "errored", None))
            continue
        try:
            text = resp.response.text
        except Exception:
            text = None
        if not text:
            out.append(_Result(key, "errored", None))
            continue
        um = getattr(resp.response, "usage_metadata", None)
        usage = _Usage(getattr(um, "prompt_token_count", 0) or 0,
                       getattr(um, "candidates_token_count", 0) or 0)
        # Persist the raw datum per cid BEFORE parsing (build_discipline → *Gate the output*):
        # lets a changed repair/validation path be re-applied to the same draws offline (runbook
        # §9 A/B loop) and makes a silent empty run visible on disk. Same shape the OpenRouter
        # driver writes, minus provider fields.
        try:
            rd = OUT_DIR / "responses"; rd.mkdir(parents=True, exist_ok=True)
            (rd / f"{id_map.get(key, key)}.json").write_text(json.dumps({
                "model": model, "choices": [{"finish_reason": "stop", "message": {"content": text}}],
                "usage": {"prompt_tokens": usage.input_tokens, "completion_tokens": usage.output_tokens}},
                ensure_ascii=False), encoding="utf-8")
        except Exception as e:
            print(f"  [{id_map.get(key, key)}] raw persist failed: {e}")
        out.append(_Result(key, "succeeded", _Msg(text, model, usage)))
    return out


# --------------------------------------------------------------------------
# Estimate (no generation; count_tokens only)
# --------------------------------------------------------------------------
def estimate(client, seeds, model, cached):
    # gemini-2.5-flash published rates (per 1M tokens); CONFIRM against current rate card.
    IN, OUT, CACHED_IN, BATCH = 0.30, 2.50, 0.075, 0.50
    sample = next((s for s in seeds if s.get("kernel_id")), seeds[0])
    parts = build_cached_messages(sample)[0]["content"]
    task_txt = parts[1]["text"]
    in_full = client.models.count_tokens(
        model=model, contents=_SYSTEM_INSTRUCTION + "\n\n" + parts[0]["text"] + "\n\n" + task_txt
    ).total_tokens
    in_task = client.models.count_tokens(model=model, contents=task_txt).total_tokens
    n = len(seeds)
    out_per = 10813  # measured Haiku mean output/story (proxy; thinking disabled)
    out_tok = n * out_per
    print(f"\n=== PRICING ESTIMATE — {n} stories, {model} ===")
    print(f"  per-request input (no cache): {in_full:,} tok | task-only (cached): ~{in_task:,} tok")
    print(f"  output proxy: {out_per:,}/story (Haiku-measured) -> {out_tok:,} tok total\n")
    print(f"  {'scenario':40s} {'in$':>8s} {'out$':>8s} {'TOTAL':>9s}")
    for label, in_cost, batch in [
        ("no cache, interactive", n * in_full / 1e6 * IN, 1.0),
        ("no cache, BATCH (-50%)", n * in_full / 1e6 * IN, BATCH),
        ("cached prefix, interactive", (n * in_task / 1e6 * IN) + (n * in_full / 1e6 * CACHED_IN), 1.0),
        ("cached prefix, BATCH (-50%)", (n * in_task / 1e6 * IN) + (n * in_full / 1e6 * CACHED_IN), BATCH),
    ]:
        oc = out_tok / 1e6 * OUT
        print(f"  {label:40s} {in_cost*batch:8.2f} {oc*batch:8.2f} {(in_cost+oc)*batch:9.2f}")
    print("\n  (output dominates; thinking disabled so no thinking-token surcharge. Batch")
    print("   discount + context cache is what the script uses by default.)")


def run(args):
    client = genai.Client(api_key=os.environ.get("GEMINI_API_KEY"))
    seeds = json.loads(Path(args.seeds).read_text(encoding="utf-8"))
    for s in seeds:
        if "constraint_id" not in s and s.get("kernel_id") and s.get("reading_id"):
            s["constraint_id"] = f"{s['kernel_id']}__{s['reading_id']}"

    if args.estimate:
        estimate(client, seeds, args.model, not args.no_cache)
        return

    FLASH_TESTSETS.mkdir(parents=True, exist_ok=True)
    FLASH_JSON.mkdir(parents=True, exist_ok=True)
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    processed = load_processed_log(FLASH_LADDER)
    backfill_ids = None
    if getattr(args, "backfill_ids", None):
        # BACKFILL MODE (OQ-345): regenerate an explicit id list IN PLACE inside an existing leg —
        # ids excluded from the uniqueness registry (else __<uuid8> gets appended and the story
        # stops pairing by filename), ladder ignored for them. Pair with --run-tag.
        backfill_ids = set(json.loads(Path(args.backfill_ids).read_text(encoding="utf-8")))
        pending = [s for s in seeds if s["constraint_id"] in backfill_ids]
        print(f"  backfill: {len(pending)} of {len(backfill_ids)} ids found in the seed pool")
    else:
        pending = [s for s in seeds if s["constraint_id"] not in processed]
    n = args.n if (args.n and args.n > 0) else len(pending)
    batch_seeds = pending[:n]
    if not batch_seeds:
        print("No unprocessed seeds (flash ladder).")
        return

    # Registry = FLASH dir + flash ladder ONLY — never the Haiku testsets/, so cids stay
    # == seed cids and the two sets pair by filename.
    registry = ({p.stem for p in FLASH_TESTSETS.glob("*.pl")} | set(processed)) - (backfill_ids or set())
    final_seeds = []
    for s in batch_seeds:
        s["constraint_id"] = unique_constraint_id(s["constraint_id"], registry)
        registry.add(s["constraint_id"])
        final_seeds.append(s)

    cache_name = None if args.no_cache else create_cache(client, args.model)
    token_acc = {"input_tokens": 0, "output_tokens": 0}
    remaining = final_seeds
    try:
        for attempt in range(1, 4):
            gen_by_id = {s["constraint_id"]: s for s in remaining}
            reqs, id_map = build_gemini_requests(remaining, cache_name)
            print(f"\n[attempt {attempt}/3] submitting {len(reqs)} Gemini requests ({args.model})...")
            batch = client.batches.create(model=args.model, src=reqs)
            print(f"  batch {batch.name}")
            batch = poll(client, batch.name, args.poll_interval)
            wrapped = _wrap_results(batch, id_map, args.model)
            process_batch_results(
                _ShimClient(wrapped), "gemini-batch", FLASH_JSON, FLASH_TESTSETS, FLASH_LADDER,
                gen_seeds_by_id=gen_by_id, rejections_path=OUT_DIR / "rejections.json",
                overwrite=True, id_map=id_map, token_acc=token_acc,
                provenance_source=(f"{PROVENANCE_SOURCE}+{args.run_tag}" if getattr(args, "run_tag", "") else PROVENANCE_SOURCE),
                sampling_params=f"max_tokens={MAX_OUTPUT_TOKENS},temperature=0.1,thinking_budget={THINKING_BUDGET}")
            done = load_processed_log(FLASH_LADDER)
            remaining = [s for s in remaining if s["constraint_id"] not in done]
            if not remaining:
                break
            print(f"  {len(remaining)} still failing after attempt {attempt}")
    finally:
        if cache_name:
            try:
                client.caches.delete(name=cache_name)
                print(f"  cache deleted: {cache_name}")
            except Exception as e:
                print(f"  cache cleanup failed (auto-expires): {e}")

    if remaining:
        (OUT_DIR / "failures.json").write_text(
            json.dumps(remaining, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"\nFAILURES: {len(remaining)} after 3 attempts -> {OUT_DIR / 'failures.json'}")
    succeeded = len(final_seeds) - len(remaining)
    print(f"\nGemini no-scope run complete: {succeeded}/{len(final_seeds)} into "
          f"{FLASH_TESTSETS.relative_to(REPO_ROOT)} (ladder: {FLASH_LADDER.name}).")
    it, ot = token_acc["input_tokens"], token_acc["output_tokens"]
    print(f"  token_acc: input={it:,} output={ot:,} "
          f"-> ~${it/1e6*0.30 + ot/1e6*2.50:.4f} (flash interactive $0.30/$2.50; batch -50%)")


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seeds", required=True)
    ap.add_argument("--n", type=int, default=0, help="next N unprocessed (0=all)")
    ap.add_argument("--model", default=DEFAULT_MODEL)
    ap.add_argument("--poll-interval", type=int, default=POLL_INTERVAL)
    ap.add_argument("--no-cache", action="store_true")
    ap.add_argument("--estimate", action="store_true", help="count tokens + price; no generation")
    ap.add_argument("--leg-suffix", default="",
                    help="write to sibling leg testsets_flash<S>/ (redraw / regime-contrast leg)")
    ap.add_argument("--thinking-budget", type=int, default=0,
                    help="Gemini thinking budget (0 = disabled, the original leg's regime)")
    ap.add_argument("--run-tag", default="", help="mark this pass: provenance_source no_scope_rebuild_<leg>+<tag>")
    ap.add_argument("--backfill-ids", default=None,
                    help="JSON list of constraint_ids to REGENERATE IN PLACE inside the leg (OQ-345); pair with --run-tag")
    args = ap.parse_args()
    global THINKING_BUDGET
    THINKING_BUDGET = args.thinking_budget
    apply_leg_suffix(args.leg_suffix)
    print(f"  leg: {FLASH_TESTSETS.relative_to(REPO_ROOT)} | provenance_source={PROVENANCE_SOURCE} "
          f"| thinking_budget={THINKING_BUDGET}")
    run(args)


if __name__ == "__main__":
    main()
