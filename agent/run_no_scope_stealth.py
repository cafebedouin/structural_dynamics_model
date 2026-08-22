#!/usr/bin/env python3
"""OpenRouter twin of generate_kernel_corpus.run_no_scope — a sixth model-named leg.

Same process as the Haiku/Flash/Sonnet/Kimi runs, only the model/provider differs:
  - SAME prompt: reuses build_messages/_static_prefix from run_no_scope_kimi (which wrap
    generate_kernel_corpus.build_cached_messages) — the task text is byte-identical to what the
    other legs saw; only the provider's role-structuring (system + user) differs.
  - SAME post-processing: generate_kernel_corpus.process_batch_results UNCHANGED, via the same
    Anthropic-result-shaped shim classes run_no_scope_kimi defines (imported, not copied).
  - DIFFERENT destinations: prolog/testsets_stealth/ + json_stealth/ + a SEPARATE ladder
    (beta_processed_stealth.txt). The uniqueness registry is the STEALTH dir ONLY, so cids stay
    == seed cids and the leg pairs with the other twins by filename (runbook §6).

Provider mechanics: OpenRouter's OpenAI-compatible API (https://openrouter.ai/api/v1).
  - SYNC ONLY: parallel /chat/completions. OpenRouter has no /files + /batches, so there is no
    -50% batch path; --workers bounds concurrency.
  - Every raw response body is persisted to outputs/no_scope_runs_stealth/responses/<cid>.json
    BEFORE parsing (build_discipline → *Gate the output, not only the input*), so a run that dies
    mid-way leaves recoverable text, and a silent empty run is visible on disk.
  - A non-"stop" finish_reason ("length" = truncated story JSON) is reported LOUDLY and counted
    as errored, never handed to the parser as if complete.

Model: stealth/ox-alpha (OpenRouter "Stealth" provider; disk-verified 2026-08-21 via
GET /api/v1/models: price 0/0, ctx 1,048,576, max_completion 131,072, reasoning MANDATORY with
default effort `max`, temperature supported, default temperature 1). Like kimi-k2.6 this is a
*thinking-model* twin: we keep only `message.content`; `message.reasoning` is discarded (it
arrives in a separate field, never mixed into content). The provenance stamp carries the model
id exactly as OpenRouter echoes it (`stealth/ox-alpha`) and what we SET (max_tokens; temperature
and reasoning effort only if overridden, else `model_default`).

Key: reads OPENROUTER_API_KEY from the environment (never hard-code it).

Usage:
  python3 -m agent.run_no_scope_stealth --seeds prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json --estimate
  python3 -m agent.run_no_scope_stealth --seeds <pool.json> --n 1          # pilot
  python3 -m agent.run_no_scope_stealth --seeds <pool.json> [--n N] [--model stealth/ox-alpha] \
      [--workers 5] [--reasoning-effort low|high|max] [--temperature T]
"""
import argparse
import json
import os
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import requests

from agent.story_generator_base import load_processed_log
from agent.generate_kernel_corpus import (
    process_batch_results, unique_constraint_id, REPO_ROOT,
)
from agent.run_no_scope_kimi import (
    _Result, _Msg, _Usage, _ShimClient, _static_prefix, build_messages, build_id_map,
    _extract,
)

BASE_URL = os.environ.get("OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1")
DEFAULT_MODEL = "stealth/ox-alpha"
# Model max_completion_tokens is 131,072 (OpenRouter record, 2026-08-21). Reasoning is mandatory
# and counts toward completion tokens; kimi-k2.6 needed ~15.5k/story at 32k. 64k leaves headroom
# for `max`-effort reasoning without letting a runaway trace run to the provider ceiling.
MAX_OUTPUT_TOKENS = 65536
SYNC_WORKERS = 5
HTTP_TIMEOUT = 1200              # max-effort reasoning stories can run >10 min (kimi-k3 pilot: 3/5 > 600s)

# Stealth destinations (pair-by-filename with the other twins; separate ladder + json)
STEALTH_TESTSETS = REPO_ROOT / "prolog" / "testsets_stealth"
STEALTH_JSON = REPO_ROOT / "json_stealth"
STEALTH_LADDER = REPO_ROOT / "prolog" / "beta_processed_stealth.txt"
OUT_DIR = REPO_ROOT / "outputs" / "no_scope_runs_stealth"
RESPONSES_DIR = OUT_DIR / "responses"
PROVENANCE_SOURCE = "no_scope_rebuild_stealth"


def apply_leg(name="stealth", suffix=""):
    """Rebind every destination to leg <name><suffix>: testsets_<leg>/, json_<leg>/,
    beta_processed_<leg>.txt, outputs/no_scope_runs_<leg>/, provenance_source
    no_scope_rebuild_<leg>. --leg-name picks the MODEL leg (any OpenRouter model: glm, nemotron,
    ...); --leg-suffix S appends for a same-model REDRAW or regime sibling that must pair with
    the base leg by filename while never touching it. The uniqueness registry is the named dir
    only (runbook §6)."""
    global STEALTH_TESTSETS, STEALTH_JSON, STEALTH_LADDER, OUT_DIR, RESPONSES_DIR, PROVENANCE_SOURCE
    leg = f"{name}{suffix}"
    if leg == "stealth":
        return
    STEALTH_TESTSETS = REPO_ROOT / "prolog" / f"testsets_{leg}"
    STEALTH_JSON = REPO_ROOT / f"json_{leg}"
    STEALTH_LADDER = REPO_ROOT / "prolog" / f"beta_processed_{leg}.txt"
    OUT_DIR = REPO_ROOT / "outputs" / f"no_scope_runs_{leg}"
    RESPONSES_DIR = OUT_DIR / "responses"
    PROVENANCE_SOURCE = f"no_scope_rebuild_{leg}"


def _api_key():
    k = os.environ.get("OPENROUTER_API_KEY")
    if not k:
        raise SystemExit("Set OPENROUTER_API_KEY in the environment.")
    return k


def _headers():
    # HTTP-Referer / X-Title are OpenRouter's optional app-attribution headers (no effect on
    # routing or billing; they label the app in the OpenRouter dashboard).
    return {"Authorization": f"Bearer {_api_key()}", "Content-Type": "application/json",
            "HTTP-Referer": "https://github.com/cafebedouin/structural_dynamics_model",
            "X-Title": "structural_dynamics_model"}


def _body(seed, static, model, reasoning_effort=None, temperature=None):
    body = {"model": model, "messages": build_messages(seed, static),
            "max_tokens": MAX_OUTPUT_TOKENS}
    if reasoning_effort == "off":
        body["reasoning"] = {"enabled": False}   # models whose reasoning is OPTIONAL (glm-5.2, nemotron-3)
    elif reasoning_effort:
        body["reasoning"] = {"effort": reasoning_effort}
    if temperature is not None:
        body["temperature"] = temperature
    return body


def sampling_stamp(reasoning_effort=None, temperature=None):
    """The provenance `sampling_params` string: records what we SET, model_default otherwise."""
    t = "model_default" if temperature is None else str(temperature)
    r = {"off": "disabled"}.get(reasoning_effort, reasoning_effort or "model_default")
    return f"max_tokens={MAX_OUTPUT_TOKENS},temperature={t},reasoning={r}"


def _persist(cid, payload):
    RESPONSES_DIR.mkdir(parents=True, exist_ok=True)
    (RESPONSES_DIR / f"{cid}.json").write_text(
        json.dumps(payload, indent=1, ensure_ascii=False), encoding="utf-8")


# --------------------------------------------------------------------------
# SYNC transport — parallel /chat/completions
# --------------------------------------------------------------------------
def _one_sync(seed, key, cid, static, model, reasoning_effort, temperature, cost_acc):
    body = _body(seed, static, model, reasoning_effort, temperature)
    for attempt in range(3):
        try:
            r = requests.post(f"{BASE_URL}/chat/completions", headers=_headers(),
                              json=body, timeout=HTTP_TIMEOUT)
            if r.status_code == 200:
                resp = r.json()
                _persist(cid, resp)  # raw datum first, before any parse
                if "error" in resp and not resp.get("choices"):
                    # OpenRouter can return 200 with a provider error object in the body.
                    print(f"  [{cid}] provider error in 200 body: {str(resp['error'])[:200]}")
                    return _Result(key, "errored", None)
                choice = (resp.get("choices") or [{}])[0]
                fr = choice.get("finish_reason")
                text, it, ot = _extract(resp)
                usage = resp.get("usage") or {}
                cost_acc["cost"] = cost_acc.get("cost", 0.0) + float(usage.get("cost") or 0.0)
                cost_acc["reasoning_tokens"] = cost_acc.get("reasoning_tokens", 0) + int(
                    ((usage.get("completion_tokens_details") or {}).get("reasoning_tokens")) or 0)
                if fr != "stop":
                    print(f"  [{cid}] finish_reason={fr!r} (native={choice.get('native_finish_reason')!r}), "
                          f"completion_tokens={ot} — NOT handed to the parser")
                    return _Result(key, "errored", None)
                if text.strip():
                    return _Result(key, "succeeded", _Msg(text, resp.get("model") or model,
                                                          _Usage(it, ot)))
                print(f"  [{cid}] empty content with finish_reason=stop")
                return _Result(key, "errored", None)
            if r.status_code in (429, 500, 502, 503, 504):
                print(f"  [{cid}] HTTP {r.status_code}, retry {attempt + 1}/3: {r.text[:120]}")
                time.sleep(10 * (attempt + 1))
                continue
            print(f"  [{cid}] HTTP {r.status_code}: {r.text[:200]}")
            return _Result(key, "errored", None)
        except (requests.RequestException, ValueError) as e:
            print(f"  [{cid}] {type(e).__name__}: {e}")
            time.sleep(10 * (attempt + 1))
    return _Result(key, "errored", None)


def run_sync(seeds, id_map, model, workers, reasoning_effort, temperature, cost_acc):
    static = _static_prefix()
    seed_by_key = {k: s for k, s in zip(id_map.keys(), seeds)}
    out = []
    with ThreadPoolExecutor(max_workers=workers) as ex:
        futs = {ex.submit(_one_sync, seed_by_key[k], k, id_map[k], static, model,
                          reasoning_effort, temperature, cost_acc): k for k in id_map}
        for f in as_completed(futs):
            out.append(f.result())
            done = sum(1 for r in out if r.result.type == "succeeded")
            print(f"  sync {len(out)}/{len(id_map)} ({done} ok)")
    return out


def results_from_responses(seeds, id_map, model):
    """--from-responses: rebuild shim results from the PERSISTED raw bodies in RESPONSES_DIR
    (no API call) so a changed repair/validation path can be re-applied to the same draws —
    the offline half of the per-model A/B loop (runbook §9). A seed with no persisted body is
    reported and counted errored. finish_reason/provider-error checks are applied as in
    _one_sync, so a truncated draw is still refused."""
    out = []
    for key, cid in id_map.items():
        p = RESPONSES_DIR / f"{cid}.json"
        if not p.exists():
            print(f"  [{cid}] no persisted response"); out.append(_Result(key, "errored", None)); continue
        resp = json.loads(p.read_text(encoding="utf-8"))
        if "error" in resp and not resp.get("choices"):
            out.append(_Result(key, "errored", None)); continue
        choice = (resp.get("choices") or [{}])[0]
        if choice.get("finish_reason") != "stop":
            out.append(_Result(key, "errored", None)); continue
        text, it, ot = _extract(resp)
        out.append(_Result(key, "succeeded", _Msg(text, resp.get("model") or model, _Usage(it, ot)))
                   if text.strip() else _Result(key, "errored", None))
    return out


def estimate(seeds, model):
    static = _static_prefix()
    sample = next((s for s in seeds if s.get("kernel_id")), seeds[0])
    msgs = build_messages(sample, static)
    approx_in = sum(len(m["content"]) for m in msgs) // 4  # ~4 chars/token heuristic
    n = len(seeds)
    print(f"\n=== ROUGH ESTIMATE — {n} stories, {model} ===")
    print(f"  per-request input ~{approx_in:,} tok (char/4 heuristic; confirm with a pilot)")
    print(f"  input total ~{n * approx_in:,} tok")
    print("  output/story is UNKNOWN until measured — reasoning is mandatory on stealth/ox-alpha")
    print("  and counts toward completion tokens. Run the pilot (--n 1) for the real number.")
    print("  Price at 2026-08-21 model record: prompt 0 / completion 0 — confirm with the pilot's usage.cost.")


def run(args):
    seeds = json.loads(Path(args.seeds).read_text(encoding="utf-8"))
    for s in seeds:
        if "constraint_id" not in s and s.get("kernel_id") and s.get("reading_id"):
            s["constraint_id"] = f"{s['kernel_id']}__{s['reading_id']}"

    if args.estimate:
        estimate(seeds, args.model)
        return

    STEALTH_TESTSETS.mkdir(parents=True, exist_ok=True)
    STEALTH_JSON.mkdir(parents=True, exist_ok=True)
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    processed = load_processed_log(STEALTH_LADDER)
    pending = [s for s in seeds if s["constraint_id"] not in processed]
    n = args.n if (args.n and args.n > 0) else len(pending)
    batch_seeds = pending[:n]
    if not batch_seeds:
        print("No unprocessed seeds (stealth ladder).")
        return

    # Registry = STEALTH dir + stealth ladder ONLY — never another leg's testsets, so cids stay
    # == seed cids and the leg pairs by filename (runbook §6).
    registry = {p.stem for p in STEALTH_TESTSETS.glob("*.pl")} | set(processed)
    final_seeds = []
    for s in batch_seeds:
        s["constraint_id"] = unique_constraint_id(s["constraint_id"], registry)
        registry.add(s["constraint_id"])
        final_seeds.append(s)

    token_acc = {"input_tokens": 0, "output_tokens": 0}
    cost_acc = {"cost": 0.0, "reasoning_tokens": 0}
    stamp = sampling_stamp(args.reasoning_effort, args.temperature)

    remaining = final_seeds
    for attempt in range(1, 2 if args.from_responses else 4):
        gen_by_id = {s["constraint_id"]: s for s in remaining}
        id_map = build_id_map(remaining)
        if args.from_responses:
            print(f"\n[from-responses] re-processing {len(remaining)} persisted draws from "
                  f"{RESPONSES_DIR.relative_to(REPO_ROOT)} (no API calls; {stamp})...")
            wrapped = results_from_responses(remaining, id_map, args.model)
        else:
            print(f"\n[attempt {attempt}/3] sync {len(remaining)} requests ({args.model}, "
                  f"workers={args.workers}, {stamp})...")
            wrapped = run_sync(remaining, id_map, args.model, args.workers,
                               args.reasoning_effort, args.temperature, cost_acc)
        process_batch_results(
            _ShimClient(wrapped), "stealth-sync", STEALTH_JSON, STEALTH_TESTSETS, STEALTH_LADDER,
            gen_seeds_by_id=gen_by_id, rejections_path=OUT_DIR / "rejections.json",
            overwrite=True, id_map=id_map, token_acc=token_acc,
            provenance_source=PROVENANCE_SOURCE,
            sampling_params=stamp)
        done = load_processed_log(STEALTH_LADDER)
        remaining = [s for s in remaining if s["constraint_id"] not in done]
        if not remaining:
            break
        print(f"  {len(remaining)} still failing after attempt {attempt}")

    if remaining:
        (OUT_DIR / "failures.json").write_text(
            json.dumps(remaining, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"\nFAILURES: {len(remaining)} after 3 attempts -> {OUT_DIR / 'failures.json'}")
    succeeded = len(final_seeds) - len(remaining)
    # Count from the artifact, never from the loop.
    on_disk = sorted(p.stem for p in STEALTH_TESTSETS.glob("*.pl")
                     if p.stem in {s["constraint_id"] for s in final_seeds})
    raw_persisted = sum(1 for s in final_seeds if (RESPONSES_DIR / f"{s['constraint_id']}.json").exists())
    print(f"\nStealth no-scope run complete: {succeeded}/{len(final_seeds)} ladder-done; "
          f"{len(on_disk)} .pl on disk in {STEALTH_TESTSETS.relative_to(REPO_ROOT)}; "
          f"{raw_persisted} raw responses in {RESPONSES_DIR.relative_to(REPO_ROOT)}.")
    it, ot = token_acc["input_tokens"], token_acc["output_tokens"]
    print(f"  token_acc: input={it:,} output={ot:,} (output INCLUDES reasoning tokens; "
          f"reasoning_tokens reported by provider={cost_acc['reasoning_tokens']:,})")
    print(f"  usage.cost summed from responses: ${cost_acc['cost']:.4f}")
    if succeeded:
        print(f"  per-story mean: input={it//succeeded:,} output={ot//succeeded:,}")


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--seeds", required=True)
    ap.add_argument("--n", type=int, default=0, help="next N unprocessed (0=all)")
    ap.add_argument("--model", default=DEFAULT_MODEL)
    ap.add_argument("--workers", type=int, default=SYNC_WORKERS)
    ap.add_argument("--reasoning-effort", default=None,
                    choices=["off", "low", "medium", "high", "xhigh", "max"],
                    help="override the model's reasoning: 'off' sends reasoning.enabled=false (only "
                         "for models whose reasoning is optional); else an effort level. Stamped.")
    ap.add_argument("--temperature", type=float, default=None,
                    help="override the model's default temperature (stamped in provenance)")
    ap.add_argument("--estimate", action="store_true", help="rough token count; no generation")
    ap.add_argument("--from-responses", action="store_true",
                    help="no API: re-process this leg's persisted raw responses for the seeds still "
                         "pending on the ladder (offline A/B of repair/validation changes)")
    ap.add_argument("--leg-name", default="stealth",
                    help="model leg name: testsets_<name>/ (glm, nemotron, ...); pair with --model")
    ap.add_argument("--leg-suffix", default="",
                    help="append to the leg name for a same-model redraw / regime sibling")
    args = ap.parse_args()
    apply_leg(args.leg_name, args.leg_suffix)
    print(f"  leg: {STEALTH_TESTSETS.relative_to(REPO_ROOT)} | provenance_source={PROVENANCE_SOURCE}")
    run(args)


if __name__ == "__main__":
    main()
