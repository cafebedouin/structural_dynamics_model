#!/usr/bin/env python3
"""Kimi (Moonshot) twin of generate_kernel_corpus.run_no_scope — the kernel-aware no-scope path.

Same process as the Haiku/Gemini runs, only the model/provider differs:
  - SAME prompt: reuses build_cached_messages(seed) verbatim (kernel context, reading_relations
    instructions, raw schema/example) — the task text is byte-identical to what Haiku/Flash saw;
    only the provider's role-structuring (system + user) differs.
  - SAME post-processing: reuses generate_kernel_corpus.process_batch_results UNCHANGED
    (provenance stamp from result.message.model, validate -> repair -> re-stamp -> generate_pl
    -> lint -> write -> ladder) via the same Anthropic-result-shaped shim the Gemini driver uses.
    The provenance block therefore records the Kimi model id (kimi-k3).
  - DIFFERENT destinations: prolog/testsets_kimi/ + json_kimi/ + a SEPARATE ladder
    (beta_processed_kimi.txt). The uniqueness registry is the KIMI dir ONLY (NOT the Haiku
    testsets/), so cids stay == seed cids and the sets pair by filename (runbook §6).

Provider mechanics: Moonshot's OpenAI-compatible API (https://api.moonshot.ai/v1).
  - --batch (the full run): OpenAI-style file batch (/files + /batches), -50%. Batch is
    MODEL-gated: kimi-k2.6 -> 200; kimi-k2.7-code / kimi-k3 -> 404 "resource_not_found". Moonshot's
    batch output rows set response.status_code == 0 on SUCCESS (see _batch_row_to_result).
  - --sync: parallel /chat/completions (pilot / small N, interactive rate, no -50%).
  - --resume-batch <id>: reprocess an already-completed batch with NO generation (recovers a dead
    poll loop without re-billing); pass the same --n.

STATUS (2026-07-19): batch VERIFIED on kimi-k2.6 (the batch-eligible model); 5-seed pilot PASSED
(5/5 valid .pl into testsets_kimi/, classify_corpus GREEN on model kimi-k2.6, reading_relations
resolved). Measured k2.6 batch: input ~29.6k / output ~15.5k tok/story (~11.7k reasoning) — k2.6 is
reasoning-heavy too, so this remains a *thinking-model* twin. Full 1000-seed remainder pending a
spend-go. Seed pool: prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json (1005; kimi
ladder skips the 5 pilot). Needs MOONSHOT_API_KEY or KIMI_API_KEY in env. Runbook §6/§7b.

CAVEAT — this is a *thinking-model* twin. Unlike the haiku/flash/sonnet twins (run thinking-off for
fairness), every kimi-k2.6 story carries mandatory reasoning tokens (~11.7k/story), so per-story
output runs higher. We extract only the final `content` (the story JSON); `reasoning_content` is
discarded. Cross-twin comparisons must carry that asymmetry (stamped in provenance as kimi-k2.6).

Key: reads MOONSHOT_API_KEY (or KIMI_API_KEY) from the environment (never hard-code it).

Usage:
  KIMI_API_KEY=... python3 -m agent.run_no_scope_kimi --seeds <chunk.json> [--n N] \
      [--sync|--batch] [--model kimi-k3] [--estimate]
"""
import argparse
import json
import os
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import requests

from agent.story_generator_base import (
    _SYSTEM_INSTRUCTION, _load_context_file, load_processed_log,
    PROMPT_PATH, SCHEMA_PATH, EXAMPLE_PATH,
)
from agent.generate_kernel_corpus import (
    build_cached_messages, process_batch_results, unique_constraint_id,
    REPO_ROOT,
)

BASE_URL = os.environ.get("MOONSHOT_BASE_URL", "https://api.moonshot.ai/v1")
# Batch is MODEL-gated: kimi-k2.6 is batch-eligible (POST /v1/batches -> 200), while kimi-k2.7-code
# and kimi-k3 404 "resource_not_found" (not batch-enabled yet — NOT a credential problem; verified
# 2026-07-18 by creating a k2.6 batch on the same key). completion_window must be an h-unit Go
# duration ("24h" works; the docs' "1d" is rejected). --model overrides (e.g. sync run on k2.7/k3).
DEFAULT_MODEL = "kimi-k2.6"
MAX_OUTPUT_TOKENS = 32000        # must cover mandatory reasoning + the story JSON
SYNC_WORKERS = 5
POLL_INTERVAL = 20
HTTP_TIMEOUT = 1200              # K3 max-reasoning stories can exceed 10 min (pilot: 3/5 > 600s)
MAX_BATCH_FILE_BYTES = 90_000_000  # Moonshot /files hard limit is 100MB; margin for the newline+encoding

# Kimi destinations (pair-by-filename with the other twins; separate ladder + json)
KIMI_TESTSETS = REPO_ROOT / "prolog" / "testsets_kimi"
KIMI_JSON = REPO_ROOT / "json_kimi"
KIMI_LADDER = REPO_ROOT / "prolog" / "beta_processed_kimi.txt"
OUT_DIR = REPO_ROOT / "outputs" / "no_scope_runs_kimi"

BATCH_TERMINAL = {"completed", "failed", "expired", "cancelled", "cancelling"}


def _api_key():
    # Accept either name: MOONSHOT_API_KEY (original convention) or KIMI_API_KEY (the .bashrc
    # export added 2026-07-19). Same Moonshot key either way.
    k = os.environ.get("MOONSHOT_API_KEY") or os.environ.get("KIMI_API_KEY")
    if not k:
        raise SystemExit("Set MOONSHOT_API_KEY (or KIMI_API_KEY) in the environment.")
    return k


def _headers():
    return {"Authorization": f"Bearer {_api_key()}", "Content-Type": "application/json"}


# --------------------------------------------------------------------------
# Anthropic-result-shaped shim — lets process_batch_results run UNCHANGED.
# It only touches: result.custom_id, result.result.type,
# result.result.message.{model, usage.input_tokens, usage.output_tokens, content[].{type,text}}.
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
# Prompt build — identical task text to Haiku/Flash; provider role-structuring differs.
# --------------------------------------------------------------------------
def _static_prefix():
    return (
        f"=== GENERATION PROMPT ===\n{_load_context_file(str(PROMPT_PATH))}\n\n"
        f"=== JSON SCHEMA ===\n{_load_context_file(str(SCHEMA_PATH))}\n\n"
        f"=== EXAMPLE JSON ===\n{_load_context_file(str(EXAMPLE_PATH))}"
    )


def build_messages(seed, static):
    task_txt = build_cached_messages(seed)[0]["content"][1]["text"]
    return [
        {"role": "system", "content": _SYSTEM_INSTRUCTION},
        {"role": "user", "content": static + "\n\n" + task_txt},
    ]


def _body(seed, static, model):
    # No temperature: kimi-k3 is reasoning-only and (like Sonnet-5/Opus-4.7+) rejects a
    # non-default sampling temperature. No thinking toggle: K3 forbids disabling it.
    return {"model": model, "messages": build_messages(seed, static),
            "max_tokens": MAX_OUTPUT_TOKENS}


def _extract(body):
    """(text, in_tok, out_tok) from a chat-completion body; text is content, NOT reasoning."""
    msg = body["choices"][0]["message"]
    text = msg.get("content") or ""
    usage = body.get("usage") or {}
    return text, int(usage.get("prompt_tokens", 0)), int(usage.get("completion_tokens", 0))


# --------------------------------------------------------------------------
# SYNC transport — parallel /chat/completions (pilot / small N)
# --------------------------------------------------------------------------
def _one_sync(seed, key, static, model):
    body = _body(seed, static, model)
    for attempt in range(3):
        try:
            r = requests.post(f"{BASE_URL}/chat/completions", headers=_headers(),
                              json=body, timeout=HTTP_TIMEOUT)
            if r.status_code == 200:
                text, it, ot = _extract(r.json())
                if text.strip():
                    return _Result(key, "succeeded", _Msg(text, model, _Usage(it, ot)))
                return _Result(key, "errored", None)
            if r.status_code in (429, 500, 502, 503, 504):
                time.sleep(5 * (attempt + 1))
                continue
            print(f"  [{key}] HTTP {r.status_code}: {r.text[:200]}")
            return _Result(key, "errored", None)
        except requests.RequestException as e:
            print(f"  [{key}] {type(e).__name__}: {e}")
            time.sleep(5 * (attempt + 1))
    return _Result(key, "errored", None)


def run_sync(seeds, id_map, model):
    static = _static_prefix()
    seed_by_key = {k: s for k, s in zip(id_map.keys(), seeds)}
    out = []
    with ThreadPoolExecutor(max_workers=SYNC_WORKERS) as ex:
        futs = {ex.submit(_one_sync, seed_by_key[k], k, static, model): k for k in id_map}
        for f in as_completed(futs):
            out.append(f.result())
            done = sum(1 for r in out if r.result.type == "succeeded")
            print(f"  sync {len(out)}/{len(id_map)} ({done} ok)")
    return out


# --------------------------------------------------------------------------
# BATCH transport — OpenAI-style file batch (/files + /batches), -50%, for the full run
# --------------------------------------------------------------------------
def _chunk_lines(lines, limit=MAX_BATCH_FILE_BYTES):
    """Greedily pack jsonl lines into <=limit-byte chunks (a request line is never split)."""
    chunks, cur, cur_bytes = [], [], 0
    for ln in lines:
        b = len(ln.encode("utf-8")) + 1  # + newline
        if cur and cur_bytes + b > limit:
            chunks.append(cur)
            cur, cur_bytes = [], 0
        cur.append(ln)
        cur_bytes += b
    if cur:
        chunks.append(cur)
    return chunks


def _submit_batch(jsonl_bytes, n, model, poll_interval):
    """Upload one (<100MB) jsonl, create + poll a batch, return its rows as shim _Results."""
    up = requests.post(f"{BASE_URL}/files",
                       headers={"Authorization": f"Bearer {_api_key()}"},
                       files={"file": ("batch.jsonl", jsonl_bytes, "application/jsonl")},
                       data={"purpose": "batch"}, timeout=HTTP_TIMEOUT)
    up.raise_for_status()
    fid = up.json()["id"]
    print(f"  uploaded input file {fid} ({n} requests, {len(jsonl_bytes)/1e6:.0f}MB)")

    cr = requests.post(f"{BASE_URL}/batches", headers=_headers(),
                       json={"input_file_id": fid, "endpoint": "/v1/chat/completions",
                             "completion_window": "24h"}, timeout=HTTP_TIMEOUT)
    if cr.status_code >= 300:
        # Batch is MODEL-gated (2026-07-19): kimi-k2.6 -> 200; kimi-k2.7-code / kimi-k3 -> 404
        # "resource_not_found". A 404 here almost always means --model is a non-batch model.
        raise SystemExit(
            f"batch create failed: HTTP {cr.status_code} {cr.text[:300]}\n"
            "Batch is model-gated on Moonshot — use --model kimi-k2.6 (k2.7-code/k3 are not "
            "batch-enabled), or run with --sync.")
    bid = cr.json()["id"]
    print(f"  batch {bid}")
    while True:
        g = requests.get(f"{BASE_URL}/batches/{bid}", headers=_headers(), timeout=HTTP_TIMEOUT)
        g.raise_for_status()
        b = g.json()
        rc = b.get("request_counts") or {}
        print(f"  batch {bid} {b.get('status')} ({rc.get('completed', 0)}/{rc.get('total', 0)})")
        if b.get("status") in BATCH_TERMINAL:
            break
        time.sleep(poll_interval)

    out = []
    ofid = b.get("output_file_id")
    if ofid:
        dl = requests.get(f"{BASE_URL}/files/{ofid}/content", headers=_headers(),
                          timeout=HTTP_TIMEOUT)
        dl.raise_for_status()
        for line in dl.text.splitlines():
            if line.strip():
                out.append(_batch_row_to_result(json.loads(line), model))
    return out


def run_batch(seeds, id_map, model, poll_interval):
    static = _static_prefix()
    seed_by_key = {k: s for k, s in zip(id_map.keys(), seeds)}
    lines = []
    for k in id_map:
        lines.append(json.dumps({
            "custom_id": k, "method": "POST", "url": "/v1/chat/completions",
            "body": _body(seed_by_key[k], static, model),
        }, ensure_ascii=False))
    # Moonshot's /files limit is 100 MB and every request inlines the full prompt (~139 KB), so a
    # full-pool jsonl (~143 MB for 1000 seeds) MUST be split into <100MB batches — one batch job
    # per chunk, run sequentially, results merged. Witnessed 2026-07-19: a 1000-request upload 400s
    # "File size is too large, max size is 100000000".
    chunks = _chunk_lines(lines)
    print(f"  {len(lines)} requests -> {len(chunks)} batch(es) "
          f"(<= {MAX_BATCH_FILE_BYTES // 10**6}MB each)")
    out = []
    for i, chunk in enumerate(chunks, 1):
        print(f"  [chunk {i}/{len(chunks)}] {len(chunk)} requests")
        jsonl = ("\n".join(chunk) + "\n").encode("utf-8")
        out.extend(_submit_batch(jsonl, len(chunk), model, poll_interval))
    seen = {r.custom_id for r in out}
    for k in id_map:
        if k not in seen:
            out.append(_Result(k, "errored", None))
    return out


# --------------------------------------------------------------------------
def _batch_row_to_result(row, model):
    """Map ONE Moonshot batch-output row to a shim _Result. Moonshot sets
    response.status_code == 0 on SUCCESS (not 200), carrying the completion in
    `body` with a null row-level error; a hard `status_code == 200` gate therefore
    discarded every valid Kimi result (witnessed 2026-07-19: batch completed 5/5,
    all rejected -> driver looped into a 2nd batch). Gate on the payload (no row
    error + a usable choices/content body); treat status_code as advisory."""
    key = row.get("custom_id")
    resp = row.get("response") or {}
    body = resp.get("body") or {}
    sc = resp.get("status_code")
    ok = (row.get("error") in (None, "", {}) and sc in (200, 0, None)
          and isinstance(body.get("choices"), list) and body["choices"])
    if ok:
        text, it, ot = _extract(body)
        if text.strip():
            return _Result(key, "succeeded", _Msg(text, model, _Usage(it, ot)))
    print(f"  [{key}] batch row not usable (status_code={sc}, error={row.get('error')})")
    return _Result(key, "errored", None)


def download_batch_results(batch_id, model):
    """Download an ALREADY-completed batch's output rows as shim _Results — no
    generation. Lets a run recover from a dead poll loop without re-billing."""
    g = requests.get(f"{BASE_URL}/batches/{batch_id}", headers=_headers(), timeout=HTTP_TIMEOUT)
    g.raise_for_status()
    b = g.json()
    ofid = b.get("output_file_id")
    if not ofid:
        raise SystemExit(f"batch {batch_id} status={b.get('status')} has no output_file_id yet.")
    dl = requests.get(f"{BASE_URL}/files/{ofid}/content", headers=_headers(), timeout=HTTP_TIMEOUT)
    dl.raise_for_status()
    return [_batch_row_to_result(json.loads(l), model) for l in dl.text.splitlines() if l.strip()]


def build_id_map(seeds):
    return {f"k{i}": s["constraint_id"] for i, s in enumerate(seeds)}


def estimate(seeds, model):
    static = _static_prefix()
    sample = next((s for s in seeds if s.get("kernel_id")), seeds[0])
    msgs = build_messages(sample, static)
    approx_in = sum(len(m["content"]) for m in msgs) // 4  # ~4 chars/token heuristic
    n = len(seeds)
    print(f"\n=== ROUGH ESTIMATE — {n} stories, {model} ===")
    print(f"  per-request input ~{approx_in:,} tok (char/4 heuristic; confirm with a pilot)")
    print(f"  input total ~{n * approx_in:,} tok")
    print("  output/story is UNKNOWN until measured — kimi-k3 reasoning is mandatory (max effort)")
    print("  and counts toward completion tokens. Run the pilot (--sync --n 5) for the real number.")


def run(args):
    seeds = json.loads(Path(args.seeds).read_text(encoding="utf-8"))
    for s in seeds:
        if "constraint_id" not in s and s.get("kernel_id") and s.get("reading_id"):
            s["constraint_id"] = f"{s['kernel_id']}__{s['reading_id']}"

    if args.estimate:
        estimate(seeds, args.model)
        return

    KIMI_TESTSETS.mkdir(parents=True, exist_ok=True)
    KIMI_JSON.mkdir(parents=True, exist_ok=True)
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    processed = load_processed_log(KIMI_LADDER)
    pending = [s for s in seeds if s["constraint_id"] not in processed]
    n = args.n if (args.n and args.n > 0) else len(pending)
    batch_seeds = pending[:n]
    if not batch_seeds:
        print("No unprocessed seeds (kimi ladder).")
        return

    # Registry = KIMI dir + kimi ladder ONLY — never the Haiku testsets/, so cids stay
    # == seed cids and the twin pairs by filename (runbook §6).
    registry = {p.stem for p in KIMI_TESTSETS.glob("*.pl")} | set(processed)
    final_seeds = []
    for s in batch_seeds:
        s["constraint_id"] = unique_constraint_id(s["constraint_id"], registry)
        registry.add(s["constraint_id"])
        final_seeds.append(s)

    token_acc = {"input_tokens": 0, "output_tokens": 0}

    if args.resume_batch:
        # Recover an ALREADY-completed batch without regenerating: its k-index custom_ids
        # (k0..kN-1, seed order) map back onto these same first-n unprocessed seeds. Valid
        # only against the registry state the batch was submitted under — pass the same --n.
        gen_by_id = {s["constraint_id"]: s for s in final_seeds}
        id_map = build_id_map(final_seeds)
        print(f"\n[resume] downloading batch {args.resume_batch} "
              f"({len(final_seeds)} seeds, {args.model})...")
        wrapped = download_batch_results(args.resume_batch, args.model)
        process_batch_results(
            _ShimClient(wrapped), "kimi-batch", KIMI_JSON, KIMI_TESTSETS, KIMI_LADDER,
            gen_seeds_by_id=gen_by_id, rejections_path=OUT_DIR / "rejections.json",
            overwrite=True, id_map=id_map, token_acc=token_acc,
            provenance_source="no_scope_rebuild_kimi",
            sampling_params=f"max_tokens={MAX_OUTPUT_TOKENS},temperature=model_default,reasoning=model_default")
        done = load_processed_log(KIMI_LADDER)
        got = [s for s in final_seeds if s["constraint_id"] in done]
        it, ot = token_acc["input_tokens"], token_acc["output_tokens"]
        print(f"\nResume {args.resume_batch}: {len(got)}/{len(final_seeds)} written to "
              f"{KIMI_TESTSETS.relative_to(REPO_ROOT)} (ladder: {KIMI_LADDER.name}).")
        print(f"  token_acc: input={it:,} output={ot:,} (output INCLUDES reasoning tokens)")
        if got:
            print(f"  per-story mean: input={it//len(got):,} output={ot//len(got):,}")
        return

    remaining = final_seeds
    transport = run_sync if args.sync else run_batch
    for attempt in range(1, 4):
        gen_by_id = {s["constraint_id"]: s for s in remaining}
        id_map = build_id_map(remaining)
        print(f"\n[attempt {attempt}/3] {'sync' if args.sync else 'batch'} "
              f"{len(remaining)} requests ({args.model})...")
        wrapped = (transport(remaining, id_map, args.model) if args.sync
                   else transport(remaining, id_map, args.model, args.poll_interval))
        process_batch_results(
            _ShimClient(wrapped), "kimi-batch", KIMI_JSON, KIMI_TESTSETS, KIMI_LADDER,
            gen_seeds_by_id=gen_by_id, rejections_path=OUT_DIR / "rejections.json",
            overwrite=True, id_map=id_map, token_acc=token_acc,
            provenance_source="no_scope_rebuild_kimi",
            # we send only max_tokens; temperature + reasoning are the model's own defaults
            # (k3 forces max reasoning, k2.7-code uses its default) — stamp reflects what we set.
            sampling_params=f"max_tokens={MAX_OUTPUT_TOKENS},temperature=model_default,reasoning=model_default")
        done = load_processed_log(KIMI_LADDER)
        remaining = [s for s in remaining if s["constraint_id"] not in done]
        if not remaining:
            break
        print(f"  {len(remaining)} still failing after attempt {attempt}")

    if remaining:
        (OUT_DIR / "failures.json").write_text(
            json.dumps(remaining, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"\nFAILURES: {len(remaining)} after 3 attempts -> {OUT_DIR / 'failures.json'}")
    succeeded = len(final_seeds) - len(remaining)
    print(f"\nKimi no-scope run complete: {succeeded}/{len(final_seeds)} into "
          f"{KIMI_TESTSETS.relative_to(REPO_ROOT)} (ladder: {KIMI_LADDER.name}).")
    it, ot = token_acc["input_tokens"], token_acc["output_tokens"]
    print(f"  token_acc: input={it:,} output={ot:,} (output INCLUDES mandatory reasoning tokens)")
    if succeeded:
        print(f"  per-story mean: input={it//succeeded:,} output={ot//succeeded:,}")


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--seeds", required=True)
    ap.add_argument("--n", type=int, default=0, help="next N unprocessed (0=all)")
    ap.add_argument("--model", default=DEFAULT_MODEL)
    ap.add_argument("--sync", action="store_true", help="parallel chat-completions (pilot/small N)")
    ap.add_argument("--batch", action="store_true", help="file batch (-50%%; the full run)")
    ap.add_argument("--poll-interval", type=int, default=POLL_INTERVAL)
    ap.add_argument("--estimate", action="store_true", help="rough token count; no generation")
    ap.add_argument("--resume-batch", default=None,
                    help="reprocess an already-completed batch id (no generation); pass same --n")
    args = ap.parse_args()
    if not args.sync and not args.batch:
        args.sync = True  # default to sync for safety on small runs
    run(args)


if __name__ == "__main__":
    main()
