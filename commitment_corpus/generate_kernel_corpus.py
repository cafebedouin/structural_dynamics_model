"""Generate a kernel-aware toy corpus: SCOPE each kernel seed, flatten readings
into one batch, generate via Haiku batch API, save run-tagged, then eyeball coherence.

Merges:
  - DRAuditOrchestrator._step_decompose  (SCOPE call, Sonnet)        from agent/c-orchestrator.py
  - generate_json_haiku batch machinery  (build/poll/process batch)  from agent/generate_json_haiku.py

Pipeline:
  kernel_seeds.json
    -> [SCOPE each seed, serial, Sonnet, kernel-aware]   -> manifests (saved as sidecar)
    -> flatten readings across all manifests             -> one seed list (each carries kernel_id/reading_id)
    -> [one Haiku batch]                                 -> constraint stories
    -> save to RUN-TAGGED dir (not main json/)
    -> coherence eyeball: per kernel, list readings + emitted type

Safety posture (matches the kernel-frame branch plan):
  - writes to json/<run_tag>/ and testsets/<run_tag>/, never the main corpus
  - processed-log is run-scoped (idempotent reruns per run_tag)
  - manifests saved to outputs/kernel_manifests/<run_tag>/ as the authoritative kernel grouping
  - --regression-check mode generates a known ordinary topic and stops, for the branch diff gate

Usage:
  python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json --run-tag run_01
  python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json --run-tag run_01 --limit 5 --dry-run
  python3 -m agent.generate_kernel_corpus --regression-check "Alberta separatism" --run-tag regress_01
  python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json --run-tag run_01 --skip-search
"""

import argparse
import datetime
import json
import sys
import time
from pathlib import Path

# Allow running as a script from repo root
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import anthropic

from agent.story_generator_base import (
    JSON_DIR,
    PROLOG_DIR,
    PROMPT_PATH,
    SCHEMA_PATH,
    EXAMPLE_PATH,
    REPO_ROOT,
    _SYSTEM_INSTRUCTION,
    _load_context_file,
    process_response,
    save_story,
    strip_json_fences,
    append_to_log,
    load_processed_log,
)

# SCOPE protocol prompt — local copy with kernel/reading extension (does not affect main pipeline)
SCOPE_PROMPT_PATH = Path(__file__).parent / "uke_scope_v2_json.md"

SCOPE_MODEL = "claude-sonnet-4-5-20250929"   # architect role: the kernel/not-kernel judgment
GEN_MODEL = "claude-haiku-4-5-20251001"      # generation: cheap batch
BATCH_POLL_INTERVAL = 30


# ---------------------------------------------------------------------------
# Output location helpers — RUN-TAGGED, never the main corpus
# ---------------------------------------------------------------------------

def run_dirs(run_tag: str):
    """Return (json_dir, testsets_dir, manifests_dir, processed_log) for this run."""
    json_dir = REPO_ROOT / "json" / run_tag
    testsets_dir = REPO_ROOT / "prolog" / "testsets" / run_tag
    manifests_dir = REPO_ROOT / "outputs" / "kernel_manifests" / run_tag
    for d in (json_dir, testsets_dir, manifests_dir):
        d.mkdir(parents=True, exist_ok=True)
    processed_log = manifests_dir / "processed.txt"
    return json_dir, testsets_dir, manifests_dir, processed_log


# ---------------------------------------------------------------------------
# Anthropic client
# ---------------------------------------------------------------------------

_client = None

def get_client():
    global _client
    if _client is None:
        _client = anthropic.Anthropic()
    return _client


def _extract_text(response) -> str:
    return "\n".join(b.text for b in response.content if hasattr(b, "text"))


def _call(prompt, model, system_instruction="", temperature=0.2, max_tokens=8192, tools=None):
    """Single Claude call with pause_turn continuation (for web_search)."""
    client = get_client()
    kwargs = {
        "model": model,
        "max_tokens": max_tokens,
        "temperature": temperature,
        "messages": [{"role": "user", "content": prompt}],
    }
    if system_instruction:
        kwargs["system"] = system_instruction
    if tools:
        kwargs["tools"] = tools

    resp = _call_with_retry(client, **kwargs)
    cont = 5
    while resp.stop_reason == "pause_turn" and cont > 0:
        cont -= 1
        kwargs["messages"] = [
            {"role": "user", "content": prompt},
            {"role": "assistant", "content": resp.content},
        ]
        resp = _call_with_retry(client, **kwargs)
    return _extract_text(resp)


def _call_with_retry(client, max_retries=3, **kwargs):
    for attempt in range(max_retries):
        try:
            return client.messages.create(**kwargs)
        except (anthropic.RateLimitError, anthropic.InternalServerError,
                anthropic.APIConnectionError):
            if attempt == max_retries - 1:
                raise
            time.sleep(2 ** attempt * 2)
        except anthropic.APIError:
            raise


# ---------------------------------------------------------------------------
# Step A: SCOPE each kernel seed (serial, Sonnet, kernel-aware)
# ---------------------------------------------------------------------------

def scope_seed(seed, scope_prompt, research_context="", axes=3):
    """Run kernel-aware SCOPE on one seed. Returns (manifest_dict | None, error)."""
    topic = f"{seed['human_readable']}\n\n{seed.get('summary','')}"
    prompt = (
        f"Analyze the following topic using the UKE_SCOPE protocol.\n\n"
        f"TOPIC: {topic}\n\n"
        f"RESEARCH CONTEXT:\n{research_context}\n\n"
        f"This topic is flagged as a candidate contested kernel. Apply the kernel/reading "
        f"decomposition (SCOPE §1.3-K): decide whether it is genuinely a contested kernel, "
        f"and if so decompose into READINGS (not flat axes), emitting the expanded "
        f"commitment_system_recognition object with is_contested_kernel and a readings array, "
        f"and tag each generation_sequence entry with its kernel_id and reading_id. "
        f"If it is NOT a real kernel (the readings would collapse), set is_contested_kernel "
        f"false and decompose normally, noting the collapse in an omega.\n\n"
        f"Select up to {axes} readings/axes for generation.\n\n"
        f"OUTPUT ONLY valid JSON — no markdown fences, no commentary outside the JSON."
    )
    try:
        text = _call(prompt, model=SCOPE_MODEL, system_instruction=scope_prompt,
                     temperature=0.2, max_tokens=8192)
    except Exception as e:
        return None, f"SCOPE call failed: {e}"
    try:
        manifest = json.loads(strip_json_fences(text))
    except json.JSONDecodeError as e:
        return None, f"JSON parse failed: {e} | raw head: {text[:200]}"
    if "generation_sequence" not in manifest:
        return None, "manifest missing generation_sequence"
    return manifest, ""


def research_seed(seed, max_uses=5):
    """Optional web-search grounding for contested-present seeds."""
    prompt = (
        f"Research this topic: factual background, key actors, structural tensions.\n\n"
        f"TOPIC: {seed['human_readable']}\n{seed.get('summary','')}"
    )
    tool = {"type": "web_search_20250305", "name": "web_search", "max_uses": max_uses}
    try:
        return _call(prompt, model=GEN_MODEL, temperature=0.1, max_tokens=4096, tools=[tool])
    except Exception:
        return ""


# ---------------------------------------------------------------------------
# Step B: Flatten manifests -> one seed list, each carrying kernel/reading context
# ---------------------------------------------------------------------------

def flatten_manifests(manifests):
    """Walk every manifest's generation_sequence; emit one gen-seed per axis/reading.

    Each gen-seed carries kernel_id, reading_id, sibling_reading_ids (when the manifest
    decomposed into readings) so the generator knows it is producing one reading of a set.
    Ordinary (non-kernel) manifests contribute flat axes with null kernel fields.
    """
    gen_seeds = []
    seen = set()
    for m in manifests:
        csr = m.get("commitment_system_recognition", {}) or {}
        is_kernel = bool(csr.get("is_contested_kernel"))
        kernel_id = csr.get("kernel_id")
        readings_by_id = {r.get("reading_id"): r for r in csr.get("readings", [])}
        family = m.get("family_id", "")
        domain = m.get("domain", "")

        for axis in m.get("generation_sequence", []):
            # generation_sequence entries may be ids or dicts depending on SCOPE output
            if isinstance(axis, str):
                cid = axis
                reading_id = None
            else:
                cid = axis.get("claim_id") or axis.get("constraint_id")
                reading_id = axis.get("reading_id")
            if not cid or cid in seen:
                continue
            seen.add(cid)

            reading = readings_by_id.get(reading_id, {}) if reading_id else {}
            gen_seeds.append({
                "constraint_id": cid,
                "human_readable": (reading.get("commitment") or cid).strip(),
                "topic_domain": domain,
                "family_id": family,
                "kernel_id": kernel_id if is_kernel else None,
                "reading_id": reading_id if is_kernel else None,
                "sibling_reading_ids": reading.get("sibling_readings", []) if is_kernel else [],
                "expected_structural_delta": reading.get("expected_structural_delta", ""),
                "summary": _axis_summary(axis, m),
            })
    return gen_seeds


def _axis_summary(axis, manifest):
    """Build a per-axis source summary the generator can use."""
    if isinstance(axis, dict):
        parts = [
            axis.get("human_readable", ""),
            axis.get("structural_delta", ""),
            f"primary observable: {axis.get('primary_observable','')}",
            f"hypothesis: {axis.get('hypothesis','')}",
        ]
        return "\n".join(p for p in parts if p)
    return manifest.get("topic_summary", "")


# ---------------------------------------------------------------------------
# Step C: Batch generation (Haiku), kernel context threaded into per-request task
# ---------------------------------------------------------------------------

def build_cached_messages(gen_seed):
    """Static prefix (prompt+schema+example) cached; per-seed task incl. kernel context not cached."""
    prompt_text = _load_context_file(str(PROMPT_PATH))
    schema_text = _load_context_file(str(SCHEMA_PATH))
    example_text = _load_context_file(str(EXAMPLE_PATH))
    static_content = (
        f"=== GENERATION PROMPT ===\n{prompt_text}\n\n"
        f"=== JSON SCHEMA ===\n{schema_text}\n\n"
        f"=== EXAMPLE JSON ===\n{example_text}"
    )

    # Per-seed task block. If kernel-tagged, include the committer context the
    # generation prompt's Rule 1/2/3 act on.
    lines = [
        "=== YOUR TASK ===",
        f"Generate a complete constraint story JSON for: {gen_seed['human_readable']}",
        f"DOMAIN: {gen_seed.get('topic_domain','General')}",
        f"CONSTRAINT_ID: {gen_seed['constraint_id']}",
    ]
    if gen_seed.get("kernel_id"):
        lines += [
            "",
            "=== KERNEL CONTEXT (committer frame) ===",
            f"This constraint is ONE READING of a contested kernel.",
            f"kernel_id: {gen_seed['kernel_id']}",
            f"reading_id (the reading you are instantiating): {gen_seed['reading_id']}",
            f"sibling readings (other constraints, NOT this one): {', '.join(gen_seed.get('sibling_reading_ids', [])) or 'none listed'}",
            f"expected structural delta for this reading: {gen_seed.get('expected_structural_delta','')}",
            "",
            "Apply the Kernels and Readings rules: generate ONLY this reading as a clean "
            "ε-invariant constraint (do not fold sibling readings in), route the committer "
            "structure to omega variables, and record the reading in commentary.kernel_context.",
        ]
    if gen_seed.get("summary"):
        lines += ["", f"SOURCE MATERIAL:\n{gen_seed['summary']}"]
    lines += ["", "Follow the schema exactly. Output ONLY valid JSON — no markdown fences."]
    task_content = "\n".join(lines)

    return [{
        "role": "user",
        "content": [
            {"type": "text", "text": static_content, "cache_control": {"type": "ephemeral"}},
            {"type": "text", "text": task_content},
        ],
    }]


def build_batch_requests(gen_seeds):
    system = [{"type": "text", "text": _SYSTEM_INSTRUCTION, "cache_control": {"type": "ephemeral"}}]
    reqs = []
    for s in gen_seeds:
        reqs.append({
            "custom_id": s["constraint_id"],
            "params": {
                "model": GEN_MODEL,
                "max_tokens": 8192,
                "system": system,
                "messages": build_cached_messages(s),
            },
        })
    return reqs


def poll_batch(client, batch_id, poll_interval):
    terminal = {"ended", "canceled", "expired"}
    while True:
        b = client.messages.batches.retrieve(batch_id)
        c = b.request_counts
        print(f"  Batch {batch_id}: {b.processing_status} — "
              f"succeeded={c.succeeded}, errored={c.errored}, processing={c.processing}")
        if b.processing_status in terminal:
            return b
        time.sleep(poll_interval)


def process_batch_results(client, batch_id, json_dir, testsets_dir, processed_log, overwrite=False):
    """Save run-tagged. save_story writes to default dirs; we redirect by writing copies."""
    succeeded = failed = 0
    kernel_membership = {}  # cid -> (kernel_id, reading_id) for the sidecar
    for result in client.messages.batches.results(batch_id):
        cid = result.custom_id
        if result.result.type != "succeeded":
            print(f"  FAIL {cid}: {result.result.type}")
            failed += 1
            continue
        raw = "".join(b.text for b in result.result.message.content if b.type == "text")
        story, errors = process_response(raw)
        if story is None:
            print(f"  FAIL {cid}: JSON parse — {errors[0] if errors else '?'}")
            failed += 1
            continue
        if errors:
            print(f"  FAIL {cid}: {len(errors)} validation error(s): {errors[:2]}")
            failed += 1
            continue
        # patch id if model diverged
        if story.get("header", {}).get("constraint_id") != cid:
            story.setdefault("header", {})["constraint_id"] = cid
        # write JSON to json/ and .pl to prolog/testsets/ (flat, main corpus)
        json_path, pl_path = save_story(story, overwrite=overwrite)
        if json_path is None:
            continue
        # capture kernel membership from the story's commitment fields if present
        kc = (story.get("commentary", {}) or {}).get("kernel_context", "")
        kernel_membership[cid] = {"kernel_context": kc}
        append_to_log(processed_log, cid)
        succeeded += 1
    return succeeded, failed, kernel_membership


# ---------------------------------------------------------------------------
# Step D: Coherence eyeball (manual check, not automated math — deferred per plan)
# ---------------------------------------------------------------------------

def coherence_eyeball(manifests, _json_dir, manifests_dir):
    """Per kernel: list its readings and (if generated) the claimed type each emitted.

    This is the MANUAL coherence check. Collapse signal: readings claiming the same type.
    Incoherence signal: readings with no shared substrate. We only surface; we do not gate.
    """
    report = ["# Kernel Coherence Eyeball", ""]
    for m in manifests:
        csr = m.get("commitment_system_recognition", {}) or {}
        if not csr.get("is_contested_kernel"):
            continue
        kid = csr.get("kernel_id", m.get("family_id", "?"))
        report.append(f"## {kid}")
        report.append(f"kernel: {csr.get('kernel_description','')}")
        types = []
        for r in csr.get("readings", []):
            rid = r.get("reading_id")
            # try to read the emitted constraint's claimed type
            cid = None
            for axis in m.get("generation_sequence", []):
                if isinstance(axis, dict) and axis.get("reading_id") == rid:
                    cid = axis.get("claim_id") or axis.get("constraint_id")
            claimed = "?"
            if cid:
                p = JSON_DIR / f"{cid}.json"
                if p.exists():
                    try:
                        story = json.loads(p.read_text())
                        claimed = story.get("base_properties", {}).get("claimed_type", "?")
                    except Exception:
                        pass
            types.append(claimed)
            report.append(f"  - {rid}: emitted type = {claimed}  ({r.get('commitment','')})")
        # flag signals
        nonq = [t for t in types if t != "?"]
        if len(nonq) >= 2 and len(set(nonq)) == 1:
            report.append(f"  ** COLLAPSE SIGNAL: all readings emit '{nonq[0]}' — may be one reading named many times")
        elif len(set(nonq)) >= 2:
            report.append(f"  -> distinct: readings differentiate ({set(nonq)})")
        report.append("")
    out = manifests_dir / "coherence_eyeball.md"
    out.write_text("\n".join(report), encoding="utf-8")
    print(f"\nCoherence eyeball written to {out}")
    return out


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seeds", default=str(Path(__file__).parent / "kernel_seeds.json"))
    ap.add_argument("--run-tag", default=datetime.datetime.now().strftime("run_%Y%m%d_%H%M%S"),
                    help="output namespace for manifests/log (default: timestamp)")
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--axes", type=int, default=3)
    ap.add_argument("--skip-search", action="store_true",
                    help="skip web-search grounding (recommended for historical library cases)")
    ap.add_argument("--overwrite", action="store_true")
    ap.add_argument("--dry-run", action="store_true", help="SCOPE + flatten only; no generation")
    ap.add_argument("--poll-interval", type=int, default=BATCH_POLL_INTERVAL)
    ap.add_argument("--regression-check", metavar="TOPIC",
                    help="SCOPE+generate ONE ordinary topic and stop, for the branch diff gate")
    args = ap.parse_args()

    json_dir, testsets_dir, manifests_dir, processed_log = run_dirs(args.run_tag)
    scope_prompt = _load_context_file(str(SCOPE_PROMPT_PATH))

    # Regression mode: one ordinary topic, prove the kernel frame is inert on non-kernels
    if args.regression_check:
        print(f"[regression] SCOPE on ordinary topic: {args.regression_check}")
        seed = {"human_readable": args.regression_check, "summary": "", "constraint_id": "regression_probe"}
        m, err = scope_seed(seed, scope_prompt, axes=args.axes)
        if err:
            print(f"[regression] SCOPE error: {err}")
            return
        csr = (m or {}).get("commitment_system_recognition", {}) or {}
        is_kernel = bool(csr.get("is_contested_kernel"))
        print(f"[regression] is_contested_kernel = {is_kernel} "
              f"(expected False for an ordinary topic)")
        (manifests_dir / "regression_manifest.json").write_text(json.dumps(m, indent=2))
        print(f"[regression] manifest saved; diff this against a main-branch run of the same topic")
        return

    seeds = json.loads(Path(args.seeds).read_text(encoding="utf-8"))
    # normalize: commitment_corpus seeds use kernel_id; generate_json_haiku seeds use constraint_id
    for s in seeds:
        if "constraint_id" not in s and "kernel_id" in s:
            s["constraint_id"] = s["kernel_id"]
    processed = load_processed_log(processed_log)
    seeds = [s for s in seeds if s["constraint_id"] not in processed] if processed else seeds
    if args.limit:
        seeds = seeds[:args.limit]
    print(f"SCOPEing {len(seeds)} kernel seeds (model={SCOPE_MODEL})")

    manifests = []
    for i, seed in enumerate(seeds):
        rc = "" if args.skip_search else research_seed(seed)
        m, err = scope_seed(seed, scope_prompt, research_context=rc, axes=args.axes)
        if err:
            print(f"  [{i+1}/{len(seeds)}] SCOPE FAIL {seed['constraint_id']}: {err}")
            continue
        m["_seed_id"] = seed["constraint_id"]
        manifests.append(m)
        csr = m.get("commitment_system_recognition", {}) or {}
        tag = "KERNEL" if csr.get("is_contested_kernel") else "ordinary"
        nr = len(csr.get("readings", []))
        print(f"  [{i+1}/{len(seeds)}] {seed['constraint_id']}: {tag} ({nr} readings)")
        (manifests_dir / f"{seed['constraint_id']}.manifest.json").write_text(
            json.dumps(m, indent=2, ensure_ascii=False), encoding="utf-8")

    gen_seeds = flatten_manifests(manifests)
    print(f"\nFlattened to {len(gen_seeds)} generation seeds "
          f"({sum(1 for s in gen_seeds if s['kernel_id'])} kernel-tagged)")

    if args.dry_run:
        for s in gen_seeds:
            print(f"  {s['constraint_id']:45s} kernel={s['kernel_id']} reading={s['reading_id']}")
        print(f"\nDRY RUN — {len(gen_seeds)} seeds would be batched")
        return

    # One Haiku batch for all readings across all kernels
    client = get_client()
    reqs = build_batch_requests(gen_seeds)
    print(f"\nSubmitting batch of {len(reqs)} generation requests...")
    batch = client.messages.batches.create(requests=reqs)
    print(f"Batch created: {batch.id}")
    poll_batch(client, batch.id, args.poll_interval)

    print("\nProcessing results...")
    succ, fail, membership = process_batch_results(
        client, batch.id, json_dir, testsets_dir, processed_log, overwrite=args.overwrite)
    print(f"\nGeneration: {succ} succeeded, {fail} failed of {len(gen_seeds)}")

    # Save authoritative kernel grouping sidecar (manifest is the source of truth)
    grouping = {}
    for m in manifests:
        csr = m.get("commitment_system_recognition", {}) or {}
        if csr.get("is_contested_kernel"):
            kid = csr.get("kernel_id")
            grouping[kid] = {
                "kernel_description": csr.get("kernel_description", ""),
                "readings": [
                    {"reading_id": r.get("reading_id"), "commitment": r.get("commitment")}
                    for r in csr.get("readings", [])
                ],
                "seed_id": m.get("_seed_id"),
            }
    (manifests_dir / "kernel_grouping.json").write_text(
        json.dumps(grouping, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"Kernel grouping sidecar: {len(grouping)} kernels -> {manifests_dir/'kernel_grouping.json'}")

    coherence_eyeball(manifests, json_dir, manifests_dir)
    print(f"\nRun '{args.run_tag}' complete. "
          f"Stories written to json/ and prolog/testsets/. "
          f"Manifests/log in outputs/kernel_manifests/{args.run_tag}/. "
          f"Eyeball coherence_eyeball.md before treating as corpus.")


if __name__ == "__main__":
    main()
