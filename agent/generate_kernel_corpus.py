"""Generate a kernel-aware toy corpus: SCOPE each kernel seed, flatten readings
into one batch, generate via Haiku batch API, save run-tagged, then eyeball coherence.

Merges:
  - DRAuditOrchestrator._step_decompose  (SCOPE call, Sonnet)        from agent/c-orchestrator.py
  - generate_json_haiku batch machinery  (build/poll/process batch)  from agent/generate_json_haiku.py

Pipeline:
  prolog/kernel_seeds.json
    -> [SCOPE each seed, serial, Sonnet, kernel-aware]   -> manifests (saved as sidecar)
    -> flatten readings across all manifests             -> one seed list (each carries kernel_id/reading_id)
    -> [one Haiku batch]                                 -> constraint stories
    -> save RUN-TAGGED: json/<run_tag>/ and prolog/testsets/<run_tag>/
    -> coherence eyeball: per kernel, list readings + emitted type

Safety posture:
  - ALL output is run-tagged: json/<run_tag>/, prolog/testsets/<run_tag>/,
    outputs/kernel_manifests/<run_tag>/. The main corpus is never written.
  - processed-log is run-scoped (idempotent reruns per run_tag)
  - manifests saved to outputs/kernel_manifests/<run_tag>/ as the authoritative kernel grouping
  - --regression-check mode SCOPEs one ordinary topic and stops, for the branch diff gate

Usage:
  python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json --run-tag run_01
  python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json --run-tag run_01 --limit 5 --dry-run
  python3 -m agent.generate_kernel_corpus --regression-check "Alberta separatism" --run-tag regress_01
  python3 -m agent.generate_kernel_corpus --seeds prolog/kernel_seeds.json --run-tag run_01 --skip-search
"""

import argparse
import json
import re
import sys
import time
from pathlib import Path

# Ensure repo root is on the path when invoked as a script
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import anthropic

from agent.story_generator_base import (
    PROLOG_DIR,
    PROMPT_PATH,
    SCHEMA_PATH,
    EXAMPLE_PATH,
    REPO_ROOT,
    TESTSETS_DIR,
    _SYSTEM_INSTRUCTION,
    _load_context_file,
    process_response,
    strip_json_fences,
    append_to_log,
    load_processed_log,
)

# generate_pl and lint_file live in python/ — story_generator_base adds it to sys.path on import
sys.path.insert(0, str(REPO_ROOT / "python"))
from generate_constraint_pl import generate_pl, validate_json  # noqa: E402
from linter import lint_file                    # noqa: E402

# SCOPE protocol prompt — patched on kernel-frame branch with §1.3-K and expanded CSR object
SCOPE_PROMPT_PATH = REPO_ROOT / "prompts" / "uke_scope_v2_json.md"

SCOPE_MODEL = "claude-sonnet-4-5-20250929"   # matches c-orchestrator.py architect
GEN_MODEL = "claude-haiku-4-5-20251001"      # matches generate_json_haiku.py
BATCH_POLL_INTERVAL = 30


# ---------------------------------------------------------------------------
# Output location helpers — ALL output is RUN-TAGGED, main corpus never written
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
    topic = f"{seed['human_readable']}\n\n{seed.get('summary', '')}"
    prompt = (
        f"Analyze the following topic using the UKE_SCOPE protocol.\n\n"
        f"TOPIC: {topic}\n\n"
        f"RESEARCH CONTEXT:\n{research_context}\n\n"
        f"This topic is flagged as a candidate contested kernel. Apply the kernel/reading "
        f"decomposition (SCOPE §1.3-K): decide whether it is genuinely a contested kernel, "
        f"and if so decompose into READINGS (not flat axes), emitting the expanded "
        f"commitment_system_recognition object with is_contested_kernel=true and a readings array. "
        f"For EACH reading selected for generation, emit a generation_sequence entry that is an "
        f"OBJECT (not a plain string) with exactly these three fields: "
        f"  {{\"claim_id\": \"<reading_id>\", \"kernel_id\": \"<kernel_id>\", \"reading_id\": \"<reading_id>\"}} "
        f"If it is NOT a real kernel (the readings would collapse), set is_contested_kernel=false "
        f"and decompose normally into plain string claim_ids, noting the collapse in an omega.\n\n"
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
        f"TOPIC: {seed['human_readable']}\n{seed.get('summary', '')}"
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

    Returns (gen_seeds, positional_recovery_count).
    positional_recovery_count: number of kernel-tagged seeds where the SCOPE model
    emitted a kernel_id but omitted reading_id from the generation_sequence entry.
    A nonzero count means the SCOPE prompt addendum isn't fully landing.
    """
    gen_seeds = []
    seen = set()
    recovery_count = 0

    for m in manifests:
        csr = m.get("commitment_system_recognition", {}) or {}
        is_kernel = bool(csr.get("is_contested_kernel"))
        kernel_id = csr.get("kernel_id")
        readings_by_id = {r.get("reading_id"): r for r in csr.get("readings", [])}
        family = m.get("family_id", "")
        domain = m.get("domain", "")

        for axis in m.get("generation_sequence", []):
            if isinstance(axis, str):
                cid = axis
                ax_kernel_id = None
                reading_id = None
            else:
                cid = axis.get("claim_id") or axis.get("constraint_id")
                # Use axis-level kernel_id — null means the model deliberately marked this
                # as a supplementary ordinary axis even within a kernel manifest.
                ax_kernel_id = axis.get("kernel_id")  # may be None/null
                reading_id = axis.get("reading_id")
            if not cid or cid in seen:
                continue
            seen.add(cid)

            # Recovery: axis has a kernel_id but no reading_id — model tagged the kernel
            # but forgot the reading. Null kernel_id = intentional ordinary axis; skip.
            if ax_kernel_id and not reading_id:
                recovery_count += 1
                seed_id = m.get("_seed_id", "?")
                print(f"  RECOVERY WARNING: {cid} in kernel manifest '{seed_id}' — "
                      f"kernel_id='{ax_kernel_id}' present but reading_id missing")

            # An axis is a kernel reading only when both kernel_id and reading_id are set
            is_reading = bool(ax_kernel_id and reading_id)
            reading = readings_by_id.get(reading_id, {}) if is_reading else {}
            gen_seeds.append({
                "constraint_id": cid,
                "human_readable": (reading.get("commitment") or cid).strip(),
                "topic_domain": domain,
                "family_id": family,
                "kernel_id": ax_kernel_id if is_reading else None,
                "reading_id": reading_id if is_reading else None,
                "sibling_reading_ids": reading.get("sibling_readings", []) if is_reading else [],
                "expected_structural_delta": reading.get("expected_structural_delta", ""),
                "summary": _axis_summary(axis, m),
            })
    return gen_seeds, recovery_count


def _axis_summary(axis, manifest):
    """Build a per-axis source summary the generator can use."""
    if isinstance(axis, dict):
        parts = [
            axis.get("human_readable", ""),
            axis.get("structural_delta", ""),
            f"primary observable: {axis.get('primary_observable', '')}",
            f"hypothesis: {axis.get('hypothesis', '')}",
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

    lines = [
        "=== YOUR TASK ===",
        f"Generate a complete constraint story JSON for: {gen_seed['human_readable']}",
        f"DOMAIN: {gen_seed.get('topic_domain', 'General')}",
        f"CONSTRAINT_ID: {gen_seed['constraint_id']}",
    ]
    if gen_seed.get("kernel_id"):
        lines += [
            "",
            "=== KERNEL CONTEXT (committer frame) ===",
            "This constraint is ONE READING of a contested kernel.",
            f"kernel_id: {gen_seed['kernel_id']}",
            f"reading_id (the reading you are instantiating): {gen_seed['reading_id']}",
            f"sibling readings (other constraints, NOT this one): "
            f"{', '.join(gen_seed.get('sibling_reading_ids', [])) or 'none listed'}",
            f"expected structural delta for this reading: {gen_seed.get('expected_structural_delta', '')}",
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


def strip_extra_properties(story: dict, prop_errors: list) -> dict:
    """Remove properties named in 'Additional properties are not allowed' errors.

    These are serialization-noise fields the model invented that aren't in the schema.
    Strips recursively so nested extra props are caught too.
    """
    extra = set()
    for e in prop_errors:
        for match in re.findall(r"'([^']+)'", e):
            if " " not in match:  # exclude 'was unexpected' / 'were unexpected' fragments
                extra.add(match)

    def remove_from(d):
        if isinstance(d, dict):
            return {k: remove_from(v) for k, v in d.items() if k not in extra}
        if isinstance(d, list):
            return [remove_from(item) for item in d]
        return d

    return remove_from(story)


def process_batch_results(client, batch_id, json_dir, testsets_dir, processed_log,
                          gen_seeds_by_id=None, rejections_path=None, overwrite=False):
    """Write each result to run-tagged dirs: json_dir/.json + testsets_dir/.pl.

    Error handling (two tracks):
    - Invented-property failures ("Additional properties are not allowed"): strip the
      extra fields and retry validation. If clean after strip, save normally.
    - interpretation_layer_present / anyOf schema failures: do NOT loosen the condition;
      instead log (kernel, reading, codification, authority, error) to rejections.json
      for post-run analysis of whether the rejection is over-strict or theory-correct.
    - Other failures: count and skip.

    Linting uses a temp file in flat prolog/testsets/ so dirname(dirname) resolves to
    prolog/ and finds config.pl (run-tagged subdir would resolve to prolog/testsets/).
    """
    succeeded = failed = 0
    kernel_membership = {}
    rejected = []
    gen_seeds_by_id = gen_seeds_by_id or {}

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

        if story.get("header", {}).get("constraint_id") != cid:
            story.setdefault("header", {})["constraint_id"] = cid

        if errors:
            prop_errors = [e for e in errors if "Additional properties are not allowed" in e]
            schema_errors = [e for e in errors
                             if "is not valid under any of the given schemas" in e
                             or "interpretation_layer_present" in e]
            other_errors = [e for e in errors if e not in prop_errors and e not in schema_errors]

            if prop_errors and not schema_errors and not other_errors:
                # Strip invented fields and retry — serialization noise only
                stripped = strip_extra_properties(story, prop_errors)
                retry_errors = validate_json(stripped)
                if retry_errors:
                    print(f"  FAIL {cid}: strip retry still invalid: {retry_errors[:2]}")
                    failed += 1
                    continue
                story = stripped
                props_removed = set()
                for e in prop_errors:
                    for m in re.findall(r"'([^']+)'", e):
                        if " " not in m:
                            props_removed.add(m)
                print(f"  STRIPPED {cid}: removed {sorted(props_removed)}")
                errors = []  # fall through to save

            elif schema_errors:
                # Capture rejection without saving — do NOT loosen the condition
                gen_seed = gen_seeds_by_id.get(cid, {})
                cs = (story.get("cs_structure") or {})
                rejection = {
                    "constraint_id": cid,
                    "kernel_id": gen_seed.get("kernel_id"),
                    "reading_id": gen_seed.get("reading_id"),
                    "kernel_codification": cs.get("kernel_codification"),
                    "authority_grounding": cs.get("authority_grounding"),
                    "interpretation_layer_present": cs.get("interpretation_layer_present"),
                    "schema_error": schema_errors[0],
                    "other_errors": other_errors,
                }
                rejected.append(rejection)
                print(f"  REJECTED {cid}: codification={cs.get('kernel_codification')} "
                      f"authority={cs.get('authority_grounding')} → rejections.json")
                failed += 1
                continue

            else:
                print(f"  FAIL {cid}: {len(errors)} error(s): {errors[:2]}")
                failed += 1
                continue

        out_json = json_dir / f"{cid}.json"
        out_pl = testsets_dir / f"{cid}.pl"
        if out_json.exists() and not overwrite:
            print(f"  SKIP {cid}: exists (use --overwrite)")
            continue

        pl_content = generate_pl(story)

        # lint via temp in flat testsets/ so dirname(dirname) resolves to prolog/
        tmp_path = TESTSETS_DIR / f".tmp_kernel_{cid}.pl"
        try:
            tmp_path.write_text(pl_content, encoding="utf-8")
            lint_errors = lint_file(str(tmp_path))
            if lint_errors:
                print(f"  LINT {cid} ({len(lint_errors)} warning(s)):")
                for e in lint_errors[:3]:
                    print(f"    - {e}")
        except Exception as e:
            print(f"  Linter crashed for {cid}: {e}")
        finally:
            tmp_path.unlink(missing_ok=True)

        out_json.write_text(json.dumps(story, indent=2, ensure_ascii=False), encoding="utf-8")
        out_pl.write_text(pl_content, encoding="utf-8")

        kc = (story.get("commentary", {}) or {}).get("kernel_context", "")
        kernel_membership[cid] = {"kernel_context": kc}
        append_to_log(processed_log, cid)
        succeeded += 1
        print(f"  OK {cid}")

    # Append rejections to the run's rejections.json
    if rejected and rejections_path:
        existing = []
        if rejections_path.exists():
            try:
                existing = json.loads(rejections_path.read_text(encoding="utf-8"))
            except Exception:
                pass
        rejections_path.write_text(
            json.dumps(existing + rejected, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"  {len(rejected)} rejection(s) logged → {rejections_path}")

    return succeeded, failed, kernel_membership, rejected


# ---------------------------------------------------------------------------
# Step D: Coherence eyeball (manual check — not a gate)
# ---------------------------------------------------------------------------

def coherence_eyeball(manifests, json_dir, manifests_dir):
    """Per kernel: list its readings and (if generated) the claimed type each emitted.

    Collapse signal: all readings emit the same type.
    Incoherence signal: readings share no substrate.
    """
    report = ["# Kernel Coherence Eyeball", ""]
    for m in manifests:
        csr = m.get("commitment_system_recognition", {}) or {}
        if not csr.get("is_contested_kernel"):
            continue
        kid = csr.get("kernel_id", m.get("family_id", "?"))
        report.append(f"## {kid}")
        report.append(f"kernel: {csr.get('kernel_description', '')}")
        types = []
        for r in csr.get("readings", []):
            rid = r.get("reading_id")
            cid = None
            for axis in m.get("generation_sequence", []):
                if isinstance(axis, dict) and axis.get("reading_id") == rid:
                    cid = axis.get("claim_id") or axis.get("constraint_id")
            claimed = "?"
            if cid:
                p = json_dir / f"{cid}.json"
                if p.exists():
                    try:
                        story = json.loads(p.read_text())
                        claimed = story.get("base_properties", {}).get("claimed_type", "?")
                    except Exception:
                        pass
            types.append(claimed)
            report.append(f"  - {rid}: emitted type = {claimed}  ({r.get('commitment', '')})")
        nonq = [t for t in types if t != "?"]
        if len(nonq) >= 2 and len(set(nonq)) == 1:
            report.append(f"  ** COLLAPSE SIGNAL: all readings emit '{nonq[0]}' — "
                          f"may be one reading named many times")
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
    ap.add_argument("--seeds", default=str(PROLOG_DIR / "kernel_seeds.json"))
    ap.add_argument("--run-tag", required=True, help="output namespace, e.g. run_01")
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--axes", type=int, default=3)
    ap.add_argument("--skip-search", action="store_true",
                    help="skip web-search grounding (for historical library cases)")
    ap.add_argument("--overwrite", action="store_true")
    ap.add_argument("--dry-run", action="store_true", help="SCOPE + flatten only; no generation")
    ap.add_argument("--poll-interval", type=int, default=BATCH_POLL_INTERVAL)
    ap.add_argument("--regression-check", metavar="TOPIC",
                    help="SCOPE one ordinary topic and stop, for the branch diff gate")
    args = ap.parse_args()

    json_dir, testsets_dir, manifests_dir, processed_log = run_dirs(args.run_tag)
    scope_prompt = _load_context_file(str(SCOPE_PROMPT_PATH))

    if args.regression_check:
        print(f"[regression] SCOPE on ordinary topic: {args.regression_check}")
        # Use the plain c-orchestrator user prompt — no kernel-awareness — to test
        # whether the patched SCOPE system prompt leaks kernel structure on its own.
        plain_prompt = (
            f"Analyze the following topic using the UKE_SCOPE protocol.\n\n"
            f"TOPIC: {args.regression_check}\n\n"
            f"RESEARCH CONTEXT:\n\n"
            f"Select exactly {args.axes} axes for generation.\n\n"
            f"Remember: OUTPUT ONLY valid JSON — no markdown fences, no commentary outside the JSON."
        )
        try:
            text = _call(plain_prompt, model=SCOPE_MODEL, system_instruction=scope_prompt,
                         temperature=0.2, max_tokens=8192)
            m = json.loads(strip_json_fences(text))
            err = "" if "generation_sequence" in m else "manifest missing generation_sequence"
        except Exception as e:
            m, err = None, str(e)
        if err:
            print(f"[regression] SCOPE error: {err}")
            return
        csr = (m or {}).get("commitment_system_recognition", {}) or {}
        is_kernel = bool(csr.get("is_contested_kernel"))
        print(f"[regression] is_contested_kernel = {is_kernel} "
              f"(expected False for an ordinary topic)")
        if is_kernel:
            print("[regression] FAIL — kernel frame leaked into ordinary topic")
        else:
            print("[regression] PASS")
        out = manifests_dir / "regression_manifest.json"
        out.write_text(json.dumps(m, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"[regression] manifest saved to {out}")
        return

    seeds = json.loads(Path(args.seeds).read_text(encoding="utf-8"))
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

    gen_seeds, recovery_count = flatten_manifests(manifests)
    kernel_tagged = sum(1 for s in gen_seeds if s["kernel_id"])
    print(f"\nFlattened to {len(gen_seeds)} generation seeds ({kernel_tagged} kernel-tagged)")
    if recovery_count:
        print(f"RECOVERY COUNT: {recovery_count} — SCOPE did not tag reading_id on "
              f"{recovery_count} kernel sequence entries. Inspect manifests and tighten "
              f"the SCOPE prompt before proceeding.")
    else:
        print("Recovery count: 0 (all kernel entries carry reading_id)")

    if args.dry_run:
        for s in gen_seeds:
            print(f"  {s['constraint_id']:45s} kernel={s['kernel_id']} reading={s['reading_id']}")
        print(f"\nDRY RUN — {len(gen_seeds)} seeds would be batched")
        return

    if recovery_count:
        pct = recovery_count / max(len(gen_seeds), 1) * 100
        print(f"\nRECOVERY NOTE: {recovery_count} axis/axes had kernel_id but no reading_id "
              f"({pct:.1f}% of seeds). Treated as ordinary axes. "
              f"Inspect manifests if count is large; single slips are expected.")
        # Hard stop only if >10% of seeds are affected — that would mean the prompt isn't landing
        if pct > 10:
            print("STOP: >10% recovery rate indicates SCOPE addendum not landing.")
            return

    client = get_client()
    reqs = build_batch_requests(gen_seeds)
    print(f"\nSubmitting batch of {len(reqs)} generation requests...")
    batch = client.messages.batches.create(requests=reqs)
    print(f"Batch created: {batch.id}")
    poll_batch(client, batch.id, args.poll_interval)

    gen_seeds_by_id = {s["constraint_id"]: s for s in gen_seeds}
    rejections_path = manifests_dir / "rejections.json"

    print("\nProcessing results...")
    succ, fail, membership, rejected = process_batch_results(
        client, batch.id, json_dir, testsets_dir, processed_log,
        gen_seeds_by_id=gen_seeds_by_id,
        rejections_path=rejections_path,
        overwrite=args.overwrite)
    print(f"\nGeneration: {succ} succeeded, {fail} failed, {len(rejected)} rejected "
          f"(codification/authority) of {len(gen_seeds)}")

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
    print(f"Kernel grouping: {len(grouping)} kernels -> {manifests_dir / 'kernel_grouping.json'}")

    coherence_eyeball(manifests, json_dir, manifests_dir)
    print(f"\nRun '{args.run_tag}' complete.")
    print(f"  JSON:      json/{args.run_tag}/")
    print(f"  Testsets:  prolog/testsets/{args.run_tag}/")
    print(f"  Manifests: outputs/kernel_manifests/{args.run_tag}/")
    print(f"Eyeball coherence_eyeball.md before promoting to main corpus.")


if __name__ == "__main__":
    main()
