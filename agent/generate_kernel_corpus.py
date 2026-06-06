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
import uuid
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
    build_prompt_parts,
    axis_source_desc,
    upstream_context,
)

# generate_pl and lint_file live in python/ — story_generator_base adds it to sys.path on import
sys.path.insert(0, str(REPO_ROOT / "python"))
from generate_constraint_pl import generate_pl, validate_json, _load_schema  # noqa: E402
from linter import lint_file                    # noqa: E402
from story_repair import repair_story           # noqa: E402  canonical deterministic repair

# SCOPE protocol prompt — patched on kernel-frame branch with §1.3-K and expanded CSR object
SCOPE_PROMPT_PATH = REPO_ROOT / "prompts" / "uke_scope_v2_json.md"

SCOPE_MODEL = "claude-sonnet-4-5-20250929"   # matches c-orchestrator.py architect
GEN_MODEL = "claude-haiku-4-5-20251001"      # matches generate_json_haiku.py
BATCH_POLL_INTERVAL = 30

# No-scope path: flat main corpus, ladder tracked by beta_processed.txt
JSON_DIR = REPO_ROOT / "json"
BETA_SEEDS = PROLOG_DIR / "beta_seeds.json"
BETA_PROCESSED = PROLOG_DIR / "beta_processed.txt"
MAX_CUSTOM_ID = 64  # Anthropic batch custom_id pattern: ^[a-zA-Z0-9_-]{1,64}$


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

def _scope_user_prompt(seed, research_context="", axes=None):
    """The per-kernel SCOPE user prompt — shared by serial scope_seed and the batch path
    so the two cannot drift."""
    topic = f"{seed['human_readable']}\n\n{seed.get('summary', '')}"
    return (
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
        f"AXIS BUDGET: a contested kernel counts as exactly ONE axis. Emit ALL of its readings "
        f"(as many as the kernel actually sustains — do not pad or trim to a target count) to "
        f"generation_sequence; do NOT cap the readings, and do NOT count each "
        f"reading as a separate axis. "
        + (
            f"You may select up to {max(axes - 1, 0)} ADDITIONAL ordinary "
            f"(non-kernel) axes only if the topic has genuinely independent structure beyond the "
            f"kernel; "
            if axes is not None else
            "You may select ADDITIONAL ordinary (non-kernel) axes only if the topic has "
            "genuinely independent structure beyond the kernel — every additional axis must "
            "survive the §3 pairwise independence test; "
        )
        + "for a topic that is only a kernel, the readings are the entire sequence.\n\n"
        f"OUTPUT ONLY valid JSON — no markdown fences, no commentary outside the JSON."
    )


def scope_seed(seed, scope_prompt, research_context="", axes=None):
    """Run kernel-aware SCOPE on one seed. Returns (manifest_dict | None, error)."""
    prompt = _scope_user_prompt(seed, research_context, axes)
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

    DEPENDENCY NOTE (2026-06-05): this path deliberately does NOT wave-sequence like
    c-orchestrator._step_generate_batch. Kernel readings are SIBLINGS (parallel by
    design; linkage is declared upfront via sibling_reading_ids — stateless
    generation) and flat controls are independent, so one batch is the correct
    shape. KNOWN GAP: `downstream_of` edges on ORDINARY axes inside kernel
    manifests are dropped silently — those stories get no upstream claimed_type
    context. Port the orchestrator's wave logic if kernel manifests start
    emitting such edges.

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

        # --- Seed from the DECLARED reading set, not just generation_sequence ---
        # SCOPE declares every reading in csr.readings (with full sibling_readings),
        # but the pipeline MAY cap generation_sequence (the orchestrator-era axes=3
        # default; now an optional --axes ceiling) and queue only a subset. Honoring the queue silently drops
        # declared readings: their siblings' edges then dangle and the kernel ships
        # incomplete (witnessed: marriage_authority declared 5, queued 3 — the 2
        # dropped were federalist_millet_reading and judicial_harmonization_reading).
        # The manifest's `readings` list is the authoritative intended set, so emit a
        # seed for every declared reading not already queued. This makes completion
        # automatic and cap-independent — it fixes existing manifests (their readings
        # already name the dropped siblings) and future ones in one place.
        if is_kernel and kernel_id:
            for r in csr.get("readings", []):
                rid = r.get("reading_id")
                if not rid:
                    continue
                # Bare reading_id — matches the generation_sequence claim_id convention
                # (downstream prefixes header.constraint_id to <kernel>__<reading> and
                # generate_constraint_pl derives kernel_id by split). Dedup on the bare
                # id so readings already queued by generation_sequence are not re-emitted.
                cid = rid
                if cid in seen:
                    continue
                seen.add(cid)
                delta = r.get("expected_structural_delta", "")
                commitment = (r.get("commitment") or cid).strip()
                gen_seeds.append({
                    "constraint_id": cid,
                    "human_readable": commitment,
                    "topic_domain": domain,
                    "family_id": family,
                    "kernel_id": kernel_id,
                    "reading_id": rid,
                    "sibling_reading_ids": r.get("sibling_readings", []),
                    "expected_structural_delta": delta,
                    "summary": commitment + (f"\n{delta}" if delta else ""),
                })

        # --- Forced-flat control on every kernel (OQ-76, generate-both primary fix) ---
        # The kernel/flat gate is stochastic on a measured boundary band and every miss
        # destroys the axiom axis irrecoverably (audits/2026-06-05_kernel_gate_replication/).
        # Rather than trusting the gate, every kernel topic ALSO gets a flat construction
        # of the SAME substrate: kernel-vs-flat is an apparatus perturbation and the
        # construction-pair diff is signal (the_perturbation_principle.md §7.1). The flat
        # author sees ONLY the substrate (kernel_description) — never the reading set —
        # so the two constructions stay independent. Asymmetric by design: flat-on-every-
        # kernel only; do NOT generalize to kernel-on-every-flat (most flat topics have no
        # kernel, and a flat-miss is the unrecoverable direction, not a kernel-miss).
        # Alignment key: narrative_ontology:flat_control_of/2 stamped by the compiler from
        # the ephemeral _flat_control_of (see process_batch_results / stamp_kernel_linkage).
        if is_kernel and kernel_id:
            fc_cid = f"{kernel_id}_flat_control"
            if fc_cid not in seen:
                seen.add(fc_cid)
                substrate = (csr.get("kernel_description") or "").strip() or kernel_id
                gen_seeds.append({
                    "constraint_id": fc_cid,
                    "human_readable": substrate,
                    "topic_domain": domain,
                    "family_id": family,
                    "kernel_id": None,           # NOT a reading
                    "reading_id": None,
                    "flat_control_of": kernel_id,
                    "sibling_reading_ids": [],
                    "expected_structural_delta": "",
                    "summary": substrate,
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
        sibling_ids = gen_seed.get("sibling_reading_ids", [])
        siblings_str = ", ".join(sibling_ids) or "none listed"
        lines += [
            "",
            "=== KERNEL CONTEXT (committer frame) ===",
            "This constraint is ONE READING of a contested kernel.",
            f"kernel_id: {gen_seed['kernel_id']}",
            f"reading_id (the reading you are instantiating): {gen_seed['reading_id']}",
            f"sibling readings (other constraints, NOT this one): {siblings_str}",
            f"expected structural delta for this reading: {gen_seed.get('expected_structural_delta', '')}",
            "",
            "Apply the Kernels and Readings rules (Rules 1–4):",
            "  Rule 1: Generate ONLY this reading as a clean ε-invariant constraint.",
            "  Rule 2: Route committer structure to omega variables.",
            "  Rule 3: Record the reading in commentary.kernel_context.",
            "  Rule 4: Populate cs_structure.reading_relations and cs_structure.axioms.",
            "",
            "For cs_structure.reading_relations: for EACH sibling reading listed above,",
            "declare the structural relationship from THIS reading to the sibling:",
            "  - forecloses: this reading's core premise logically rules out the sibling's",
            "    core premise in any single framework (rare — use only when one premise",
            "    directly contradicts the other such that no framework could hold both).",
            "  - coexists_with: both readings remain live positions held by different parties;",
            "    neither rules out the other within any single party's framework.",
            "  - influences: this reading creates structural downstream pressure on the sibling",
            "    (changes legitimacy conditions or resource availability) without foreclosing it.",
            "",
            "For cs_structure.axioms: declare 1–2 foundational normative claims that",
            "distinguish THIS reading from its siblings. Use snake_case atom names unique",
            "across the sibling set. Assign status: holdable (live claim), overridden",
            "(superseded within this reading's tradition), or foreclosed (ruled out by",
            "this reading's own commitments).",
        ]
    if gen_seed.get("flat_control_of"):
        lines += [
            "",
            "=== FLAT CONSTRUCTION (construction perturbation control) ===",
            "Author the commitment stated above as ONE single constraint story — the FLAT",
            "construction of this substrate.",
            "  - Do NOT decompose it into readings.",
            "  - Do NOT author cs_structure.reading_relations or cs_structure.axioms.",
            "  - You are not being shown any reading set; author from the substrate alone.",
            "Where the commitment is contested, let the contestation land where flat",
            "authoring naturally puts it: perspectival disagreement across (P,T,E,S) seats,",
            "and omegas naming the open questions. Author the claim and metrics you believe",
            "are descriptively true of the substrate as a single constraint.",
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
                "max_tokens": 16384,  # raised 2026-06-05 with the orchestrator: stories now ~6-7k tokens; batch truncation = parse-fail
                "system": system,
                "messages": build_cached_messages(s),
            },
        })
    return reqs


def build_indexed_batch_requests(gen_seeds):
    """Like build_batch_requests but with short index custom_ids (g0, g1, ...) so the
    real constraint_id — the module name / filename — can exceed the 64-char batch
    custom_id cap. Returns (requests, id_map: custom_id -> constraint_id)."""
    system = [{"type": "text", "text": _SYSTEM_INSTRUCTION, "cache_control": {"type": "ephemeral"}}]
    reqs, id_map = [], {}
    for i, s in enumerate(gen_seeds):
        cidk = f"g{i}"
        id_map[cidk] = s["constraint_id"]
        reqs.append({
            "custom_id": cidk,
            "params": {
                "model": GEN_MODEL,
                "max_tokens": 16384,  # raised 2026-06-05 with the orchestrator: stories now ~6-7k tokens; batch truncation = parse-fail
                "system": system,
                "messages": build_cached_messages(s),
            },
        })
    return reqs, id_map


def poll_batch(client, batch_id, poll_interval):
    terminal = {"ended", "canceled", "expired"}
    # Transient API errors (503 overloaded, rate limit, connection/timeout) must NOT kill a
    # long run mid-poll — the batch keeps processing server-side. Retry the poll instead.
    transient = (anthropic.InternalServerError, anthropic.APIConnectionError,
                 anthropic.RateLimitError, anthropic.APITimeoutError)
    fails = 0
    while True:
        try:
            b = client.messages.batches.retrieve(batch_id)
            fails = 0
        except transient as e:
            fails += 1
            if fails > 30:
                raise
            print(f"  transient poll error ({type(e).__name__}); retry {fails} in {poll_interval}s")
            time.sleep(poll_interval)
            continue
        c = b.request_counts
        print(f"  Batch {batch_id}: {b.processing_status} — "
              f"succeeded={c.succeeded}, errored={c.errored}, processing={c.processing}")
        if b.processing_status in terminal:
            return b
        time.sleep(poll_interval)


def strip_extra_properties(story: dict, schema: dict) -> tuple:
    """Remove extra properties at their exact JSON paths (path-aware).

    Uses jsonschema error objects with .absolute_path to remove only at the
    specific location where the extra property appears. Avoids collateral damage
    when a required field (e.g., Omega.description) has the same name as an
    extra field elsewhere in the document.

    Returns (stripped_story, sorted_list_of_removed_field_names).
    """
    import copy
    try:
        import jsonschema
    except ImportError:
        return story, []

    validator_cls = getattr(jsonschema, "Draft7Validator", jsonschema.Draft4Validator)
    error_objects = list(validator_cls(schema).iter_errors(story))

    removals = []  # (path_list, set_of_field_names)
    for e in error_objects:
        if e.validator != "additionalProperties":
            continue
        path = list(e.absolute_path)
        fields = {m for m in re.findall(r"'([^']+)'", e.message) if " " not in m}
        if fields:
            removals.append((path, fields))

    if not removals:
        return story, []

    result = copy.deepcopy(story)
    removed = set()
    for path, fields in removals:
        target = result
        try:
            for key in path:
                target = target[key]
        except (KeyError, IndexError, TypeError):
            continue
        if isinstance(target, dict):
            for f in fields:
                if f in target:
                    target.pop(f)
                    removed.add(f)

    return result, sorted(removed)


def process_batch_results(client, batch_id, json_dir, testsets_dir, processed_log,
                          gen_seeds_by_id=None, rejections_path=None, overwrite=False,
                          id_map=None):
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
    _schema = _load_schema()  # load once; used by path-aware strip

    for result in client.messages.batches.results(batch_id):
        # custom_id may be a short transport key (no-scope uses index ids because the batch
        # custom_id is capped at 64 chars); resolve back to the real constraint_id.
        cid = (id_map or {}).get(result.custom_id, result.custom_id)
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
            # Deterministic repair, then re-validate: path-aware strip (additionalProperties)
            # + comprehensive repair_story (required defaults, id transliteration, null/clamp).
            # Repair edits the story to MEET the schema — it loosens nothing. Only what is
            # still invalid after repair fails. CS anyOf/interpretation_layer rejections and
            # conditional allOf/then (claimed-type vs metrics) survive repair → fail-closed,
            # which is correct: those are semantic, and clamping them would fabricate data.
            repaired, props_removed = strip_extra_properties(story, _schema)
            repaired = repair_story(repaired, _schema)
            retry_errors = validate_json(repaired)

            if not retry_errors:
                story = repaired
                tag = f": stripped {props_removed}" if props_removed else ""
                print(f"  REPAIRED {cid}{tag}")
                errors = []  # fall through to save
            else:
                schema_errors = [e for e in retry_errors
                                 if "is not valid under any of the given schemas" in e
                                 or "interpretation_layer_present" in e]
                if schema_errors:
                    gen_seed = gen_seeds_by_id.get(cid, {})
                    cs = (repaired.get("cs_structure") or {})
                    rejected.append({
                        "constraint_id": cid,
                        "kernel_id": gen_seed.get("kernel_id"),
                        "reading_id": gen_seed.get("reading_id"),
                        "kernel_codification": cs.get("kernel_codification"),
                        "authority_grounding": cs.get("authority_grounding"),
                        "interpretation_layer_present": cs.get("interpretation_layer_present"),
                        "schema_error": schema_errors[0],
                        "other_errors": [e for e in retry_errors if e not in schema_errors],
                    })
                    print(f"  REJECTED {cid}: codification={cs.get('kernel_codification')} "
                          f"authority={cs.get('authority_grounding')} → rejections.json")
                    failed += 1
                    continue
                print(f"  FAIL {cid}: {len(retry_errors)} error(s) after repair: {retry_errors[:2]}")
                failed += 1
                continue

        out_json = json_dir / f"{cid}.json"
        out_pl = testsets_dir / f"{cid}.pl"
        if out_json.exists() and not overwrite:
            print(f"  SKIP {cid}: exists (use --overwrite)")
            continue

        # Mint story_uid FIRST — generate_pl gates the entire CS block (including
        # cs_story_uid AND cs_kernel_id) on story_uid being present. Must precede
        # _kernel_id injection or kernel_id is silently dropped when uid is absent.
        # ALWAYS overwrite: story_uid is a per-generation-event surrogate identity minted
        # by the pipeline, never authored by the content model. The model copies the
        # example's UUID, so setdefault would let duplicates through (CS validation rejects
        # the corpus: "duplicate story_uid"). Overwrite guarantees uniqueness.
        story.setdefault("header", {})["story_uid"] = str(uuid.uuid4())

        # Inject kernel_id from manifest (not from model output — preserves Rule 2).
        # The model routes committer structure to omegas; the manifest is the authoritative
        # source of which kernel this reading belongs to.
        gen_seed = gen_seeds_by_id.get(cid, {})
        manifest_kernel_id = gen_seed.get("kernel_id")
        if manifest_kernel_id:
            story["_kernel_id"] = manifest_kernel_id
        # Flat-control alignment key (OQ-76) — injected from the manifest, like _kernel_id
        manifest_fco = gen_seed.get("flat_control_of")
        if manifest_fco:
            story["_flat_control_of"] = manifest_fco

        pl_content = generate_pl(story)

        # Strip the ephemeral keys before writing the JSON (not schema fields)
        story.pop("_kernel_id", None)
        story.pop("_flat_control_of", None)

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
# Unified generation backend (2026-06-06) — the single "manifest → corpus" path both
# c-orchestrator and the --scope flow call. Heals the silent fork (Build-Discipline
# Pattern 2) that dropped recognized kernel readings (OQ-79).
#
# Design (approved plan cc-audit-brief-golden-pebble.md):
#  - SEED TYPES, dispatched by builder (kept polymorphism, not deletable debt):
#      flat axis     -> c-orchestrator framing (axis_source_desc + build_prompt_parts)
#      reading/control -> gkc framing (build_cached_messages)
#  - FLAT seeds are resolved against manifest["axes"] (NOT flatten_manifests, which never
#    looks up axes) and carry the axis dict + manifest ref, because axis_source_desc reads
#    structural_delta/primary_observable/hypothesis/beneficiary/victim from the axis and
#    domain/csr from the manifest, and upstream_context reads axis.downstream_of.
#  - The wave loop is c-orchestrator's _step_generate_batch loop, generalized so an item is
#    a seed: for a PURE-FLAT manifest the seed set + per-item build reduce EXACTLY to
#    c-orchestrator's behavior (W1/W2 byte-identity). Readings/controls have no
#    downstream_of -> always wave 1, so only flat seeds inject upstream context.
#  - Request DEFAULTS (model/max_tokens/system/temperature) are caller-supplied: c-orch
#    passes sonnet + string system; gkc passes haiku + list system. Necessary for
#    byte-identity and correct (the two front-ends genuinely use different models).
# ---------------------------------------------------------------------------

def _flat_seeds_from_manifest(m):
    """Resolve FLAT (ordinary) axes the way c-orchestrator does: from manifest['axes'] via
    generation_sequence order, skipping dups and entries with no matching axis (those are
    kernel readings, handled by flatten_manifests). Carries the axis dict + manifest ref so
    axis_source_desc/upstream_context get exactly what they read."""
    axes_by_id = {a["claim_id"]: a for a in m.get("axes", [])}
    seeds, seen = [], set()
    for entry in m.get("generation_sequence", []):
        cid = entry if isinstance(entry, str) else (entry.get("claim_id") or entry.get("constraint_id"))
        if not cid or cid in seen:
            continue
        seen.add(cid)
        axis = axes_by_id.get(cid)
        if not axis:
            continue  # kernel reading — not a flat axis; flatten_manifests emits it
        seeds.append({
            "seed_type": "flat",
            "constraint_id": cid,
            "_axis": axis,
            "_manifest": m,
            "downstream_of": axis.get("downstream_of") or [],
        })
    return seeds


def _seed_messages(seed, generated_by_id):
    """Dispatch request MESSAGES by seed type — the flat/kernel framing polymorphism.
    Flat path is byte-identical to c-orchestrator's _step_generate_batch assembly."""
    if seed.get("seed_type") == "flat":
        m, cid, axis = seed["_manifest"], seed["constraint_id"], seed["_axis"]
        source_desc = axis_source_desc(m, cid, axis)
        ctx = upstream_context(axis, generated_by_id, cid)
        static_prefix, dynamic_tail = build_prompt_parts(source_desc, ctx)
        return [{"role": "user", "content": [
            {"type": "text", "text": static_prefix, "cache_control": {"type": "ephemeral"}},
            {"type": "text", "text": dynamic_tail},
        ]}]
    # reading / flat_control -> gkc framing (handles KERNEL CONTEXT / FLAT CONSTRUCTION blocks)
    return build_cached_messages(seed)


def generate_from_manifests(manifests, json_dir, testsets_dir, processed_log, *,
                            model, max_tokens, system, temperature,
                            rejections_path=None, manifest_file=None, progress=None):
    """The unified backend. Returns (succeeded_ids, failed_ids, fail_reasons)."""
    progress = progress or (lambda *a, **k: None)
    client = get_client()

    # --- Seed set: flat axes (c-orch resolution) + kernel readings/controls (flatten) ---
    flat_seeds = []
    for m in manifests:
        flat_seeds.extend(_flat_seeds_from_manifest(m))
    kr_all, recovery = flatten_manifests(manifests)
    kr_seeds = []
    for s in kr_all:
        if s.get("kernel_id"):
            s["seed_type"] = "reading"
        elif s.get("flat_control_of"):
            s["seed_type"] = "flat_control"
        else:
            continue  # flatten's own flat seeds — we build flat seeds ourselves (byte-identity)
        s["downstream_of"] = []  # readings/controls are wave-1 (siblings/independent)
        kr_seeds.append(s)
    if recovery:
        progress("generate", f"RECOVERY: {recovery} kernel entries missing reading_id")
    all_seeds = flat_seeds + kr_seeds   # flat first; pure-flat -> identical to c-orch items order

    # Retry-the-gaps: a frozen-manifest run skips seeds whose story already landed.
    if manifest_file:
        all_seeds = [s for s in all_seeds
                     if not (json_dir / f"{s['constraint_id']}.json").exists()]

    run_ids = {s["constraint_id"] for s in all_seeds}
    generated_by_id = {}
    succeeded_ids, failed_ids, fail_reasons = set(), set(), {}

    # --- Wave loop (generalized c-orchestrator._step_generate_batch) ---
    remaining = list(all_seeds)
    wave_no = 0
    while remaining:
        wave_no += 1
        wave = [s for s in remaining
                if not any(u in run_ids and u not in generated_by_id and u not in failed_ids
                           for u in (s.get("downstream_of") or []))]
        if not wave:
            progress("generate", f"Wave {wave_no}: dependency cycle among "
                                 f"{[s['constraint_id'] for s in remaining]} — generating without upstream")
            wave = remaining
        remaining = [s for s in remaining if s not in wave]

        requests, idmap = [], {}
        for idx, seed in enumerate(wave):
            cid = seed["constraint_id"]
            custom_id = f"w{wave_no}i{idx}"
            idmap[custom_id] = cid
            requests.append({"custom_id": custom_id, "params": {
                "model": model, "max_tokens": max_tokens, "temperature": temperature,
                "system": system, "messages": _seed_messages(seed, generated_by_id)}})

        progress("generate", f"Wave {wave_no}: batch of {len(requests)} ({', '.join(idmap.values())})")
        try:
            batch = client.messages.batches.create(requests=requests)
            poll_batch(client, batch.id, 15)
        except Exception as e:
            progress("generate", f"Wave {wave_no} batch failed: {e}")
            for c in idmap.values():
                failed_ids.add(c); fail_reasons[c] = f"wave batch error: {e}"[:300]
            continue

        wave_by_id = {s["constraint_id"]: s for s in wave}
        process_batch_results(client, batch.id, json_dir, testsets_dir, processed_log,
                              gen_seeds_by_id=wave_by_id, rejections_path=rejections_path,
                              overwrite=True, id_map=idmap)
        # Re-read saved stories for downstream-wave upstream context + success accounting.
        for cid in idmap.values():
            p = json_dir / f"{cid}.json"
            if p.exists():
                succeeded_ids.add(cid)
                try:
                    generated_by_id[cid] = json.loads(p.read_text(encoding="utf-8"))
                except (json.JSONDecodeError, OSError):
                    pass
            else:
                failed_ids.add(cid); fail_reasons.setdefault(cid, "no story saved (see process log)")

    # --- OQ-79 guard, demoted to a DEFENSIVE ASSERTION (post-merge meaning) ---
    # Pre-merge, a recognized kernel's readings were silently dropped because c-orchestrator
    # resolved only manifest["axes"]. Now readings are seeds, so they are always accounted for
    # (succeeded or failed like any seed). A reading that is in NEITHER never became a seed —
    # that is a BACKEND BUG (flatten_manifests regression), not a retryable generation failure:
    # warn loudly, do NOT write the failures ledger (it is not a re-run-able drop).
    accounted = succeeded_ids | failed_ids
    for m in manifests:
        csr = m.get("commitment_system_recognition") or {}
        if not csr.get("is_contested_kernel"):
            continue
        for r in csr.get("readings", []):
            rid = r.get("reading_id")
            if rid and rid not in accounted:
                progress("generate",
                         f"⚠ BACKEND BUG (OQ-79 defensive guard): recognized reading {rid!r} of "
                         f"kernel {csr.get('kernel_id')!r} never became a seed — flatten_manifests "
                         f"regression; not a retryable failure.")

    # --- Post-steps (gkc's existing kernel machinery; no-ops on a pure-flat run) ---
    stamp_kernel_linkage(kr_seeds, json_dir, testsets_dir)
    validate_reading_relation_integrity(kr_seeds, json_dir, testsets_dir)
    emit_axiom_contradiction_facts(manifests, json_dir, testsets_dir)

    return succeeded_ids, failed_ids, fail_reasons


# ---------------------------------------------------------------------------
# Step D: Kernel-linkage join (post-batch stamp)
# ---------------------------------------------------------------------------

def _extract_cs_fact(pl_text, functor, cid):
    """Return the first positional arg of narrative_ontology:functor(cid, <ARG>)., or None."""
    m = re.search(
        rf"narrative_ontology:{re.escape(functor)}\({re.escape(cid)},\s*'?([^')]+)'?\)",
        pl_text,
    )
    return m.group(1).strip() if m else None


def stamp_kernel_linkage(gen_seeds, json_dir, testsets_dir):
    """Post-batch: ensure every generated story has cs_story_uid; stamp cs_kernel_id
    on contested readings.  Idempotent: already-correct files are skipped.

    Policy:
      is_contested_kernel == True  -> write cs_kernel_id (even if only reading so far)
      is_contested_kernel == False -> write NO cs_kernel_id; ensure cs_story_uid only
    """
    seed_map = {
        s["constraint_id"]: (s.get("kernel_id"), bool(s.get("kernel_id")),
                             s.get("flat_control_of"))
        for s in gen_seeds
    }

    n_stamped = n_standalone = n_skipped = n_uuid_minted = n_flat_stamped = 0
    producer_gaps = []
    no_cs_structure = []  # has .pl + .json but json lacks cs_structure — cannot stamp via generate_pl

    for cid, (kernel_id, is_contested, fco) in seed_map.items():
        pl_path = testsets_dir / f"{cid}.pl"
        if not pl_path.exists():
            producer_gaps.append(cid)
            continue

        pl_text = pl_path.read_text(encoding="utf-8")
        existing_uid = _extract_cs_fact(pl_text, "cs_story_uid", cid)
        existing_kid = _extract_cs_fact(pl_text, "cs_kernel_id", cid)
        existing_fco = _extract_cs_fact(pl_text, "flat_control_of", cid) if fco else None

        # Collision guard: manifest says one kernel, file says another
        if is_contested and existing_kid and existing_kid != kernel_id:
            sys.exit(
                f"KERNEL_ID MISMATCH in {pl_path}:\n"
                f"  file has:     {existing_kid}\n"
                f"  manifest has: {kernel_id}\n"
                f"This is a real collision — do not auto-overwrite. Resolve manually."
            )
        # Same guard for the construction-pair alignment key (OQ-76)
        if fco and existing_fco and existing_fco != fco:
            sys.exit(
                f"FLAT_CONTROL_OF MISMATCH in {pl_path}:\n"
                f"  file has:     {existing_fco}\n"
                f"  manifest has: {fco}\n"
                f"This is a real collision — do not auto-overwrite. Resolve manually."
            )

        # Already complete?
        uid_ok = existing_uid is not None
        kernel_ok = (not is_contested) or (existing_kid is not None)
        fco_ok = (not fco) or (existing_fco is not None)
        if uid_ok and kernel_ok and fco_ok:
            n_skipped += 1
            continue

        # Load story JSON to regenerate
        json_path = json_dir / f"{cid}.json"
        if not json_path.exists():
            producer_gaps.append(cid)
            continue

        story = json.loads(json_path.read_text(encoding="utf-8"))

        # generate_pl gates the entire CS block on cs_structure being present.
        # If the story JSON lacks cs_structure (model failed or pre-CS story),
        # stamping via generate_pl is impossible — report and skip. EXCEPTION:
        # flat_control_of emission is independent of the CS block, so a flat
        # control missing only its alignment key is stamped regardless.
        if not story.get("cs_structure"):
            if not (fco and not fco_ok):
                no_cs_structure.append(cid)
                continue

        # Mint story_uid into JSON if absent
        if not story.get("header", {}).get("story_uid"):
            minted = str(uuid.uuid4())
            story.setdefault("header", {})["story_uid"] = minted
            json_path.write_text(
                json.dumps(story, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
            )
            n_uuid_minted += 1

        # Build generation copy with ephemeral keys
        story_for_gen = dict(story)
        story_for_gen.pop("_kernel_id", None)
        story_for_gen.pop("_flat_control_of", None)
        if is_contested:
            story_for_gen["_kernel_id"] = kernel_id
        if fco:
            story_for_gen["_flat_control_of"] = fco

        new_pl = generate_pl(story_for_gen)
        pl_path.write_text(new_pl, encoding="utf-8")

        if fco:
            n_flat_stamped += 1
        elif is_contested:
            n_stamped += 1
        else:
            n_standalone += 1

    print(f"\nKernel linkage stamp ({testsets_dir.name}):")
    print(f"  {n_stamped:3d} stamped with kernel_id")
    print(f"  {n_flat_stamped:3d} flat controls stamped with flat_control_of")
    print(f"  {n_standalone:3d} standalone (contested=false, uid ensured)")
    print(f"  {n_skipped:3d} already-correct (skipped)")
    print(f"  {n_uuid_minted:3d} UUID minted")
    if no_cs_structure:
        print(f"\n  NO-CS-STRUCTURE ({len(no_cs_structure)}) — json lacks cs_structure; cannot stamp via generate_pl:")
        for cid in no_cs_structure:
            print(f"    {cid}")
    if producer_gaps:
        print(f"\n  PRODUCER GAPS — story in gen_seeds but no .pl or .json ({len(producer_gaps)}):")
        for cid in producer_gaps:
            print(f"    {cid}")


def validate_reading_relation_integrity(gen_seeds, json_dir, testsets_dir):
    """Hard-fail referential integrity for cs_reading_relation targets (OQ-58).

    Every generated edge's target must resolve to a DECLARED reading in the same
    kernel — i.e. the canonical reading file ``<kernel>__<short>.pl`` must exist on
    disk (the authoritative manifest). There is NO plausible-form escape hatch: a
    well-formed but absent ``<kernel>__<name>`` fails exactly like a typo, because a
    well-formed string is not evidence the reading was intended (that evidence is in
    the source narrative, not the edge). Unresolved edges are routed to the
    QUARANTINE bucket — reported and written for a reviewed disposition — never
    silently attached, auto-rewritten, or pre-classified as missing-vs-typo. This is
    the flow guard that stops the name-form / dangling under-count recurring at the
    source; the disposition policy is ISSUES.md OQ-58, the stock view is
    cs_kernel_registry:cs_reading_relation_unresolved/4.
    """
    quarantine = []
    for s in gen_seeds:
        kernel_id = s.get("kernel_id")
        if not kernel_id:
            continue
        cid = s["constraint_id"]
        jp = json_dir / f"{cid}.json"
        if not jp.exists():
            continue
        try:
            cs = (json.loads(jp.read_text(encoding="utf-8")).get("cs_structure") or {})
        except (json.JSONDecodeError, OSError):
            continue
        for rr in cs.get("reading_relations") or []:
            target = rr.get("sibling_id", "")
            rel = rr.get("relation", "")
            canon = target if "__" in target else f"{kernel_id}__{target}"
            if not (testsets_dir / f"{canon}.pl").exists():
                quarantine.append({"kernel": kernel_id, "source": cid,
                                   "target": target, "canonical": canon, "relation": rel})

    print(f"\nReading-relation referential integrity ({testsets_dir.name}):")
    print(f"  {len(quarantine):3d} UNRESOLVED edge(s) -> quarantine "
          f"(reviewed disposition, OQ-58; no auto-write)")
    if quarantine:
        # Run-scoped quarantine for run-tagged input: testsets_dir.parent of a
        # run-tagged dir IS the flat corpus, so the old unconditional `.parent`
        # silently clobbered any flat-corpus quarantine (KNOWN_STATE 2026-06-04,
        # OQ-71 pilot). Flat input keeps the original path.
        if testsets_dir.resolve() == TESTSETS_DIR.resolve():
            qf = testsets_dir.parent / "cs_reading_relation_quarantine.json"
        else:
            qf = testsets_dir / "cs_reading_relation_quarantine.json"
        qf.write_text(json.dumps(quarantine, indent=2, ensure_ascii=False) + "\n",
                      encoding="utf-8")
        print(f"  quarantine written: {qf}")
        for q in quarantine[:10]:
            print(f"    {q['kernel']} :: {q['source']} -{q['relation']}-> {q['target']}")
        if len(quarantine) > 10:
            print(f"    ... +{len(quarantine) - 10} more")
    return quarantine


# ---------------------------------------------------------------------------
# Step E: Coherence eyeball (manual check — not a gate)
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
# Step E: Emit axiom contradiction facts (post-generation)
# ---------------------------------------------------------------------------

def _get_foundational_axiom(json_dir, constraint_id):
    """Return the first foundational axiom atom from a generated JSON, or None."""
    p = json_dir / f"{constraint_id}.json"
    if not p.exists():
        return None
    try:
        story = json.loads(p.read_text(encoding="utf-8"))
        for axiom in (story.get("cs_structure") or {}).get("axioms", []):
            if axiom.get("role") == "foundational":
                return axiom.get("atom")
    except Exception:
        return None
    return None


def emit_axiom_contradiction_facts(manifests, json_dir, testsets_dir):
    """After generation, emit cs_axiom_contradiction/2 facts per kernel.

    Reads axiom_contradictions from SCOPE manifest (independent authored signal).
    Looks up foundational atom from each reading's generated JSON.
    Writes <kernel_id>_contradictions.pl to testsets_dir.
    Does NOT derive contradiction from forecloses edges.

    Returns (files_written, total_declared, dropped_reading_failed, dropped_no_axiom).
    The drop counts are the sweep denominator: a low firing count may mean readings
    failed validation, not that kernels don't contradict — report the denominator.

    Known under-detection: the strict test ("would A require B false") catches
    logical-negation contradictions but may decline on operatively-incompatible-but-
    not-logically-negating readings (madhhab / licensed-plurality signature). If the
    corpus shows zero licensed-plurality signals, first hypothesis is "strict test under-
    fired on soft contradictions," not "no licensed plurality exists." Flag in sweep.
    """
    files_written = 0
    total_declared = 0
    dropped_reading_failed = 0
    dropped_no_axiom = 0

    for m in manifests:
        csr = m.get("commitment_system_recognition", {}) or {}
        if not csr.get("is_contested_kernel"):
            continue
        kernel_id = csr.get("kernel_id")
        pairs = csr.get("axiom_contradictions", [])
        if not pairs:
            continue

        reading_to_cid = {}
        for axis in m.get("generation_sequence", []):
            if isinstance(axis, dict) and axis.get("reading_id"):
                cid = axis.get("claim_id") or axis.get("constraint_id")
                if cid:
                    reading_to_cid[axis["reading_id"]] = cid

        facts = []
        basis_comments = []
        for pair in pairs:
            total_declared += 1
            rid_a, rid_b = pair.get("reading_a"), pair.get("reading_b")
            cid_a = reading_to_cid.get(rid_a)
            cid_b = reading_to_cid.get(rid_b)

            # Reading failed schema validation — no generated JSON exists
            if not cid_a or not (json_dir / f"{cid_a}.json").exists() \
                    or not cid_b or not (json_dir / f"{cid_b}.json").exists():
                dropped_reading_failed += 1
                print(f"  DROP (reading-failed) {rid_a}↔{rid_b} in {kernel_id}: "
                      f"no generated JSON for one or both readings")
                continue

            atom_a = _get_foundational_axiom(json_dir, cid_a)
            atom_b = _get_foundational_axiom(json_dir, cid_b)
            if not atom_a or not atom_b:
                dropped_no_axiom += 1
                print(f"  DROP (no-axiom) {rid_a}↔{rid_b} in {kernel_id}: "
                      f"no foundational axiom in generated JSON")
                continue

            facts.append(f"narrative_ontology:cs_axiom_contradiction({atom_a}, {atom_b}).")
            facts.append(f"narrative_ontology:cs_axiom_contradiction({atom_b}, {atom_a}).")
            if pair.get("basis"):
                basis_comments.append(f"% {rid_a}↔{rid_b}: {pair['basis']}")

        if not facts:
            continue

        out = testsets_dir / f"{kernel_id}_contradictions.pl"
        lines = [
            f"% Axiom contradictions for kernel: {kernel_id}",
            "% Source: SCOPE axiom_contradictions declaration (independent of edge types).",
            "% contradiction + coexists_with edge = licensed plurality",
            "% contradiction + forecloses edge    = real closure",
            "",
            ":- multifile narrative_ontology:cs_axiom_contradiction/2.",
            "",
        ]
        if basis_comments:
            lines += basis_comments + [""]
        lines += facts
        out.write_text("\n".join(lines) + "\n", encoding="utf-8")
        print(f"  Axiom contradictions: {len(facts)//2} pair(s) → {out.name}")
        files_written += 1

    return files_written, total_declared, dropped_reading_failed, dropped_no_axiom


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Decompose mode: batch-SCOPE kernels -> reading SEEDS (constraint-story seeds)
# ---------------------------------------------------------------------------

READINGS_POOL = PROLOG_DIR / "kernel_readings_pool.json"
DECOMPOSE_DIR = REPO_ROOT / "outputs" / "decompose"


def build_scope_batch_requests(kernel_seeds, scope_prompt, axes):
    """One Sonnet SCOPE request per kernel; cache the (large, shared) scope prompt."""
    system = [{"type": "text", "text": scope_prompt, "cache_control": {"type": "ephemeral"}}]
    reqs, idmap = [], {}
    for i, seed in enumerate(kernel_seeds):
        cidk = f"k{i}"
        idmap[cidk] = seed
        reqs.append({"custom_id": cidk, "params": {
            "model": SCOPE_MODEL, "max_tokens": 8192, "system": system,
            "messages": [{"role": "user", "content": _scope_user_prompt(seed, "", axes)}]}})
    return reqs, idmap


def run_decompose(args):
    """Batch-SCOPE kernels into reading SEEDS and append them to the readings pool.

    Emits constraint-story SEEDS (constraint_id=<kernel_id>__<reading_id>, kernel_id,
    reading_id, siblings, summary) — NOT full stories. The no-scope generator turns the
    seeds into stories in a later step. Idempotent: decomposed kernel_ids are logged."""
    DECOMPOSE_DIR.mkdir(parents=True, exist_ok=True)
    manifests_sidecar = DECOMPOSE_DIR / "manifests"
    manifests_sidecar.mkdir(exist_ok=True)
    decomposed_log = DECOMPOSE_DIR / "decomposed.txt"

    kernels = json.loads(Path(args.decompose).read_text(encoding="utf-8"))
    done = load_processed_log(decomposed_log)
    pending = [k for k in kernels if k.get("kernel_id") and k["kernel_id"] not in done]
    if args.n and args.n > 0:
        pending = pending[:args.n]
    if not pending:
        print("No undecomposed kernels remaining.")
        return
    print(f"Decomposing {len(pending)} kernels (model={SCOPE_MODEL})")

    scope_prompt = _load_context_file(str(SCOPE_PROMPT_PATH))
    client = get_client()
    reqs, idmap = build_scope_batch_requests(pending, scope_prompt, args.axes)
    batch = client.messages.batches.create(requests=reqs)
    print(f"SCOPE batch created: {batch.id}")
    poll_batch(client, batch.id, args.poll_interval)

    manifests, scope_fail = [], 0
    for result in client.messages.batches.results(batch.id):
        seed = idmap.get(result.custom_id, {})
        kid = seed.get("kernel_id", result.custom_id)
        if result.result.type != "succeeded":
            print(f"  SCOPE FAIL {kid}: {result.result.type}")
            scope_fail += 1
            continue
        raw = "".join(b.text for b in result.result.message.content if b.type == "text")
        try:
            m = json.loads(strip_json_fences(raw))
        except json.JSONDecodeError as e:
            print(f"  SCOPE PARSE FAIL {kid}: {e}")
            scope_fail += 1
            continue
        if "generation_sequence" not in m:
            print(f"  SCOPE no generation_sequence {kid}")
            scope_fail += 1
            continue
        m["_seed_id"] = kid
        csr = m.get("commitment_system_recognition", {}) or {}
        tag = "KERNEL" if csr.get("is_contested_kernel") else "collapsed"
        print(f"  {kid}: {tag} ({len(csr.get('readings', []))} readings)")
        manifests.append(m)
        (manifests_sidecar / f"{kid}.manifest.json").write_text(
            json.dumps(m, indent=2, ensure_ascii=False), encoding="utf-8")
        append_to_log(decomposed_log, kid)

    gen_seeds, recovery = flatten_manifests(manifests)
    # Namespace kernel-reading ids so the same reading name can't collide across kernels.
    for s in gen_seeds:
        if s.get("kernel_id") and s.get("reading_id"):
            s["constraint_id"] = f"{s['kernel_id']}__{s['reading_id']}"

    pool = json.loads(READINGS_POOL.read_text(encoding="utf-8")) if READINGS_POOL.exists() else []
    existing = {s["constraint_id"] for s in pool}
    added = [s for s in gen_seeds if s["constraint_id"] not in existing]
    pool.extend(added)
    READINGS_POOL.write_text(json.dumps(pool, indent=2, ensure_ascii=False) + "\n",
                             encoding="utf-8")
    print(f"\nDecomposed {len(manifests)} kernels ({scope_fail} failed) -> "
          f"{len(gen_seeds)} reading seeds ({len(added)} new) -> {READINGS_POOL.name} "
          f"(pool now {len(pool)})")
    if recovery:
        print(f"  recovery count {recovery} (kernel_id present but reading_id missing) — "
              f"inspect manifests if large")


# ---------------------------------------------------------------------------
# No-scope mode: consume a SEED POOL (readings + plain) -> full stories, flat corpus
# ---------------------------------------------------------------------------

def unique_constraint_id(base, registry):
    """Collision-proof by construction: readable base, else base__<uuid8>."""
    if base not in registry:
        return base
    cand = f"{base}__{uuid.uuid4().hex[:8]}"
    while cand in registry:
        cand = f"{base}__{uuid.uuid4().hex[:8]}"
    return cand


def run_no_scope(args):
    """Generate full stories from a seed pool (reading-seeds and/or plain seeds), writing
    flat into prolog/testsets/ + json/, advancing the beta_processed.txt ladder. The
    trailing N = next N UNPROCESSED seeds; failures retried up to 3x then logged."""
    seeds = json.loads(Path(args.seeds).read_text(encoding="utf-8"))
    for s in seeds:
        if "constraint_id" not in s:
            if s.get("kernel_id") and s.get("reading_id"):
                s["constraint_id"] = f"{s['kernel_id']}__{s['reading_id']}"
            elif s.get("kernel_id"):
                s["constraint_id"] = s["kernel_id"]

    # OQ-71: optional run-tagged routing. Request CONSTRUCTION is identical either
    # way (gate-witnessed payload identity); only output destinations and the
    # idempotence ladder differ. Run-tagged output is glob-isolated from the main
    # corpus (corpus_loader's non-recursive testsets/*.pl glob).
    if getattr(args, "run_tag", None):
        json_dir, testsets_dir, manifests_dir, processed_log = run_dirs(args.run_tag)
        out_dir = manifests_dir
    else:
        json_dir, testsets_dir, processed_log = JSON_DIR, TESTSETS_DIR, BETA_PROCESSED
        out_dir = REPO_ROOT / "outputs" / "no_scope_runs"

    processed = load_processed_log(processed_log)
    pending = [s for s in seeds if s["constraint_id"] not in processed]
    n = args.n if (args.n and args.n > 0) else len(pending)
    batch_seeds = pending[:n]
    if not batch_seeds:
        print("No unprocessed seeds to generate.")
        return

    # Collision-proof naming, checked against the live corpus + ladder. The constraint_id
    # is the module name / filename and may exceed 64 chars (the batch custom_id cap applies
    # only to the transport key, which is a short index — see build_indexed_batch_requests).
    # Run-tagged runs check BOTH the main corpus and their own dir (ids must stay
    # globally unique so a later overlay/flatten cannot collide).
    registry = ({p.stem for p in TESTSETS_DIR.glob("*.pl")}
                | {p.stem for p in testsets_dir.glob("*.pl")}
                | set(processed))
    final_seeds = []
    for s in batch_seeds:
        fid = unique_constraint_id(s["constraint_id"], registry)
        s["constraint_id"] = fid
        registry.add(fid)
        final_seeds.append(s)

    json_dir.mkdir(parents=True, exist_ok=True)
    out_dir.mkdir(parents=True, exist_ok=True)
    client = get_client()

    remaining = final_seeds
    for attempt in range(1, 4):
        gen_by_id = {s["constraint_id"]: s for s in remaining}
        reqs, id_map = build_indexed_batch_requests(remaining)
        print(f"\n[attempt {attempt}/3] submitting {len(reqs)} generation requests...")
        batch = client.messages.batches.create(requests=reqs)
        print(f"  batch {batch.id}")
        poll_batch(client, batch.id, args.poll_interval)
        process_batch_results(
            client, batch.id, json_dir, testsets_dir, processed_log,
            gen_seeds_by_id=gen_by_id,
            rejections_path=out_dir / "rejections.json",
            # overwrite=True: the no-scope rebuild's idempotence source is the
            # processed ladder (pending is already filtered by it above), NOT
            # json-file existence. json/ still holds the PRE-REBUILD corpus (~4067 files),
            # so plain seeds reuse archive ids whose stale json/<id>.json exists — without
            # overwrite they hit the out_json.exists() SKIP and silently never save.
            overwrite=True, id_map=id_map)
        done = load_processed_log(processed_log)
        remaining = [s for s in remaining if s["constraint_id"] not in done]
        if not remaining:
            break
        print(f"  {len(remaining)} still failing after attempt {attempt}")

    if remaining:
        (out_dir / "failures.json").write_text(
            json.dumps(remaining, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"\nFAILURES: {len(remaining)} seeds unresolved after 3 attempts -> "
              f"{out_dir / 'failures.json'}")
    succeeded = len(final_seeds) - len(remaining)
    print(f"\nNo-scope run complete: {succeeded}/{len(final_seeds)} generated into "
          f"{testsets_dir.relative_to(REPO_ROOT)} (ladder: {processed_log.name}).")


def main():
    ap = argparse.ArgumentParser(
        description="Default mode: no-scope generation from a seed pool into flat "
                    "prolog/testsets/. Use --decompose to batch-SCOPE kernels into reading "
                    "seeds, or --scope for the legacy serial-SCOPE+generate (run-tagged).")
    ap.add_argument("n", nargs="?", type=int, default=0,
                    help="number of items to process (next N unprocessed): seeds in "
                         "no-scope mode, kernels in --decompose mode. 0 = all.")
    ap.add_argument("--seeds", default=None,
                    help="seed pool (default: beta_seeds.json no-scope, kernel_seeds.json --scope)")
    ap.add_argument("--decompose", metavar="KERNELS_JSON", default=None,
                    help="batch-SCOPE these kernels into reading seeds (appends to pool) and stop")
    ap.add_argument("--scope", action="store_true",
                    help="legacy serial-SCOPE + generate, run-tagged (requires --run-tag)")
    ap.add_argument("--run-tag", default=None, help="output namespace for --scope mode")
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--axes", type=int, default=None,
                    help="Optional budget ceiling on additional non-kernel axes "
                         "(default: none — SCOPE §3 distinctness decides)")
    ap.add_argument("--skip-search", action="store_true",
                    help="skip web-search grounding (for historical library cases)")
    ap.add_argument("--overwrite", action="store_true")
    ap.add_argument("--dry-run", action="store_true", help="SCOPE + flatten only; no generation")
    ap.add_argument("--poll-interval", type=int, default=BATCH_POLL_INTERVAL)
    ap.add_argument("--regression-check", metavar="TOPIC",
                    help="SCOPE one ordinary topic and stop, for the branch diff gate")
    args = ap.parse_args()

    # --- Mode dispatch ---
    if args.decompose:
        run_decompose(args)
        return
    if not args.scope and not args.regression_check:
        if args.seeds is None:
            args.seeds = str(BETA_SEEDS)
        run_no_scope(args)
        return

    # --- legacy --scope / --regression-check flow (run-tagged) ---
    if not args.run_tag:
        ap.error("--scope and --regression-check require --run-tag")
    if args.seeds is None:
        args.seeds = str(PROLOG_DIR / "kernel_seeds.json")
    json_dir, testsets_dir, manifests_dir, processed_log = run_dirs(args.run_tag)
    scope_prompt = _load_context_file(str(SCOPE_PROMPT_PATH))

    if args.regression_check:
        print(f"[regression] SCOPE on ordinary topic: {args.regression_check}")
        # Use the plain c-orchestrator user prompt — no kernel-awareness — to test
        # whether the patched SCOPE system prompt leaks kernel structure on its own.
        # Keep mirrored with c-orchestrator._step_decompose's axes_instruction.
        if args.axes is None:
            axes_instruction = (
                "Select every axis that survives the §3 pairwise independence test. "
                "Do not pad to a fixed count and do not truncate distinct axes to fit one."
            )
        else:
            axes_instruction = (
                f"Select every axis that survives the §3 pairwise independence test, "
                f"up to a budget ceiling of {args.axes} axes. Do not pad to reach the ceiling."
            )
        plain_prompt = (
            f"Analyze the following topic using the UKE_SCOPE protocol.\n\n"
            f"TOPIC: {args.regression_check}\n\n"
            f"RESEARCH CONTEXT:\n\n"
            f"{axes_instruction}\n\n"
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
            extra = f" FLAT_CONTROL_OF={s['flat_control_of']}" if s.get("flat_control_of") else ""
            print(f"  {s['constraint_id']:45s} kernel={s['kernel_id']} reading={s['reading_id']}{extra}")
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

    print("\nStamping kernel linkage...")
    stamp_kernel_linkage(gen_seeds, json_dir, testsets_dir)
    validate_reading_relation_integrity(gen_seeds, json_dir, testsets_dir)

    coherence_eyeball(manifests, json_dir, manifests_dir)

    print("\nEmitting axiom contradiction facts...")
    n_files, n_declared, n_drop_fail, n_drop_axiom = emit_axiom_contradiction_facts(
        manifests, json_dir, testsets_dir)
    n_fired = n_declared - n_drop_fail - n_drop_axiom
    print(f"  Contradiction pairs declared:            {n_declared}")
    print(f"  Dropped — reading failed validation:     {n_drop_fail}")
    print(f"  Dropped — no foundational axiom in JSON: {n_drop_axiom}")
    print(f"  Fired (cs_axiom_contradiction/2 written): {n_fired}")
    print(f"  Kernels with contradiction .pl files:    {n_files}")
    if n_declared > 0 and n_fired == 0:
        print("  NOTE: zero contradiction pairs fired — check drop counts above.")
        print("        Low firing may mean readings failed validation, not that")
        print("        kernels don't contradict (report denominator, not just count).")
    if n_fired == n_declared and n_fired > 0:
        print("  NOTE: if corpus shows zero licensed-plurality signals, first hypothesis")
        print("        is strict test under-fired on soft contradictions (operative")
        print("        incompatibility vs. logical negation), not absence of plurality.")
    # Save contradiction summary to manifests dir for sweep reference
    contra_summary = {
        "declared": n_declared,
        "dropped_reading_failed": n_drop_fail,
        "dropped_no_axiom": n_drop_axiom,
        "fired": n_fired,
        "kernels_with_contradiction_files": n_files,
    }
    (manifests_dir / "contradiction_summary.json").write_text(
        json.dumps(contra_summary, indent=2, ensure_ascii=False), encoding="utf-8")

    print(f"\nRun '{args.run_tag}' complete.")
    print(f"  JSON:      json/{args.run_tag}/")
    print(f"  Testsets:  prolog/testsets/{args.run_tag}/")
    print(f"  Manifests: outputs/kernel_manifests/{args.run_tag}/")
    print(f"Eyeball coherence_eyeball.md before promoting to main corpus.")


if __name__ == "__main__":
    main()
