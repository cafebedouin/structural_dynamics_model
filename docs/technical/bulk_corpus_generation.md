# Bulk corpus generation — runbook

Read this before building a corpus from scratch, bulk-adding to one, or making a
second-model twin. It captures the process witnessed in the 2026-06-13 rebuild (988 Haiku
stories + a 971-story gemini-2.5-flash twin, reconciled into `testsets_haiku/` +
`testsets_flash/` + `testsets/`). Provenance for every claim here: ISSUES.md OQ-75,
KNOWN_STATE 2026-06-13.

## 1. Which generation path

| Path | Model/API | Use for |
| --- | --- | --- |
| `generate_kernel_corpus.py` `run_no_scope` | Anthropic batch (Haiku, `GEN_MODEL`) | **The** kernel/reading corpus path. Threads kernel context (`build_cached_messages`), pipeline-stamps provenance. |
| `agent/run_no_scope_gemini.py` | google.genai batch (gemini-2.5-flash) | Faithful Gemini twin of the above — reuses `build_cached_messages` + `process_batch_results` verbatim via an Anthropic-result-shaped adapter; only the batch API + destinations differ. |
| `agent/generate_json.py` | google.genai | **Do NOT use for kernel corpora.** Its `build_prompt` is the OLD non-kernel prompt: no kernel_id/reading context, no `cs_structure.reading_relations`, and pre-provenance-fix. Reuse only its google.genai mechanics (batch + context cache), never its prompt. |
| `agent/c-orchestrator.py` | unified backend | Full topic→research→decompose→generate→essay pipeline; different entry point. |

## 2. The seed pipeline

**Two-phase by design — DECOMPOSE then GENERATE — and the split is what enables batch
processing.** A kernel is not turned into stories in one pass. `generate_kernel_corpus.run_decompose`
(and the `c-orchestrator.py` `decompose` step) first **batch-SCOPEs** every kernel into reading
*seeds* — one Sonnet SCOPE request per kernel, all in one Anthropic batch (`build_scope_batch_requests`)
— writing per-kernel manifests and appending the flattened reading-seeds to
`prolog/kernel_readings_pool.json`. Only then does the **no-scope GENERATE** phase
(`run_no_scope`, batched) turn those pooled reading-seeds into full stories. The pre-decomposition is
the reason the pool exists as an intermediate artifact: decomposing all kernels up front yields a
flat list of independent reading-seeds that one batch can generate in parallel, instead of a serial
topic→scope→story chain per kernel. (It also means a reading-seed already carries its `kernel_id` +
`sibling_reading_ids`, so the GENERATE phase can author `cs_reading_relation` edges and
`stamp_kernel_linkage` can stamp `cs_kernel_id` — i.e. the committer axis is born at decompose time,
not generate time.)

`agent/build_never_generated_seeds.py` reads the SCOPE manifests in gitignored
`outputs/**/*.manifest.json` (+ `outputs/**/manifests/*.json`) and emits one reading-seed per
declared reading of every never-generated contested kernel. Each seed:
`constraint_id` (= `kernel_id__reading_id`), `kernel_id`, `reading_id`, `sibling_reading_ids`,
`expected_structural_delta`, `summary`, `topic_domain`, `family_id`.

- **The emitted count is MANIFEST-POOL-DEPENDENT, not fixed.** The builder is stable, but the
  pool grows: the same builder emitted 304 readings / 101 kernels on `corpus-rebuild-304` and
  **1005 / 331** weeks later — pure manifest growth. Always measure and cite the count you get;
  never a remembered one.
- **A fresh worktree off `main` has ZERO manifests** (`outputs/` is gitignored, doesn't travel
  with the branch) → the builder emits 0. Either run the builder in a tree that has the manifest
  pool, or use the saved resolved pool at **`prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json`**
  (1005 readings, committed so future rebuilds are reproducible without the manifests).

## 3. Provenance is pipeline-authored (the five-defect fix, commit `2e3e1998`)

The content LLM cannot know its own commits/model/sampling-params and only fabricates them
(witnessed: 24/24 pilot stories claimed Sonnet/Opus; every one was Haiku). The fix, which any
new generation path MUST preserve:

1. **Stamp from the API result.** `process_batch_results` sets
   `story["provenance"] = _provenance_stamp(result_model, …)` BEFORE validation, always
   overwriting the model's copy. `result_model = result.result.message.model` — never hardcoded
   (the writer is shared across models; the Gemini adapter sets `.model` to the Flash id, so
   Flash stories stamp `gemini-2.5-flash`).
2. **Strip provenance from the model-facing schema** (`_strip_provenance_from_context`) to stop
   soliciting fabrication + wasting tokens. (The kernel path `build_cached_messages` shows the
   raw schema; the stamp overwrites regardless — strip is an optimization, not the guarantee.)
3. **`story_provenance/8` + `story_seed/3` must be `:- multifile`** in `narrative_ontology.pl`,
   like every per-story predicate, or SWI keeps only 1 of N facts silently. Witness at scale:
   N testsets load ⇒ N `story_provenance` facts, zero "Redefined static procedure" warnings.
4. **Re-stamp after `repair_story`** (it drops the top-level provenance block).
5. **`generate_pl` raises on referential-integrity violations** the schema can't express (OQ-92
   ghost-seat); the batch writer CATCHES it → that story FAILs, the run continues.

## 4. Per-chunk recipe (serialized — OQ-77: one batch at a time)

Never run two pipelines/topic-runs concurrently against shared `testsets/`+`outputs/`. Within a
batch, parallelism is fine; across runs, serialize. Per chunk (~50 whole kernels worked well):

1. **Carve** the chunk grouped by kernel (whole-kernel cuts — readings of a kernel reference each
   other). Pin `chunk_size = len(seeds)`; use that variable in every check, not a literal.
2. **Strip the OQ-121 ladder fossil.** `prolog/beta_processed.txt` is a pre-reset fossil (~1100
   entries, ~0 surviving outputs). **Any seed whose id is in it regenerates NOTHING and reports
   success** (fails open). Strip the chunk's ids first; the witness is `pending` before→after
   (`< chunk_size` → `== chunk_size`), not a single count. Helper: `agent/_pilot_ladder_strip.py`.
3. **Generate** (background batch; `run_no_scope` retries failures up to 3×).
4. **Run the OQ-58 integrity sweep MANUALLY** — the no-scope path SKIPS
   `validate_reading_relation_integrity`. It routes dangling `cs_reading_relation` targets to
   `prolog/cs_reading_relation_quarantine.json` (CAUGHT, not crashed). It is **last-run-scoped**
   (overwrites). Paste the entries as the witness they were caught; "no failures" with an
   unexamined quarantine is indistinguishable from "integrity never ran."
5. **`run_pipeline`** to classify. Witness: `manifest.n_constraints` delta == `chunk_size`.
6. **Commit the chunk** (commit-as-you-go; in-flight work is what compaction destroys). The
   witness is the diff / per-entry check, never a global count delta (multi-writer corollary).

## 5. Recurring failure modes (expect them; they are not regressions)

- **`status: 'contested'` axiom-status enum violation** (valid set: `holdable | overridden |
  foreclosed`). The dominant retry-exhausting failure for BOTH models (33 occurrences in two
  Haiku chunks). A prompt/schema constraint pinning axiom-status to the enum would cut both the
  failures and the retry cost. Also seen: directionality-kind enums (`'conceptual'` etc.).
- **Naming-drift quarantine** (≈6–17 edges/chunk, all caught): the model mangles sibling-edge
  targets — appends `_reading` where the file has none, uses single-underscore
  (`us_constitution_interpretive_originalist`), or drops the kernel prefix
  (`bitcoin_electronic_cash_reading`). Within-kernel edges that should resolve land in quarantine
  instead. A target-normalizer at emission (normalize suffix, re-prefix) would resolve most.
- **Grid first-contact gate.** A grid-authoring story that fires a plausibility indicator halts
  `run_pipeline` (`[GRID-GATE] EXCLUDED`). **Operator ruling (increment-0): REGENERATE the story
  (a fresh stochastic draw usually passes), do NOT waive.** Only record a waiver in
  `python/grid_audit_ledger.json` if the firing is genuinely intended. Procedure: delete the bad
  `.pl`+`.json`, strip its id from the ladder, regenerate that one seed, re-run the pipeline.
- **Failure rate.** Haiku ~1.7% (17/1005), Flash ~3.4% (34/1005 — weaker on the strict schema).
  Failures are NAMED in `outputs/no_scope_runs*/failures.json` — but that file is OVERWRITTEN per
  run, so capture each chunk's failed ids to a cumulative record before the next run clobbers it.

## 6. Two-model twin corpus (same seeds, swap the model)

To build a comparison corpus (same prompts/seeds, different model — robust testing):

- Generate the SAME seed pool with the second model into a **separate testset dir**
  (`testsets_flash/`), **separate ladder** (`beta_processed_flash.txt`), and **separate json dir**
  (`json_flash/`). If you reuse the first model's ladder, every id reads "already processed" and
  nothing generates (the OQ-121 fail-open again).
- **CRITICAL: the filename-uniqueness registry must be the NEW dir only, NOT the first model's
  `testsets/`.** `run_no_scope`'s registry globs `testsets/*.pl`; if the first model already wrote
  every id there, `unique_constraint_id` appends `__<uuid8>` to every twin cid and **the two sets
  no longer pair by filename.** `run_no_scope_gemini.py` scopes its registry to the flash dir for
  exactly this reason.
- **Generation is stochastic** (CLAUDE.md determinism frontier): the two models fail on DIFFERENT
  seeds, so the two sets are not identical. **Reconcile by filename, do not force:** intersection →
  stays in both dirs (the controlled comparison set, verified `set(A)==set(B)`); symmetric
  difference + any other-model baseline → the standard `testsets/`.
- Keep the model fair: `run_no_scope_gemini.py` sets `thinking_budget=0` (Haiku ran without
  extended thinking) so output == story length.
- **Loading a twin for analysis needs a `corpus_path` overlay with `asserta` (or `retractall`
  first) — NEVER plain `assertz`.** `config.pl`'s default `param(corpus_path, testsets)` is the
  first clause and the loader takes the first solution; a plain `assertz` appends after it and is
  **silently ignored** (witnessed: loaded 44 instead of 960, no error). Also: `run_pipeline`'s
  `JSON_DIR` is hardcoded to `json/`, so a twin-comparison harness must repoint its json source at
  the matching mirror (`json_haiku/` / `json_flash/`).

## 7. Cost (OQ-80 token accounting)

- `run_no_scope` / the Gemini driver sum API usage into `token_acc` at receipt (spend is real even
  when a story later fails validation — never a 0-default).
- Measured: Haiku batch ≈ $27 for 988 stories. The Gemini driver's printed `~$N` uses the
  interactive full-input rate as an UPPER BOUND; real billed is far lower (batch −50% + context
  cache on the ~31k-token prompt prefix; Flash output is concise, ~4.5k tok/story vs Haiku ~10.8k).
- **Estimate before a big run** with `run_no_scope_gemini.py --estimate` (count_tokens only, zero
  generation): per-request input × N + a prior run's output/story as the output proxy.

## 7b. Kimi (K3) twin — sync-only, reasoning-inflated, PAUSED (2026-07-18)

`agent/run_no_scope_kimi.py` is the Moonshot/Kimi twin (`testsets_kimi/` + `json_kimi/` +
`beta_processed_kimi.txt`), same Anthropic-result-shaped shim as the Gemini driver. Two facts a
future run needs:

- **`kimi-k3` is reasoning-ONLY** (`supports_thinking_type: "only"`, `think_efforts` = `["max"]`):
  thinking CANNOT be disabled, so §6's "keep it fair with `thinking_budget=0`" does NOT apply. This
  twin is a *thinking-model* twin — output runs ~16.5k tok/story (vs Haiku 10.8k, Flash 4.5k), the
  reasoning inflation. We extract `content`; `reasoning_content` is discarded. Cross-twin comparisons
  carry that asymmetry (stamped in provenance as `kimi-k3`).
- **Batch-create is NOT provisioned on the staff/preview key** (witnessed 2026-07-18): file-upload +
  batch-list work, but a fully valid `POST /v1/batches` (file exists, endpoint == the API's own
  stated valid value, `completion_window` a valid Go duration) 404s "resource_not_found". So the
  full run is **sync-only at the interactive rate** — measured **$0.289/story** (no −50% batch
  discount), ≈ $291 for the ~1005-seed pool. **PAUSED at 5 pilot stories** pending batch enablement
  on the account (operator ruling). **Resume:** `python3 -m agent.run_no_scope_kimi --seeds
  prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json --batch` once create works (the kimi
  ladder skips the 5 done), or `--sync` to run at interactive rate now. Needs `MOONSHOT_API_KEY` in
  the env (never in the repo).

## 8. Pointers

- Drivers: `agent/generate_kernel_corpus.py` (`run_no_scope`), `agent/run_no_scope_gemini.py`,
  `agent/run_no_scope_kimi.py` (Kimi K3 twin — §7b).
- Helpers: `agent/_pilot_ladder_strip.py` (OQ-121 strip + witness), `agent/build_never_generated_seeds.py`.
- This build's saved records: `prolog/kernels/rebuild_2026-06-13/` (seed pool, reconcile sets,
  per-model failure lists, README).
- Trackers: ISSUES.md **OQ-75** (rebuild), KNOWN_STATE 2026-06-13. Corpus-loading mechanics +
  the overlay tripwire: CLAUDE.md → Corpus Loading.
