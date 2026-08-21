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

## 7b. Kimi (Moonshot) twin — kimi-k2.6 BATCH, reasoning-inflated (2026-07-19)

`agent/run_no_scope_kimi.py` is the Moonshot/Kimi twin (`testsets_kimi/` + `json_kimi/` +
`beta_processed_kimi.txt`), same Anthropic-result-shaped shim as the Gemini driver. What a future
run needs (supersedes the 2026-07-18 "sync-only / batch unprovisioned" reading, which was wrong):

- **Batch IS available — it was MODEL-gated, not account-gated (witnessed 2026-07-19).** `POST
  /v1/batches` returns 200 on **`kimi-k2.6`**; `kimi-k2.7-code` and `kimi-k3` 404
  "resource_not_found" (not batch-enabled). The earlier "account-blocked" conclusion tested only the
  non-eligible models. `completion_window` must be an h-unit Go duration (`"24h"` works; the docs'
  `"1d"` is rejected). So **the twin is `kimi-k2.6` via `--batch`** (`DEFAULT_MODEL`).
- **Moonshot's batch output rows set `response.status_code == 0` on SUCCESS (not 200)**, carrying
  the completion in `body` with a null row-level `error`. The driver originally gated on
  `status_code == 200`, so it **discarded every valid result and looped into a fresh batch**
  (witnessed: pilot completed 5/5, all rejected, a 2nd batch auto-created + billed). Fixed in
  `_batch_row_to_result` (gate on payload, not status_code). If you touch the batch download path,
  keep that: **do NOT reinstate a hard `status_code == 200` check.**
- **`--resume-batch <id>`** reprocesses an already-completed batch WITHOUT regenerating (recovers a
  dead poll loop without re-billing). Its `k{i}` custom_ids map back onto the same first-`--n`
  unprocessed seeds, so pass the same `--n` and run against the same ladder state.
- **kimi-k2.6 is reasoning-HEAVY too** (not the "cheaper non-thinking" model the k2.7-code note
  assumed): measured **input ≈29.6k / output ≈15.5k tok/story**, of which **~11.7k are reasoning
  tokens**. So this stays a *thinking-model* twin (like k3); cross-twin comparisons carry that
  asymmetry. We extract `content`; `reasoning_content` is discarded. Prompt caching fires
  (~28.7k cached input tok/story).
- **Key:** reads `MOONSHOT_API_KEY` OR `KIMI_API_KEY` from the env (never in the repo).
- **Moonshot's `/files` limit is 100 MB** and every batch request inlines the full ~139 KB prompt,
  so a full-pool jsonl is ~143 MB for 1000 seeds — over the cap. `run_batch` auto-splits into
  sub-90 MB chunks (`_chunk_lines`, one batch job per chunk, sequential, merged); 1000 seeds → 2
  batches (~630 + ~370). Do NOT remove the chunking — a single-file full-pool upload 400s
  "File size is too large" (witnessed 2026-07-19).
- **Run the full pool:** `python3 -u -m agent.run_no_scope_kimi --seeds
  prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json --batch` (kimi ladder skips the
  pilot stories already done). Use `python3 -u` — the driver block-buffers stdout to a file
  otherwise, so progress is invisible until exit.
- **Batch tail-latency is SIZE-dependent — keep batches ≤ ~335 (witnessed 2026-07-20).** A
  350-request batch stalled at ~332/350 for hours (last ~30 stuck at +2/hr, riding toward the 24h
  window); 335- and 336-request batches completed cleanly with no stall. The auto-chunker splits a
  1000-pool into 630+370 — the 630 chunk is in the stall-prone zone. For a large run prefer several
  `--n 335` passes (the ladder makes them resumable) over one full-pool `--batch`.
- **Cancel returns the completed rows — harvest, don't wait out a stall.** `POST /batches/<id>/cancel`
  populates `output_file_id` with whatever finished; `--resume-batch <id> --n <same-N>` writes those
  with no regeneration (recovered 329/350 from a stalled batch this way). Also **reserve balance**:
  Moonshot reserves cost against `max_tokens` (32000), so a ~630-request batch over-reserves past a
  ~$50 balance and fails `failed_precondition: insufficient balance` before running (check `GET
  /v1/users/me/balance`). k2.6 batch actual cost ≈ $0.043/story.
- **Status 2026-07-20: `testsets_kimi/` COMPLETE at n=1005** (5 pilot + 329 harvest + 335 + 336;
  classify_corpus GREEN on kimi-k2.6). It is the fifth full leg alongside haiku/flash/sonnet — but
  see the regime caveat (kimi is thinking-on, the Claude twins were generated thinking-off) before
  reading cross-model differences as model-quality; `audits/2026-07-20_five_leg_twin_comparison/`.

## 7c. Stealth (OpenRouter) twin — `stealth/ox-alpha` SYNC, reasoning-inflated (2026-08-21)

`agent/run_no_scope_stealth.py` is the OpenRouter twin (`testsets_stealth/` + `json_stealth/` +
`beta_processed_stealth.txt`, raw responses under `outputs/no_scope_runs_stealth/responses/`). It
IMPORTS the kimi driver's shim / prompt builder / `_extract` (one copy, not a fork) and talks
OpenRouter's OpenAI-compatible `/chat/completions`. What a future run needs:

- **Sync only.** OpenRouter has no `/files` + `/batches`, so there is no −50% batch path and no
  `--resume-batch`; concurrency is `--workers` (20 used for the full run). The ladder makes any
  run resumable (`--n 0` picks up every unprocessed seed).
- **Model facts, disk-verified from `GET /api/v1/models` on 2026-08-21:** price 0/0 (free that
  week — the driver sums `usage.cost` from every response and prints it, so a price change shows
  in the artifact, not the price card), ctx 1,048,576, max_completion 131,072, **reasoning
  MANDATORY** (default effort `max`; `--reasoning-effort low|high|max` overrides and is stamped),
  temperature supported (default 1; `--temperature` overrides and is stamped). Re-read the model
  record before a new run — stealth models are pre-release and can be renamed/repriced.
- **Thinking-model twin, like kimi-k2.6.** Pilot story: input 30,234 / output **32,962** tok, of
  which ~80% is reasoning (125,771 chars of `message.reasoning` vs 31,185 of story); ~10 min per
  story at `max` effort. We keep `message.content` only; reasoning arrives in a SEPARATE field and
  is discarded. Cross-twin comparisons carry the same regime asymmetry as the kimi leg (the Claude
  twins were generated thinking-off).
- **Provenance model string is the OpenRouter slug WITH the vendor prefix** (`stealth/ox-alpha`,
  echoed from the response body), so `classify_corpus('testsets_stealth',
  'pipeline_output.stealth.json', 'stealth/ox-alpha')` is the certification call (prefix match).
- **Output is gated, not only input:** every response body is persisted BEFORE parsing, a
  `finish_reason != "stop"` (truncation) is reported and counted as errored, and a 200 body that
  carries a provider `error` object is refused. The completion line counts `.pl` files ON DISK and
  raw responses on disk, not the loop.
- **Key:** `OPENROUTER_API_KEY` from the env (never in the repo). Preflight without spend:
  `GET /api/v1/key` (auth), `GET /api/v1/credits` (balance), `GET /api/v1/models` (slug, price,
  `supported_parameters`, `reasoning.mandatory`).
- **Pilot 2026-08-21:** `--n 1` → 1/1 OK, `classify_corpus` GREEN (`n_stories` 1, `h1_band` 3),
  `module_boundary_check` GREEN with the leg registered in `CORPUS_DIRS`. Full 1004-seed run
  launched the same day (`python3 -u -m agent.run_no_scope_stealth --seeds
  prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json --n 0 --workers 20`); result in
  KNOWN_STATE 2026-08-21 (stealth leg entry) once landed. Registration order for a finished leg:
  `python/module_boundary_check.py` `CORPUS_DIRS` → `python/shared/corpus_legs.py` `LIVE_LEGS` →
  `python/corpus_census_check.py` `STAMPED_FILE_COUNTS` → `corpus_census_baseline.json` via
  `--repin --cause … --authorized-by …` (operator-authorized) → the multi-leg harnesses.

## 8. Pointers

- Drivers: `agent/generate_kernel_corpus.py` (`run_no_scope`), `agent/run_no_scope_gemini.py`,
  `agent/run_no_scope_kimi.py` (Kimi kimi-k2.6 batch twin — §7b), `agent/run_no_scope_stealth.py`
  (OpenRouter stealth/ox-alpha sync twin — §7c).
- Helpers: `agent/_pilot_ladder_strip.py` (OQ-121 strip + witness), `agent/build_never_generated_seeds.py`.
- This build's saved records: `prolog/kernels/rebuild_2026-06-13/` (seed pool, reconcile sets,
  per-model failure lists, README).
- Trackers: ISSUES.md **OQ-75** (rebuild), KNOWN_STATE 2026-06-13. Corpus-loading mechanics +
  the overlay tripwire: CLAUDE.md → Corpus Loading.
