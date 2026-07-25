# OQ-254 Step 1 witness — generation_run_id join key wired (2026-07-25)

## Edits
- `schemas/constraint_story_schema.json` — `Provenance.generation_run_id` added as an
  OPTIONAL property (required list unchanged; pre-wiring stories stay valid). Needed
  because `Provenance` is `additionalProperties: false` and the batch path re-validates
  AFTER stamping — without the schema edit the stamp would fail validation wholesale.
- `agent/generate_kernel_corpus.py` — `_provenance_stamp` gains `generation_run_id="none"`
  and emits it; `process_batch_results` threads the per-seed run id into both stamp sites
  (initial + post-repair); `flatten_manifests` (3 seed-append sites) and
  `_flat_seeds_from_manifest` stamp `generation_run_id` from the manifest's
  `_generation_run_id`; the batch decompose path sets `m["_generation_run_id"] = kid`
  (= sidecar filename stem) beside `_seed_id`.
- `agent/c-orchestrator.py` — `_persist_manifest` mints
  `manifest["_generation_run_id"] = "<family_id>_<ts>"` (== manifest filename stem,
  equal by construction) BEFORE the write attempt, so a failed persist still yields
  stories whose run_id the readout reports `run_id_authored_manifest_unreachable` (loud)
  rather than `'none'` (silent); serial generate path injects the key into
  `story_dict["provenance"]` before save.
- No Prolog arity changes: `epsilon_provenance/5` arg 4 slot already existed
  (`generate_constraint_pl.py:868-870` reads `prov.get("generation_run_id", "none")`).

## Witness — fixture compile pair (two-sided, schema validation ON)

```
=== UNSTAMPED (validation + emit) ===
narrative_ontology:epsilon_provenance(ability_ceiling_reading, 0.68, 'claude-sonnet-4-5-20250929', 'none', direct).
=== STAMPED (validation + emit) ===
narrative_ontology:epsilon_provenance(ability_ceiling_reading, 0.68, 'claude-sonnet-4-5-20250929', 'fixture_fam_20260725_120000', direct).
```

Both fixtures passed default schema validation (no `Validation errors` emitted; a failure
exits 1 before any .pl output). `py_compile` clean on all three edited Python files.

## Declared OPEN (graduation step)
The LLM-path end-to-end join (a real topic run producing a story whose run_id resolves to
its manifest) graduates at the next operator topic run. Compile-side is witnessed above;
API round-trips are pre-flight, not the witness (CLAUDE.md: a model-path change stays OPEN
until a full run passes).
