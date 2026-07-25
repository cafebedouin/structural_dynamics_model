# OQ-254 Step 2 witness — manifest self-provenance stamp (2026-07-25)

## Edits
- `agent/generate_kernel_corpus.py` — new `_scope_manifest_provenance(model, axes_ceiling,
  topic)` (beside `_provenance_stamp`, reusing `_git_commit_of`): scope_model,
  scope_prompt_commit (`prompts/uke_scope_v2_json.md`), scope_schema_commit
  (`python/scope_manifest_schema.json`), persisted_at, axes_ceiling, topic. Injected at
  the batch decompose sidecar write (beside the `_generation_run_id` mint).
- `agent/c-orchestrator.py` — `_persist_manifest` gains `topic` param (threaded from
  `_step_decompose`) and stamps `manifest["_provenance"]` via the shared helper (lazy
  import, inside the never-kill-the-run try).

## Witness — unit-drive of `_persist_manifest` (pasted run)

```
[decompose] Manifest persisted: .../manifest_unit_drive/unitdrive_fam_20260725_124548.manifest.json
files: ['unitdrive_fam_20260725_124548.manifest.json']
run_id: unitdrive_fam_20260725_124548 == stem: True
_provenance: {
  "scope_model": "claude-sonnet-5",
  "scope_prompt_commit": "d179423dc12550308c604592b1d199a951739c4c",
  "scope_schema_commit": "43ee9613b2ccba5f278157c4cbe534645e1f544e",
  "persisted_at": "2026-07-25T12:45:48",
  "axes_ceiling": 3,
  "topic": "unit drive topic string"
}
```

run_id equals the filename stem by construction; commit hashes are real (per-file
`git log -1` via `_git_commit_of`). The gkc batch-site stamp uses the same helper —
same-shape output; its live firing graduates with the next batch decompose run
(declared OPEN with the Step-1 LLM-path item).
