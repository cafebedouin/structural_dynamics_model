# OQ-254 Step 3 witness — tracked manifest location + archive (2026-07-25)

## Edits (write-site moves; generator-forward)
- `agent/c-orchestrator.py` `_persist_manifest` → `agent/decompose_manifests/<run_tag|flat>/`
  (processed.txt stays in `outputs/kernel_manifests/` — only the Q-record moves).
- `agent/generate_kernel_corpus.py` batch decompose sidecar →
  `agent/decompose_manifests/decompose/`.
- `agent/generate_kernel_corpus.py` legacy `--scope` flow (third same-species site, found
  during implementation — writes scope manifests feeding `flatten_manifests`): gains the
  same mint (`_generation_run_id` = filename stem) + `_provenance` stamp + tracked write to
  `agent/decompose_manifests/<run_tag>/`. Left out it would silently recreate the 'none'
  stratum on that path. (`regression_manifest.json` NOT moved — a PASS/FAIL probe
  artifact, not a Q-record.)

## Witness — unit-drive to the tracked path

```
[decompose] Manifest persisted: .../agent/decompose_manifests/flat/unitdrive_tracked_20260725_124804.manifest.json
landed: ['agent/decompose_manifests/flat/unitdrive_tracked_20260725_124804.manifest.json']
=== git check-ignore (empty output = NOT ignored) ===
check-ignore exit: 1          # exit 1 = path NOT ignored
?? agent/decompose_manifests/  # shows as trackable untracked
```
(Synthetic drive artifact deleted after the witness — the paste is the record.)

## Witness — archive copy (515 files, md5-verified)

```
copied 515
=== archive count ===
515
=== md5 spot check (3 pairs) ===
      2 a7ef2a08f3c67c525fd41a8b76fe1386   # count 2 = original+copy identical
      2 f074e7483a09d18e84e2391487ed8b5c
      2 e80b03b8bcec8034fe2397778fb32ee0
```

Archive at `agent/decompose_manifests/archive_pre_2026-07-25/` with README declaring
archive-not-read-surface + the `joined_archive_not_authoritative` resolver token rule.
Originals in `outputs/` untouched.

## OPEN (manual approval)
The `_step_commit` extension (stage this run's manifest file alongside the story files)
is prepared as an UNCOMMITTED working-tree edit — per the operator's fork ruling it lands
only after eyes on the diff. It is the one place where a wrong edit is both expensive and
quiet (the refusal logic stands between the pipeline and `git add -A`).
