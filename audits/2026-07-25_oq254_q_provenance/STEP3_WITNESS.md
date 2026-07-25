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

## `_step_commit` extension — operator-amended, then landed

Operator review of the first diff (2026-07-25) required two repairs before commit:
1. **Trusted → checked:** `_last_manifest_path` was trusted; the amended version verifies
   via the join key — the manifest filename stem must equal the `generation_run_id` stamped
   on every story being committed; append only on match. Reachability finding for the
   staleness case: one orchestrator instance per process + unconditional decompose means
   within-instance staleness cannot fire today (belt-and-braces there) — but the check IS
   load-bearing on the `--manifest-file` frozen path, where a pre-wiring manifest (stories
   `'none'`) or a renamed one (stem ≠ embedded id, join would never resolve) mismatches and
   is recorded, not staged.
2. **No silent narrowing:** every non-staged outcome (no path / file missing / outside repo
   / stem mismatch) lands in `StepResult.data["manifest"]` and the progress line — the
   first diff's `pass` + falsy-skip was the absence-presenting-as-presence shape this OQ
   was about, inside the fix's own commit step.

### Witness — three-case drive of the amended `_step_commit` (temp git repos, real commits)

```
== A matched: status=success data={'sha': '0ed0b70', 'files': 3, 'manifest': 'staged: fam_1'}
   committed files: ['json/s1.json', 'manifests/fam_1.manifest.json', 'testsets/s1.pl']
== B mismatched (story 'none'): status=success data={... 'manifest': "not_staged: stem 'fam_1' != story run_ids ['none'] (stale or unstamped manifest)"}
   committed files: ['json/s1.json', 'testsets/s1.pl']
== C no _last_manifest_path: status=success data={... 'manifest': 'not_staged: no _last_manifest_path on this run'}
   committed files: ['json/s1.json', 'testsets/s1.pl']
```

Matched commits the manifest; mismatched/no-path exclude it with the reason recorded —
two-sided, and the join key is doing exactly the job it was minted for.
