# OQ-254 Step 4 witness — q_provenance_readout + pipeline phase (2026-07-25)

## Artifacts
- `python/q_provenance_readout.py` — four story-side tokens (`joined` /
  `joined_archive_not_authoritative` / `no_run_id_authored` with counted breakdown /
  `run_id_authored_manifest_unreachable`), manifest-side census, planted two-sided
  controls run on EVERY invocation (exit non-zero on control failure).
- `python/run_pipeline.py` — Phase 9d `_phase_q_provenance_readout` (sequential,
  pure JSON, `_phase_epsilon_authorship_readout` mold), writes
  `outputs/q_provenance_readout.{json,md}`.

## Witness — controls (one per non-null token, two-sided)

```
  control joined                                   -> joined                                   PASS
  control joined_archive_not_authoritative         -> joined_archive_not_authoritative         PASS
  control run_id_authored_manifest_unreachable     -> run_id_authored_manifest_unreachable     PASS
  control no_run_id_authored                       -> no_run_id_authored                       PASS   (run_id 'none')
  control no_run_id_authored                       -> no_run_id_authored                       PASS   (none_authored fact)
  control no_run_id_authored                       -> no_run_id_authored                       PASS   (emission missing)
all controls PASS
```

## Witness — readout on current state (the honest picture)

```
joined                                   0
joined_archive_not_authoritative         0
no_run_id_authored                       205
run_id_authored_manifest_unreachable     0
no_run_id_authored breakdown: {'no_epsilon_provenance_fact': 129, 'fact_authored_run_id_none': 76}
manifests: live=0 archive=515
```

All live stories are the declared loud-null stratum; `joined` is exercised only by the
planted control — the join is WIRED, not JOINED (graduates at the next operator topic run).

## Witness — pipeline behavior preservation (same-day baseline pair)

Baseline `pipeline_output.json`: run 2026-07-25T17:39:08Z at `f15e2a8` (session-start
HEAD). After the phase addition, full `python3 python/run_pipeline.py`:

```
  epsilon_authorship_readout     ok       [0.1s]
  q_provenance_readout           ok       [0.1s]
  Total time: 29.5s                                # exit 0, all steps ok
=== mtime after ===
2026-07-25 12:52:17 (was 12:39:35)                 # output REWRITTEN, not stale
=== corpus fingerprint (cat testsets/*.pl | md5sum), before == after ===
927b5e3ac19c86283e408b36fe2b678a == 927b5e3ac19c86283e408b36fe2b678a   # frozen pair
per_constraint equal: True
manifest diffs: code_commit f15e2a8 -> 7f29bfe, pipeline_run_at re-stamp only
```

Exit 0 AND mtime advanced (the false-pass guard), corpus md5-frozen around both halves,
`per_constraint` byte-identical, manifest diff exactly the expected re-stamps.
