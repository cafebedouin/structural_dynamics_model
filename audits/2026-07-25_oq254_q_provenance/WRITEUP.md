# OQ-254 — Q-provenance of the topic decomposition: recon + minimal wiring (RESOLVED)

**Date:** 2026-07-25. **Scope (operator-ruled):** recon writeup + minimal wiring, one pass.
**Evidence in this dir:** `STEP0_EVIDENCE.md` (re-witnessed census + controlled greps;
`manifest_census.py` / `manifest_census_output.json`), `STEP1_WITNESS.md` (join key),
`STEP2_WITNESS.md` (self-stamp), `STEP3_WITNESS.md` (tracked location + archive),
`STEP4_WITNESS.md` (readout + pipeline behavior preservation).
**Commits:** `01d503aa`, `f1436bd4`, `2d7432a0`, `7f29bfea`, `c200fcd2`, + the closing
docs commit.

## Verdict

OQ-254's headline — "the ε discipline audits the answer, not the question" — is **false as
written** (correction marked on the ISSUES entry, not folded silently). The 515-manifest
census shows the Q-choice IS declared, richly: `selection_reason` on 2596/2598 axes (2 genuinely
unauthored, 6 schema-variant with the rationale authored elsewhere — all 8 classified
exhaustively, not sampled), `deferral_reason` on 1022/1022 deferred entries, kernel verdict on
486/515, all 55 empty-`deferred_axes` manifests validated legitimately-nothing-deferred against
a two-sided discriminator control. The real defect was **Pattern 6**: the declaration was
unreachable from every read site — gitignored (`/outputs/`, 0 tracked), no self-provenance, no
story-side join (`epsilon_provenance/5` arg 4 = `'none'` in 71/71 live emissions,
`_last_manifest_path` write-only) — so "no rationale recorded" and "rationale on someone's
laptop" were indistinguishable.

This is the second same-direction inference (from v8 prose about what the engine does)
corrected by code contact, kin to OQ-255 §8's Q6-channel misidentification.

## What landed (generator-forward; no backfill)

1. **Join key (the whole fix):** `generation_run_id` == manifest filename stem, minted at
   decompose, threaded through all THREE scope-manifest write paths (c-orchestrator
   `_persist_manifest`, gkc batch decompose sidecar, legacy `--scope` flow — the third found
   during implementation) into story `provenance` → `epsilon_provenance/5` arg 4. Schema:
   optional `Provenance.generation_run_id` (required list unchanged).
2. **Manifest self-stamp:** `_provenance` block (scope model, prompt commit, schema commit,
   timestamp, axes ceiling, topic) at every scope-manifest write.
3. **Tracked location:** `agent/decompose_manifests/`; the 515 pre-existing manifests copied
   (md5-verified) to `archive_pre_2026-07-25/` with an archive-not-read-surface README.
4. **Standing readout:** `python/q_provenance_readout.py` (four tokens: `joined` /
   `joined_archive_not_authoritative` / `no_run_id_authored` + counted breakdown /
   `run_id_authored_manifest_unreachable`; planted two-sided controls on every invocation)
   wired as run_pipeline Phase 9d. Behavior preservation witnessed: exit 0, mtime advanced,
   `per_constraint` byte-equal over an md5-frozen corpus pair.

## Open at close (declared, with graduation steps)

- **WIRED, not JOINED:** all 205 live stories are `no_run_id_authored` (129 no-fact / 76
  `'none'`); `joined` is exercised only by a planted control. Graduates at the next operator
  topic run (a model swap/generation-path change stays OPEN until a full run passes — the
  fixture pair is compile-side witness only).
- **`_step_commit` extension** (stage the run's manifest with its stories): prepared as an
  uncommitted working-tree diff, pending operator eyes per the manual-approval fork ruling.
- **§3 foreclosure as structure:** deliberately unbundled → OQ-256 (waits on a first exercise
  of OQ-255's hand-enumerated foreclosure branch — read surface must not precede its reader).
