# WRITEUP — OQ-261 forced-gluing experiment (IN FLIGHT: C3 blocked on ruling R2)

**Executed:** 2026-08-07 (C1 recon + C2 proposal only; no C3 execution has run).
**OQ:** OQ-261.
**Verdict (scoped to what has run):** RECON complete and witnessed; PROPOSAL
pre-registered and FROZEN awaiting operator sign-off (R2) — no experimental verdict
exists yet; H_perf/H_topic remain undecided.
**Manifest cite:** `pipeline_output.json` `pipeline_run_at=2026-08-07T23:44:21Z`,
`n_constraints=225`, `code_commit_short=f724379` (corpus fingerprints in RECON.md).

## State

- **C1 done** — `RECON.md`: fiat family fully resolved post edge-naming reconciliation
  (30/30 edges; `real_closure`, H1r=2, Plur=13); observer-frame family H¹ 73/80/3 with
  two clean blocs, all values in-spectrum; per-story stakeholder reads (all 7 stories
  `manufactured_consensus_candidate[_untypeable]`); flat-control fingerprint (same-topic,
  NOT same-substrate); three-frame mismatch note.
- **C2 done, amended to v2** — `PROPOSAL.md` (v1 at `83a647ea`, v2 per operator
  amendments 1–4 the same day): pre-registered H_perf vs H_topic discriminator with
  blinding declared + three partition variants, pooled-level sparsity floor
  (below-floor = NULL), a 16-family `real_closure` comparator column (mechanical
  agent/excluded partition; corrects the session-report "11" → 13 newly typed),
  numeric positive-control criteria (≥85/156 obstructed + exact join invariant),
  Cell-3 probe-bug license pinned to n_real<2, engine-witness validity note
  (registry 24/25 failure is off the pair-matching path), and a corpus-freeze
  constraint for the duration of C3.
- **C3/C4 NOT run** — blocked on **R2**: operator sign-off of the v2 freeze.
  On sign-off, C3 executes compute-only probes (`performance_presheaf_probe.pl`, to be
  added here) and this file is rewritten as the closing writeup quoting the frozen
  proposal.

## Evidence map

- `RECON.md` — C1 findings (this file's §State summarizes it).
- `family_frame_probe.pl` — the C1 probe (read-only; run from `prolog/`).
- `family_frame_probe_output.txt` — raw C1 probe output (edge table, obstruction,
  H¹ histogram + spectrum check, seat reads).
- `flat_control_fingerprint.txt` — substrate comparison table (operator rider).
- `verdict_join_headlines.txt` — OQ-98 headline verdicts for the 7 stories (2 null).
- `PROPOSAL.md` — the frozen pre-registration (R2 pending).
