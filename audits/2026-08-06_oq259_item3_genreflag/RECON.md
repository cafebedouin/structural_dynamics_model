# RECON — OQ-259 item 3 (genre-flag reproduction arm)

Read-only findings carried from the planning sessions plus in-session re-witnesses
(2026-08-06). Anything labeled HYPOTHESIS is this session's unblinded read and is NOT
adjudication — the blinded B0 calls supersede it.

## 1. Arm-0 grep (carried from planning; re-witnessed by manifest dump this session)

Both Arm-0 Biopower redraw manifests carry a debate-genre omega under a CHURNED name
(`omega_debate_format_artifact` in both, vs the origin's
`omega_debate_genre_distortion`). Dump command:

```
python3 -c "import json; d=json.load(open('<manifest>')); print(d['omegas'], d['fracture_scan'])"
```

**HYPOTHESIS (unblinded in-session read, filed per plan):** origin passes strict
(i)+(ii); RUN1 (i)-pass, (ii)-marginal (its consequence clause is coverage, not
fidelity); RUN2 plausible strict pass. Territory-level 2/2, strict 1/2–2/2 — STRICT
carries the variance; TERRITORY is near-saturated.

## 2. Mode comparability (CONFIRMED)

Origin and both Arm-0 runs were `--dry-run --skip-search`:
- `audits/2026-08-03_kritik_ingest/biopower_dryrun.log` — `research skipped`,
  `[dry_run] Manifest assembled`
- `audits/2026-08-05_oq259_emphasis_discriminator/arm0_biopower_run{1,2}.log` — same
  markers.
A Part B null cannot be a mode artifact.

## 3. Specimen independence (MEASURED, both probes; re-run this session)

- Exact-line probe (>150 chars), carried from planning: Afropessimism NW × {Cap K NW,
  Biopower NW} = 0 shared lines; CNDI × Biopower NW = 9 shared lines.
- 8-word shingle probe re-run this session (`shingle_probe.py`, output
  `shingle_probe_output.txt`): AFRO_NW 31,593 shingles; × CAPK_NW = 0.0000%; ×
  BIOP_NW = 0.0000%; control CNDI × BIOP_NW = 21.1182% (7,388 / 34,984). Matches the
  planning-session measurement (21.12%).

**Thin-control caveat (pinned):** the positive control shows the probe DETECTS
card-sharing where it exists (CNDI and Biopower NW are heavily card-shared); it is not
a sensitivity calibration — it does not establish the minimum overlap the probe would
resolve. The 0.1% specimen threshold is therefore a pinned conservative bound read
against a >200× separation (0.0000% vs 21.12%), not a measured detection floor.

## 4. What is answerable

- B0 (free): the origin file's STRICT/TERRITORY reproduce-rate at n=2 under blinded
  adjudication — answerable now; this audit runs it.
- B1 (spend): the pre-specified (i)+(ii) flag's k=3 reproduce-rate on an independent
  arsenal — answerable only after the operator checkpoint (table + shingle control +
  two-sided P2 calibration + B0 result).
