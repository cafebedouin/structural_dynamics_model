# OQ-259 item 3 (B0) — origin's STRICT genre-flag did not reproduce in either same-input redraw (0/2); TERRITORY tier reproduced 2/2; B1 spend arm pending operator checkpoint

**Executed:** 2026-08-06
**OQ:** OQ-259 (item 3 — genre-flag reproduction arm; standard from OQ-264's k=3-unanimous ruling)
**Verdict:** Under blinded, pre-registered two-tier adjudication of all 14 candidate carriers across the origin Biopower manifest and its two Arm-0 same-input redraws, the strict (i)+(ii) conjunction appears ONLY in the origin (blind call: STRICT) and in neither redraw (STRICT 0/2); the weaker TERRITORY tier reproduces 2/2 — item 3's bar is one the origin file itself did not clear at n=2 (pre-registered disclosure), and the B1 fresh-file arm (Afropessimism NW, 3 redraws) awaits the operator go with this result in the checkpoint package.
**Substrate:** no pipeline run (manifest-text adjudication only; corpus untouched). Manifests: origin `biopower_k_nhi_debate_2026_20260803_102652` + Arm-0 redraws `…_20260805_144612` / `…_20260805_144823` (all `--dry-run --skip-search`, mode-comparable per RECON §2).
**Evidence map:**
- `RECON.md` — recon findings; unblinded in-session read filed as HYPOTHESIS; thin-control caveat on the shingle probe
- `PREREGISTRATION.md` — frozen at `db708cc7` before adjudication: rubric, carrier definition, redaction–scorability decisions, declared degraded blind, tally rule, disclosure rule, interpretation table (pinned before any B1 call), B1 specimen + thresholds
- `build_b0_packet.py` — deterministic packet builder (seed 259)
- `PACKET.md` — the 14-item blinded packet (md5 `ae8878d6…`, pinned in prereg)
- `B0_CALLS.md` — verbatim adjudicator calls, committed `12ee7f55` BEFORE the mapping
- `b0_mapping.json` — label→item mapping (md5 `baa682f4…` matches prereg pin), added after calls
- `B0_TALLY.md` — per-manifest tiers, rates, fired disclosure, hypothesis-vs-blind divergence
- `shingle_probe.py` / `shingle_probe_output.txt` — B1 specimen-independence witness: Afropessimism NW × {Cap K NW, Biopower NW} = 0.0000% shared 8-word shingles; positive control CNDI × Biopower NW = 21.1182% (thresholds <0.1% / ≥5% pinned in prereg)

## What ran (B0, the free arm)

1. Prereg frozen (`db708cc7`) with packet md5 and WITHHELD mapping md5 both pinned.
2. Fresh-instance adjudicator (no tools, no repo access; packet + rubric inline)
   scored all 14 items: 1 STRICT, 3 TERRITORY-only, 10 NEITHER.
3. Calls committed (`12ee7f55`) before the mapping file entered the tree; mapping md5
   verified against the prereg pin on reveal.

## Result

| Manifest | Blind tier | Carrier |
|---|---|---|
| ORIGIN | **STRICT** | `omega_debate_genre_distortion` (ITEM-D) |
| RUN1 | TERRITORY | `omega_debate_format_artifact` (ITEM-I; coverage clause fails (ii)) |
| RUN2 | TERRITORY | `omega_debate_format_artifact` (ITEM-A; structure-import clause fails (ii)) + `omega_reading_reduction_risk` (ITEM-G) |

**STRICT reproduce-rate 0/2; TERRITORY 2/2.** The name churned in both redraws while
the TERRITORY content reproduced — consistent with OQ-264 (names are never identity;
content-level matching only). The strict conjunction's failing prong was (ii) both
times, each via a different non-fidelity consequence clause (coverage; structure-import).

## What this changes

- The pre-registered disclosure FIRES: any future rows-1–3 reading in B1 is stated
  against "the origin itself did not clear the strict bar at n=2."
- Part D's quote framing: `omega_debate_genre_distortion` remains an OBSERVATION
  (origin passes strict blind) whose strict form is redraw-brittle and whose territory
  is redraw-stable at n=2; all of this is n=2 draw-language, never a rate.
- The unblinded hypothesis (strict 1/2–2/2) was MORE generous than the blind call —
  recorded in B0_TALLY.md as the divergence the blinding was declared to catch.

## Open / pending

- **B1 (spend, ~230–300K input tok): NOT run.** Gated on the operator checkpoint
  package (interpretation table + shingle control + two-sided P2 calibration + this
  B0 result). Specimen and thresholds pinned in PREREGISTRATION.md.
- Part C (T Framework graduation dry-run) separately gated; its pinned input md5 is
  minted in `audits/2026-08-06_oq259_item2_tframework/`.
