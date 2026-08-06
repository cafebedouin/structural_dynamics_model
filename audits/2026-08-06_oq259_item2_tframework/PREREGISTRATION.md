# PREREGISTRATION — OQ-259 item 2, Part C: T Framework graduation dry-run (staged spend)

Frozen before run 1. Authored fresh this session (not pasted from the plan file or
chat). Operator go recorded 2026-08-06 with two rulings folded in: (1) promotion is
**P1-only** (the P2 mechanical gate is unbuildable — `P2_CALIBRATION.md`, ruled same
day; judged-step P2 rejected as post-hoc-licence shape); (2) write-up language is
**draw-level only** unless the 3/3 grammar below is met.

## Input (pinned)

- File: `agent/analysis/originals/k_files/T Framework - Michigan 2026 BCFP.md`
- md5: `a365da8aa11e5039807275bcc662f956` (the Part A re-minted pinned-recipe
  baseline; verified immediately before each run, and after each run for
  input-untouched)
- Regime: emphasis-blind (matches the 2026-08-03 comparanda; the emphasis-aware
  variant is out of scope — `emphasis_extract.py` supports `--highlight-colors cyan`
  if ever revived; the file is cyan-dominant per OQ-259 item-2 note (b))
- Mode: `python3 agent/c-orchestrator.py --dry-run --skip-search --input-file <file>`
  — matches the origin-comparable mode (dry-run stops at manifest; no stories
  generated, no corpus update, no commit step reached)
- Serialization: no concurrent pipeline or topic run; runs executed one at a time
- Corpus-untouched check after each run: `git status --porcelain prolog/testsets/
  json/` must be empty of new entries attributable to the run (dry-run writes only
  `agent/decompose_manifests/flat/<id>.manifest.json`)

## Fixed reference layer

`TAG_INVENTORY_TFRAMEWORK.txt` — mechanical `grep -n '^#\{1,3\} ' <file>` on the
pinned baseline (39 header lines; committed with this prereg). Used descriptively in
the write-up (which sections surface at reading altitude), NOT as a gate — per the
P2 ruling, no reading↔TAG match quantity gates anything.

## Promotion rule (mechanical, pinned before run 1)

- **P1 (the only promotion predicate):** the run's manifest at JSON path
  `commitment_system_recognition.is_contested_kernel` == `true`. Nothing else fires
  promotion. A missing/empty `commitment_system_recognition` is a P1 FAIL (the Cap K
  r2 empty-CSR shape), not an error.
- **Staged spend (symmetric confirmatory-draw rule — operator caution resolved by
  choosing the symmetric option, and it strictly dominates: the graduation grammar
  below requires kernel presence 3/3, so a run-2 miss already kills graduation and a
  post-miss run 3 buys no graduation-relevant information):**
  - Run 2 fires iff run 1 passes P1.
  - Run 3 fires iff runs 1 AND 2 both pass P1.
  - This SUPERSEDES the plan's declared promote-on-1-of-2 asymmetry (that clause was
    written when P2 existed; the operator's caution — promotion resting on a
    single-draw kernel mint while Cap K showed kernel-minting churns 1/2 — is resolved
    by the symmetric rule rather than by declaring an n=1 budget decision).
- **HALT (budget rule, unchanged from the plan):** run 1 no-kernel → ONE confirmatory
  draw; two consecutive no-kernel draws → stop. Pinned language: a 0/2 is a budget
  stop; the file's kernel-minting stability class is UNMEASURED at n=2 — the write-up
  may not read it as a file property or an instrument failure.

## Graduation verdict grammar (pinned, unchanged per operator ruling)

"Graduated second meta-layer file" requires BOTH: kernel presence 3/3 across runs
(name-blind subject+stance — the OQ-264 k=3-unanimous standard; kernel ids/names are
never identity), AND promotion-feature (P1) unanimity across runs 1–3. Anything less:
draw-level language only, churn-floor caveat inherited. One draw → draw-level
language only. NO stability/reproduction claim about any manifest feature may rest on
an unblinded read (KNOWN_STATE 2026-08-06 tripwire); if a cross-run presence call is
close, it gets the blinded-packet treatment before being cited.

## Budget

Run 1 ~190–210K input tok (fits the 975K cap; the Part A baseline is 672,832 B);
worst case ~620K if both confirmatory stages fire. Operator go covers the staged
spend under the rules above.

## What the write-up may claim (pinned)

- Draw-level observations per run (axes selected, kernel minted or not, readings,
  omegas, fracture flags), named per-draw.
- P1 outcomes per run and the staged-rule path actually taken.
- Descriptive TAG-altitude notes (which inventory sections surface as readings vs
  absorb) — explicitly non-gating.
- The graduation verdict ONLY via the grammar above. No "detection", no file-property
  claims from fewer than 3 unanimous draws, no stability claims from unblinded reads.
