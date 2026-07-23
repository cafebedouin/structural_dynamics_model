# C-LATENT witness (2026-07-23)

Producer commit for mechanisms 1–4 (all zero gate-pass victims per census). Engine edits:
`boltzmann_compliance.pl` (scope_invariance_test empty→`no_data`; compute_cross_index_coupling
grid<2→FAIL, uncached; detect_nonsensical_coupling empty-grid→FAIL),
`purity_scoring.pl` (F/SI/CC/EX no-data→`unknown`),
`signature_detection.pl` (purity_test_* no-data→`unknown(...)` verdicts; `structural_purity`
R3 aggregation via new `aggregate_purity_tests/2`: witnessed failure fires through unknowns
→ `contaminated`; clean-with-unknown → `inconclusive(no_data)`; all-pass → subtype),
`drl_boltzmann_analysis.pl` (:218 coupling fallback `0.0`→`no_data` token, number-guarded —
output-identical non-escalation), `context_profile_mining.pl` (:186 `0.0`→`unknown`).

## RED→GREEN

7 producer tests written RED at HEAD (pasted in session: "7 tests failed, 0 passed") —
m1/m2/m3/m4 termini, bare-constraint headline (`purity_score = unknown` never 1.0),
structural_purity abstention, aggregation polarity both directions. Post-edit: **14/14 green**
(7 producer + 7 preflight/scaffold incl. golden `0.3541666666666667` unchanged and the
injection end-to-end). Per-file `swipl -g halt -l` clean on all 5 engine files.

## Caller sweeps (pasted in session, each with positive control)

- `cross_index_coupling`: control = sweep hits purity_scoring:64 + the sites edited. Unedited
  hits: epistemic-gated (fingerprint_coupling, boltzmann_compliant gate-pass arm — gate-pass
  grid≥2 by census), explicit-fallback (`-> ; unknown/-1.0/[]`), or R3-existential
  (metric_drift, abductive; plan-designated leave). FNL :1113 unreachable for grid<2 rows
  (requires `non_compliant`, impossible at coupling 0.0/absent).
- `detect_nonsensical_coupling`: edited purity_scoring:89 + signature:1307 hit (control);
  remaining = shadow-audit fallback `[]`, fcr test-4 both-don't-fire, gated fingerprint arm.
- `scope_invariance_test`: edited sites hit (control); :1187 requires `invariant` (no_data
  fails it same as variant([])); fcr :1574 `scope_variant(variant([]))` reachable only inside
  `false_ci_rope` AFTER `appears_as_rope` — probed on live testsets: **18/18 m1 rows
  appears_as_rope=no, fcr=quiet** (pasted).
- `excess_extraction`: edited sites hit (control); left-by-design: `boltzmann_compliance:589`
  T3 `pass(no_extraction_data)` (documented design intent "Mountains often have ε ≈ 0",
  outside plan's edit list — declared residue), metric_drift sites existential (R3),
  drl_boltzmann_analysis:147 `Factor = 0.5` **declared residue** (reformability heuristic,
  not a purity site; `unknown` cascades into `type_to_dirac_class(tangled_rope,...)` with no
  witnessable surface — provenance comment added at the site, routed via close).

## Pipeline witness — BYTE-IDENTICAL (stop-condition (i) branch NOT taken)

Method: `git stash` engine edits → baseline `classify_corpus` (testsets n=199, flash n=960,
serialized, exit 0) → `stash pop` → edited runs (exit 0) → per_constraint JSON diff
(manifest excluded — re-stamped per run). Corpus md5 fingerprints identical across all four
runs (testsets `ca49716f…`, flash `6c6a2dbd…` — no drift mid-witness; an operator
c-orchestrator run finished BEFORE baseline, corpus 199 includes its 4 stories).

```
[testsets] base n=199 edit n=199 → BYTE-IDENTICAL per_constraint
[flash]    base n=960 edit n=960 → BYTE-IDENTICAL per_constraint
```

**Differ positive control (planted difference):** one perturbed `purity_score` in a copy →
`DIFF ability_ceiling_reading: keys=['purity_score'] — 1 differing rows` (pasted). The
instrument discriminates; the byte-identical verdict is a measured-clean.

Artifacts: `clatent_witness.py` / `clatent_diff.py` (drivers), `clatent_witness_{base,edit}.log`,
`oq60_clatent_edit_{testsets,flash}.json.gz` (edited-engine dumps = the pre-C-FLOOR baseline).
haiku / kernel_v1 legs: **OPEN** — discharged at C-FLOOR (per witness-economy ruling).

## Synthetic controls (pasted)

All-bare + mixed dual on a 3-story scratch corpus (2 bare + real
`alignment_constraint_narrowing`) via `classify_corpus`:

```
alignment_constraint_narrowing purity_score= 0.354167 purity_band= contaminated
oq60_bare_a purity_score= None purity_band= None
oq60_bare_b purity_score= None purity_band= None
```

0/2 bare scorable, N>0, no fabricated pristine; scored row unchanged. NOTE for C-FLOOR join:
JSON serializes purity to 6 decimals (`0.354167`) vs census TSV full precision
(`0.3541666666666667`) — join numerically at JSON precision, not string-equal.

## Census probe v2

`census_oq60_v2.pl` added: v1's m1 tag (`variant([])`) and m2 cross-check (`coupling == 0.0`)
read RETIRED tokens post-C-LATENT — running v1 on this engine would report m1=0 vacuously.
v2 tags m1 via `no_data`, cross-checks m2 via coupling-FAILS, prints gate-pass victim counts,
and its bare positive control expects `purity = unknown`.
