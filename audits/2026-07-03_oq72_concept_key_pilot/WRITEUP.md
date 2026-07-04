# OQ-72 concept-key pilot — WRITEUP

Executed 2026-07-03 → 2026-07-04. OQ-72 RESOLVED at the scoped altitude "mechanism
demonstrated" (mixed haiku/live 10-kernel pilot). Checkpoints R1/R2/R3 operator-ratified
(`R1_RULING.md`, `R2_RULING.md`, `R3_RULING.md`). Commits: `5494cbee` (Phase 0),
`13c1573a` (Phase 1 + R1), `0c4d7152` (Phase 2), `21052305` (R2), `b5aa0cb0` (Phase 3),
`046233a0` (Phases 4–5), close commit (Phase 6).

## What was built

- `prolog/axiom_concept_registry.pl` — NEW CANONICAL: 71 ratified `axiom_diff:axiom_concept/2`
  facts (tranche 1), kernel-scoped atoms `<kernel_id>__<slot>`, seat + provenance header.
  Loaded from `stack.pl`. The ratified TSV (`assignments_ratified.tsv`) is the audit witness.
- `python/axiom_concept_bake.py` — fail-closed baker (refuses unratified/malformed/conflicting
  rows; C6 witness below).
- `python/audits/oq72_axiom_inventory.py`, `oq72_lexical_adversary.py`, `oq72_witness_sweep.py`
  — the pilot instruments (each with its own positive control).
- `prolog/tests/test_axiom_diff.pl` — new `axiom_concept_registry` plunit unit (registry-driven
  concept-key regression, fixture-local); westphalia tests re-frozen fixture-local (see Defects
  fixed).

## Control results (all pre-registered in PROPOSAL.md before any run)

| control | result | witness |
|---|---|---|
| C1 within-kernel positive (3 named live contradiction pairs) | **3/3 aligned** under blind assignment (bar ≥2/3; 0 misses — the R1 diagnostic rider had nothing to classify) | `assignments_proposed.tsv`; check pasted in Phase-3 commit turn |
| C2 contradiction-specific (≥1 live pair in REAL diff output) | **3/3 surfaced** — digital_money as DISPARITY `[conventional,deontological]\|[conventional]` (the westphalia same-concept/opposed-grounding shape); moral_causation + visual as agree cells | `sweep_results.tsv`, sweep output pasted in session |
| C3 non-degeneracy floor (≥1 blind→(agree/disparity) conversion in ≥7/10 kernels) | **10/10** (even tordesillas, the predicted non-contributor) | same sweep; 42 pairs, exact_name all-blind on every pair |
| C4 negative control (lexical adversary) | fired: planted pair merged (shared=3, jac=0.6; run hard-fails if silent); 2 real candidates; operator marked (a) marriage pair TRULY-DISTINCT → proposer had split it (PASS); (b) vatican pair ruled same-subject (not a false merge) | `adversary_control.log`, `adversary_candidates.tsv`, R3_RULING |
| C5 join/regression (westphalia inversion) | green (7/7 unit run incl. new registry tests; registry survives cleanup: 71 facts post-run) | session paste, 2026-07-04 |
| C6 ratification gate (baker refusal) | REFUSED planted `ratified_status=PENDING` row, exit 1, wrote nothing | `c6_baker_refusal.log` |

**Bars:** false-merge 0/71 (bar ≤10%). **Kill legs (a) and (b): NOT TRIPPED.**

## Findings beyond the build

1. **Epistemic reframe (R1, operator ruling):** the mechanical key does not make the axiom axis
   discovered — it relocates the ruling to vocabulary granularity and makes the seat ratified
   and auditable. §7.1's yield-divergence survives with three grades: discovered / ratified /
   silent-seat. OQ-75(b) inherits a labeled asymmetry (ruling pending, logged there).
2. **Criterion-scope observation (pre-registered at R2, before the sweep):**
   `cs_axiom_contradiction` is not universally same-subject — 2 of visual_evidentiary's 3 pairs
   oppose ACROSS subjects (`verification_feasibility` × `truth_warrant_source`) and cannot align
   under any assignment. A Phase-5 zero on those two pairs is by-construction. Scale-up must not
   assume contradiction⟹same-subject.
3. **The R2 corollary of the kill-condition rider:** for contradiction-bearing kernels,
   vocabulary granularity at R2 predetermines C1's passability — the same-subject criterion is
   exercised at vocabulary time, not at R3 diagnosis. One slot makes the control passable, not
   passing (availability ≠ assignment): blind assigners still had to land each pole.
4. **Blindness honesty:** the assignment pass (33 fresh subagents, one reading file + ratified
   vocabulary each) is the unbiasable step; the vocabulary draft is the biasable step and was
   disclosed as such (the drafter's context contained the three named C1 pairs by Phase-0
   design); R2 ratification is the mitigation, and the two open calls the drafter left to the
   blind assigners were both ruled by them from file prose (perimeter axiom; point_of_capture —
   the latter reversing the reviewer's name-level tilt on body evidence).

## Defects found and fixed en route (pre-existing, not registry-caused)

- `tests/test_axiom_diff.pl` westphalia tests froze pre-regime-swap corpus content: since the
  2026-06-20 twin regeneration NO leg carries the 4 mapped axiom names, so the concept tests
  were silently unrunnable-green (manual-run only, not in the gate). Fixed by freezing the
  OQ-59 ruling's substrate as fixture facts — file now corpus-independent under the standard
  plunit command.
- The same tests' cleanup ran blanket `retractall(axiom_diff:axiom_concept(_,_))` — harmless
  while the seat was empty, but it would have silently wiped the baked registry mid-session.
  Scoped to the 4 fixture names; witnessed by post-run registry count 71.

## Cross-occurrence disclosure

`animals_are_rights_bearing_individuals` recurs in the flash twin of the same kernel/reading
(`census_cross_occurrence.log`; probe positive-controlled). Ratified at R3 with the reach
recorded in the registry provenance block. The registry KEY is the bare axiom name — per-leg
scoping does not exist by construction.

## Not claimed / next steps (landed in substrate)

- NOT claimed: axiom-axis coverage of any corpus. The live scale-up (Haiku-batch proposer over
  ~321 remaining haiku + ~10 live multi-reading kernels, same PROPOSAL→ratify→bake path, R2/R3
  human) is a separate operator spend-go — recorded in the OQ-72 resolution.
- SCOPE-time concept-slot emission: declared absence GAP-24 (`docs/design/design_gaps.md`).
- OQ-75(b) parity-vs-label ruling: blocked_on_human sub-item in OQ-75.
