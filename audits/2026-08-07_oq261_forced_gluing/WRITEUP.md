# WRITEUP — OQ-261 forced-gluing experiment (CLOSED)

**Executed:** 2026-08-07 (C1 recon, C2 proposal) / 2026-08-08 (C3 execution, R2-signed).
**OQ:** OQ-261.
**Verdict (scoped):** H_perf — "the performance presheaf always admits a global
section" — is CONTRADICTED under every partition variant that clears the pre-committed
sparsity floor (pooled H¹=45 frozen, H¹=55 inclusive; restrictive NULL by sparsity) and
supported nowhere; the fiat family's topic obstruction penetrates the authored
performance seats, whose types are story-derived and inherit the two-bloc structure.
The ballot's guaranteed total verdict therefore CANNOT be modeled as sectioning the
authored performance seats of this corpus; the discard structure a topic-presheaf
ballot would need is real and exact (minimum discard = 3 = precisely one bloc, both
blocs achieve it). No engine object is named; no promotion.
**Manifest cite:** `pipeline_output.json` `pipeline_run_at=2026-08-07T23:44:21Z`,
`n_constraints=225`, `code_commit_short=f724379`. Corpus md5-frozen through C3
(`c3_fingerprints_before.txt` == `c3_fingerprints_after.txt` == RECON baseline).

## Pre-registration

`PROPOSAL.md` v2 was signed R2 as written (2026-08-08) with two riders recorded in it
before any probe ran (control non-independence + re-run pre-commitment;
proxy-disagreement branch rule). This writeup quotes the frozen cells; it does not
amend them. v1 history: `83a647ea`; v2: `769edec0`.

## Results by frozen cell

**Cell 1 (decides H_perf) — three variants, per-reading n in
`performance_presheaf_probe_output.txt`:**

| variant | pooled n | n_real | contributing | floor (≥6 ∧ ≥4/6) | H0 | H¹ |
|---|---|---|---|---|---|---|
| V_frozen | 21 | 14 | 5/6 | met | 0 | **45** |
| V_inclusive | 23 | 16 | 5/6 | met | 0 | **55** |
| V_restrictive | 9 | 5 | 5/6 | **not met (n_real 5<6)** | — | **NULL** |

The frozen enumeration named all-glue / all-obstruct / glue-obstruct-mixed; the
realized shape (obstruct, obstruct, NULL) sits between the named cells and is reported
at exactly that altitude: **contradicted wherever decidable, supported nowhere, one
variant undetermined by sparsity.** Neither the pre-registered "falsified as stated"
label (which required all three) nor any support claim is licensed; what is licensed:
the load-bearing prediction failed both times it could be tested.

**Mechanism (why it obstructs):** bloc A readings' performance seats type `rope`,
bloc B's type `scaffold` — seat types are derived per-story
(`dr_type_for_stakeholder` is position-indexed within the story; seats have no
cross-story identity, per the CS identity model), so pooling imports each story's
typing and the performance frame inherits the topic's bloc structure. This is NOT
tautological obstruction: the flat control's 7 seats all type `scaffold` (H¹=0) and
the comparator's `fetterley_transfer_kernel` GLUES at/above floor — glue is reachable
by the same read.

**Cell 2 (premise check) — PASSES:** discard minimum = **3**, achieved by exactly 2
subsets: remove bloc A entire, or bloc B entire. A ballot modeled on the topic presheaf
must discard a full bloc to emit a total verdict — the discard is real, visible, and
exactly bloc-shaped.

**Cell 3 (symmetry control):** frozen/inclusive n_real=2, H¹=0; restrictive n_real=1,
null/null. Expected values exactly; **no probe-bug witness** (no numeric H at
n_real<2 anywhere).

**Cell 4 (control × topic):** control pooled stakeholder vector n_real=7, H¹=0 —
obstruction is the family's property, not the topic's. Premise checks all hold; no
abort condition fired.

## Comparator base rate (16 `real_closure` families, mechanical agent/excluded proxy)

**15/16 obstruct; 1/16 glues (`fetterley_transfer_kernel`); 0 NULL** (all rows
cleared the normalized floor). Fiat's proxy read (H¹=104, obstruct) AGREES with the
hand partition's decidable variants, so per rider 2 no material disagreement exists and
the column stands as a base rate: pooled seat-frame gluing over `real_closure`
families is rare (1/16) — fiat is typical, not special, and Cell 1's obstruction is
not a fiat-local artifact.

## Positive control (probe validity, numeric criteria)

`positive_control_probe_output.txt`: state_execution_authority triplet, 156 contexts —
**obstructed = 97 ≥ 85 (criterion 1 PASS)**; **Σ H¹ = 164 == same-run
`cs_kernel_divergence/4` count = 164 (criterion 2 PASS, rider-1 caveat below)**. The
registry's 253/468 record did NOT reproduce (live count 164) — pre-committed handling:
both numbers reported, the ≥85 floor stands (97 clears it). The 253 record is
engine-state-stale (measured before multiple classification-regime changes); the floor
derivation from it remains valid as a floor.

## What this resolves (OQ-261)

- The corrected mapping's testable half ran and produced a verdict: **in this corpus
  the authored performance seats cannot carry the ballot's total verdict** — the
  substitute presheaf, as operationalized over authored seats, obstructs exactly like
  the topic. The ballot's "always a winner" property must therefore live in an object
  this corpus does not author (a per-round performance record with seat identity
  across stories). "Verdict channels that section a substitute presheaf" is NOT named
  as an engine object — per the frozen placement rule, nothing is promoted to
  `reading_registry.pl`.
- The positive product is Cell 2: the **bloc-shaped discard minimum** — a forced
  total verdict over a `real_closure` family has an exact, computable discard cost
  (here: one entire bloc), which is the DR-native restatement of "the ballot cannot
  distinguish resolved from out-performed": what it drops is not noise but a bloc.
- Base rate: seat-frame gluing is rare across `real_closure` families (1/16), so any
  future substitute-presheaf model must expect obstruction as the default, not the
  exception.

## POST-HOC follow-ups (2026-08-08, operator review after close — NOT pre-registered;
## labeled per the unguarded-axis-swap discipline)

**Symmetric read — Cell 1's interpretive weight is DEMOTED.**
`posthoc_symmetric_read_probe.pl` ran `obstruction_from_vector` over three pooled seat
sets: performance (V_frozen), topic-community (the complement), and all agent seats.
H¹ densities: **0.4945 / 0.60 / 0.4952** — and in EVERY set H¹ equals exactly
(#rope)·(#scaffold): each pooled sub-vector is a pure two-token rope/scaffold split.
The bloc structure penetrates ANY sub-vector pooled across the six readings, so **Cell
1's obstruction carries no independent information about performance seats
specifically — it is a restatement of the family's bloc structure through story-derived
seat typing.** The scoped verdict stands (H_perf-as-operationalized did fail where
decidable) but its evidential weight against the performance-presheaf CONCEPT is
minimal: the operationalization could not have isolated it. Caveats: the topic-community
set is itself thin (n_real=5, contrib=3 — below the symmetric floor), so the strong
comparison is perf ≈ all (dense, 6/6 contributing, densities within 0.001); the flat
control's glue is a single story (no cross-story structure to inherit) and fetterley is
one case — neither demonstrates independence.

**253 → 164 diagnosis (`posthoc_253_164_diagnosis.txt`): engine-regime drift.** The
three fixture files are byte-stable since the record era (one commit ever — the
2026-06-05 archive move); `cs_kernel_divergence/4` is edge-free and byte-identical
through Item B (B retyped kernels via edges only — ruled out by mechanism); the live
per-reading type sets moved substantially vs the record (snare appears in all three
readings, unknown vanishes from retributive/deterrence; the record's specific
naturalized witness still reproduces). Verdict: the record was correct-then and is
engine-state-stale; 164 is the current-engine truth; **the OQ-266 re-run is a fixture
question, not a corpus question.** Note the ≥85 floor was derived from the
non-reproducing 253 — it passed (97), but on contested provenance; the invariant
(rider-1-caveated) is what carried the control.

## Residue (declared)

1. **Rider 1 (control non-independence):** criterion 2 compares against
   `cs_kernel_divergence/4` while `test_cs_kernel_registry` is red on
   `divergence_silent_at_observed_agreement_context` (a silent-drop shape). Both
   criteria passed, but the invariant is not definition-identical while that fixture
   is red. **Pre-committed: re-run `positive_control_probe.pl` after the OQ-266-class
   fixture rot is fixed** (obligation logged on OQ-266). Clean re-run hardens the
   control retroactively; a failed re-run demotes C3's cells to draw-level
   observations.
2. **Frozen-cell enumeration gap:** the realized Cell-1 shape (obstruct/obstruct/NULL)
   was not among the three named outcomes; verdict stated at scoped altitude above.
3. **Restrictive variant undetermined — and it is a BOUNDARY read:** pooled n_real=5
   against a floor of 6, one seat short. Not a comfortable margin; a single additional
   real round-participant seat would have decided it. Not evidence either way, and not
   robustly NULL either.
4. **Registry record staleness:** 253/468 → live 164 (auto-memory updated same
   session); any future consumer of the 253 figure must re-witness.
5. **Story-derived seat typing bounds the operationalization:** with no cross-story
   seat identity, any pooled seat-frame read partially reflects story typing. The
   fetterley/control glue rows show the read is not vacuous, but a corpus authoring
   per-round performance objects would be the stronger substrate (next-experiment
   material, not this OQ's).
6. Carried from RECON: `verdict_join` authored-null for 2/6 readings; the ~20
   untracked `testsets/*_contradictions.pl` files remain a next-session item (corpus
   stayed frozen through C3, witnessed).

## Evidence map

- `RECON.md` — C1 read-only recon (edge table, obstruction, family H¹, seat reads,
  flat-control fingerprint, frame-mismatch note).
- `PROPOSAL.md` — frozen v2 + R2 riders (the pre-registration this writeup reports
  against).
- `family_frame_probe.pl` / `family_frame_probe_output.txt` — C1 probe + raw output.
- `flat_control_fingerprint.txt`, `verdict_join_headlines.txt` — C1 tables.
- `performance_presheaf_probe.pl` / `performance_presheaf_probe_output.txt` — C3 main
  probe + raw output (cells 1–4, comparator, MC tokens).
- `positive_control_probe.pl` / `positive_control_probe_output.txt` — C3 probe
  validity control + raw output.
- `c3_fingerprints_before.txt` / `c3_fingerprints_after.txt` — corpus-freeze witness.
- `posthoc_symmetric_read_probe.pl` / `posthoc_symmetric_read_probe_output.txt` —
  POST-HOC symmetric read (Cell-1 demotion; 2026-08-08 operator follow-up).
- `posthoc_253_164_diagnosis.txt` — POST-HOC divergence-record diagnosis
  (engine-regime drift; fixture files stable; Item B ruled out by mechanism).
