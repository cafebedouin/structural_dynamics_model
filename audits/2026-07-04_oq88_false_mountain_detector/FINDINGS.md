# OQ-88 FINDINGS — false-mountain as kernel-false-negative detector

**Date:** 2026-07-04 (sweep executed same day; kernel_v1 classification run_at
2026-07-05T04:31:53Z UTC). **Pre-registration:** `PROPOSAL.md` in this directory, frozen before
any run. **Script:** `python/audits/oq88_false_mountain_detector.py` (copy archived here).
**Raw results:** `oq88_sweep_results.json`.

## Verdict (pre-registered three-cell, per-cell evidence grade)

| Cell | Result | Grade |
|---|---|---|
| 1. Positive fires | **YES — both full-D positives fire** (`demographic_skill_mismatch_c0` live; `collapse_mechanism_ambiguity` World3 archive-lineage). N=2, each layer witnessed per Pin 2. | **WITNESSED** |
| 2. Negative over-fires | **No over-fire observed — but no labeled true-negative exists**, and the base rate carries almost no power (kernel_v1 flinch tail = 0 of 1 firing). | **INFERRED (weak)** |
| 3. D′ (= D ∧ regime-omega) recovers | **MOOT — the discriminator SATURATES**: regime-omega present on 4/4 live flat candidates, 4/4 routing-unknown, 1/1 kernel_v1 firing. D′ ≡ D on every observed case; zero discriminating power as operationalized. | **INFERRED / instrument-saturated** |

**Pre-registered cell mapping → D is a gate-signal CANDIDATE (cell: pos ✓ ∧ neg clean), with the
mandatory caveat spoken: cell 2 is base-rate-inferred, so this is a candidate, not a witnessed
gate.** The Ω_P auto-route seat decision — should an authored inevitability framing route to a
necessity-vs-contingency kernel? — is **the operator's residual**, not resolved here. Given the
saturation finding (below), the practically safe landing if the operator declines the full ruling
is the light seat the plan anticipated: **false-mountain flag prompts an operator kernel-vs-flat
review** — and that review is cheap (3 unreviewed live candidates + 4 routing-unknown; list below).

## Phase 1 — live routing partition (n=128 live corpus, 9 alert-firing Layer-B)

| Bucket | n | Members |
|---|---|---|
| **flat (D fires)** | 4 | `demographic_skill_mismatch_c0` (known positive), `longevity_mismatch`, `scale_ceiling_c0`, `technology_diffusion_asymmetry` |
| **kernel-routed (caught, excluded)** | 1 | `neutron_star_bombardment_reading` → kernel `actinide_replenishment_mechanism` (CSR-truthy manifest verified directly; it lives in the `flat/` *directory*, which does not imply flat *routing* — CSR is the source of truth) |
| **routing-unknown (fail-closed)** | 4 | `architectural_pattern_validity`, `demographic_resource_allocation`, `propagation_speed_asymmetry`, `validation_judgment_separation` — generated stories (story_provenance present) with no manifest on disk |
| **routing-ambiguous** | 0 | — |
| undetermined-seat (no alert, h1 null) | 1 | `organization_floor_c0` (mountain→rope, `h1_band=null`, no alert; flat-routed; reported, folded in neither direction per OQ-51) |

**Coverage finding (new, witnessed):** 4/9 of the live firing set is routing-unknown — Layer A
has a ~44% coverage hole on exactly the set the detector wants to gate. As a *gate*, D can only
ever fire on manifest-lineage stories; fail-closed on the rest. Any wiring of D must state this.

## Phase 2 — controls (all green; probe NOT broken)

- **Positive, full-D N=2 (Pin 2):** demographic = live Layer B ∧ china 163143 flat manifest ∧
  not in kernel map. World3 = archived Layer B (`oq90_piton_refinement/pipeline_output.preedit.json`,
  commit 411db0e; all 3 sibling outputs consistent) ∧ 171605 flat manifest ∧ not in kernel map.
  Severity regime note: World3's alert is `severe` (pre-OQ-128 split) — the pinned predicate keys
  on alert *type*, so the positive fires; a severity-pinned predicate would have missed it by
  regime accident (recon amendment, recorded in PROPOSAL.md before the run).
- **Dispatch (two-sided):** reader found the known mountain→rope; join resolved BOTH known flat
  manifests; `_c0` suffix normalization exercised on the live positive; known kernel-routed live
  constraint (`ability_ceiling_reading`) landed in the kernel bucket. Map sizes: kernel 407 ids,
  flat 269 ids.
- **Discriminator sanity (Pin 3):** the World3 183123-draw regime trace is capturable (Tier 1:
  `paradigm`, `belief_system`, `reorganiz`) — logged as *discriminator-is-representable*, NOT
  D′-validated-on-World3 (cross-draw). Within-draw: the 171605 full-D manifest matches only
  Tier 2 (`construct`); the surviving World3 `.pl` (`archives/datasets/kernel_v2_test2/pl/`)
  matches Tier 1 (`paradigm`) + Tier 2.

## Phase 3 — kernel_v1 flinch-tail base rate (Pin 1: base rate, NOT a negative control)

Fresh `classify_corpus('archives/datasets/kernel_v1', 'pipeline_output_kernel_v1_oq88.json', None)`
at HEAD `e438723b` (dirty: prose-only `signature_detection.pl` explanation strings), 1,106/1,106
classified. The prior `pipeline_output_kernel_v1.json` (e8189d1, 2026-07-02) was NOT reused —
output-changing engine commits landed since (OQ-138 route/maxent-on-lever, OQ-205 ε fail-closed)
— and was not overwritten.

- Claimed-type distribution: tangled_rope 779, snare 191, rope 78, **mountain 41**, scaffold 10,
  piton 7. Of 41 claimed mountains: 38 stay mountain, 2 → rope, 1 → scaffold.
- **Alert-firing false-mountains: 1/1,106** (`maxwell_demon_impossibility`, informational,
  h1=0, regime-omega Tier 2 present) ⇒ **flinch tail = 0/1**.
- Channel liveness (didn't-look control): 1,082/1,106 constraints carry ≥1 verdict_join alert —
  the alert channel demonstrably fires on this output; the near-zero false-mountain rate is
  measured-empty, not didn't-look.
- Undetermined edge: `statutory_debt_ceiling__constitutional_nullity_reading` (mountain→rope,
  **no** false-summit alert, h1=4) — a second alert-free mountain→rope shape, reported as-is.
- **Escalation to original_v5/v6: DECLINED**, per the pre-registered rule — the tail is empty,
  not dense; and with the discriminator saturated (below), more breadth would measure the same
  ceiling, not more discrimination.

## The load-bearing negative finding: the D′ discriminator saturates

The pre-registered regime-omega instrument (Tier 1 ∨ Tier 2 over manifest omegas ∪ testset
`omega_variable/3` facts) is present on **every** false-mountain measured — live flat (4/4),
live routing-unknown (4/4), kernel_v1 (1/1). Root cause is visible in the corpus itself: this
corpus's omega-authoring convention routinely frames uncertainty as natural-vs-constructed
(Tier 2 terms `natural`/`construct` appear in the omega text of nearly every story in the
neighborhood). Consequences:

1. **D′ cannot serve as the refined gate** — the pre-registered recovery hypothesis is
   unfalsifiable on this corpus at this operationalization. If cell 2 ever turns bad, the
   fallback is the operator-review prompt, not D′.
2. The manifest-omega-only source is more selective (2/4 live flat present: `scale_ceiling_c0`
   Tier 1 `regime`; `technology_diffusion_asymmetry` Tier 2 `inevitab`; demographic and
   longevity ABSENT) — but it would *miss the demographic positive*, so it is not a viable
   discriminator either; recorded as a shape for any future instrument, not a result.
3. Anchor-derivation caveat from PROPOSAL.md stands: the tiers were in-sample on both positives;
   saturation makes the in-sample worry moot in the direction that matters (nothing was excluded).

## Operator handoffs (the residuals — Ω_P, not self-resolved)

1. **The Ω_P auto-route ruling** (from the OQ): should an authored inevitability/"mountain"/
   "irreducible" framing route to seat necessity-vs-contingency as competing readings? Evidence
   posture: detector candidate stands (cell 1 witnessed), cell 2 weakly inferred, refinement D′
   unavailable (saturated). The light-seat alternative — false-mountain flag prompts an operator
   kernel-vs-flat review — is compatible with everything measured and may need only prompt-wiring.
2. **Optional witnessability upgrade (Pin 3):** label ONE of the following as a confirmed flinch
   (authored mountain, computed rope, genuinely no contestable seat) and cell 2 upgrades to
   witnessed on a re-run (the labeled flinch must itself satisfy D — positive-control-of-the-
   negative): live flat candidates `longevity_mismatch`, `scale_ceiling_c0`,
   `technology_diffusion_asymmetry`; routing-unknown `architectural_pattern_validity`,
   `demographic_resource_allocation`, `propagation_speed_asymmetry`,
   `validation_judgment_separation`.

## Artifacts

- `PROPOSAL.md` — frozen pre-registration (predicate, tiers, three pins, cells, halt conditions)
- `oq88_sweep_results.json` — full raw sweep output (partition, controls, kernel_v1 rows)
- `oq88_false_mountain_detector.py` — archived copy of the sweep script (canonical:
  `python/audits/`)
- `classify_kernel_v1.log` / `classify_kernel_v1.stderr.log` — kernel_v1 classification witness
  (manifest line: n=1106, commit e438723, dirty)
- `outputs/pipeline_output_kernel_v1_oq88.json` — kernel_v1 per-constraint output (gitignored;
  regenerable via the logged classify_corpus call)

**Code/corpus state:** live output `pipeline_output.json` run_at 2026-07-04T15:30:49Z, n=128,
commit 23b7faa (prolog delta 23b7faa..HEAD = `giant_component_analysis.pl` only — not in the
Layer-B path); kernel_v1 output at HEAD e438723b. Archived World3 Layer B: commit 411db0e
(2026-06-11). Cited per the engine-regime × corpus rule (KNOWN_STATE 2026-07-02).
