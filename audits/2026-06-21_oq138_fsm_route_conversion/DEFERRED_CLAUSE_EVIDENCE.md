# OQ-138 deferred clauses — evidence pass (READ only; no conversion)

Date 2026-06-21, live corpus (92 seats). Probes: `deferred_clause_census.pl`, `fcr_ablation.pl`,
`/tmp/counts2.pl` (true unbound cascade-winners). **The diff is the operator's partition input; no
clause is converted here.** Counting note: a BOUND-arg `constraint_signature(C, Sig)` query inflates
counts (cascade-cut bypass, `signature_detection_wiring.md` §1) — all counts below are the UNBOUND
true-cascade-winner numbers.

## Headline: most overrides are INERT or load-bearing-elsewhere — the "N fires" figure overstates blast radius

| clause | cascade-winners | CHANGE `dr_type` | inert (metric already = target) | disposition the evidence points to |
|---|---|---|---|---|
| coupling_invariant_rope | 6 | 6 (scaffold→rope) | 0 | **KEEP candidate** — rope is consumed downstream |
| false_ci_rope | 25 | 12 | 13 | route + victim-discriminate (like FSM) |
| constructed_high_extraction | 50 | **3** | 47 | nearly-free convert (override inert on 94%) |
| constructed_low_extraction | 1 | 0 | 1 | inert |
| constructed_constraint | 0 | 0 | 0 | fully shadowed on live |
| coordination_scaffold | 0 | 0 | 0 | fully shadowed on live |

"Inert" = the override clause's input pattern (mountain/unknown) doesn't match the seat's metric type,
so the identity fallback keeps the metric type unchanged. An inert override manufactures nothing;
converting it is a no-op for those seats.

## coupling_invariant_rope — KEEP candidate (route-vs-keep is the operator's seat)
6 cascade-winners, ALL `scaffold→rope` (a POSITIVE certification), base green, vic 0–5
(`architectural_pattern_validity` vic0, `demographic_resource_allocation` vic2,
`demographic_skill_mismatch_c0` vic2, `propagation_speed_asymmetry` vic5, `scale_ceiling_c0` vic2,
`validation_judgment_separation` vic2).

**Consumer grep (positive-controlled): `dr_type='rope'` IS consumed.** The plan's "nothing reads rope
→ route informational" hypothesis is FALSIFIED. Positive control: the type-literal grep finds 131
branches for known-consumed types (mountain/snare/tangled_rope), so it would find rope branches if they
existed — and it does:
- `dirac_classification.pl:287` `type_to_dirac_class(rope, _, _, first_class)` — rope ⇒ **first_class**
  (the OPPOSITE of the `second_class` that floored the FSM seats).
- `drl_purity_network.pl:190,203` `type_contamination_strength(rope, 0.1)`, `type_immunity(rope, 1.0)`.
- `grothendieck_cohomology.pl:432` `extraction_rank(rope, 1)`; `boltzmann_compliance.pl:364`;
  `maxent_classifier.pl` (rope profiles/overrides); `maxent_report.pl:277`.

So CI-rope is not a free "route a comment": the rope certification PROPAGATES to the same diagnostic
subsystems (dirac/purity/cohomology/boltzmann) that drove the FSM verdict. Reverting `rope→scaffold`
would flip these 6 seats' dirac class (first→second) and immunity (1.0→lower). **Ruling deferred:** keep
unless these 6 are coordination-washed; the discriminant for genuine-vs-washed coordination is the next
evidence to gather (not collected here — needs a coordination-genuineness signal, not a type diff).

## false_ci_rope — route + victim-discriminate, SAME unmask story as FSM (milder)
25 cascade-winners; **12 CHANGE** (scaffold→tangled_rope ×8, scaffold→piton ×3, snare→tangled_rope ×1);
13 inert (unknown→unknown, rope→rope, tangled_rope→tangled_rope).

**Hook ablation (`fcr_override_enabled` 1→0, fresh process, no stale cache):** the 12 changed seats
revert `tangled_rope→scaffold` and the verdict goes `green-yellow-correction → yellow-yellow-commentary`
([stack] surface). **My "scaffold stays green" guess was REFUTED** — routing FCR ALSO unmasks tensions
(base green→yellow; likely →red on the full pipeline, same as FSM, milder because scaffold is a softer
reversion than mountain). So FCR is the same structural situation as FSM: the discriminant won't be
cleanly headline-visible (base unmasks), it lives in the commentary layer.

**Victim split is present** (12 changed = 6 vic=0 + 6 vic>0):
- vic=0: conceptual_framework, divine_legitimacy_substrate, fictional_construct, lausanne_minority_protections, llm_synthesis_capacity, neutron_star_bombardment
- vic>0: basic_law_interpretive_boundary (3), jewish_sovereignty__cultural_zionist (1), press_reformation_causation__strategic_deployment (2), + the 3 piton seats below.

**Piton sub-case (3 seats, scaffold→piton, all vic>0):** press_reformation_causation__mutual_shaping,
refugee_convention_text, statutory_debt_ceiling__coordination_scaffold. `piton` is the OQ-90 FCR-branch
refinement (NOT a plain override; `dr_signature` stays false_ci_rope while `dr_type` becomes piton, via
`resolve_with_perspectival_check`). These need separate handling — the OQ-90 interaction means routing
FCR cannot blindly revert them to scaffold. **Ruling deferred** (operator's partition call); the diff +
victim discriminant are in hand.

## constructed_* — override INERT on 94% of seats; 3 real changes
constructed_high_extraction: 50 cascade-winners, **only 3 CHANGE** (all `unknown→snare`:
`equal_protection_kernel__colorblind_reading`, `institutional_trust_erosion_c0`,
`shinbutsu_ontological_commitment__incoherence_reading`); 47 inert (`snare→snare` — metric already
produces snare). constructed_low_extraction: 1, inert. constructed_constraint / coordination_scaffold:
0 live cascade-winners (fully shadowed by FCR/FNL).

So the "41 fires" framing overstates: the constructed override does real work on **3 live seats**, all
lifting `unknown→snare`. **No config hook** (body-bearing), so the diff was taken via the
metric_based_type_indexed→dr_type comparison (the path that substitutes for an ablation hook).
Converting constructed would: leave 47 inert seats byte-identical; revert the 3 changed seats
`snare→unknown` (honest abstain, OQ-37 pattern) + the constructed signature comments. **Ruling
deferred:** whether those 3 should abstain-to-unknown or keep-snare is the operator's call; the diff is
in hand and the blast radius is 3, not 41.

## What is NOT yet gathered (named, for honesty)
- CI-rope genuine-vs-washed coordination signal (the discriminant a keep-vs-convert ruling needs).
- FCR/constructed full-pipeline (vs [stack]) routed verdicts — the [stack] ablation shows the unmask
  direction; the report-surface color (yellow vs red) needs a pipeline ablation run, same as the FSM
  step-3 witness. Cheap follow-up, not blocking the partition ruling.
