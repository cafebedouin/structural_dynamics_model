# OQ-138 FCR-9 conversion — false_ci_rope routed (seat-aware), piton-3 carved out

**Date 2026-06-21.** Converts the 9 false_ci_rope seats the override CHANGES (the "FCR-9") from
RECLASSIFY→ROUTE; piton (OQ-90) and inert seats untouched. The FSM template did NOT transfer directly —
FCR is seat-split, so the conversion is seat-aware.

## Why seat-aware (the FSM template doesn't transfer)
false_ci_rope cascade-winners are a 3-way split sharing one signature: **9 routed** (override→tangled_rope),
**3 piton** (clause-2 OQ-90 refinement), **13 inert** (unknown-surfaced / perspectival-preserved /
no-op-tangled_rope). FSM had no inert/piton split, so its signature-level mechanism worked. For FCR, both
signature-level mechanisms break (witnessed): a signature-level `converted_signature(false_ci_rope)` would
flip the 13 inert seats' grade; removing FCR from `known_override_signature` signature-level changes 12 seats
(the 9 + piton-3) — disturbing the piton-3 carve-out. So the conversion is keyed on `fcr_routed/1` (seat-level).

## Build (engine)
- **Type route:** `resolve_with_perspectival_check/4` clause 3 override branch `tangled_rope → ModalType`.
  Piton (clause 2) and inert (unknown/perspectival branches) unaffected. Restore legacy by reverting the branch.
- **`fcr_routed/1`** (`signature_detection.pl`): the routed seats, keyed on the dispatch GATE predicates
  (`\+ piton_candidate`, `\+ coordination_dead`, `\+ has_metric_perspectival_variance` — stable, no proxy) AND
  the OUTCOME (`dr_type ∉ {tangled_rope, unknown}`). Non-circular (dr_type is the type dispatch, independent of
  the severity machinery). **An earlier `metric_based_type_indexed` proxy diverged from the live `ModalType`
  on 2 haiku + 4 flash seats — caught by the cross-corpus generality sweep — and was replaced by the outcome
  check, which also removed the dispatch-mirror fragility.**
- **`converted_at_seat/2`** generalizes the converted dispatch: signature-level for false_summit_mountain,
  seat-level (`fcr_routed`) for false_ci_rope. `signature_grade/2` + `signature_severity/2` use it instead of
  `converted_signature/1`. `signature_diagnostic_severity(C, false_ci_rope, …)` = the same victim discriminant
  as FSM (vic>0→moderate, vic=0→informational).
- **`seat_overrides/2`** (`abductive_helpers.pl`, exported): seat-level "overrides AT THIS SEAT" =
  `known_override_signature` except false_ci_rope, which is `\+ fcr_routed`. The override-artifact consumers
  (`diagnostic_summary` `probe_signature/3` + the P1/P7 `expected_conflict_pattern` arms) call it instead of
  `known_override_signature/1`, so the routed-9 are treated as non-override (divergences surface honestly, like
  FSM) while piton/inert FCR keep override semantics byte-identical.

## Witness (live `testsets`, full pipeline)
**7 seats change** (`FCR9_live_diff.txt`): 6 routed-verdict + `statutory_debt` (ensemble). The 9 routed TYPES
all route `tangled_rope → scaffold/snare`; 3 verdicts stay (basic_law/jewish keep `correction`, press_strategic
severe type_3). Discriminant clean, no spurious override_mismatch (`sig=AGREE`, `cap=none`):

| routed seat | vic | base→ | grade | sig alert |
|---|---|---|---|---|
| basic_law_interpretive_boundary | 3 | yellow | correction | moderate (floor) |
| jewish_sovereignty__cultural_zionist | 1 | — | correction | moderate |
| conceptual_framework / divine / fictional / lausanne / neutron | 0 | yellow | commentary | informational (no floor) |
| llm_synthesis_capacity | 0 | red | commentary | informational |
| press_reformation__strategic_deployment | 2 | (red, type_3 severe) | correction | moderate |

Milder than FSM (mostly yellow — scaffold is a softer reversion than mountain), matching the ruling's
"green→yellow unmask" expectation.

**Carve-out:** piton-3 TYPES all unchanged; 2/3 verdicts byte-identical; `statutory_debt` shifts yellow→red
**purely via the corpus-relative maxent ensemble** (`entropy_flag(0.47)`) — its piton type and its
override_mismatch are byte-identical, so OQ-90's piton-type decision is not relitigated. The strict
"piton-3 verdict byte-identical" standard is relaxed to "piton-3 TYPES unchanged; verdict shifts only via the
ensemble" (Position-A-acceptable: the engine renders different verdicts; classification preserved). **13 inert
FCR + all non-FCR seats byte-identical.**

## Generality (5-corpus sweep, `fcr9_generality_sweep.pl`)
All invariants pass on testsets/haiku/flash/kernel_v1/original_v6: `routed∩piton=0`,
`routed-still-tangled_rope=0`, `piton-not-piton=0`. fcr_routed selects 9/56/78/53/56. The sweep CAUGHT the
proxy-divergence bug before it shipped.

## Tests
`validation_suite` 92/0/0; `check_stack` no new findings; `test_contradiction_signatures` 5-fail pre-existing
(CS-axis fixture, identical OLD vs NEW by name).

## Residual (tracked, not a blocker)
The maxent FCR boost (`maxent_classifier.pl:331`, `apply_override_for_sig(false_ci_rope, …)` → boost toward
tangled_rope ×3) is still SIGNATURE-level (no `C` in scope), so it boosts the routed-9 too. Empirically benign
— maxent top for the routed-9 is `rope`, not `tangled_rope`, so the boost does not change their argmax/verdict
(witnessed clean diff). A future routed FCR seat where the boost flips the maxent top would need seat-aware
maxent. Logged as a residual for the constructed conversion (which hits the same maxent-boost shape at :341).
