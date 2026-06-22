# OQ-138 constructed-3 conversion — constructed_high_extraction unknown→snare routed (seat-aware, CLAIM discriminant)

**Date 2026-06-21.** Converts the 3 live `constructed_high_extraction` seats the override lifts `unknown→snare`
from RECLASSIFY→ROUTE (revert to the honest abstain `unknown`); the diagnostic rides a CLAIM-discriminated
severity that preserves seat #2's floor. Scoped to constructed_high unknown-input (the live changers);
constructed_high mountain-input + constructed_low/constraint have 0 live changers (separate sub-item).

## The claim-discriminant (new — FSM/FCR used victim, this uses the authored claim)
The 3 routed seats are all vic>0, so the victim discriminant doesn't distinguish them — the AUTHORED CLAIM
does. A MOUNTAIN claim over a high-extraction finding is the concealment (false-summit shape) → `severe`
floor; non-mountain claims already admit structure → `informational` (route). This replaces the floor the
manufactured snare used to carry: pre-conversion #2 was RED via `type_1_false_summit(severe)` (claimed
mountain, dr_type=snare); post-conversion dr_type=unknown so type_1 reads informational, and the constructed
signature's `severe` keeps #2 RED. The floor SOURCE moved (type_1 → signature); the headline is preserved.

## Build (engine)
- Type route: `resolve_modal_signature_conflict(unknown, constructed_high_extraction, _)` `snare → unknown`.
- `constructed_routed/1`: cascade-winner `constructed_high_extraction` with post-conversion `dr_type=unknown`
  (outcome-keyed, robust). Uses the UNBOUND cascade winner (`constraint_signature(C,Sig), Sig==...`) — a
  bound-arg query trips on the constructed_high DETECTOR even when false_ci_rope shadows it (e.g.
  `superheavy_decay`, an FCR inert seat — caught by the sweep showing 4 routed when only 3 changed; §1 gotcha).
  **The same bound→unbound fix was applied to `fcr_routed/1` (behavior-preserving: FCR counts unchanged).**
- `converted_at_seat(C, constructed_high_extraction) :- constructed_routed(C)`; `signature_diagnostic_severity`
  claim-discriminant (mountain→severe, else→informational); `seat_overrides(C, constructed_high_extraction) :-
  \+ constructed_routed(C)` (routed seats non-override in probe_signature/P1/P7).

## Witness (live `testsets`, pipeline)
- **#2 (institutional_trust_erosion, claimed mountain) KEEPS RED — byte-identical** (`yellow|red|correction`),
  now floored by `signature_correction=severe` (type_1 dropped to informational). **The kill condition: the
  claim-discriminant holds #2's floor — witnessed, the protein-equivalent positive case.**
- #1/#3 (equal_protection, shinbutsu; claimed tangled_rope) route to `yellow|yellow|commentary`
  (informational, no floor) — cleaner than the bare-abstain probe's red, because `seat_overrides` removed the
  spurious override_mismatch that had inflated them.
- **Only 2 verdicts change; 47 inert constructed_high + all non-constructed byte-identical.** The 3 routed
  seats' `dr_type` goes `snare → unknown` (the route).

## Generality (5-corpus sweep)
`routed-not-unknown=0` and `mountain-routed→severe` (mtn-no-severe=0) on testsets/haiku/flash/kernel_v1/
original_v6 — the kill condition holds GENERALLY, not just for #2 on live. constructed_routed = 3/63/11/0/0.

## Maxent residual (the operator's load-bearing warning — confirmed for constructed)
`maxent_classifier.pl:341` boosts constructed_high toward tangled_rope ×3, signature-level (no `C`). For the
routed #1/#3 (dr_type=unknown), the boost flips `maxent_top` to `tangled_rope` at the pipeline surface (unlike
FCR, where top stayed `rope`). It contributes a maxent disagreement to #1/#3's base but does NOT change the
headline (yellow; #2 is red via the severe, maxent_top=snare). **Benign-here, but the warning was right that
constructed couldn't inherit FCR's luck.** Seat-aware maxent (plumb `C` through `apply_override_for_sig`)
remains a tracked GAP shared across FCR/constructed.

## Tests
`validation_suite` 92/0/0; `check_stack` clean; gate GREEN.
