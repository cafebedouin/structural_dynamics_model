# Current Priorities

**Updated:** 2026-05-28  
**Source:** Current state + `docs/unknown_reading_review.md` (May 27)  
**Update this file** after any planning session or significant shift in focus.
Keep it short and specific — vague entries defeat the purpose.

---

## Active (do these before anything else)

1. **Gap A: Author CS fields on the three cyclopean-point testsets**  
   `prolog/testsets/disparity_as_depth_signal.pl`, `cyclopean_point_as_manufactured_center.pl`,
   `power_asymmetry_in_legibility.pl`. Add `cs_kernel_id`, `cs_reading_relation`, and
   `cs_axiom` facts pointing to a shared kernel, following the `autonomy_reading.pl`
   template. Infrastructure is already built — this is authoring. Unlocks Gap B and the
   CS verdict machinery on the constraints where it matters most. Partially advances OQ-04.

2. **OQ-10: Cross-reading comparison tooling**  
   Build the tool that discovers all readings of a kernel via `cs_reading_relation/3`,
   runs the engine on each, and reports which findings are reading-robust vs
   reading-specific. Makes the altar essay's Ω_E (are the cyclopean-point verdicts
   reading-robust?) answerable. Depends on Gap A being closed first (needs `cs_kernel_id`
   and `cs_reading_relation` populated on real testsets). Smallest useful version: a
   Python script that takes a kernel_id, runs the product-site export on each linked
   reading, and diffs H¹ and orbit_signature outputs. See OQ-10 in
   `ISSUES.md` for implementation sketch.

3. **OQ-01: Rope gate Chi ≤ 0 bypass — resolve theoretical status**  
   `drl_core.pl:356`. Must be resolved before v6's conditional H0 confirmation reads cleanly.
   Either confirm it's intentional modeling (document the design choice in logic.md) or
   identify it as an artifact requiring a guard. Low-effort investigation.

## Near-term (do after active items, or if an active item is blocked)

4. **Gap B: Reading declaration in enhanced_report.py**  
   Once Gap A is closed, emit a "Reading Declaration" section in the enhanced report:
   which kernel, which reading, which co-existing readings are unrun, which are
   unrunnable. This is the fix for the engine presenting reading-bounded findings as
   verdicts about the kernel itself. Mostly a Python change on top of Gap A's CS fields.

5. **Gap C: Add seat_declaration_status to CS authority grounding**  
   New field: `declared | concealed | ambiguous`. Fire a verdict when `concealed` or
   `ambiguous` disagrees with structural signals (low theater_ratio, beneficiary presence).
   Modest addition to `cs_pattern_detection.pl` and the JSON schema. Directly implements
   the Seat Theorem's declared-vs-concealed distinction as a first-class engine concept.

6. **OQ-07: Runtime verify cs_drift_mismatch on UID 72c8aa61**  
   `prolog/testsets/conceptual_emergence_reading.pl`. Single Prolog REPL query. ~5 minutes.

## Backlog (track but don't start without discussion)

- **Gap D:** Add Type A/B/C (drift / structural / indexical) annotations to theorem and
  finding output. Mapping already exists implicitly in the theorem structure; making it
  explicit is mostly a reporting-layer labeling change.
- **Gap E:** Self-referential note in `disparity_as_depth_signal` report when
  `false_summit_mountain` fires on a constraint the engine's own architecture is downstream
  of. Special case in enhanced_report.py; requires a manual annotation.
- **OQ-03:** DR self-application — run the engine against DR-the-framework as a constraint
- **OQ-04:** CS 1:N reading structure (partially subsumed by Gap A; revisit after Gap A done)
- **OQ-02:** write_entries determinism — mitigated by green cut; audit
  `integrate_signature_with_modal/3` and rope gate disjunction when bandwidth allows
- **OQ-06:** CS off-case fixtures for cs_drift_unacknowledged / cs_axiom_foreclosed
- **OQ-08:** DR/CS context asymmetry annotation in mismatch reports (one-line report change)
- **OQ-09:** §2.3 Jaccard range overshoot — defer to v6 authoring session

---

## Just completed (2026-05-28)

- **Ship v6 paper — §2.3 correction** — DONE. `docs/observers_not_humans_v6.md` authored.
  H0 conditionally confirmed (sign-flip load-bearing in tangled_rope, 14.6× concentration
  over snare+rope). §2.3 and §3.3 unified as one finding. Rope-gate bypass (OQ-01)
  flagged as conditional assumption. OQ-05 resolved.

---

*To redirect a session: point the model at this file and say "re-read PRIORITIES.md."  
Source document for last restructure: `docs/unknown_reading_review.md` (May 27); v6 completion 2026-05-28.*
