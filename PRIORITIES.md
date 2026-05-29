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

3. **OQ-10: Cross-reading comparison tooling**  
   Build the tool that discovers all readings of a kernel via `cs_reading_relation/3`,
   runs the engine on each, and reports which findings are reading-robust vs
   reading-specific. Makes the altar essay's Ω_E (are the cyclopean-point verdicts
   reading-robust?) answerable. Depends on Gap A being closed first (needs `cs_kernel_id`
   and `cs_reading_relation` populated on real testsets). Smallest useful version: a
   Python script that takes a kernel_id, runs the product-site export on each linked
   reading, and diffs H¹ and orbit_signature outputs. See OQ-10 in
   `ISSUES.md` for implementation sketch.

4. **OQ-01: Rope gate Chi ≤ 0 bypass — resolve theoretical status**  
   `drl_core.pl:356`. Must be resolved before v6's conditional H0 confirmation reads cleanly.
   Either confirm it's intentional modeling (document the design choice in logic.md) or
   identify it as an artifact requiring a guard. Low-effort investigation.

## Tier 2: Handle in natural flow (lower stakes; do alongside other work)

These are real but not latent-trap work. OQ-27 is a credibility-risk fix; OQ-23, OQ-24,
OQ-28 are documentation hygiene and scope clarifications. Attach to the next relevant
edit rather than scheduling as standalone work.

- **OQ-27: H¹ definition — specify signature-resolved orbit in v6.13 Theorem 2**  
  Amend phrasing to clarify H¹ operates on signature-resolved types, not raw
  classify_from_metrics output. Add comment at `cohomological_obstruction.pl` confirming
  the path goes through `dr_type` (signature-resolved). Prevents readers who run
  `classify_at_time` from concluding the paper is wrong.

- **OQ-23: coexists_with exclusion — documented intent or enforced guard?**  
  Add a one-paragraph self-flagging note in the FPN architecture-note open-items section
  making it impossible to miss on future edits. (Likely outcome: loud documentation, per
  the build's mark-drift discipline, rather than a guard.)

- **OQ-24: forecloses absence — add engine-level comment**  
  One-line comment at `compute_edge_contamination/7` citing the FPN injection test and
  stating that forecloses is structurally excluded by gradient-orthogonality. Attach to
  next network-module edit.

- **OQ-28: Seat Theorem v1.1 asymmetry — mark scope-clarifications as such**  
  One-sentence preamble in v1.1 changelog: edits (2) and (3) are scope-clarifications,
  not result-claims, and do not require run-grounding for that reason. (Edit (1) is
  witnessed; clarify why the others are not.)

## Near-term (do after active items, or if an active item is blocked)

5. **Gap B: Reading declaration in enhanced_report.py**  
   Once Gap A is closed, emit a "Reading Declaration" section in the enhanced report:
   which kernel, which reading, which co-existing readings are unrun, which are
   unrunnable. This is the fix for the engine presenting reading-bounded findings as
   verdicts about the kernel itself. Mostly a Python change on top of Gap A's CS fields.

6. **Gap C: Add seat_declaration_status to CS authority grounding**  
   New field: `declared | concealed | ambiguous`. Fire a verdict when `concealed` or
   `ambiguous` disagrees with structural signals (low theater_ratio, beneficiary presence).
   Modest addition to `cs_pattern_detection.pl` and the JSON schema. Directly implements
   the Seat Theorem's declared-vs-concealed distinction as a first-class engine concept.

7. **OQ-07: Runtime verify cs_drift_mismatch on UID 72c8aa61**  
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

- **OQ-26: Resolve ε generation-dependency caveat** — RESOLVED (option a). 
  `docs/deferential_realism_paper_v6.13.1.md` amended: Axiom 2 now clarifies that 
  ε-invariance holds **across observer positions** but **not across generation runs**. 
  New **Generation-dependence note** scopes all ε-dependent statistics (H¹ distributions, 
  classification proportions, divergence counts) to "one coherent generation," making the 
  published record honest about what is and isn't a point estimate. Consistent with v7 §6. 
  Option (b) (constraining generation for run-reproducibility) deferred as separate item.

- **OQ-25: Seal the chimera load-discipline** — RESOLVED (both options). `docs/cs_load_discipline.md`
  documents the invariant, grouping-key rationale, and regeneration protocol. `prolog/config_validation.pl`
  enforces it: new `config_violation/1` clause fires when any ConstraintAtom with `cs_story_uid/2`
  has conflicting ε values, halting before any CS-layer predicate runs. Verified clean on live corpus;
  synthetic conflict correctly rejected. See ISSUES.md OQ-25 for resolution detail.

- **Ship v6 paper — §2.3 correction** — DONE. `docs/observers_not_humans_v6.md` authored.
  H0 conditionally confirmed (sign-flip load-bearing in tangled_rope, 14.6× concentration
  over snare+rope). §2.3 and §3.3 unified as one finding. Rope-gate bypass (OQ-01)
  flagged as conditional assumption. OQ-05 resolved.

---

*To redirect a session: point the model at this file and say "re-read PRIORITIES.md."  
Source document for last restructure: `docs/unknown_reading_review.md` (May 27); v6 completion 2026-05-28.*
