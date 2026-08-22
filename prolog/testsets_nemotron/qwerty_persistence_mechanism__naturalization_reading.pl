% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Layout Persistence — Natural Adequacy Reading
 *   domain: economic_history/technology_studies/path_dependence_theory
 *
 * SUMMARY:
 *   This constraint story represents the naturalization reading of the QWERTY
 *   persistence kernel: the claim that QWERTY became dominant and persists
 *   because it achieved genuine adequacy through iterative improvement in the
 *   late 19th century, and that alternative layouts (notably Dvorak) failed
 *   in fair market competition because their measured advantages were small,
 *   contested, or non-existent for typical users. The reading holds that the
 *   switching costs observed today reflect genuine human capital investment
 *   in a layout that was, and remains, functionally adequate. No systematic
 *   beneficiary class extracts rents from QWERTY's persistence; the
 *   arrangement is a settled coordination equilibrium. The measurement series
 *   tracks the slow accretion of institutional framing (ergonomic standards,
 *   educational curricula, OS defaults) that raises theater_ratio and
 *   suppression_requirement modestly over time without changing the
 *   fundamental extraction profile.
 *
 * KEY AGENTS:
 *   - touch_typists: Primary coordinated population (moderate/biographical/constrained/global) — invested skill in QWERTY through education and practice; switching requires genuine retraining
 *   - keyboard_manufacturers: Coordinated producers (powerful/biographical/mobile/global) — produce to the dominant standard; no evidence of active suppression of alternatives
 *   - ergonomics_researchers: Analytical observers (analytical/generational/analytical/global) — study layout efficiency; contested findings on Dvorak advantage
 *   - alternative_layout_advocates: Excluded voices (moderate/biographical/constrained/global) — advocate Dvorak/Colemak; argue institutional inertia blocks fair comparison
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.08).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Layout Persistence — Natural Adequacy Reading").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies/path_dependence_theory").

domain_priors:emerges_naturally(qwerty_persistence_mechanism__naturalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '14fd543b-0097-4f80-badb-098ea34ffdfa').
narrative_ontology:cs_kernel_codification('14fd543b-0097-4f80-badb-098ea34ffdfa', implicit).
narrative_ontology:cs_authority_grounding('14fd543b-0097-4f80-badb-098ea34ffdfa', practice).
narrative_ontology:cs_reading_relation('14fd543b-0097-4f80-badb-098ea34ffdfa', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('14fd543b-0097-4f80-badb-098ea34ffdfa', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('14fd543b-0097-4f80-badb-098ea34ffdfa', foundational, qwerty_achieved_genuine_adequacy).
narrative_ontology:cs_axiom_status(qwerty_achieved_genuine_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('14fd543b-0097-4f80-badb-098ea34ffdfa', qwerty_achieved_genuine_adequacy, empirically_contingent).
narrative_ontology:cs_axiom('14fd543b-0097-4f80-badb-098ea34ffdfa', foundational, dvorak_advantage_not_robust).
narrative_ontology:cs_axiom_status(dvorak_advantage_not_robust, holdable).
narrative_ontology:cs_axiom_grounding('14fd543b-0097-4f80-badb-098ea34ffdfa', dvorak_advantage_not_robust, empirically_contingent).
narrative_ontology:cs_reference_frame('14fd543b-0097-4f80-badb-098ea34ffdfa', settled_coordination_equilibrium).
narrative_ontology:cs_drift_state('14fd543b-0097-4f80-badb-098ea34ffdfa', contemporary_digital_typing_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('14fd543b-0097-4f80-badb-098ea34ffdfa', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, touch_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, software_developers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, qwerty_genuine_adequacy_thesis).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, dvorak_advantage_empirically_contested).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, switching_costs_reflect_skill_investment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invested thousands of hours developing QWERTY touch-typing skill through formal education and daily practice. Benefit from universal compatibility across devices, OSes, and applications. Switching requires genuine motor-skill retraining (months to proficiency) and ecosystem reconfiguration (shortcuts, bindings, muscle memory). No coercion prevents switching; the cost is real skill investment in a standard that works.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, touch_typists, beneficiary,
    moderate, biographical, constrained, global).

% Produce keyboards to the dominant standard because that is what the market demands. Compete on hardware features (switches, form factor, wireless, lighting) not layout. No licensing fees or layout royalties; QWERTY is a zero-cost coordination standard. Would produce alternative layouts if demand existed (some do — niche mechanical keyboards offer Dvorak/Colemak keycaps).
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, beneficiary,
    powerful, biographical, mobile, global).

% Study keyboard layout efficiency through controlled experiments, field studies, and biomechanical modeling. Findings on Dvorak advantage are contested: early studies (Navy 1944, GSA 1970s) showed modest gains; later studies with better controls show smaller or negligible differences for typical users. Methodological debates persist (training duration, crossover design, ecological validity). No institutional pressure to favor QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, ergonomics_researchers, observer,
    analytical, generational, analytical, global).

% Advocate Dvorak, Colemak, Workman, and other layouts claiming superior ergonomics or efficiency. Experience institutional inertia: OS defaults, educational curricula, workplace standards, and certification exams all assume QWERTY. Argue this constitutes de facto suppression of fair comparison. Can and do switch personally (software remapping, custom hardware), but face ecosystem friction. Not systematically blocked — alternatives are legally and technically available.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, alternative_layout_advocates, excluded,
    moderate, biographical, constrained, global).

% Build applications assuming QWERTY-based shortcut conventions (Ctrl+C/V/X/Z, WASD gaming, vi/hjkl navigation). Benefit from a stable universal target — one layout to test, document, and support. Switching costs for the ecosystem would be enormous (every keybinding, tutorial, muscle-memory reference). No extractive intent — the coordination value is genuine and the standard is open.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, software_developers, beneficiary,
    organized, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, stable, zero-licensing-cost text input standard that enables skill portability across devices, operating systems, applications, and generations. Solves the coordination problem of 'which layout do we teach, build for, and assume?' by settling on the historically dominant one that achieved adequacy.
% TRANSFER_FUNCTION: Moves nothing systematically. No rents, fees, or transfers flow from the layout itself. Switching costs are borne voluntarily by individuals who choose to retrain; they are not extracted by a beneficiary. Manufacturers, developers, and users all coordinate on QWERTY because it works well enough and everyone else uses it.
% ABSENT_VOICES: Historical typists (1874–1930) who experienced the original layout competition are absent — they would testify on whether QWERTY's early dominance reflected adequacy or contingent factors (Remington sales network, typing school adoption). Early Dvorak proponents (1930s–1970s) who claimed suppression by manufacturers are absent — their testimony would support or refute the beneficiary_extraction_reading. Modern non-typing populations (mobile-first, voice-first users) are absent — their emerging coordination equilibrium may render the QWERTY question obsolete.
% DISAPPEARANCE_RATIONALE: If QWERTY disappeared overnight, global text input would reorganize chaotically: billions of users would need to relearn typing; all software shortcut conventions would break; educational curricula, certification exams, and workplace standards would require simultaneous revision. The world rearranges because the constraint is a genuine coordination equilibrium — not because it extracts, but because it coordinates.
% FOUNDING_PROBLEM: Mechanical typewriter key jamming: early typewriters jammed when adjacent typebars struck in rapid succession. QWERTY separated common digraphs to reduce jams. This problem was resolved by electronic switching (no mechanical typebars) by the 1970s.
% FOUNDING_PROBLEM_CORROBORATION: The mechanical jamming problem is historically documented and uncontested — typewriter engineering histories, patent records, and the Dvorak layout's explicit design rationale (optimizing for electronic typing) all corroborate that the founding problem is dead. The naturalization reading acknowledges this but claims QWERTY acquired a new coordination function (universal text input standard) that justifies persistence. The lock_in_reading uses the dead founding problem as evidence of path dependence. The beneficiary_extraction_reading claims manufacturers exploited the dead problem to maintain market position. Corroboration for the founding problem's death comes from outside all three readings: typewriter engineering history, not layout partisans.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_mechanism__naturalization_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is near-zero (0.08) because no party systematically collects rents from QWERTY's persistence — manufacturers compete on hardware, not layout licensing; typists bear switching costs they voluntarily incurred. Suppression is low (0.12) because alternatives exist (Dvorak, Colemak, custom layouts) and are legally/technically available; the constraint does not actively block them. Theater ratio rises from near-zero to 0.15 as ergonomic standards and educational curricula institutionalize QWERTY without active enforcement — the 'performance' of standardization replaces the functional coordination that originally established it. Accessibility collapse is high (0.72) because the installed base of skills, tools, and muscle memory makes alternatives practically inaccessible for most users, but this reflects genuine network effects of a settled standard, not engineered exclusion. Resistance is low (0.22) because the constraint is not experienced as coercive — most users have no desire to switch.
 *
 * PERSPECTIVAL GAP:
 *   The naturalization reading and the lock_in reading share the same observable outcome (QWERTY dominance) but disagree on the causal mechanism: path-dependent lock-in vs. genuine adequacy. The engine computes per-seat classifications from structural data — from the touch_typist seat this appears as a mountain (high accessibility_collapse, near-zero extraction); from the alternative_layout_advocate seat the same metrics may compute as a scaffold or rope if they experience the institutional framing as suppressing fair comparison. The claimed_type (mountain) is the author's structural judgment; the engine's per-seat output is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims declared because the naturalization reading asserts no systematic extraction — QWERTY is a coordination equilibrium where all parties benefit from standardization and no party pays to maintain it. Touch_typists have constrained exit (switching costs are real skill investment), not trapped or identity_locked. Manufacturers are mobile — they produce what the market demands. Researchers are analytical. The directionality derivation chain produces low d for all seats, consistent with a mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mechanical typewriter key jamming — is dead (resolved by electronic switching). The naturalization reading claims the arrangement persists because QWERTY became adequate for electronic typing too; the lock_in reading claims it persists despite the founding problem's death. This reading's mandatrophy_resolved status is contested: the arrangement outlived its founding problem but acquired a new coordination function (universal human-computer text input standard) that justifies its persistence. The theater_ratio trajectory captures the transition from functional to institutional maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalization,
    'Is the persistence of QWERTY genuinely explained by its adequacy and fair competition, or does this reading itself serve as a naturalizing cover story?',
    'Comparative analysis of keyboard adoption histories in markets with different incumbent structures; examination of whether ''adequacy'' criteria were defined post-hoc to match QWERTY''s properties.',
    'If the adequacy claim is post-hoc or criteria-dependent, the constraint shifts from mountain to rope or scaffold depending on whether coordination function remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalization, conceptual, 'Whether this reading reflects a stable structural fact or a contingent naturalization of a path-dependent outcome.').

omega_variable(
    dvorak_advantage_measurement,
    'Is the empirical contestation of Dvorak''s advantage a genuine unresolved scientific question or an artifact of measurement methodology and institutional inertia?',
    'Modern controlled studies with adequate training periods, cross-over designs, and ecological validity; re-examination of original Navy and GSA studies for methodological flaws.',
    'If Dvorak advantage is robust under modern methods, the naturalization reading loses its empirical footing and the constraint reclassifies toward lock_in or extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_advantage_measurement, empirical, 'Whether the empirical basis for contested Dvorak advantage withstands modern scrutiny.').

omega_variable(
    switching_cost_naturalness,
    'Do typing skill switching costs reflect genuine human capital investment, or do they embed path-dependent standardization effects that advantage the incumbent?',
    'Decomposition of switching costs into pure motor-skill retraining vs. ecosystem reconfiguration (shortcuts, muscle memory for application-specific bindings, toolchain adaptation).',
    'If switching costs are predominantly ecosystem reconfiguration rather than motor skill, the ''genuine investment'' claim weakens and coordination costs become extractive barriers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_naturalness, empirical, 'Decomposition of switching cost components to test the genuine-adequacy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 1874, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_nat_tr_t1874, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1874, 0.02).
narrative_ontology:measurement(qwerty_nat_tr_t1900, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(qwerty_nat_tr_t1932, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1932, 0.08).
narrative_ontology:measurement(qwerty_nat_tr_t1960, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(qwerty_nat_tr_t1984, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1984, 0.14).
narrative_ontology:measurement(qwerty_nat_tr_t2024, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(qwerty_nat_be_t1874, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1874, 0.05).
narrative_ontology:measurement(qwerty_nat_be_t1900, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(qwerty_nat_be_t1932, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1932, 0.06).
narrative_ontology:measurement(qwerty_nat_be_t1960, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1960, 0.07).
narrative_ontology:measurement(qwerty_nat_be_t1984, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1984, 0.08).
narrative_ontology:measurement(qwerty_nat_be_t2024, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2024, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_nat_su_t1874, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1874, 0.02).
narrative_ontology:measurement(qwerty_nat_su_t1900, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(qwerty_nat_su_t1932, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1932, 0.08).
narrative_ontology:measurement(qwerty_nat_su_t1960, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(qwerty_nat_su_t1984, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1984, 0.11).
narrative_ontology:measurement(qwerty_nat_su_t2024, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__naturalization_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% Kernel qwerty_persistence_mechanism decomposes into three constraint stories linked by network.affects_constraints. This reading (naturalization) claims mountain with ε ≈ 0.08; lock_in_reading claims rope/tangled_rope with higher ε; beneficiary_extraction_reading claims snare with identifiable beneficiaries. The ε values differ by design — they are different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
