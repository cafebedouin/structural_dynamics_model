% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Persistence via Path-Dependent Lock-In
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   This constraint is the lock-in reading of the
 *   qwerty_persistence_mechanism kernel. It models QWERTY dominance as a
 *   path-dependent coordination equilibrium: a historically contingent
 *   standard that persists because network effects make uncoordinated
 *   switching individually irrational, despite the existence of technically
 *   superior alternatives. The reading asserts no individual beneficiary
 *   extracts from the arrangement; the persistence is a market failure
 *   without extraction, producing collective suboptimality rather than
 *   concentrated rents.
 *
 * KEY AGENTS:
 *   - Typists (organized/constrained): Bear the diffuse efficiency cost of the suboptimal layout but cannot individually exit due to network externalities.
 *   - Alternative layout advocates (moderate/constrained): Excluded from mainstream coordination; their empirical demonstrations of superior layouts cannot overcome lock-in dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.2).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Persistence via Path-Dependent Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '8a5df13c-1073-4d57-ae6f-f863d273b9fb').
narrative_ontology:cs_kernel_codification('8a5df13c-1073-4d57-ae6f-f863d273b9fb', formalized).
narrative_ontology:cs_authority_grounding('8a5df13c-1073-4d57-ae6f-f863d273b9fb', self_enforcing).
narrative_ontology:cs_reading_relation('8a5df13c-1073-4d57-ae6f-f863d273b9fb', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_reading_relation('8a5df13c-1073-4d57-ae6f-f863d273b9fb', qwerty_persistence_mechanism__beneficiary_extraction_reading, forecloses).
narrative_ontology:cs_axiom('8a5df13c-1073-4d57-ae6f-f863d273b9fb', foundational, coordination_failure_without_extraction).
narrative_ontology:cs_axiom_status(coordination_failure_without_extraction, holdable).
narrative_ontology:cs_axiom_grounding('8a5df13c-1073-4d57-ae6f-f863d273b9fb', coordination_failure_without_extraction, empirically_contingent).
narrative_ontology:cs_axiom('8a5df13c-1073-4d57-ae6f-f863d273b9fb', foundational, path_dependence_over_adequacy).
narrative_ontology:cs_axiom_status(path_dependence_over_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('8a5df13c-1073-4d57-ae6f-f863d273b9fb', path_dependence_over_adequacy, empirically_contingent).
narrative_ontology:cs_reference_frame('8a5df13c-1073-4d57-ae6f-f863d273b9fb', path_dependent_coordination_equilibrium).
narrative_ontology:cs_drift_state('8a5df13c-1073-4d57-ae6f-f863d273b9fb', digital_input_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8a5df13c-1073-4d57-ae6f-f863d273b9fb', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use QWERTY because it is the universal hardware and software standard; bear a diffuse, persistent efficiency cost relative to ergonomically superior layouts. Personal switch to an alternative layout is possible but socially costly due to shared equipment, employer standards, and the need to retrain muscle memory without collective coordination.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typists, payer,
    organized, biographical, constrained, global).

% Promote demonstrably more efficient layouts such as Dvorak or Colemak but remain structurally excluded from mainstream adoption. Their research and advocacy are known to niche communities but cannot overcome the network externalities that make individual switching irrational without a coordinating mechanism.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_advocates, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, universally shared keyboard layout so that typists, device manufacturers, software vendors, and training institutions can interoperate without per-device or per-application relearning; solves the coordination problem of universal input compatibility.
% TRANSFER_FUNCTION: No concentrated transfer occurs. A diffuse efficiency cost is borne by all typists relative to a counterfactual superior layout, with no corresponding agent capturing the surplus; the arrangement locks in a suboptimal equilibrium rather than extracting to a beneficiary.
% ABSENT_VOICES: Alternative layout advocates and ergonomic researchers are present in niche communities but excluded from the default standard-setting conversation; their absence from mainstream coordination means the efficiency critique is marginalized despite empirical support.
% DISAPPEARANCE_RATIONALE: If the QWERTY arrangement disappeared overnight, typists and manufacturers would need to coordinate on a new standard; typing education, software layouts, and hardware production would reorganize around a new equilibrium, though the transition would be slowed by retraining costs and legacy equipment.
% FOUNDING_PROBLEM: Mechanical typewriter jamming in the 1870s required a layout that separated frequently paired keys to prevent physical collision; later, the need for universal interoperability across devices and training institutions locked in the early historical choice.
% FOUNDING_PROBLEM_CORROBORATION: Historical engineering historians and typewriter museum curators attest the jamming problem was real and decisive in the 19th century; contemporary ergonomics researchers outside any benefiting party attest the mechanical problem is technologically obsolete, though the coordination legacy persists.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.20) because no party captures a concentrated transfer; the cost is a diffuse efficiency tax on all typists. Suppression is minimal (0.05) because no agent actively enforces QWERTY or suppresses alternatives. Theater ratio is negligible (0.05) because there is no performative maintenanceâno one theatrically defends the layout as optimal. Accessibility collapse is high (0.75) because, once the standard is understood as arbitrary, alternatives remain technically available but socially and economically inaccessible due to coordination costs. Resistance is near-zero (0.05) because no organized party fights to preserve QWERTY as an agenda, and alternative advocates lack leverage to force coordination. The temporal series show a flat profile: the constraint is stable because the equilibrium is self-enforcing, not because of drift or intensification.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is muted in this reading because there is no concentrated beneficiary. Both typists and alternative advocates experience the constraint as a suboptimal equilibrium from which exit is structurally difficult. The absence of an agenda-setter with a low directionality value means the engine will not find a beneficiary seat whose classification diverges sharply from the payer seat; the constraint reads similarly across positions because the mechanism is pure coordination failure rather than asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   With no beneficiaries declared and no victims declared, directionality reverts to the canonical fallback for each power atom. Typists, as the organized payer seat, bear a small diffuse cost without subsidy; their structural relationship is near-symmetric but slightly target-leaning because they carry the efficiency loss. Alternative advocates are excluded and therefore not directly governed by the constraint's directional flow. No agent sits at the beneficiary end, which is the defining structural feature of the lock-in reading: the arrangement extracts from no one and benefits no one in particular, yet persists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing mechanical typewriter jammingâis technologically dead. However, the arrangement has not atrophied into a piton because it continues to perform a live coordination function (universal interoperability). The mismatch between dead founding problem and world-rearranging disappearance verdict is a zombie signal only if the constraint is maintained theatrically without function. Here, the theater ratio is near-zero and the coordination function remains real, so the classification resists mandatrophic mislabeling. The persistence is path-dependence, not institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_gap_empirical_status,
    'Does the QWERTY layout impose a measurable, persistent productivity loss relative to optimized alternatives, or have learning effects and ergonomic adaptation neutralized the gap?',
    'Controlled longitudinal typing studies comparing QWERTY against Dvorak and Colemak on modern hardware, adjusting for practice effects and task type.',
    'If the gap is negligible, the ''collective suboptimality'' premise weakens and the constraint shifts toward the naturalization reading; if substantial, the lock-in reading is empirically supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_gap_empirical_status, empirical, 'Whether QWERTY''s technical inferiority is still live or has been neutralized.').

omega_variable(
    kernel_reading_contest,
    'Is QWERTY persistence explained by path-dependent lock-in without extraction, active beneficiary extraction, or naturalized adequacy?',
    'Archival analysis of manufacturer conduct and standard-setting body records, combined with comparative efficiency meta-analysis, to determine whether extraction, coordination failure, or empirical adequacy is the dominant mechanism.',
    'Resolution would reassign the constraint to a different readingâpotentially shifting classification from rope to tangled_rope/snare if extraction is documented, or toward mountain if adequacy is established.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity between the three sibling readings of the QWERTY persistence kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lockin_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(qwerty_lockin_tr_t20, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(qwerty_lockin_tr_t40, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement(qwerty_lockin_tr_t60, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(qwerty_lockin_tr_t80, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(qwerty_lockin_tr_t100, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(qwerty_lockin_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(qwerty_lockin_be_t20, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(qwerty_lockin_be_t40, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(qwerty_lockin_be_t60, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 60, 0.2).
narrative_ontology:measurement(qwerty_lockin_be_t80, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 80, 0.2).
narrative_ontology:measurement(qwerty_lockin_be_t100, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 100, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__lock_in_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the qwerty_persistence_mechanism kernel, decomposed per the Îµ-invariance principle because the lock-in, naturalization, and beneficiary-extraction framings have different Îµ values, different beneficiary/victim structures, and different empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
