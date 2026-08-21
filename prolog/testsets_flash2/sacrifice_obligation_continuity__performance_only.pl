% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation: Physical Performance Only Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the
 *   sacrifice obligation continuity kernel. It asserts that the commandment
 *   to offer sacrifices can only be fulfilled through physical performance,
 *   not through study or prayer. The current generation, lacking a Temple, is
 *   therefore unable to fulfill this core obligation, placing them in a state
 *   of unredeemed spiritual debt. Study is framed as preparation for a future
 *   messianic restoration, not as a substitute for performance. This reading
 *   generates high extractiveness (spiritual guilt, unfulfillable obligation)
 *   and high suppression (no legitimate alternative means of fulfillment).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.9).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation: Physical Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6').
narrative_ontology:cs_kernel_codification('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', fixed_text).
narrative_ontology:cs_authority_grounding('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', lineage).
narrative_ontology:cs_interpretation_layer_present('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6').
narrative_ontology:cs_reading_relation('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', foundational, physical_performance_is_sole_fulfillment).
narrative_ontology:cs_axiom_status(physical_performance_is_sole_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', physical_performance_is_sole_fulfillment, deontological).
narrative_ontology:cs_axiom('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', secondary, study_is_preparation_not_substitution).
narrative_ontology:cs_axiom_status(study_is_preparation_not_substitution, holdable).
narrative_ontology:cs_axiom_grounding('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', study_is_preparation_not_substitution, conventional).
narrative_ontology:cs_reference_frame('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', pre_destruction_temple_practice).
narrative_ontology:cs_drift_state('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', post_destruction_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6cbc2e57-8dda-4911-ad0e-e1b12f93f8e6', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, future_generations).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the obligation to perform sacrifices but unable to do so due to the absence of the Temple. They experience a constant state of unfulfilled commandment, leading to spiritual guilt and a sense of incompleteness. Study is a placeholder, not a remedy.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_adherents, payer,
    powerless, biographical, identity_locked, global).

% Maintain the textual tradition and interpret the laws of sacrifice, preparing for a future restoration. Their authority is reinforced by their role as custodians of an unfulfillable but binding commandment. They benefit from the ongoing need for their interpretive guidance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_scholars, beneficiary,
    institutional, generational, constrained, global).

% Are the intended recipients of the preserved knowledge and the potential fulfillers of the obligation. The current generation's unfulfilled state is framed as a necessary step for their eventual redemption.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).

% The hypothetical community that will eventually fulfill the sacrifice obligation. Their existence provides the ultimate justification for the 'performance_only' reading, as it posits a future where the constraint can be satisfied.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_era_community, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the continuity of a core religious obligation and the associated textual tradition across generations, ensuring readiness for a future restoration of ritual practice.
% TRANSFER_FUNCTION: Transfers spiritual burden (guilt, unfulfilled obligation) from the current generation to a future, messianic generation, while transferring interpretive authority and textual custodianship to rabbinic scholars.
% ABSENT_VOICES: Adherents of the 'study_as_performance' reading, who would argue that their current textual engagement fully satisfies the commandment, are excluded from the 'performance_only' framework, which deems their efforts insufficient for fulfillment.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the spiritual landscape for current adherents would fundamentally shift, removing the burden of unfulfilled obligation. Rabbinic authority tied to the preservation of unperformable rituals would diminish, and the entire framework of messianic expectation regarding ritual restoration would need to be re-evaluated.
% FOUNDING_PROBLEM: The destruction of the Temple and the cessation of physical sacrifices created a crisis of religious practice and continuity for a central commandment.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts and historical commentaries from across centuries attest to the ongoing problem of unfulfilled sacrifice. While the 'performance_only' reading is one interpretation, the underlying problem of the Temple's absence is universally acknowledged within the tradition.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because adherents are perpetually unable to fulfill a core commandment, leading to spiritual burden without remedy. Suppression is high because this reading actively denies any alternative (like study or prayer) as a valid substitute for physical performance, trapping adherents in a state of unfulfilled obligation. Theater ratio is low because the commitment to future restoration is genuine, and the scholarly work is seen as a necessary, albeit preparatory, function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of current generation adherents, this constraint is a snare, imposing an unfulfillable obligation that generates spiritual debt. From the perspective of rabbinic scholars, it is a rope, coordinating the preservation of a vital tradition and ensuring future continuity. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Current generation adherents are the primary victims, bearing the spiritual cost of unfulfilled obligation (high d). Rabbinic scholars benefit from their role as custodians of the tradition and interpreters of the unfulfillable law, reinforcing their authority (low d). Future generations are conceptual beneficiaries, as the current generation's efforts are directed towards enabling their eventual fulfillment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fulfillment_mechanism_ambiguity,
    'Is physical performance the only valid mechanism for fulfilling the sacrifice obligation, or can other forms of engagement (e.g., study, prayer) serve as substitutes?',
    'Theological consensus shift within the tradition, or the emergence of a widely accepted alternative interpretive framework that redefines ''fulfillment''.',
    'If alternative mechanisms are accepted, the extractiveness and suppression of this reading would significantly decrease, potentially reclassifying it from a snare to a rope or even a piton (if the original mandate atrophies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fulfillment_mechanism_ambiguity, conceptual, 'Ambiguity regarding the acceptable means of fulfilling the sacrifice commandment.').

omega_variable(
    messianic_restoration_certainty,
    'What is the certainty and timeline of the messianic restoration that would enable the physical performance of sacrifices?',
    'Empirical observation of messianic events (if such a framework were to emerge) or a definitive theological pronouncement on the nature and timing of the restoration.',
    'If the restoration is deemed highly uncertain or indefinitely postponed, the ''future generations'' beneficiary becomes increasingly theoretical, intensifying the current generation''s unredeemed state and potentially increasing extractiveness. If imminent, it might reduce the sense of unfulfillable burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_restoration_certainty, empirical, 'Uncertainty about the future conditions for fulfilling the obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__performance_only, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__performance_only, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__performance_only, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 500, 0.82).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1000, 0.83).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1500, 0.84).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 500, 0.87).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1500, 0.89).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_continuity' kernel. Its high extractiveness and suppression are a direct consequence of its strict 'performance_only' interpretation, which contrasts sharply with other readings that offer alternative modes of fulfillment or suspension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
