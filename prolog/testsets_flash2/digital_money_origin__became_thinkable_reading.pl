% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin: Concept Became Thinkable
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story describes the emergence of digital money as a
 *   concept becoming technically and institutionally conceivable, prior to
 *   its widespread implementation. It focuses on the intellectual and
 *   institutional coordination required to define what digital money is and
 *   how it could function, setting the stage for later practical
 *   applications. This reading emphasizes the role of early conceptualizers
 *   and the exclusion of alternative framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.25).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.4).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin: Concept Became Thinkable").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'ad5771f2-713e-49d0-bffb-8ae781133793').
narrative_ontology:cs_kernel_codification('ad5771f2-713e-49d0-bffb-8ae781133793', distributed).
narrative_ontology:cs_authority_grounding('ad5771f2-713e-49d0-bffb-8ae781133793', expertise).
narrative_ontology:cs_interpretation_layer_present('ad5771f2-713e-49d0-bffb-8ae781133793').
narrative_ontology:cs_reading_relation('ad5771f2-713e-49d0-bffb-8ae781133793', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_reading_relation('ad5771f2-713e-49d0-bffb-8ae781133793', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('ad5771f2-713e-49d0-bffb-8ae781133793', foundational, conceptual_precedes_implementation).
narrative_ontology:cs_axiom_status(conceptual_precedes_implementation, holdable).
narrative_ontology:cs_axiom_grounding('ad5771f2-713e-49d0-bffb-8ae781133793', conceptual_precedes_implementation, conventional).
narrative_ontology:cs_axiom('ad5771f2-713e-49d0-bffb-8ae781133793', foundational, institutional_feasibility_is_origin).
narrative_ontology:cs_axiom_status(institutional_feasibility_is_origin, holdable).
narrative_ontology:cs_axiom_grounding('ad5771f2-713e-49d0-bffb-8ae781133793', institutional_feasibility_is_origin, conventional).
narrative_ontology:cs_reference_frame('ad5771f2-713e-49d0-bffb-8ae781133793', early_cybernetics_and_institutional_design).
narrative_ontology:cs_drift_state('ad5771f2-713e-49d0-bffb-8ae781133793', contemporary_cryptocurrency_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ad5771f2-713e-49d0-bffb-8ae781133793', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, computer_scientists).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, excluded_from_conceptual_framing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the individuals and groups who first conceptualized digital money within existing institutional frameworks, benefiting from the intellectual leadership and influence that came with defining the terms of a new monetary paradigm. Their work laid the groundwork for future implementation.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, beneficiary,
    institutional, generational, mobile, global).

% The technical experts whose innovations made the concept of digital money feasible. They gained recognition and funding for their research, shaping the early technical specifications and possibilities of digital currency.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, computer_scientists, beneficiary,
    powerful, biographical, mobile, global).

% These are groups or individuals whose alternative conceptualizations of digital money (e.g., decentralized, community-based) were not considered or were actively marginalized by the dominant institutional and technical discourse. They bore the cost of having their ideas excluded from the foundational definitions.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, excluded_from_conceptual_framing, payer,
    powerless, biographical, trapped, global).

% Government bodies and central banks who observed the conceptual development of digital money, initially from a distance, later engaging with its implications for policy and regulation. They were not direct beneficiaries or victims of the initial conceptual emergence but would later become key players.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinated the intellectual and technical efforts of various researchers and institutions towards a shared understanding of what digital money could be, enabling a common conceptual framework for future development.
% TRANSFER_FUNCTION: Transferred intellectual capital and definitional power to those who first articulated the concept, from those whose alternative visions were not adopted.
% ABSENT_VOICES: Advocates for radically decentralized or non-state-backed forms of digital money were largely absent from the initial institutional and technical discussions, which focused on integrating digital concepts into existing financial structures. Their perspectives would have challenged the foundational assumptions.
% DISAPPEARANCE_RATIONALE: If the concept of digital money had never become technically and institutionally conceivable, the entire trajectory of modern finance, payment systems, and even the internet's commercialization would be fundamentally different. The world would have rearranged around alternative technological and monetary innovations.
% FOUNDING_PROBLEM: The problem was the absence of a coherent, technically feasible, and institutionally acceptable concept for non-physical, electronically transferable value, hindering innovation in finance and information technology.
% FOUNDING_PROBLEM_CORROBORATION: Computer scientists and institutional historians corroborate that the conceptual problem was indeed solved, leading to subsequent implementation efforts. Monetary authorities, while initially skeptical, later acknowledged the conceptual groundwork as a precursor to practical digital currencies.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).
:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is relatively low because the primary 'cost' was the exclusion of alternative conceptual paths, rather than direct financial extraction. Suppression (0.4) reflects the intellectual and institutional gatekeeping that prioritized certain conceptual frameworks over others. Theater ratio is low (0.1) as the conceptual work was genuinely foundational. Accessibility collapse (0.7) is high because once a dominant conceptual framework emerged, it became difficult to introduce radically different ideas. Resistance (0.15) was low, as the contest was primarily intellectual and not yet involving widespread public or market participation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries of this conceptual emergence would see it as a necessary and productive coordination effort, while those whose ideas were excluded would perceive a subtle form of intellectual suppression. The engine's classification will reflect the overall low extraction but acknowledge the asymmetric benefits of conceptual leadership.
 *
 * DIRECTIONALITY LOGIC:
 *   Early institutional architects and computer scientists are beneficiaries, gaining influence and defining the terms of the new paradigm. Those with alternative conceptualizations are victims, as their ideas were marginalized. Monetary authorities are observers, initially distant but later engaging with the implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_practical_origin,
    'Is the ''origin'' of digital money best defined by its conceptual emergence, its first practical use, or its formal regulatory recognition?',
    'Historical analysis of the impact of each phase on subsequent developments and public perception. If conceptual emergence proves to be the most significant inflection point for all subsequent forms of digital money, this reading is strengthened.',
    'If resolved towards conceptual emergence, this reading''s early origin date is validated. If resolved towards practical use or regulatory recognition, this constraint would be reclassified as a ''precursor'' rather than an ''origin'', and its extractiveness might be lower as it''s less about direct control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_practical_origin, conceptual, 'Ambiguity in defining the ''origin'' point of digital money.').

omega_variable(
    exclusion_mechanism_ambiguity,
    'Was the exclusion of alternative conceptual framings a structural necessity for coherent development, or a consequence of power dynamics within early institutional and technical circles?',
    'Counterfactual historical analysis: could a more diverse set of conceptual framings have coexisted and still led to a coherent development path? Examination of early academic and policy debates for evidence of active suppression vs. natural convergence.',
    'If structural necessity, the suppression metric might be re-evaluated as a coordination cost. If power dynamics, the suppression is more clearly extractive, potentially shifting the classification towards a Tangled Rope or Snare for the ''excluded'' seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_ambiguity, empirical, 'Structural vs. power-dynamic basis for conceptual exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1970, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__became_thinkable_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(digi_be_t1975, digital_money_origin__became_thinkable_reading, base_extractiveness, 1975, 0.2).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__became_thinkable_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(digi_be_t1985, digital_money_origin__became_thinkable_reading, base_extractiveness, 1985, 0.23).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__became_thinkable_reading, base_extractiveness, 1990, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__became_thinkable_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(digi_su_t1975, digital_money_origin__became_thinkable_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__became_thinkable_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(digi_su_t1985, digital_money_origin__became_thinkable_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__became_thinkable_reading, suppression_requirement, 1990, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_origin' kernel. This 'became_thinkable_reading' focuses on the conceptual and institutional conceivability, preceding the 'first_held_reading' (practical use) and 'regulatory_recognition_reading' (formal recognition). Each reading defines the origin at a different point, leading to different beneficiary/victim sets and metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
