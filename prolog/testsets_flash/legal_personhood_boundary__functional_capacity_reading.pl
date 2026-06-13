% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood Boundary: Functional Capacity Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint defines legal personhood based on demonstrable cognitive
 *   capacities such as rationality, sentience, and self-awareness,
 *   irrespective of species. It is a reading of the broader
 *   'legal_personhood_boundary' kernel. While theoretically inclusive, its
 *   practical application often defaults to human-centric interpretations,
 *   leading to the exclusion and exploitation of non-human sentient beings.
 *   The structural delta for this reading is that non-human animals,
 *   ecosystems (if represented), and future AI may enter the victim set,
 *   human exceptionalism loses legal privilege, and property rights over
 *   sentient beings become contestable.
 *
 * KEY AGENTS:
 *   - human_persons: Primary beneficiary (institutional/arbitrage) — retain legal privilege, define criteria
 *   - legal_systems_maintaining_status_quo: Agenda setter (institutional/constrained) — enforce existing boundaries, resist expansion
 *   - non_human_sentient_beings: Primary victim (powerless/trapped) — denied personhood, subject to exploitation
 *   - animal_rights_advocates: Payer/Observer (organized/constrained) — bear costs of advocacy, challenge status quo
 *   - future_advanced_ai: Potential victim (powerless/trapped) — may meet criteria but denied personhood
 *   - philosophers_ethicists: Observer (analytical/analytical) — define and debate criteria for personhood
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.6).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.7).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary: Functional Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '2858cba8-45f9-4260-8d12-381498776084').
narrative_ontology:cs_kernel_codification('2858cba8-45f9-4260-8d12-381498776084', formalized).
narrative_ontology:cs_authority_grounding('2858cba8-45f9-4260-8d12-381498776084', lineage).
narrative_ontology:cs_interpretation_layer_present('2858cba8-45f9-4260-8d12-381498776084').
narrative_ontology:cs_reading_relation('2858cba8-45f9-4260-8d12-381498776084', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('2858cba8-45f9-4260-8d12-381498776084', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('2858cba8-45f9-4260-8d12-381498776084', foundational, personhood_requires_demonstrable_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_demonstrable_capacity, holdable).
narrative_ontology:cs_axiom_grounding('2858cba8-45f9-4260-8d12-381498776084', personhood_requires_demonstrable_capacity, empirically_contingent).
narrative_ontology:cs_axiom('2858cba8-45f9-4260-8d12-381498776084', foundational, species_is_not_a_criterion_for_personhood).
narrative_ontology:cs_axiom_status(species_is_not_a_criterion_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('2858cba8-45f9-4260-8d12-381498776084', species_is_not_a_criterion_for_personhood, deontological).
narrative_ontology:cs_reference_frame('2858cba8-45f9-4260-8d12-381498776084', enlightenment_rationality_framework).
narrative_ontology:cs_drift_state('2858cba8-45f9-4260-8d12-381498776084', contemporary_neuroscience_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2858cba8-45f9-4260-8d12-381498776084', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, human_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, legal_systems_maintaining_status_quo).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, non_human_sentient_beings).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is substantial because, despite the theoretical inclusiveness, current legal systems largely fail to extend personhood to non-human entities that demonstrably meet the functional criteria, allowing their exploitation. Suppression (0.7) is high due to the active legal and cultural enforcement of anthropocentric boundaries and the suppression of challenges to these boundaries. Theater ratio (0.2) is relatively low, as the legal system genuinely adjudicates personhood, but the 'functional capacity' argument is often selectively applied. Resistance (0.75) is high due to ongoing advocacy from animal rights groups and philosophical challenges.
 *
 * PERSPECTIVAL GAP:
 *   Human persons and legal systems experience this as a stable, rational boundary, while non-human sentient beings and their advocates experience it as an arbitrary, extractive barrier. The 'functional capacity' argument, while seemingly objective, is often interpreted in ways that maintain human privilege, creating a significant divergence in how the constraint is perceived and experienced.
 *
 * DIRECTIONALITY LOGIC:
 *   Human persons are beneficiaries (d=0.0-0.1) as they retain their personhood and the benefits of a system that largely excludes others. Legal systems are agenda-setters and beneficiaries (d=0.1-0.2) as they maintain the existing order and derive legitimacy from it. Non-human sentient beings are clear victims (d=0.9-1.0) as they are denied personhood despite meeting criteria, leading to their exploitation. Animal rights advocates are payers (d=0.7-0.8) as they bear the costs of challenging this entrenched system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to define personhood based on capacity is theoretically sound, but its application has atrophied into a mechanism that primarily serves to maintain existing power structures. The 'functional capacity' argument, intended to be inclusive, has become a cover for continued anthropocentric extraction. Resolving this mandatrophy would require a genuine, non-speciesist application of the stated criteria.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of functional capacity, or is it a constructed boundary serving anthropocentric interests?',
    'Philosophical consensus on criteria for sentience/rationality, and legal reform to align personhood with these criteria across species.',
    'If genuinely functional, the constraint is a Rope; if constructed, it is a Snare or Tangled Rope, extracting from non-persons. This reading is ''functional_capacity_reading'' of the ''legal_personhood_boundary'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading of the legal personhood kernel.').

omega_variable(
    scope_of_functional_capacity,
    'What specific cognitive capacities (e.g., sentience, self-awareness, rationality) are sufficient for personhood, and how are they empirically demonstrated across diverse species and potential AI?',
    'Interdisciplinary scientific consensus (neuroscience, ethology, AI research) on the presence and degree of these capacities, leading to a graded or threshold-based legal framework.',
    'A clearer, empirically grounded definition would reduce ambiguity and potentially expand the victim set to include more non-human entities, increasing the measured extraction from them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_functional_capacity, empirical, 'Ambiguity in defining and measuring ''functional capacity'' for personhood.').

omega_variable(
    property_rights_conflict,
    'How would the extension of personhood based on functional capacity impact existing property rights over sentient beings (e.g., livestock, research animals, pets)?',
    'Legal precedents and legislative action establishing new categories of rights for non-human persons, potentially leading to a redefinition or abolition of property rights over them.',
    'If property rights are maintained over functionally capable beings, the constraint''s extractiveness and suppression would be higher, as it would represent a legal fiction masking exploitation. If abolished, the constraint would shift towards a Rope for non-human persons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_conflict, preference, 'Conflict between functional personhood and existing property rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_welfare_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, environmental_protection_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_ethics_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legal_personhood_boundary' kernel, each with distinct structural properties and implications for rights and duties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
