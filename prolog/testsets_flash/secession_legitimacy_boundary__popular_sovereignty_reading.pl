% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Secession Legitimacy
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint describes the 'popular sovereignty' reading of the
 *   secession legitimacy boundary, where a democratic majority within a
 *   provincial boundary holds ultimate sovereignty, and a referendum result
 *   is considered self-legitimating for secession. This reading posits that
 *   the provincial majority can unilaterally exit, and federal authority is
 *   subordinate to their popular will. Claims of 'extraction' by the federal
 *   government are considered valid if perceived by the majority. This is a
 *   highly contested interpretation within federal systems, often clashing
 *   with constitutional and treaty-based claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.6).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.7).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Reading of Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '6b5b6440-935b-4ee0-af29-540fede35a0c').
narrative_ontology:cs_kernel_codification('6b5b6440-935b-4ee0-af29-540fede35a0c', distributed).
narrative_ontology:cs_authority_grounding('6b5b6440-935b-4ee0-af29-540fede35a0c', practice).
narrative_ontology:cs_reading_relation('6b5b6440-935b-4ee0-af29-540fede35a0c', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('6b5b6440-935b-4ee0-af29-540fede35a0c', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b5b6440-935b-4ee0-af29-540fede35a0c', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('6b5b6440-935b-4ee0-af29-540fede35a0c', foundational, provincial_popular_will_is_supreme).
narrative_ontology:cs_axiom_status(provincial_popular_will_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('6b5b6440-935b-4ee0-af29-540fede35a0c', provincial_popular_will_is_supreme, deontological).
narrative_ontology:cs_axiom('6b5b6440-935b-4ee0-af29-540fede35a0c', foundational, referendum_is_self_legitimating).
narrative_ontology:cs_axiom_status(referendum_is_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('6b5b6440-935b-4ee0-af29-540fede35a0c', referendum_is_self_legitimating, conventional).
narrative_ontology:cs_reference_frame('6b5b6440-935b-4ee0-af29-540fede35a0c', unfettered_self_determination).
narrative_ontology:cs_drift_state('6b5b6440-935b-4ee0-af29-540fede35a0c', contemporary_federal_legal_challenges, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b5b6440-935b-4ee0-af29-540fede35a0c', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_populace).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, minority_within_province).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it purports a coordination function (allowing a democratic expression of self-determination) but inherently involves asymmetric extraction. The provincial majority and government benefit from the perceived right to self-determination, while the federal government, internal minorities, and Indigenous treaty holders bear the costs of potential fragmentation and loss of rights. Active enforcement is required to assert and defend this interpretation against federal constitutional claims and to manage internal dissent. Extractiveness is moderate-high (0.6) due to the potential for significant resource and territorial transfers. Suppression is high (0.7) as this reading often requires suppressing alternative constitutional interpretations and the voices of internal minorities or treaty holders. Theater ratio is low (0.2) as the assertion of popular sovereignty is a direct, functional claim, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the provincial majority and government, this constraint is a legitimate expression of democratic will and a mechanism for self-determination. From the federal government's perspective, it is an illegitimate challenge to constitutional order. From the perspective of internal minorities and Indigenous treaty holders, it is a potentially highly extractive and suppressive mechanism that disregards their rights and interests.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial majority populace and provincial government are primary beneficiaries (d near 0.0) as they gain the power of self-determination and control over provincial resources. The federal government is a primary target (d near 1.0) as it stands to lose territory, resources, and authority. Minority within the province and Indigenous treaty holders are also targets (d near 1.0) as their rights and status could be significantly diminished or ignored in a unilateral secession. The 'extraction' claims are valid if perceived by the majority, which further amplifies the directionality for the provincial majority.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (popular sovereignty) is asserted as a foundational principle. However, if the underlying grievances driving the secession movement are resolved, but the provincial government continues to assert this reading for other, potentially extractive, reasons (e.g., resource control), it could drift towards a Snare. The classification as Tangled Rope already captures the hybrid nature of coordination and extraction inherent in this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of popular sovereignty, or a strategic interpretation to justify unilateral action?',
    'Analysis of historical precedents, international legal opinions on self-determination, and the specific context of the referendum''s framing and conduct.',
    'If a genuine expression, the constraint is a legitimate (though potentially disruptive) mechanism for political change. If strategic, it functions as a Snare, leveraging democratic rhetoric for extractive ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''popular_sovereignty_reading'' of the ''secession_legitimacy_boundary'' kernel. Sibling readings (constitutional_impossibility_reading, grievance_threshold_reading, treaty_primacy_reading) would alter the beneficiary/victim structure and the perceived legitimacy of unilateral secession.').

omega_variable(
    federal_authority_subordination,
    'To what extent is federal authority truly subordinate to provincial popular will in practice, given the federal government''s coercive capacity?',
    'Observation of federal response to a hypothetical secession referendum: does the federal government respect the outcome or deploy legal/economic/military countermeasures?',
    'If federal authority is effectively subordinate, the provincial majority''s exit options are mobile. If federal coercion overrides popular will, the provincial majority is constrained or trapped, and the constraint is a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_authority_subordination, empirical, 'The structural delta for this reading is that provincial majority can exit unilaterally and federal authority is subordinate to popular will. This omega addresses the empirical reality of that subordination.').

omega_variable(
    extraction_claim_validity,
    'Are ''extraction'' claims valid if the provincial majority perceives them, or must they meet an objective standard of injustice?',
    'Legal and political consensus on the definition of ''structural injustice'' in federal systems, and whether perceived grievances alone constitute a legitimate basis for secession.',
    'If perceived grievances are sufficient, the constraint''s legitimacy is self-referential to the provincial majority. If an objective standard is required, the constraint''s legitimacy is contestable by external observers, potentially reclassifying it as a Snare if the objective standard is not met.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_claim_validity, preference, 'This reading asserts that ''extraction'' claims are valid if the majority perceives them. This omega questions the epistemic grounding of such claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'secession_legitimacy_boundary' kernel. Its structural delta is that a provincial majority can exit unilaterally, federal authority is subordinate to popular will, and 'extraction' claims are valid if the majority perceives them. It directly influences and is influenced by other readings of this kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
