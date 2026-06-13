% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta (1215) as Baronial Privilege
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story models Magna Carta (1215) as a specific feudal
 *   contract primarily benefiting landowning barons by limiting the arbitrary
 *   power of King John. In this reading, 'free men' refers exclusively to the
 *   contracting parties (the barons), and the protections offered are not
 *   universal but specific to the feudal relationship between the monarch and
 *   his direct vassals. The constraint's scope is limited to the direct
 *   relationship between the King and the barons, with no explicit or
 *   implicit extension to commoners, women, or non-landowners.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.3).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.6).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta (1215) as Baronial Privilege").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '38042cde-65b6-4780-8c73-6bb462400469').
narrative_ontology:cs_kernel_codification('38042cde-65b6-4780-8c73-6bb462400469', fixed_text).
narrative_ontology:cs_authority_grounding('38042cde-65b6-4780-8c73-6bb462400469', lineage).
narrative_ontology:cs_interpretation_layer_present('38042cde-65b6-4780-8c73-6bb462400469').
narrative_ontology:cs_reading_relation('38042cde-65b6-4780-8c73-6bb462400469', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('38042cde-65b6-4780-8c73-6bb462400469', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('38042cde-65b6-4780-8c73-6bb462400469', foundational, feudal_contract_supremacy).
narrative_ontology:cs_axiom_status(feudal_contract_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('38042cde-65b6-4780-8c73-6bb462400469', feudal_contract_supremacy, conventional).
narrative_ontology:cs_axiom('38042cde-65b6-4780-8c73-6bb462400469', foundational, free_men_equals_landowners).
narrative_ontology:cs_axiom_status(free_men_equals_landowners, holdable).
narrative_ontology:cs_axiom_grounding('38042cde-65b6-4780-8c73-6bb462400469', free_men_equals_landowners, empirically_contingent).
narrative_ontology:cs_reference_frame('38042cde-65b6-4780-8c73-6bb462400469', feudal_contract_framework).
narrative_ontology:cs_drift_state('38042cde-65b6-4780-8c73-6bb462400469', contemporary_constitutional_theory, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('38042cde-65b6-4780-8c73-6bb462400469', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, king_john).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).
:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it genuinely coordinated a collective action problem among the barons to limit the King's arbitrary power, with clear beneficiaries (barons) and a clear target (the King). Extraction is moderate (0.3) as it limits the King's ability to extract arbitrarily, but the barons themselves are not extracting from a broader population through this specific document. Suppression (0.6) reflects the need for active enforcement by the barons to hold the King to the terms, but it's not suppressing alternatives for a wider populace. Theater ratio is low (0.1) as its function was direct and immediate for its intended beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the landowning barons, this was a crucial coordination mechanism to secure their rights against an overreaching monarch. From the King's perspective, it was a forced concession that constrained his power. For commoners and other non-contracting parties, the document had little direct impact, as its protections did not extend to them.
 *
 * DIRECTIONALITY LOGIC:
 *   The landowning_barons are the primary beneficiaries (d near 0.0) as the constraint directly protects their feudal rights and property. King_John is the primary target (d near 1.0) as his arbitrary power is curtailed. Commoners and other non-landowners are largely outside the scope of this specific reading, neither directly benefiting nor being directly targeted by its provisions.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the original Magna Carta as a universal rights document, which would obscure its specific historical context and the limited scope of its initial application. By focusing on its function as a feudal contract, it highlights a genuine coordination problem among the elite without projecting later interpretations onto the 1215 text. The constraint's mandate was to stabilize the feudal relationship, which it did for a time, before later reinterpretations broadened its scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'Is ''free men'' in Magna Carta (1215) limited to landowning barons, or does it encompass a broader class of individuals?',
    'Historical linguistic analysis of 13th-century legal texts and social structures, combined with contemporary interpretations of feudal law.',
    'If limited to barons, the constraint is a specific feudal contract (Rope); if broader, it begins to approach a universal rights document (Tangled Rope or Snare, depending on enforcement and extraction from non-barons).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_free_men, empirical, 'Ambiguity in the definition of ''free men'' in Magna Carta.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is the ''baronial_privilege_reading'' of the ''magna_carta_1215'' kernel. How would the classification change under the ''universal_rights_reading'' or ''living_document_reading''?',
    'Analyzing the structural properties (beneficiaries, victims, extractiveness, suppression) of the alternative readings as distinct constraints.',
    'The ''universal_rights_reading'' would likely yield a Tangled Rope or Snare due to broader application and potential for extraction from the state, while the ''living_document_reading'' would emphasize adaptive interpretation, potentially shifting classification over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative readings of Magna Carta on its classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1315).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__baronial_privilege_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magn_tr_t50, magna_carta_1215__baronial_privilege_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(magn_tr_t100, magna_carta_1215__baronial_privilege_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(magn_be_t50, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(magn_be_t100, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 100, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(magn_su_t50, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(magn_su_t100, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Magna Carta (1215) kernel. Each reading has a different structural interpretation of its beneficiaries, scope, and impact, leading to different classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
