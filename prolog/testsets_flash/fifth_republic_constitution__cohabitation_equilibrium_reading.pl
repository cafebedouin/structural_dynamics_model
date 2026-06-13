% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Constitution: Cohabitation Equilibrium Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This constraint describes the 'cohabitation equilibrium' reading of the
 *   French Fifth Republic Constitution, where a dual executive (President and
 *   Prime Minister) from opposing political parties must negotiate authority
 *   allocation. This reading emphasizes mutual constraint and shared power,
 *   particularly in domestic policy, leading to periods of both stability and
 *   tension. The constraint's extractiveness is moderate but unstable, as it
 *   extracts policy coherence and political stability, while benefiting
 *   whichever actor controls key policy domains.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.4).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Constitution: Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '48178d22-1b11-40df-b29d-97d0ed8e21a4').
narrative_ontology:cs_kernel_codification('48178d22-1b11-40df-b29d-97d0ed8e21a4', fixed_text).
narrative_ontology:cs_authority_grounding('48178d22-1b11-40df-b29d-97d0ed8e21a4', lineage).
narrative_ontology:cs_interpretation_layer_present('48178d22-1b11-40df-b29d-97d0ed8e21a4').
narrative_ontology:cs_reading_relation('48178d22-1b11-40df-b29d-97d0ed8e21a4', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('48178d22-1b11-40df-b29d-97d0ed8e21a4', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('48178d22-1b11-40df-b29d-97d0ed8e21a4', foundational, dual_executive_mutual_constraint).
narrative_ontology:cs_axiom_status(dual_executive_mutual_constraint, holdable).
narrative_ontology:cs_axiom_grounding('48178d22-1b11-40df-b29d-97d0ed8e21a4', dual_executive_mutual_constraint, conventional).
narrative_ontology:cs_axiom('48178d22-1b11-40df-b29d-97d0ed8e21a4', foundational, negotiated_authority_allocation).
narrative_ontology:cs_axiom_status(negotiated_authority_allocation, holdable).
narrative_ontology:cs_axiom_grounding('48178d22-1b11-40df-b29d-97d0ed8e21a4', negotiated_authority_allocation, conventional).
narrative_ontology:cs_reference_frame('48178d22-1b11-40df-b29d-97d0ed8e21a4', constitutional_balance_of_powers).
narrative_ontology:cs_drift_state('48178d22-1b11-40df-b29d-97d0ed8e21a4', contemporary_political_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('48178d22-1b11-40df-b29d-97d0ed8e21a4', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_france).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_of_france).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, political_stability).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the costs of policy incoherence and potential gridlock inherent in cohabitation, where the President and Prime Minister may pull in different directions. Suppression (0.40) is moderate, as the constitutional framework actively enforces the power-sharing, but political actors retain significant agency. The theater ratio (0.20) is low, indicating that the power-sharing is a genuine, if often contentious, function of the system, not merely performative. The temporal measurements reflect the ebb and flow of cohabitation periods, with extractiveness and suppression rising during intense cohabitation and falling during periods of political alignment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President or Prime Minister during cohabitation, the constraint is a necessary framework for governance, enabling a balance of power. From the perspective of the electorate or those concerned with policy outcomes, it can appear as an extractive mechanism that sacrifices efficiency for political compromise. The engine will compute these divergences based on the declared roles and attributes.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and Prime Minister, along with the National Assembly, are beneficiaries in that they gain power and influence over policy domains during cohabitation, albeit through negotiation. The French electorate is a payer, bearing the costs of potential policy incoherence and instability. Abstract entities like 'policy_coherence' and 'political_stability' are victims, as they are directly diminished by the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_stability_vs_gridlock,
    'Is cohabitation a mechanism for stable power-sharing and democratic accountability, or does it primarily lead to political gridlock and policy incoherence?',
    'Comparative analysis of policy outcomes and governmental effectiveness during cohabitation periods versus periods of unified executive control, across multiple Fifth Republic governments.',
    'If primarily gridlock, the constraint''s extractiveness is higher than currently measured, as it consistently sacrifices effective governance. If primarily stable power-sharing, the extractiveness is lower, reflecting a functional compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_stability_vs_gridlock, empirical, 'Ambiguity regarding the functional outcome of cohabitation.').

omega_variable(
    presidential_vs_parliamentary_primacy,
    'To what extent does the ''cohabitation equilibrium'' reading genuinely balance presidential and parliamentary power, versus merely shifting the locus of extraction between them?',
    'Detailed analysis of legislative output, executive decrees, and judicial review during cohabitation, focusing on which branch ultimately prevails in contested policy areas.',
    'If one branch consistently dominates, the reading''s claim of ''equilibrium'' is weakened, and the constraint might lean more towards a ''snare'' for the subordinate branch, or a ''tangled_rope'' with a more concentrated beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_vs_parliamentary_primacy, conceptual, 'The true balance of power during cohabitation.').

omega_variable(
    reading_naturalness_vs_political_strategy,
    'Is the ''cohabitation equilibrium'' reading an inherent structural feature of the Fifth Republic Constitution, or a political strategy adopted by actors to legitimize power-sharing when forced by electoral outcomes?',
    'Analysis of constitutional debates and political discourse surrounding the adoption of the 1962 direct presidential election, and how interpretations of executive power evolved before and after the first cohabitation periods.',
    'If primarily a political strategy, the ''naturalness'' of this reading is diminished, suggesting a higher degree of constructedness and potential for manipulation, which could increase its effective extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_naturalness_vs_political_strategy, conceptual, 'Whether the cohabitation reading is a natural outcome or a strategic interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1986, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(fift_tr_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2022, 0.18).

% Extraction over time
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.65).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2002, 0.45).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2012, 0.4).
narrative_ontology:measurement(fift_be_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2022, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.5).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2002, 0.35).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2012, 0.3).
narrative_ontology:measurement(fift_su_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2022, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Fifth Republic Constitution kernel, focusing on the cohabitation equilibrium. It is structurally distinct from the 'hyper_presidential_reading' and 'parliamentary_constraint_reading' due to differing interpretations of executive-legislative power balance and resulting beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
