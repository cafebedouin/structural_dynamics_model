% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations: Punitive Liability Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'punitive liability' reading of the
 *   Versailles Treaty's reparations clauses, particularly Article 231 (the
 *   'War Guilt Clause'). Under this reading, Germany bore unique moral and
 *   financial responsibility for the total costs of World War I, justifying
 *   quasi-unlimited reparations claims by the Allied creditor states. This
 *   interpretation led to significant economic extraction from Germany and
 *   substantial suppression of its fiscal sovereignty, contributing to
 *   political instability. The constraint is claimed as a 'snare' due to its
 *   highly extractive nature and the suppression of German alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.85).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.75).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations: Punitive Liability Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, 'c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce').
narrative_ontology:cs_kernel_codification('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', fixed_text).
narrative_ontology:cs_authority_grounding('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', lineage).
narrative_ontology:cs_interpretation_layer_present('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce').
narrative_ontology:cs_reading_relation('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', foundational, german_sole_war_guilt).
narrative_ontology:cs_axiom_status(german_sole_war_guilt, holdable).
narrative_ontology:cs_axiom_grounding('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', german_sole_war_guilt, conventional).
narrative_ontology:cs_axiom('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', foundational, unlimited_reparations_justified).
narrative_ontology:cs_axiom_status(unlimited_reparations_justified, holdable).
narrative_ontology:cs_axiom_grounding('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', unlimited_reparations_justified, instrumental).
narrative_ontology:cs_reference_frame('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', post_war_punitive_justice).
narrative_ontology:cs_drift_state('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', interwar_economic_realities, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1ea83d4-8263-4a1a-bf8f-afaffc7d6fce', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_fiscal_sovereignty).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the reparations demands were immense, far exceeding Germany's immediate capacity and diverting significant national wealth. Suppression (0.75) is high due to the threat of Allied occupation (e.g., the Ruhr Crisis) and the lack of viable alternatives for Germany to avoid payment without severe consequences. The theater ratio (0.1) is low, as the enforcement was very real and directly impacted the German economy. Resistance (0.9) was extremely high, manifesting in political opposition, economic crises, and ultimately, repudiation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Allied creditor states, this was a legitimate claim for damages and a just consequence of war. From the German perspective (workers, taxpayers, and the government), it was an unjust, punitive, and economically crippling burden. The engine's classification will reflect this divergence based on the declared beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states are clear beneficiaries (d=0.0-0.1) as they received payments and sought to rebuild their economies. German workers and taxpayers are the primary victims (d=0.9-1.0), bearing the direct costs through taxation and economic hardship. German fiscal sovereignty is also a victim, as external claims dictated national economic policy. The enforcement mechanisms (e.g., occupation threats) ensured high directionality towards Germany.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to compensate for war damages and assign responsibility. However, the punitive interpretation led to an overextension of this mandate, transforming it into a mechanism for severe extraction. The high extractiveness and suppression, coupled with the high resistance, indicate that the coordination function (rebuilding Europe) was overshadowed by the extractive function, preventing it from being a 'rope' or 'tangled_rope'. The 'snare' classification accurately captures this dynamic, where the coordination story (justice, rebuilding) served as cover for punitive extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of international law or a punitive interpretation of the Versailles Treaty?',
    'Analysis of alternative readings of Article 231 and subsequent international legal developments regarding war guilt and reparations.',
    'If a punitive interpretation, the constraint''s legitimacy is weakened, and its classification shifts from a ''rope'' (coordination) to a ''snare'' (extraction) from the German perspective. This story instantiates the punitive_liability_reading of the versailles_reparations_clauses kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as the punitive_liability_reading of the versailles_reparations_clauses kernel.').

omega_variable(
    reparations_economic_capacity_ambiguity,
    'To what extent were the reparations claims truly quasi-unlimited, or were they implicitly bounded by Germany''s economic capacity?',
    'Historical economic analysis of Germany''s actual payment capacity versus the demands, and the impact of the Dawes and Young Plans.',
    'If implicitly bounded by capacity, the ''limited_responsibility_reading'' gains strength, reducing the perceived extractiveness and suppression of this ''punitive_liability_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reparations_economic_capacity_ambiguity, empirical, 'Ambiguity regarding the true limits of reparations claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(vers_be_t5, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 5, 0.85).
narrative_ontology:measurement(vers_be_t10, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement(vers_be_t15, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 15, 0.88).
narrative_ontology:measurement(vers_be_t20, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vers_su_t5, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(vers_su_t10, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(vers_su_t15, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(vers_su_t20, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, german_hyperinflation_of_1923).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, rise_of_nazism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'versailles_reparations_clauses' kernel, focusing on the punitive interpretation of German liability. Its high extractiveness and suppression contrast with the 'limited_responsibility_reading' and are directly challenged by the 'repudiation_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
