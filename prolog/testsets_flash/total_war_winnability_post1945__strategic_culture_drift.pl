% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Winnability (Strategic Culture Drift Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the ideational shift in strategic culture
 *   post-1945, where the concept of 'total war winnability' dropped from
 *   elite discourse, not due to structural impossibility or normative
 *   illegitimacy, but due to a change in how strategic communities
 *   conceptualized and discussed warfare. It is a reading of the
 *   'total_war_winnability_post1945' kernel, focusing on the role of
 *   strategic culture. The constraint is classified as a Piton because the
 *   capacity for total war remains, but the discourse around its viability
 *   has atrophied, maintained by institutional inertia and the self-interest
 *   of those invested in limited war frameworks.
 *
 * KEY AGENTS:
 *   - defense_intellectuals: Beneficiary (organized/identity_locked) — invested in limited war frameworks.
 *   - limited_war_theorists: Agenda Setter (institutional/identity_locked) — shape strategic discourse.
 *   - strategic_flexibility: Victim (powerless/trapped) — lost capacity for comprehensive planning.
 *   - military_planners: Payer (institutional/constrained) — constrained by prevailing strategic culture.
 *   - political_leaders: Payer (powerful/constrained) — inherit a limited strategic outlook.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.2).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.4).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.2).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Winnability (Strategic Culture Drift Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '2f247056-4db6-49c0-bbbe-348a0bb18552').
narrative_ontology:cs_kernel_codification('2f247056-4db6-49c0-bbbe-348a0bb18552', implicit).
narrative_ontology:cs_authority_grounding('2f247056-4db6-49c0-bbbe-348a0bb18552', practice).
narrative_ontology:cs_interpretation_layer_present('2f247056-4db6-49c0-bbbe-348a0bb18552').
narrative_ontology:cs_reading_relation('2f247056-4db6-49c0-bbbe-348a0bb18552', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f247056-4db6-49c0-bbbe-348a0bb18552', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_axiom('2f247056-4db6-49c0-bbbe-348a0bb18552', foundational, strategic_culture_shapes_feasibility).
narrative_ontology:cs_axiom_status(strategic_culture_shapes_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('2f247056-4db6-49c0-bbbe-348a0bb18552', strategic_culture_shapes_feasibility, empirically_contingent).
narrative_ontology:cs_reference_frame('2f247056-4db6-49c0-bbbe-348a0bb18552', post_nuclear_strategic_rationality).
narrative_ontology:cs_drift_state('2f247056-4db6-49c0-bbbe-348a0bb18552', contemporary_geopolitical_flux, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f247056-4db6-49c0-bbbe-348a0bb18552', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_theorists).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_planners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).
:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because the constraint doesn't directly extract material resources, but rather limits intellectual and strategic options. Suppression is moderate (0.4) as alternative strategic views are marginalized rather than actively censored. Theater ratio is high (0.7) because the 'unwinnability' narrative is largely performative, masking the underlying capacity for total war and the institutional inertia that maintains the limited war focus. Accessibility collapse is low (0.3) because the physical capacity for total war remains, but the ideational pathways to it are obscured. Resistance is low (0.1) as the ideational shift is deeply embedded in institutional culture.
 *
 * PERSPECTIVAL GAP:
 *   Defense intellectuals and limited war theorists perceive this as a beneficial coordination mechanism that prevents catastrophic thinking. Military planners and political leaders, while operating within this framework, may experience it as a constraint on their strategic options, leading to a divergence in perceived constraint type.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals and limited war theorists are beneficiaries (d near 0.0) as their careers and intellectual frameworks are validated and reinforced. Strategic flexibility and military planners are victims/payers (d near 1.0) as their options are curtailed and resources diverted. Political leaders are also payers, constrained by the inherited strategic culture.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing global catastrophe) is still live, but the mechanism (ideational shift away from total war winnability) has atrophied into a Piton. The original problem of preventing nuclear war is still relevant, but the constraint's current form primarily serves to maintain the intellectual dominance of limited war frameworks, rather than actively preventing total war. This prevents mislabeling it as a Snare, as the extraction is diffuse and primarily intellectual, not material, and the constraint persists more by inertia than active, concentrated benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideational_vs_structural_causation,
    'To what extent is the decline in total war discourse due to ideational shifts in strategic culture versus underlying structural changes (e.g., nuclear deterrence, economic interdependence)?',
    'Comparative historical analysis of strategic cultures in different geopolitical contexts, controlling for structural factors. Counterfactual analysis exploring how discourse might have evolved under different ideational trajectories.',
    'If structural factors are dominant, this constraint''s extractiveness and theater_ratio might be lower, as the ideational shift would be a symptom rather than a primary cause. If ideational factors are dominant, the Piton classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideational_vs_structural_causation, empirical, 'Distinguishing ideational from structural drivers of strategic culture.').

omega_variable(
    piton_vs_snare_ambiguity,
    'Is the benefit to defense intellectuals from maintaining limited war frameworks concentrated enough to reclassify this as a Snare, or is it truly diffuse enough for a Piton?',
    'Detailed analysis of funding flows, career paths, and institutional power structures within strategic studies. Quantify the direct material and reputational gains from adherence to the ''limited war'' paradigm.',
    'If benefits are highly concentrated and actively defended, the constraint would shift towards a Snare, indicating more direct extraction. If benefits remain primarily inertial and diffuse, the Piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_snare_ambiguity, empirical, 'Assessing the concentration of benefits to distinguish Piton from Snare.').

omega_variable(
    strategic_culture_framing_ambiguity,
    'Is ''strategic culture'' a sufficiently coherent and independent causal factor, or is it merely a reflection of deeper structural or normative forces?',
    'Conceptual analysis and empirical case studies demonstrating the independent causal power of strategic culture in shaping state behavior, distinct from material capabilities or international norms.',
    'If strategic culture is not an independent causal factor, this reading might collapse into the ''structural_contraction_reading'' or ''normative_reading_drop'' siblings, requiring re-evaluation of the constraint''s primary mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_culture_framing_ambiguity, conceptual, 'Conceptual coherence and causal independence of strategic culture as a driver.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.5).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.15).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2005, 0.2).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.08).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__normative_reading_drop).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This reading focuses on ideational shifts in strategic culture, while siblings address structural and normative factors. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
