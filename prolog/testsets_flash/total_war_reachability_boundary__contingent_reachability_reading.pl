% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Contingent Reachability of Total War (Technology-Dependent)
 *   domain: international_relations/strategic_studies/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This constraint represents the 'contingent reachability' reading of the
 *   total war boundary, arguing that the feasibility of total war is not
 *   permanently foreclosed but is instead dependent on the current
 *   technological equilibrium. The current contraction of strategic space
 *   (making total war seem 'unwinnable') is viewed as a temporary piton, an
 *   atrophied capability that could reverse with new technological
 *   developments. Beneficiaries are states investing in destabilizing
 *   technologies, as they gain leverage, while victims are global populations
 *   if deterrence fails.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.4).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.3).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Contingent Reachability of Total War (Technology-Dependent)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '4bc33217-a884-42cc-8e08-ce2d4f5d4cc9').
narrative_ontology:cs_kernel_codification('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', distributed).
narrative_ontology:cs_authority_grounding('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', diffuse_epistemic).
narrative_ontology:cs_reading_relation('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', foundational, strategic_feasibility_is_technology_dependent).
narrative_ontology:cs_axiom_status(strategic_feasibility_is_technology_dependent, holdable).
narrative_ontology:cs_axiom_grounding('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', strategic_feasibility_is_technology_dependent, empirically_contingent).
narrative_ontology:cs_axiom('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', secondary, current_strategic_stability_is_transient).
narrative_ontology:cs_axiom_status(current_strategic_stability_is_transient, holdable).
narrative_ontology:cs_axiom_grounding('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', current_strategic_stability_is_transient, empirically_contingent).
narrative_ontology:cs_reference_frame('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', post_cold_war_technological_equilibrium).
narrative_ontology:cs_drift_state('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', contemporary_multi_polar_tech_race, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4bc33217-a884-42cc-8e08-ce2d4f5d4cc9', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_tech).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_population).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a piton because the capability for total war is atrophied but not eliminated; its persistence is due to a combination of institutional inertia (maintaining arsenals) and a perceived, but potentially reversible, technological barrier. Extractiveness is moderate (0.4) as resources are still diverted to maintaining deterrence and counter-deterrence capabilities. Suppression is low (0.3) because the constraint isn't actively enforced by a central authority, but rather emerges from the strategic landscape. Theater ratio is high (0.6) as much of nuclear strategy involves performative signaling and maintenance of capabilities that are theoretically unusable. The measurements show a slight decrease in extractiveness and suppression over time, reflecting a period of relative strategic stability, while theater ratio increases as the performative aspect of deterrence grows relative to its direct utility.
 *
 * PERSPECTIVAL GAP:
 *   States investing in destabilizing technologies might perceive this as a scaffold, a temporary state to be overcome, while the global population experiences it as a constant, low-level threat. Analytical observers might see it as a piton, a stable but potentially reversible condition.
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in destabilizing technologies are beneficiaries (d=0.0-0.2) as they gain strategic leverage and potentially shift the equilibrium in their favor. The global population is a victim (d=0.8-1.0) as they bear the existential risk if the constraint fails. Other states (non-investors) are payers (d=0.5-0.7) as they must maintain defensive capabilities or participate in arms control without actively seeking to destabilize the current state.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing total war) is still live, but the mechanism (technological barriers) is seen as potentially atrophied and reversible. This classification prevents mislabeling it as a permanent 'mountain' (as in the contraction reading) or a stable 'rope' (as in the dropping reading), highlighting its contingent and potentially unstable nature. It flags the risk of mandatrophy if technological shifts render the current deterrence framework obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_reversal_probability,
    'What is the probability and timeline for a technological breakthrough that fundamentally alters the reachability of total war, making it ''winnable'' again?',
    'Expert assessment from military strategists, defense technologists, and intelligence agencies, updated with new R&D disclosures and doctrine shifts.',
    'If the probability is high and near-term, the constraint is a true scaffold, requiring active management to prevent collapse. If low and distant, it leans more towards a stable piton or even a rope, where the current state is more enduring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_reversal_probability, empirical, 'Uncertainty about future technological shifts impacting strategic stability.').

omega_variable(
    kernel_reading_distinction,
    'Is the ''contingent reachability'' reading fundamentally distinct from the ''contraction'' or ''dropping'' readings, or merely a temporal phase of one of them?',
    'Analysis of the core assumptions about technology''s role in strategic stability: if technology is a primary driver of feasibility, it''s distinct. If it''s secondary to political will or inherent nuclear effects, it''s a phase.',
    'If distinct, it validates this as a separate constraint. If a phase, it suggests this constraint should be nested within a broader ''total_war_reachability_boundary'' constraint, with this reading as a temporal state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying the conceptual boundary between different readings of total war reachability.').

omega_variable(
    piton_vs_scaffold_ambiguity,
    'Is the current contraction of total war reachability a true piton (atrophied capability, inertial persistence) or a scaffold (temporary support for a transition to a new, more stable equilibrium)?',
    'Observing the trajectory of investment in destabilizing technologies and the rhetoric of strategic competition. If investment accelerates and rhetoric normalizes ''limited'' nuclear war, it''s a scaffold for a dangerous transition. If it remains low and deterrence rhetoric holds, it''s a piton.',
    'If a scaffold, the constraint is more dynamic and carries higher risk of collapse. If a piton, it''s more stable but still requires vigilance against reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_scaffold_ambiguity, empirical, 'Distinguishing between inertial decay and temporary support for a transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(tota_tr_t10, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(tota_tr_t20, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(tota_tr_t30, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(tota_be_t10, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(tota_be_t20, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(tota_be_t30, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 30, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tota_su_t10, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(tota_su_t20, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(tota_su_t30, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_reachability_boundary' kernel. This 'contingent_reachability_reading' emphasizes the technology-dependent and reversible nature of total war's feasibility, contrasting with readings that posit permanent contraction or stable deterrence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
