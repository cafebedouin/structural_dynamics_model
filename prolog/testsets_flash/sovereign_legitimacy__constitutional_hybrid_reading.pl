% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Reading of Sovereign Legitimacy
 *   domain: Political Philosophy / Constitutional Theory / Legitimacy Studies
 *
 * SUMMARY:
 *   This constraint describes the 'constitutional hybrid' reading of
 *   sovereign legitimacy, where authority is dual-sourced:
 *   ceremonial/symbolic authority is inherited (e.g., a monarch), and
 *   political authority is delegated (e.g., an elected parliament).
 *   Constitutional law mediates the boundary between these two sources. This
 *   reading aims to provide stability by integrating historical tradition
 *   with modern democratic principles, but it introduces ambiguities and
 *   constrains those who seek a 'purer' form of either monarchical or
 *   republican rule. The constraint is claimed as a Rope due to its genuine
 *   coordination function in managing this dual legitimacy, but it carries a
 *   low-to-moderate extractiveness due to the costs of maintaining both
 *   systems and the ambiguity inherent in the compromise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.25).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Reading of Sovereign Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "Political Philosophy / Constitutional Theory / Legitimacy Studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '7377070a-4eb0-4449-ab19-30078bae671c').
narrative_ontology:cs_kernel_codification('7377070a-4eb0-4449-ab19-30078bae671c', formalized).
narrative_ontology:cs_authority_grounding('7377070a-4eb0-4449-ab19-30078bae671c', lineage).
narrative_ontology:cs_interpretation_layer_present('7377070a-4eb0-4449-ab19-30078bae671c').
narrative_ontology:cs_reading_relation('7377070a-4eb0-4449-ab19-30078bae671c', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('7377070a-4eb0-4449-ab19-30078bae671c', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('7377070a-4eb0-4449-ab19-30078bae671c', foundational, dual_source_legitimacy).
narrative_ontology:cs_axiom_status(dual_source_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7377070a-4eb0-4449-ab19-30078bae671c', dual_source_legitimacy, conventional).
narrative_ontology:cs_axiom('7377070a-4eb0-4449-ab19-30078bae671c', foundational, constitutional_mediation_supremacy).
narrative_ontology:cs_axiom_status(constitutional_mediation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('7377070a-4eb0-4449-ab19-30078bae671c', constitutional_mediation_supremacy, conventional).
narrative_ontology:cs_reference_frame('7377070a-4eb0-4449-ab19-30078bae671c', post_glorious_revolution_settlement).
narrative_ontology:cs_drift_state('7377070a-4eb0-4449-ab19-30078bae671c', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7377070a-4eb0-4449-ab19-30078bae671c', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_lawyers).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low-to-moderate (0.35) because the compromise reduces the potential for high extraction from either pure absolutism or unchecked majoritarianism, but it still incurs costs for maintaining two distinct institutions and managing their interface. Suppression is low (0.25) as the system generally enjoys broad consent, and dissenters (absolutists, pure republicans) are not actively suppressed but rather marginalized by the prevailing constitutional consensus. Theater ratio is low (0.15) as both the symbolic and political functions are genuinely active, though the symbolic role can sometimes appear performative. The historical measurements show a gradual decrease in extractiveness and suppression as the system matured and gained acceptance, with the symbolic role becoming more stable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (monarch, elected officials, constitutional lawyers), this system is a stable, legitimate, and effective compromise. From the perspective of the victims (absolutist monarchists, pure republicans), it is an illegitimate, internally contradictory, and unsatisfying arrangement that prevents the realization of a 'true' form of sovereignty. The engine's per-seat classification should reflect this divergence, with beneficiaries experiencing it as a Rope and victims as a Snare or Tangled Rope due to the constraint on their ideological exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch and elected officials are beneficiaries, as they both derive legitimacy and power (symbolic or political) from this arrangement. Constitutional lawyers also benefit from the complexity of mediating the boundary. Citizens are payers, bearing the costs of maintaining the dual system and navigating its ambiguities. Absolutist monarchists and pure republicans are victims, as their preferred, 'pure' systems are foreclosed by this hybrid, and their ideological commitments are constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_legitimacy_ambiguity,
    'Is the dual-sourced legitimacy of the constitutional hybrid a strength (resilience through compromise) or a weakness (inherent instability due to unresolved tension)?',
    'Comparative historical analysis of hybrid vs. pure systems during periods of crisis: if hybrids demonstrate greater resilience, it''s a strength; if they fracture along the dual-source line, it''s a weakness.',
    'If a strength, the constraint''s coordination function is higher and extractiveness lower than measured; if a weakness, the inherent instability implies higher long-term costs and potential for collapse, increasing effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_legitimacy_ambiguity, empirical, 'Whether the dual legitimacy is a source of strength or weakness.').

omega_variable(
    boundary_mediation_effectiveness,
    'How effectively does constitutional law mediate the boundary between inherited and delegated authority, and what are the costs of its interpretive flexibility?',
    'Analysis of constitutional crises and judicial review outcomes: frequent, severe boundary disputes indicate low effectiveness; stable, accepted precedents indicate high effectiveness. Costs are measured by the resources expended on legal interpretation and political contention.',
    'If mediation is highly effective, the constraint operates more like a Rope; if it''s consistently contested and costly, it leans towards a Tangled Rope due to the ongoing extraction of resources for boundary maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_mediation_effectiveness, empirical, 'Effectiveness and cost of constitutional boundary mediation.').

omega_variable(
    reading_framing_underdetermination,
    'Is the ''constitutional_hybrid_reading'' the most defensible framing of sovereign legitimacy in this context, or would a ''monarchical_reading'' or ''republican_reading'' offer a more coherent account?',
    'A conceptual analysis of the historical and normative commitments of the political system. If the system''s actual operation consistently aligns more closely with the core tenets of a sibling reading, then the hybrid framing is less defensible.',
    'If a sibling reading is more coherent, the classification of this constraint would shift to reflect that underlying structure (e.g., if it''s truly a ''monarchical_reading'' in disguise, it might be a Snare for republicans).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Alternative framings of sovereign legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 1700, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1700, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(sove_tr_t1800, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(sove_tr_t1900, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1900, 0.14).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(sove_be_t1700, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1700, 0.45).
narrative_ontology:measurement(sove_be_t1800, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(sove_be_t1900, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1700, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(sove_su_t1800, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1800, 0.35).
narrative_ontology:measurement(sove_su_t1900, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sovereign_legitimacy' kernel. Other readings include 'monarchical_reading' and 'republican_reading', each representing a distinct structural claim about the source of legitimate authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
