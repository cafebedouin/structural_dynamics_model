% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market as Natural Default (Beneficiary-Maintained Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint represents the 'beneficiary-maintained' reading of the
 *   'market as natural default' kernel. It describes how incumbent
 *   beneficiaries (finance, corporate elites) actively defend and promote the
 *   idea that market-based economic arrangements are natural, inevitable, and
 *   optimal. This defense involves public relations, institutional capture,
 *   and the suppression of alternative economic imaginaries. The constraint
 *   is claimed as a Tangled Rope because it offers a coordination function (a
 *   stable, shared economic framework) while simultaneously extracting
 *   benefits for specific actors through active maintenance and suppression
 *   of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.65).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market as Natural Default (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, 'd8f41056-ead7-4450-92e0-f2c424156de9').
narrative_ontology:cs_kernel_codification('d8f41056-ead7-4450-92e0-f2c424156de9', implicit).
narrative_ontology:cs_authority_grounding('d8f41056-ead7-4450-92e0-f2c424156de9', extraction).
narrative_ontology:cs_interpretation_layer_present('d8f41056-ead7-4450-92e0-f2c424156de9').
narrative_ontology:cs_reading_relation('d8f41056-ead7-4450-92e0-f2c424156de9', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8f41056-ead7-4450-92e0-f2c424156de9', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('d8f41056-ead7-4450-92e0-f2c424156de9', foundational, market_naturalness_is_engineered).
narrative_ontology:cs_axiom_status(market_naturalness_is_engineered, holdable).
narrative_ontology:cs_axiom_grounding('d8f41056-ead7-4450-92e0-f2c424156de9', market_naturalness_is_engineered, empirically_contingent).
narrative_ontology:cs_axiom('d8f41056-ead7-4450-92e0-f2c424156de9', secondary, alternatives_are_actively_suppressed).
narrative_ontology:cs_axiom_status(alternatives_are_actively_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('d8f41056-ead7-4450-92e0-f2c424156de9', alternatives_are_actively_suppressed, empirically_contingent).
narrative_ontology:cs_reference_frame('d8f41056-ead7-4450-92e0-f2c424156de9', market_as_spontaneous_order).
narrative_ontology:cs_drift_state('d8f41056-ead7-4450-92e0-f2c424156de9', contemporary_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d8f41056-ead7-4450-92e0-f2c424156de9', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, finance_sector).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, corporate_elites).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, general_public).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, labor_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes narratives of market efficiency and naturalness through lobbying, media influence, and funding think tanks. Benefits directly from deregulation and the absence of alternatives, which are framed as distortions of a natural order.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, finance_sector, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the ideological framing of markets as natural, which legitimizes their accumulation of wealth and power. They support the finance sector's efforts to maintain this narrative, often through shared institutional networks.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, corporate_elites, beneficiary,
    powerful, biographical, mobile, global).

% Bears the costs of market failures, inequality, and reduced public services, which are often presented as unavoidable consequences of natural market forces. Their ability to imagine or advocate for alternatives is constrained by the dominant narrative.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, general_public, payer,
    powerless, biographical, constrained, national).

% Struggles to organize and advocate for worker rights and collective bargaining when market outcomes are framed as natural and inevitable. Their efforts to introduce non-market interventions are often dismissed as interfering with natural economic laws.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, labor_movements, payer,
    organized, generational, constrained, national).

% Proposes alternative economic models (e.g., cooperative economies, public banking, democratic planning) but finds their ideas marginalized or dismissed as utopian and unnatural within mainstream discourse, which is heavily influenced by incumbent beneficiaries.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, advocates_for_alternatives, excluded,
    moderate, generational, constrained, global).

% Analyze the historical contingency and political construction of markets, challenging the 'natural default' narrative. Their work often faces institutional resistance from departments and funding bodies aligned with mainstream economic thought.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, critical_economists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Presents a coherent, stable framework for economic activity, reducing perceived uncertainty and providing a shared (though contested) language for economic policy and social organization.
% TRANSFER_FUNCTION: Legitimizes the existing distribution of wealth and power, transferring social acceptance and political inertia to incumbent beneficiaries by framing their gains as natural outcomes of an inevitable system.
% ABSENT_VOICES: Advocates for alternative economic models and critical economists are often excluded from mainstream policy debates and media platforms, where their perspectives would challenge the naturalization narrative.
% DISAPPEARANCE_RATIONALE: If the 'market as natural default' narrative vanished overnight, the legitimacy of current economic structures would collapse. Demands for alternative arrangements and redistribution would intensify, leading to a fundamental reorganization of political and economic power.
% FOUNDING_PROBLEM: The problem of legitimizing existing power structures and wealth distribution in the face of historical contingency and social choice, presenting them as inevitable and fair rather than constructed.
% FOUNDING_PROBLEM_CORROBORATION: Critical economists, political scientists, and historians (outside the benefiting parties) attest that the problem of legitimation is ongoing and central to maintaining the current economic order, citing historical analyses of market construction and ideological struggle.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).
:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the primary 'extraction' is ideological and political, legitimizing existing wealth distribution rather than direct monetary transfer from a single transaction. Suppression is high (0.65) due to the active marginalization of alternative economic thought and policy. Theater ratio is moderate (0.40) as the 'naturalness' claim requires continuous performance through media, education, and policy advocacy, even as its functional justification for universal benefit wanes. The increasing trend in extractiveness and suppression reflects the hardening of this ideological position over time, particularly since the rise of neoliberalism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, the 'natural market' is a self-evident truth that underpins prosperity. From the perspective of victims and excluded parties, it is a constructed ideology actively maintained to legitimize extraction and suppress alternatives. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The finance sector and corporate elites are clear beneficiaries and agenda-setters, actively shaping the narrative to their advantage. The general public and labor movements are victims, bearing the costs of policies justified by this narrative and facing constrained options for change. Advocates for alternatives are excluded, their voices systematically marginalized. Critical economists act as observers, analyzing the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a genuine Mountain (natural law) or a simple Rope (pure coordination). The active defense and identifiable beneficiaries, coupled with the suppression of alternatives, clearly indicate an extractive component that requires ongoing maintenance, rather than a self-evident truth or a purely beneficial coordination mechanism. The 'founding problem' of legitimizing existing power structures remains 'live' because the underlying social choices are perpetually contested, requiring continuous ideological enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_defense_vs_passive_forgetting,
    'What proportion of the market''s ''natural default'' status is due to active, beneficiary-driven defense and what proportion is due to a passive historical forgetting of alternatives?',
    'Detailed historical and sociological studies tracing the mechanisms of ideological reproduction and the institutional memory of economic alternatives over time.',
    'If active defense is dominant, the constraint is more extractive and coercive (closer to Snare); if passive forgetting is dominant, it might lean more towards a Piton (inertial) or a different form of Tangled Rope where enforcement is less about active suppression and more about maintaining a knowledge gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_defense_vs_passive_forgetting, empirical, 'Distinguishing active ideological maintenance from historical amnesia in market naturalization.').

omega_variable(
    legitimation_vs_efficiency_function,
    'To what extent does the ''market as natural default'' narrative genuinely coordinate economic activity efficiently, versus primarily serving to legitimize existing power structures?',
    'Comparative analysis of economic systems with different foundational narratives, assessing their efficiency and equity outcomes, alongside critical discourse analysis of how ''efficiency'' is defined and measured within the dominant narrative.',
    'If legitimation is the primary function, the constraint''s extractiveness is higher and its coordination function is more theatrical; if genuine efficiency is dominant, the extractiveness might be a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimation_vs_efficiency_function, conceptual, 'The true balance between coordination and legitimation functions of market naturalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(mark_be_t1988, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1988, 0.38).
narrative_ontology:measurement(mark_be_t1996, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1996, 0.41).
narrative_ontology:measurement(mark_be_t2004, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2004, 0.43).
narrative_ontology:measurement(mark_be_t2012, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2012, 0.44).
narrative_ontology:measurement(mark_be_t2020, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(mark_su_t1988, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1988, 0.58).
narrative_ontology:measurement(mark_su_t1996, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1996, 0.61).
narrative_ontology:measurement(mark_su_t2004, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2004, 0.63).
narrative_ontology:measurement(mark_su_t2012, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2012, 0.64).
narrative_ontology:measurement(mark_su_t2020, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, hybrid_amnesia_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, neoliberal_policy_consensus).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, financial_deregulation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_as_natural_default' kernel, focusing on active maintenance by beneficiaries. The other readings ('lapsed_alternative_reading' and 'hybrid_amnesia_reading') offer alternative explanations for market dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
