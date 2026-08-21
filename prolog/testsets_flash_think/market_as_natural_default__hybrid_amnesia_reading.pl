% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market as Natural Default (Hybrid Amnesia Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint describes the 'market as natural default' through a
 *   'hybrid amnesia' reading. It posits a two-stage process: an initial
 *   period (roughly 1930s-1970s) where historical alternatives to market
 *   dominance genuinely lapsed from collective memory due to various
 *   historical contingencies and shifts in political economy. This 'lapsed
 *   closure' then created conditions for identifiable beneficiaries
 *   (incumbent capital holders, neoliberal policymakers) to actively capture
 *   and weaponize this pre-existing amnesia (roughly 1980s-present),
 *   defensively rationalizing the market's default status as natural and
 *   inevitable, thereby enabling increasing extraction. The constraint is
 *   CLAIMED as a mountain (the 'natural default' framing) while the authored
 *   metrics describe a substantially extractive, actively enforced, and
 *   increasingly theatrical operation, allowing the engine to measure this
 *   divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.8).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market as Natural Default (Hybrid Amnesia Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).
domain_priors:emerges_naturally(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '0fbfb273-2e6c-49c5-bc06-60d892848c29').
narrative_ontology:cs_kernel_codification('0fbfb273-2e6c-49c5-bc06-60d892848c29', implicit).
narrative_ontology:cs_authority_grounding('0fbfb273-2e6c-49c5-bc06-60d892848c29', extraction).
narrative_ontology:cs_interpretation_layer_present('0fbfb273-2e6c-49c5-bc06-60d892848c29').
narrative_ontology:cs_reading_relation('0fbfb273-2e6c-49c5-bc06-60d892848c29', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('0fbfb273-2e6c-49c5-bc06-60d892848c29', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('0fbfb273-2e6c-49c5-bc06-60d892848c29', foundational, historical_contingency_of_market_dominance).
narrative_ontology:cs_axiom_status(historical_contingency_of_market_dominance, holdable).
narrative_ontology:cs_axiom_grounding('0fbfb273-2e6c-49c5-bc06-60d892848c29', historical_contingency_of_market_dominance, empirically_contingent).
narrative_ontology:cs_axiom('0fbfb273-2e6c-49c5-bc06-60d892848c29', secondary, cognitive_biases_in_economic_memory).
narrative_ontology:cs_axiom_status(cognitive_biases_in_economic_memory, holdable).
narrative_ontology:cs_axiom_grounding('0fbfb273-2e6c-49c5-bc06-60d892848c29', cognitive_biases_in_economic_memory, empirically_contingent).
narrative_ontology:cs_reference_frame('0fbfb273-2e6c-49c5-bc06-60d892848c29', pre_keynesian_laissez_faire).
narrative_ontology:cs_drift_state('0fbfb273-2e6c-49c5-bc06-60d892848c29', contemporary_post_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fbfb273-2e6c-49c5-bc06-60d892848c29', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoliberal_policymakers).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, labor).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_sector).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, displaced_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the market's 'natural' status, which legitimizes their accumulation of wealth and power. They actively fund narratives and policies that reinforce this default, inheriting and weaponizing the historical amnesia.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Implement policies (deregulation, privatization, austerity) that treat the market as the optimal, natural allocator of resources, often citing efficiency. They are key in maintaining the ideological default.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoliberal_policymakers, agenda_setter,
    institutional, biographical, mobile, national).

% Bears the costs of market naturalization through wage stagnation, precarity, and reduced social safety nets, with limited collective bargaining power against the 'natural' forces of the market.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, labor, payer,
    powerless, biographical, constrained, national).

% Faces continuous pressure for privatization and budget cuts, as public services are framed as inefficient distortions of the 'natural' market. Its capacity to provide alternatives is systematically eroded.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_sector, payer,
    organized, generational, constrained, national).

% Directly impacted by market-driven resource allocation (e.g., land grabs, environmental degradation, gentrification) with few avenues for redress or resistance against what is presented as economic inevitability.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, displaced_communities, payer,
    powerless, immediate, trapped, local).

% Analyze the historical contingency of market dominance, documenting the processes of forgetting and the suppression of alternatives. Their insights challenge the 'natural default' narrative but often struggle for mainstream recognition.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% Advocate for historically suppressed or forgotten economic alternatives (e.g., socialized industries, cooperative models, public banking). They are often marginalized in mainstream discourse, their proposals dismissed as 'unrealistic' or 'anti-market'.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, advocacy_groups_for_alternatives, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate resource allocation, production, and exchange across society, ostensibly through decentralized price signals and competition.
% TRANSFER_FUNCTION: Transfers wealth, power, and risk from labor, public goods, and vulnerable communities to incumbent capital holders and those who benefit from the market's 'natural' status, justified by claims of efficiency and inevitability.
% ABSENT_VOICES: Advocates for historically suppressed or forgotten economic alternatives (e.g., socialized industries, cooperative models, planned economies) are structurally marginalized; they would argue for a re-evaluation of economic fundamentals but are kept out by the ideological framing of market naturalness.
% DISAPPEARANCE_RATIONALE: If the belief in the market as a natural, inevitable default vanished overnight, it would open a profound ideological and political space for re-evaluating economic structures. This would lead to significant policy shifts, redistribution of wealth, and the re-emergence of alternative economic models, fundamentally reorganizing society.
% FOUNDING_PROBLEM: To efficiently allocate resources and foster innovation, replacing less efficient, more centralized, or politically controlled systems that were perceived to stifle economic growth and individual liberty.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent capital holders and neoliberal policymakers assert the market's natural efficiency and problem-solving capacity. Economic historians and critical political economists, from outside the benefiting parties, corroborate that initial problems with prior systems were real but argue the market's current 'natural' status is a constructed default, not an inherent property, and that its original problem-solving function has been superseded by rent-seeking.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__hybrid_amnesia_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.45) reflects the significant transfer of wealth and power to beneficiaries, which has accumulated over time. `Suppression` is high (0.80) because the 'natural default' narrative actively marginalizes and discredits alternatives, requiring continuous ideological maintenance. `Theater_ratio` (0.60) is substantial, indicating that a large portion of the justification for market naturalness is performative, masking underlying power dynamics. `Accessibility_collapse` (0.95) is near-total, as the very idea of systemic alternatives has largely vanished from mainstream discourse. `Resistance` (0.20) is low at the structural level due to the deep entrenchment of the ideology. The `coercion_grid` illustrates this two-stage process, showing initial lower stakes and suppression, evolving to high stakes and suppression as amnesia is weaponized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, the market's default status is a natural, efficient, and beneficial arrangement. From the perspective of victims, it is a constructed ideological framework that enables significant extraction and suppresses viable alternatives. The engine's classification will highlight this divergence between the claimed 'mountain' and the high extraction/suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders and neoliberal policymakers are clear beneficiaries and agenda-setters, actively shaping and benefiting from the market's 'natural' status. Labor, the public sector, and displaced communities are the primary targets, bearing the costs of this naturalization. Economic historians act as analytical observers, while advocacy groups for alternatives are structurally excluded from mainstream policy discourse, their voices suppressed by the dominant narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_origin,
    'Is the market''s default status a natural, inevitable outcome of human interaction and economic principles, or is it a historically constructed and ideologically maintained arrangement?',
    'Comparative historical analysis of societies with different economic organizing principles, and empirical studies on the impact of policy choices versus inherent market forces.',
    'If natural, the constraint is a genuine mountain. If constructed, it is a snare or tangled rope, and its persistence depends on active maintenance and suppression of alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_origin, conceptual, 'Ambiguity regarding the market''s inherent vs. constructed nature.').

omega_variable(
    amnesia_vs_active_suppression,
    'What proportion of the suppression of economic alternatives is due to genuine historical amnesia (passive forgetting) versus active, contemporary suppression by beneficiaries (deliberate marginalization)?',
    'Content analysis of policy debates and media narratives over time, combined with historical research into the mechanisms of alternative economic model disappearance and re-emergence.',
    'If primarily amnesia, the constraint''s initial extractiveness was lower and its persistence more inertial. If primarily active suppression, its extractiveness is more deliberate and its persistence more coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_vs_active_suppression, empirical, 'Distinguishing passive forgetting from active suppression of alternatives.').

omega_variable(
    reversibility_of_forgetting,
    'To what extent can the ''lapsed alternatives'' be genuinely recovered and re-implemented, or are they permanently foreclosed by path dependence and the entrenchment of the ''natural default''?',
    'Case studies of successful implementation of alternative economic models in contemporary contexts, and analysis of the political and social costs of such transitions.',
    'If reversible, the accessibility collapse is less severe than it appears, and resistance efforts have higher potential efficacy. If irreversible, the constraint is more deeply entrenched and exit options are more limited.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_forgetting, empirical, 'The potential for recovering forgotten economic alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mark_tr_t15, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(mark_tr_t45, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(mark_tr_t60, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(mark_tr_t75, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 75, 0.55).
narrative_ontology:measurement(mark_tr_t90, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 90, 0.6).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(mark_be_t15, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(mark_be_t45, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 45, 0.35).
narrative_ontology:measurement(mark_be_t60, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(mark_be_t75, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 75, 0.43).
narrative_ontology:measurement(mark_be_t90, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 90, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mark_su_t15, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(mark_su_t45, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 45, 0.64).
narrative_ontology:measurement(mark_su_t60, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(mark_su_t75, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 75, 0.77).
narrative_ontology:measurement(mark_su_t90, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 90, 0.8).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=90
narrative_ontology:measurement(mark_grid_01, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(class), 0, 0.6).
narrative_ontology:measurement(mark_grid_02, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(class), 90, 0.9).
narrative_ontology:measurement(mark_grid_03, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(individual), 0, 0.4).
narrative_ontology:measurement(mark_grid_04, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(individual), 90, 0.8).
narrative_ontology:measurement(mark_grid_05, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(organizational), 0, 0.5).
narrative_ontology:measurement(mark_grid_06, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(organizational), 90, 0.85).
narrative_ontology:measurement(mark_grid_07, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(structural), 0, 0.7).
narrative_ontology:measurement(mark_grid_08, market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse(structural), 90, 0.95).
narrative_ontology:measurement(mark_grid_09, market_as_natural_default__hybrid_amnesia_reading, resistance(class), 0, 0.3).
narrative_ontology:measurement(mark_grid_10, market_as_natural_default__hybrid_amnesia_reading, resistance(class), 90, 0.35).
narrative_ontology:measurement(mark_grid_11, market_as_natural_default__hybrid_amnesia_reading, resistance(individual), 0, 0.4).
narrative_ontology:measurement(mark_grid_12, market_as_natural_default__hybrid_amnesia_reading, resistance(individual), 90, 0.3).
narrative_ontology:measurement(mark_grid_13, market_as_natural_default__hybrid_amnesia_reading, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(mark_grid_14, market_as_natural_default__hybrid_amnesia_reading, resistance(organizational), 90, 0.4).
narrative_ontology:measurement(mark_grid_15, market_as_natural_default__hybrid_amnesia_reading, resistance(structural), 0, 0.25).
narrative_ontology:measurement(mark_grid_16, market_as_natural_default__hybrid_amnesia_reading, resistance(structural), 90, 0.2).
narrative_ontology:measurement(mark_grid_17, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(class), 0, 0.2).
narrative_ontology:measurement(mark_grid_18, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(class), 90, 0.8).
narrative_ontology:measurement(mark_grid_19, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(individual), 0, 0.1).
narrative_ontology:measurement(mark_grid_20, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(individual), 90, 0.7).
narrative_ontology:measurement(mark_grid_21, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(organizational), 0, 0.15).
narrative_ontology:measurement(mark_grid_22, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(organizational), 90, 0.75).
narrative_ontology:measurement(mark_grid_23, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(structural), 0, 0.25).
narrative_ontology:measurement(mark_grid_24, market_as_natural_default__hybrid_amnesia_reading, stakes_inflation(structural), 90, 0.85).
narrative_ontology:measurement(mark_grid_25, market_as_natural_default__hybrid_amnesia_reading, suppression(class), 0, 0.4).
narrative_ontology:measurement(mark_grid_26, market_as_natural_default__hybrid_amnesia_reading, suppression(class), 90, 0.75).
narrative_ontology:measurement(mark_grid_27, market_as_natural_default__hybrid_amnesia_reading, suppression(individual), 0, 0.2).
narrative_ontology:measurement(mark_grid_28, market_as_natural_default__hybrid_amnesia_reading, suppression(individual), 90, 0.6).
narrative_ontology:measurement(mark_grid_29, market_as_natural_default__hybrid_amnesia_reading, suppression(organizational), 0, 0.3).
narrative_ontology:measurement(mark_grid_30, market_as_natural_default__hybrid_amnesia_reading, suppression(organizational), 90, 0.7).
narrative_ontology:measurement(mark_grid_31, market_as_natural_default__hybrid_amnesia_reading, suppression(structural), 0, 0.5).
narrative_ontology:measurement(mark_grid_32, market_as_natural_default__hybrid_amnesia_reading, suppression(structural), 90, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, privatization_mandate).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, austerity_policies).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, deregulation_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'market_as_natural_default' kernel, focusing on the hybrid process of historical amnesia leading to beneficiary capture. It is linked to 'lapsed_alternative_reading' and 'beneficiary_maintained_reading' as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
