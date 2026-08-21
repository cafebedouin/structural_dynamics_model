% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'lapsed alternative' reading of market
 *   naturalization, where market dominance persists not due to active defense
 *   or concentrated benefit, but because viable alternatives have atrophied
 *   through non-use or historical path dependence. It functions as a Piton:
 *   its primary function (active market shaping/defense) has atrophied, but
 *   the structure remains due to inertia and the high accessibility collapse
 *   for alternatives. The metrics reflect low active extraction and
 *   suppression, but high theatricality in any 'maintenance' efforts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.25).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.3).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, 'f45c4007-0cea-4c74-9d35-8775a46d0424').
narrative_ontology:cs_kernel_codification('f45c4007-0cea-4c74-9d35-8775a46d0424', implicit).
narrative_ontology:cs_authority_grounding('f45c4007-0cea-4c74-9d35-8775a46d0424', practice).
narrative_ontology:cs_reading_relation('f45c4007-0cea-4c74-9d35-8775a46d0424', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('f45c4007-0cea-4c74-9d35-8775a46d0424', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('f45c4007-0cea-4c74-9d35-8775a46d0424', foundational, market_dominance_is_inertial).
narrative_ontology:cs_axiom_status(market_dominance_is_inertial, holdable).
narrative_ontology:cs_axiom_grounding('f45c4007-0cea-4c74-9d35-8775a46d0424', market_dominance_is_inertial, conventional).
narrative_ontology:cs_axiom('f45c4007-0cea-4c74-9d35-8775a46d0424', foundational, alternatives_atrophied_naturally).
narrative_ontology:cs_axiom_status(alternatives_atrophied_naturally, holdable).
narrative_ontology:cs_axiom_grounding('f45c4007-0cea-4c74-9d35-8775a46d0424', alternatives_atrophied_naturally, empirically_contingent).
narrative_ontology:cs_reference_frame('f45c4007-0cea-4c74-9d35-8775a46d0424', uncontested_market_equilibrium).
narrative_ontology:cs_drift_state('f45c4007-0cea-4c74-9d35-8775a46d0424', contemporary_regulatory_scrutiny, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f45c4007-0cea-4c74-9d35-8775a46d0424', '').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, small_businesses).
narrative_ontology:constraint_victim(market_naturalization__lapsed_alternative_reading, potential_entrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These firms benefit from the lack of competition due to historical market closures, but in this reading, they are not actively maintaining the dominance through coercive means. Their position is sustained by inertia and the atrophy of alternatives, not active defense.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, incumbent_firms, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the diffuse costs of reduced choice and potentially higher prices due to market dominance. Their options are limited by the lack of viable alternatives, which have atrophied over time.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, consumers, payer,
    powerless, immediate, constrained, local).

% Struggle to compete against entrenched dominant players. They face high barriers to entry and growth, not due to active suppression by incumbents, but because the market structure has solidified around the dominant firms in the absence of alternatives.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, small_businesses, payer,
    moderate, biographical, constrained, regional).

% Would enter the market if viable alternatives existed or if the dominance was actively challenged. In this reading, their exclusion is due to the natural atrophy of market dynamism, not active gatekeeping.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, potential_entrants, excluded,
    powerless, generational, trapped, national).

% Analyze the historical evolution of market structures, often identifying periods where alternatives atrophied, leading to current dominance. They provide the analytical framework for this 'lapsed closure' reading.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, if uncompetitive, market structure for goods and services, where established players fulfill demand without significant disruption from new entrants.
% TRANSFER_FUNCTION: Diffusely transfers potential innovation, competitive pricing, and consumer choice from the broader market to the benefit of incumbent firms, primarily through the absence of alternatives.
% ABSENT_VOICES: Potential innovators, new market entrants, and advocates for market decentralization are absent; their alternatives never gained traction or atrophied through non-use, rather than being actively suppressed.
% DISAPPEARANCE_RATIONALE: If market dominance vanished overnight, the market structure would rapidly diversify. New entrants and alternative business models would emerge to fill the void, leading to significant shifts in pricing, product offerings, and distribution channels as the 'lapsed' alternatives are revived or new ones created.
% FOUNDING_PROBLEM: The initial problem was often the need for scale and efficiency in nascent industries, leading to natural monopolies or first-mover advantages that solidified market structures.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and some independent analysts corroborate that the initial problems of nascent industry scale and efficiency were largely solved decades ago. The current persistence of dominance, in this reading, is due to inertia and the atrophy of alternatives, not ongoing necessity. Incumbent firms would likely contest this 'dead' status, claiming ongoing efficiency benefits.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).
:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because, in this reading, there's no active rent-seeking, only the diffuse costs of a non-competitive market. Suppression is also low (0.30) as alternatives have atrophied rather than being actively suppressed. Theater ratio is high (0.70) because any claims of 'active competition' or 'market dynamism' are largely performative, masking the underlying inertia. Accessibility collapse is very high (0.85) as the core premise is that alternatives have genuinely disappeared or become unviable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent firms, this market dominance might be seen as a natural outcome of efficiency and consumer preference, requiring no active intervention. From the perspective of consumers and small businesses, it's a frustrating lack of choice and opportunity, even if no single actor is actively 'to blame.' The engine's classification as a Piton captures this sense of an atrophied, inertial structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent firms are the agenda-setters, benefiting from the status quo without active maintenance. Consumers and small businesses are payers, bearing the diffuse costs of limited choice and competition. Potential entrants are excluded, not by active gatekeeping, but by the structural inertia of the market. Economic historians serve as analytical observers, providing the framework for this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hidden_beneficiaries_ambiguity,
    'Is there truly no identifiable beneficiary class, or are there hidden beneficiaries who subtly maintain the lapsed closure without overt action?',
    'Detailed forensic economic analysis tracing indirect benefits and lobbying efforts over time, even if not directly related to ''active maintenance'' of the market structure itself.',
    'If hidden beneficiaries are identified, the constraint''s extractiveness and suppression might be higher than currently assessed, potentially reclassifying it towards a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_beneficiaries_ambiguity, empirical, 'Whether the ''lapsed'' nature truly implies no active beneficiaries.').

omega_variable(
    purely_lapsed_vs_hybrid,
    'Is market dominance purely a lapsed closure, or does it combine elements of lapsed alternatives with subtle, active maintenance (the ''hybrid'' reading)?',
    'Comparative case studies across different market sectors, analyzing the proportion of persistence attributable to inertia versus ongoing, even if subtle, strategic actions by incumbents.',
    'If a significant ''hybrid'' component is found, the constraint''s extractiveness and suppression would increase, moving it closer to a Tangled Rope, as active maintenance implies some degree of coordination and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purely_lapsed_vs_hybrid, conceptual, 'Distinguishing purely inertial dominance from a hybrid of inertia and subtle active maintenance.').

omega_variable(
    mechanism_of_alternative_atrophy,
    'What is the precise mechanism by which alternatives atrophied? Was it purely ''natural'' market forces, or were there historical, now-invisible, suppressive actions?',
    'Deep historical institutional analysis, examining regulatory capture, anti-competitive practices, or network effects that, while no longer ''active,'' set the conditions for atrophy.',
    'If historical suppressive actions are identified as the cause of atrophy, the ''lapsed'' nature becomes a consequence of past Snare-like behavior, influencing the historical trajectory of extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_of_alternative_atrophy, empirical, 'Understanding the historical causes of alternative atrophy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1990, market_naturalization__lapsed_alternative_reading, theater_ratio, 1990, 0.65).
narrative_ontology:measurement(mark_tr_t1996, market_naturalization__lapsed_alternative_reading, theater_ratio, 1996, 0.67).
narrative_ontology:measurement(mark_tr_t2002, market_naturalization__lapsed_alternative_reading, theater_ratio, 2002, 0.68).
narrative_ontology:measurement(mark_tr_t2008, market_naturalization__lapsed_alternative_reading, theater_ratio, 2008, 0.69).
narrative_ontology:measurement(mark_tr_t2014, market_naturalization__lapsed_alternative_reading, theater_ratio, 2014, 0.7).
narrative_ontology:measurement(mark_tr_t2020, market_naturalization__lapsed_alternative_reading, theater_ratio, 2020, 0.7).

% Extraction over time
narrative_ontology:measurement(mark_be_t1990, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(mark_be_t1996, market_naturalization__lapsed_alternative_reading, base_extractiveness, 1996, 0.23).
narrative_ontology:measurement(mark_be_t2002, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2002, 0.24).
narrative_ontology:measurement(mark_be_t2008, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2008, 0.25).
narrative_ontology:measurement(mark_be_t2014, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement(mark_be_t2020, market_naturalization__lapsed_alternative_reading, base_extractiveness, 2020, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1990, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(mark_su_t1996, market_naturalization__lapsed_alternative_reading, suppression_requirement, 1996, 0.29).
narrative_ontology:measurement(mark_su_t2002, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2002, 0.3).
narrative_ontology:measurement(mark_su_t2008, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(mark_su_t2014, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2014, 0.3).
narrative_ontology:measurement(mark_su_t2020, market_naturalization__lapsed_alternative_reading, suppression_requirement, 2020, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
