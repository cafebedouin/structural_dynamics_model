% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__naturalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Price Formation as Natural Equilibrium (Naturalist Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'naturalist' reading of price formation,
 *   asserting that prices emerge from an objective equilibrium between
 *   scarcity and preference, akin to a natural law. From this perspective,
 *   prices are discovered, not constructed, and any policy intervention is
 *   seen as distorting a naturally efficient process. This is a Mountain
 *   constraint, as it describes an inherent, unchangeable economic reality,
 *   with negligible extraction or suppression. The metrics reflect this view,
 *   showing low extractiveness and suppression, and high accessibility
 *   collapse (alternatives to this fundamental process are seen as
 *   non-existent).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.05).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.02).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, '2b88e945-bec3-4961-beaf-973a26853e37').
narrative_ontology:cs_kernel_codification('2b88e945-bec3-4961-beaf-973a26853e37', implicit).
narrative_ontology:cs_authority_grounding('2b88e945-bec3-4961-beaf-973a26853e37', diffuse_epistemic).
narrative_ontology:cs_reading_relation('2b88e945-bec3-4961-beaf-973a26853e37', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b88e945-bec3-4961-beaf-973a26853e37', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b88e945-bec3-4961-beaf-973a26853e37', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('2b88e945-bec3-4961-beaf-973a26853e37', foundational, prices_reflect_objective_scarcity_and_preference).
narrative_ontology:cs_axiom_status(prices_reflect_objective_scarcity_and_preference, holdable).
narrative_ontology:cs_axiom_grounding('2b88e945-bec3-4961-beaf-973a26853e37', prices_reflect_objective_scarcity_and_preference, empirically_contingent).
narrative_ontology:cs_axiom('2b88e945-bec3-4961-beaf-973a26853e37', foundational, market_equilibrium_is_efficient).
narrative_ontology:cs_axiom_status(market_equilibrium_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('2b88e945-bec3-4961-beaf-973a26853e37', market_equilibrium_is_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('2b88e945-bec3-4961-beaf-973a26853e37', perfect_competition_equilibrium).
narrative_ontology:cs_drift_state('2b88e945-bec3-4961-beaf-973a26853e37', contemporary_financialized_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b88e945-bec3-4961-beaf-973a26853e37', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and firms acting in markets, whose aggregated preferences and scarcity perceptions are believed to 'discover' the natural price. They are subject to these prices but do not actively enforce them.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, economic_agents, observer,
    moderate, biographical, constrained, local).

% Government officials and regulators who, from this reading's perspective, often attempt to intervene in price formation, leading to distortions and inefficiencies. Their actions are seen as attempts to defy a natural process.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Economists and researchers who study market dynamics, seeking to understand and model the underlying natural forces of supply and demand that determine prices. They observe the constraint's operation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, market_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of scarce resources by signaling their relative value through price, enabling efficient matching of supply and demand across diverse economic agents.
% TRANSFER_FUNCTION: Facilitates the exchange of goods and services for money, reflecting the underlying value determined by scarcity and preference, without inherent extraction beyond transaction costs.
% ABSENT_VOICES: Those who believe prices are primarily constructed by power dynamics, institutional rules, or financial flows are absent from this reading's core assertion. They would argue that 'natural' prices mask underlying extraction or policy choices.
% DISAPPEARANCE_RATIONALE: If the 'natural equilibrium' aspect of price formation vanished, the underlying economic forces of scarcity and preference would still exist, and prices would still emerge, albeit perhaps with more friction or less 'natural' efficiency. The concept itself is a description of an underlying reality, not an active enforcement mechanism.
% FOUNDING_PROBLEM: To explain how complex economies allocate resources and coordinate production and consumption without central planning, and to demonstrate the efficiency of market mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream neoclassical economics and many market participants attest that the problem of resource allocation is fundamental and that market prices are the most efficient solution. Critics from other schools of thought (e.g., institutional economics, Marxist economics) contest the 'natural' and 'efficient' aspects, but acknowledge the problem of allocation.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_unchanged).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__naturalist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(price_formation_kernel__naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(price_formation_kernel__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and suppression (0.02) reflect the core tenet of this reading: price formation is a neutral, efficient mechanism, not a tool for rent extraction or coercion. The high accessibility collapse (0.9) signifies that alternatives to this fundamental market process are considered non-existent or inherently inefficient. Resistance is low (0.05) because, from this perspective, one cannot 'resist' a natural law, only misunderstand or attempt to interfere with it to one's detriment. The claimed type is 'mountain' because it posits an immutable, underlying economic reality.
 *
 * PERSPECTIVAL GAP:
 *   Other readings (institutional, georgist, financialization) would strongly dispute the 'natural' and 'non-extractive' character of price formation, arguing that it is heavily influenced by human institutions, power dynamics, and financial structures. This reading, however, holds that these are secondary effects or distortions of a more fundamental, natural process. The engine's classification will highlight this divergence between the claimed Mountain type and the potentially higher extractiveness/suppression that other readings would attribute to the same phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   From this naturalist reading, there are no direct beneficiaries or victims of price formation itself, as it is a neutral process. Economic agents are subject to it, but it does not 'extract' from them in a structural sense; it merely reflects their aggregated choices and the underlying scarcity. Policy makers are seen as actors who might interfere with this natural process, but not as agents who are structurally extracted from or benefit from the 'natural' price formation itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is price formation a natural equilibrium process reflecting objective scarcity and preference, or is it fundamentally constructed by human institutions, power dynamics, and financial systems?',
    'Empirical analysis of price determinants across different regulatory and institutional regimes; historical analysis of price behavior in the absence of specific institutional structures.',
    'If found to be primarily constructed, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope or Snare), with identifiable beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, conceptual, 'Ambiguity between natural economic law and human-constructed economic system.').

omega_variable(
    policy_intervention_efficacy,
    'Do policy interventions (e.g., rent control, zoning, subsidies) merely distort a natural price equilibrium, or can they effectively shape price formation towards socially desirable outcomes without creating significant deadweight loss?',
    'Comparative empirical studies of policy interventions in different markets, measuring their impact on efficiency, equity, and unintended consequences.',
    'If interventions are found to be effective and non-distorting, it challenges the ''natural'' and ''efficient'' claims of this reading, suggesting a more malleable system amenable to design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intervention_efficacy, empirical, 'The efficacy and impact of policy interventions on price formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1900, price_formation_kernel__naturalist_reading, theater_ratio, 1900, 0.01).
narrative_ontology:measurement(pric_tr_t1950, price_formation_kernel__naturalist_reading, theater_ratio, 1950, 0.01).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__naturalist_reading, theater_ratio, 2000, 0.01).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__naturalist_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(pric_be_t1900, price_formation_kernel__naturalist_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(pric_be_t1950, price_formation_kernel__naturalist_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__naturalist_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__naturalist_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1900, price_formation_kernel__naturalist_reading, suppression_requirement, 1900, 0.02).
narrative_ontology:measurement(pric_su_t1950, price_formation_kernel__naturalist_reading, suppression_requirement, 1950, 0.02).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__naturalist_reading, suppression_requirement, 2000, 0.02).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__naturalist_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
