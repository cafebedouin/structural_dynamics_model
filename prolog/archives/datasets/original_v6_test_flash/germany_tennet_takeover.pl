% ============================================================================
% CONSTRAINT STORY: germany_tennet_takeover
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_germany_tennet_takeover, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: germany_tennet_takeover
 *   human_readable: German Government Stake in TenneT Germany
 *   domain: economic/political
 *
 * SUMMARY:
 *   The German government's decision to acquire a stake in TenneT Germany, a
 *   critical electricity grid operator, reflects a strategic effort to secure
 *   energy independence and facilitate the transition to renewable energy
 *   sources. This intervention involves a complex interplay of economic,
 *   political, and social factors, impacting diverse stakeholders from
 *   taxpayers to foreign investors.
 *
 * KEY AGENTS:
 *   - German Government: Primary beneficiary (institutional/arbitrage) - Gains control over strategic infrastructure.
 *   - German Citizens: Secondary beneficiary (powerless/trapped) - Aims for energy security, but bears the cost via taxes.
 *   - Renewable Energy Sector: Tertiary beneficiary (powerful/mobile) - Benefits from grid stability and renewable energy transition.
 *   - Taxpayers: Primary victim (powerless/trapped) - Bear the financial burden of the bailout.
 *   - Foreign Investors: Secondary victim (moderate/constrained) - Face constrained opportunities in the German energy market.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(germany_tennet_takeover, 0.55).
domain_priors:suppression_score(germany_tennet_takeover, 0.4).
domain_priors:theater_ratio(germany_tennet_takeover, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(germany_tennet_takeover, extractiveness, 0.55).
narrative_ontology:constraint_metric(germany_tennet_takeover, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(germany_tennet_takeover, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(germany_tennet_takeover, tangled_rope).
narrative_ontology:human_readable(germany_tennet_takeover, "German Government Stake in TenneT Germany").
narrative_ontology:topic_domain(germany_tennet_takeover, "economic/political").

domain_priors:requires_active_enforcement(germany_tennet_takeover).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, german_government).
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, german_citizens).
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, renewable_energy_sector).
narrative_ontology:constraint_victim(germany_tennet_takeover, taxpayers).
narrative_ontology:constraint_victim(germany_tennet_takeover, foreign_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Taxpayers bear the cost of the bailout and have limited ability to influence the decision.
constraint_indexing:constraint_classification(germany_tennet_takeover, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The government benefits from increased control over critical infrastructure and ensures energy security.
constraint_indexing:constraint_classification(germany_tennet_takeover, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The government's intervention presents a complex mix of strategic coordination and potential extraction from taxpayers and market participants.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The renewable energy sector benefits from the stabilization of the grid and the promotion of green energy transition. They can also seek alternatives but depend on the grid.
constraint_indexing:constraint_classification(germany_tennet_takeover, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% Foreign investors face constrained options given the government's strategic decision, which hinders potential acquisitions.
constraint_indexing:constraint_classification(germany_tennet_takeover, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germany_tennet_takeover_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(germany_tennet_takeover, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(germany_tennet_takeover_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate to high. Taxpayers bear the cost of the government stake, while the government secures a vital asset. Suppression (0.40): Moderate. The government intervention limits foreign investment opportunities. Theater Ratio (0.20): Low. The government's actions are primarily functional, focused on energy security rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   German Taxpayers see the intervention as a snare, as they bear the cost with limited ability to influence the decision. The German Government sees it as a rope, ensuring control over critical infrastructure. Foreign investors perceive it as a restriction (snare) since they face constrained acquisition options. The Renewable Energy Sector sees it as a coordination mechanism (rope) helping towards energy transition. The Analytical Observer sees a mix of strategic coordination and potential extraction, leading to a tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The German government benefits directly from securing a vital national asset, hence a low 'd' value. Taxpayers bear the financial cost, resulting in a high 'd' value. The renewable energy sector indirectly benefits, yielding a low 'd' value, but has mobility. Foreign investors are constrained by the intervention, leading to a high 'd' value, but also constrained. The analytical observer considers the complexities leading to an intermediate 'd' value.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by considering the benefits of energy security and the renewable energy transition alongside the costs borne by taxpayers. The tangled rope classification reflects the mixed nature of the intervention, balancing strategic coordination with potential extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_vs_security,
    'To what extent does government ownership improve energy security compared to private ownership with regulatory oversight?',
    'Comparative analysis of grid stability, investment levels, and innovation rates under different ownership models.',
    'If government ownership proves significantly more secure, the intervention is justified. If not, it represents unnecessary economic intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_vs_security, empirical, 'Tradeoff between efficiency and security in energy infrastructure ownership.').

omega_variable(
    alternative_funding,
    'Were there viable alternative funding mechanisms to secure TenneT''s operations without direct government ownership?',
    'Analysis of potential private investment, public-private partnerships, and regulatory incentives.',
    'If viable alternatives existed, the government intervention represents a potentially inefficient use of public funds and market distortion. If not, the intervention is justified as a last resort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding, empirical, 'Availability of alternative funding mechanisms to avoid government ownership.').

omega_variable(
    market_distortion,
    'What is the long-term impact of government intervention on market competition and foreign investment in the German energy sector?',
    'Longitudinal tracking of investment flows, market concentration, and regulatory changes.',
    'If intervention leads to reduced competition and foreign investment, it undermines market efficiency. If it creates a stable and predictable environment, it can attract long-term investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_distortion, empirical, 'Long-term impact on competition and investment in the energy sector.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(germany_tennet_takeover, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(germ_tr_t0, germany_tennet_takeover, theater_ratio, 0, 0.1).
narrative_ontology:measurement(germ_tr_t5, germany_tennet_takeover, theater_ratio, 5, 0.15).
narrative_ontology:measurement(germ_tr_t10, germany_tennet_takeover, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(germ_be_t0, germany_tennet_takeover, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(germ_be_t5, germany_tennet_takeover, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(germ_be_t10, germany_tennet_takeover, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(germany_tennet_takeover, resource_allocation).
narrative_ontology:affects_constraint(germany_tennet_takeover, german_energy_transition).
narrative_ontology:affects_constraint(germany_tennet_takeover, eu_energy_independence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
