% ============================================================================
% CONSTRAINT STORY: alternative_sovereignty_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alternative_sovereignty_scaffold, []).

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
 *   constraint_id: alternative_sovereignty_scaffold
 *   human_readable: The Decentralized Parallel
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Decentralized Parallel is a framework of crypto-networks and
 *   peer-to-peer protocols intended to provide coordination and governance
 *   outside traditional state structures. It is marketed and designed as a
 *   'Scaffold'—a temporary, enabling technology allowing populations to
 *   migrate away from failing institutional 'Snares.' However, its internal
 *   structure is characterized by significant risk, performative rhetoric,
 *   and mechanisms for value extraction, leading to a wide perspectival gap.
 *
 * KEY AGENTS:
 *   - Protocol Developers: The builders of the system (organized/mobile), who see it as a Scaffold.
 *   - Early Adopters & VCs: Primary financial beneficiaries (powerful/arbitrage), who see it as a Rope for capital coordination.
 *   - Users in Failed States: Primary users of the 'sovereignty' function (powerless/mobile), who experience it as a risky but necessary Tangled Rope.
 *   - Exploited Retail Users: Victims of scams and volatility (powerless/trapped), who experience it as a Snare.
 *   - Incumbent Nation-States: The established order being challenged (institutional/constrained), which sees it as a rival Tangled Rope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alternative_sovereignty_scaffold, 0.4).
domain_priors:suppression_score(alternative_sovereignty_scaffold, 0.5).
domain_priors:theater_ratio(alternative_sovereignty_scaffold, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, extractiveness, 0.4).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(alternative_sovereignty_scaffold, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alternative_sovereignty_scaffold, tangled_rope).
narrative_ontology:human_readable(alternative_sovereignty_scaffold, "The Decentralized Parallel").
narrative_ontology:topic_domain(alternative_sovereignty_scaffold, "technological/social").

domain_priors:requires_active_enforcement(alternative_sovereignty_scaffold).
narrative_ontology:has_sunset_clause(alternative_sovereignty_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, protocol_developers).
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, early_adopters_and_vcs).
narrative_ontology:constraint_beneficiary(alternative_sovereignty_scaffold, users_in_failed_states).
narrative_ontology:constraint_victim(alternative_sovereignty_scaffold, incumbent_nation_states).
narrative_ontology:constraint_victim(alternative_sovereignty_scaffold, exploited_retail_users).
narrative_ontology:constraint_victim(alternative_sovereignty_scaffold, global_financial_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPLOITED RETAIL USER (SNARE) — Lured by the promise of high returns, their capital is extracted through scams, hacks, or protocol failures with no recourse. For them, the system is pure extraction. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.68.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIGRANT IN FAILED STATE (TANGLED ROPE) — The system offers a genuine coordination benefit (capital preservation, exit), but at the cost of high volatility, fees, and risk. It is both a tool and a danger. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.55.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: PROTOCOL DEVELOPER (SCAFFOLD) — As the builders, they see the system as a temporary structure to bootstrap a new form of sovereignty. They acknowledge the risks and extraction as necessary evils on the path to a stable, decentralized future (the sunset clause). d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.19.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EARLY ADOPTER / VC (ROPE) — Experiences the system as a pure coordination mechanism for allocating capital and governing new protocols. The 'extraction' is perceived as legitimate investment returns. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.005.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INCUMBENT NATION-STATE (TANGLED ROPE) — Views the system as a challenge to its monetary and legal sovereignty. It recognizes some coordination benefits (innovation) but primarily sees an extractive threat (tax evasion, capital flight) that requires active enforcement (regulation) to contain. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The system's structure combines a genuine coordination function (enabling action outside state channels) with significant asymmetric extraction (VC allocations, scams, fees). Despite its marketing as a Scaffold, its metrics (ε=0.40, suppression=0.50) place it firmly in the Tangled Rope category. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.55.
constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alternative_sovereignty_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alternative_sovereignty_scaffold, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(alternative_sovereignty_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.40): Moderate. While claiming to be non-extractive, the system facilitates significant value transfer through gas fees, VC token allocations, MEV, scams, and protocol-level inflation. Suppression (0.50): Moderate. The system doesn't suppress the state, but within its ecosystem, network effects and technical complexity create high barriers to entry and suppress non-participation for those who need an exit. Theater Ratio (0.65): High. The rhetoric of 'decentralization' and 'trustlessness' often masks centralized points of failure and governance capture by capital, making much of the discourse performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The system's builders (developers) genuinely see a Scaffold, a temporary tool for liberation. Its financiers (VCs) see a clean Rope for coordinating investment. Its most desperate users (migrants) see a Tangled Rope, a lifeline that is also a source of risk. Its most naive users (retail victims) fall into a Snare. The incumbent power (the state) sees a rival Tangled Rope. The analytical classification of Tangled Rope reflects the objective reality that the system has both a genuine coordination function and a significant, asymmetric extractive function.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries like VCs have arbitrage exit, driving their directionality `d` near zero and classifying the system as a Rope. Victims like scammed users are trapped, maximizing `d` and pushing the effective extraction `χ` into the Snare category. Agents with mixed incentives and partial exit, like migrants and nation-states, perceive it as a Tangled Rope. The developers, as organized agents who believe in the mission, have a low `d` that allows them to perceive the system as the Scaffold they intend it to be.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a key mandatrophy: a system's claimed or intended function (Scaffold) can be structurally different from its objective classification (Tangled Rope). The framework does not force a single 'correct' type. Instead, it models how the developers' Scaffold perspective is a valid, indexical truth for them, while the retail victim's Snare perspective is an equally valid truth for their position. The analytical classification of Tangled Rope serves as the objective baseline, acknowledging that the structure contains both the coordination function the developers build and the extractive function the victims experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_clause_viability,
    'Is the ''sunset clause'' a structural reality or merely aspirational marketing?',
    'Longitudinal analysis of protocol governance and token distributions to see if they trend towards broader, more stable utility or remain captured by early investors and speculative cycles.',
    'If viable, the Scaffold perspective is strengthened. If aspirational, the system is a permanent Tangled Rope or Snare, and the ''scaffold'' claim is theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_viability, empirical, 'Whether the system can ever transition from speculative scaffold to stable infrastructure.').

omega_variable(
    governance_capture,
    'Can decentralized governance models resist capture by concentrated token holders (''whales'') and VCs?',
    'Analysis of voting patterns and proposal outcomes in major DAOs, correlating token concentration with decision-making power.',
    'If capture is inevitable, the ''decentralized'' claim is theatrical (high theater_ratio) and the system functions as a Snare for non-elites. If it can be resisted, the Rope/Scaffold classifications are more credible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governance_capture, empirical, 'The inevitability of governance capture by capital concentration.').

omega_variable(
    state_symbiosis,
    'Is the Decentralized Parallel a true alternative to the state, or a parasitic system reliant on state-provided infrastructure (internet, property rights, legal recourse for exchanges)?',
    'Mapping the dependencies of crypto networks on legacy infrastructure and legal systems.',
    'If fully dependent, it cannot be a true ''alternative sovereignty'' and is better modeled as a Tangled Rope within the existing system. If it can achieve infrastructural independence, the Scaffold-to-Rope thesis is more plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_symbiosis, conceptual, 'Whether the system is a true alternative or merely symbiotic with the state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alternative_sovereignty_scaffold, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alte_tr_t2015, alternative_sovereignty_scaffold, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(alte_tr_t2021, alternative_sovereignty_scaffold, theater_ratio, 2021, 0.7).
narrative_ontology:measurement(alte_tr_t2024, alternative_sovereignty_scaffold, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(alte_be_t2015, alternative_sovereignty_scaffold, base_extractiveness, 2015, 0.2).
narrative_ontology:measurement(alte_be_t2021, alternative_sovereignty_scaffold, base_extractiveness, 2021, 0.45).
narrative_ontology:measurement(alte_be_t2024, alternative_sovereignty_scaffold, base_extractiveness, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alternative_sovereignty_scaffold, global_infrastructure).
narrative_ontology:affects_constraint(alternative_sovereignty_scaffold, failing_state_snare).
narrative_ontology:affects_constraint(alternative_sovereignty_scaffold, fiat_currency_debasement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
