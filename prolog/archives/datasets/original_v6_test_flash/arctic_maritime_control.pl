% ============================================================================
% CONSTRAINT STORY: arctic_maritime_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_maritime_control, []).

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
 *   constraint_id: arctic_maritime_control
 *   human_readable: Arctic Maritime Control Regime
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The Arctic Maritime Control Regime represents the set of international
 *   laws, treaties, and national claims that govern access to and resource
 *   extraction from the Arctic Ocean and its surrounding territories. The
 *   regime seeks to balance resource exploitation with environmental
 *   protection and the rights of Arctic communities, but faces challenges
 *   from climate change, geopolitical competition, and uneven distribution of
 *   benefits. This regime creates a situation of mixed coordination and
 *   extraction.
 *
 * KEY AGENTS:
 *   - Arctic Council Nations: Primary beneficiaries (institutional/arbitrage) - control access and resources.
 *   - Non-Arctic Nations: Primary victims (powerless/trapped) - excluded from decision-making.
 *   - Indigenous Arctic Communities: Secondary victims (moderate/constrained) - face environmental and cultural disruption, benefit partially from regulated extraction.
 *   - Shipping Companies: Utilize the new Arctic shipping routes (powerful/mobile).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_maritime_control, 0.55).
domain_priors:suppression_score(arctic_maritime_control, 0.4).
domain_priors:theater_ratio(arctic_maritime_control, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_maritime_control, extractiveness, 0.55).
narrative_ontology:constraint_metric(arctic_maritime_control, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(arctic_maritime_control, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_maritime_control, tangled_rope).
narrative_ontology:human_readable(arctic_maritime_control, "Arctic Maritime Control Regime").
narrative_ontology:topic_domain(arctic_maritime_control, "geopolitical").

domain_priors:requires_active_enforcement(arctic_maritime_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_maritime_control, arctic_council_nations).
narrative_ontology:constraint_beneficiary(arctic_maritime_control, shipping_companies).
narrative_ontology:constraint_victim(arctic_maritime_control, non_arctic_nations).
narrative_ontology:constraint_victim(arctic_maritime_control, indigenous_arctic_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Non-Arctic nations are largely excluded from decision-making processes and resource exploitation, and are becoming increasingly trapped as climate change opens up the Arctic for resource extraction by other countries.
constraint_indexing:constraint_classification(arctic_maritime_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Indigenous communities face constrained mobility and limited influence, yet benefit somewhat from increased infrastructure development and regulated resource management, but they bear the extraction costs of environmental damage and cultural disruption.
constraint_indexing:constraint_classification(arctic_maritime_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Arctic Council nations benefit from the established legal framework and resource exploitation opportunities, and can arbitrage within the legal framework to their advantage.
constraint_indexing:constraint_classification(arctic_maritime_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Shipping companies benefit from new Arctic shipping routes, which reduce transit times. However, they bear the costs of complying with the Polar Code and other regulations.
constraint_indexing:constraint_classification(arctic_maritime_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% An analytical observer sees the Arctic Maritime Control Regime as a tangled rope, balancing coordination (resource management, safety regulations) with extraction (unequal access, environmental risks).
constraint_indexing:constraint_classification(arctic_maritime_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_maritime_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_maritime_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_maritime_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_maritime_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(arctic_maritime_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-High. The regime enables resource extraction and shipping, which can lead to environmental damage and cultural disruption for indigenous communities. Non-Arctic nations are excluded from access and benefit. Suppression (0.40): Moderate. The regime limits access and resource exploitation by non-Arctic nations and imposes regulations on shipping, but it is not completely restrictive. Theater ratio (0.30): Low. The regime does have performative elements, such as symbolic gestures toward environmental protection and indigenous rights, but also has functional coordination like resource management and safety regulations.
 *
 * PERSPECTIVAL GAP:
 *   The regime is viewed differently depending on the observer's position. Arctic nations see coordination and benefit, while non-Arctic nations see exclusion. Indigenous communities experience both benefits and costs. This perspectival gap shows it is a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the power and exit options of each agent. Arctic nations have high power and the ability to arbitrage, leading to a low effective extraction. Non-Arctic nations have low power and are trapped, leading to high effective extraction. Shipping companies have a moderate power with mobility, while indigenous communities have constrained mobility with some power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_change_impact,
    'How will accelerated climate change and ice melt affect the enforceability and equity of the Arctic Maritime Control Regime?',
    'Monitoring sea ice extent, tracking shipping activity, and assessing environmental impact assessments.',
    'Greater accessibility could undermine existing regulations and intensify resource competition, potentially shifting the regime towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_change_impact, empirical, 'Impact of climate change on regime effectiveness').

omega_variable(
    resource_distribution_equity,
    'To what extent are the benefits of resource extraction distributed equitably among Arctic nations, indigenous communities, and the global community?',
    'Analyzing revenue sharing agreements, assessing community development initiatives, and evaluating environmental protection measures.',
    'If benefits are concentrated, the regime will be perceived as a snare by non-beneficiaries. More equitable distribution would reinforce the rope elements of the regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_distribution_equity, preference, 'Equity of resource distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_maritime_control, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arct_tr_t0, arctic_maritime_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arct_tr_t10, arctic_maritime_control, theater_ratio, 10, 0.25).
narrative_ontology:measurement(arct_tr_t20, arctic_maritime_control, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(arct_be_t0, arctic_maritime_control, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(arct_be_t10, arctic_maritime_control, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(arct_be_t20, arctic_maritime_control, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_maritime_control, resource_allocation).
narrative_ontology:affects_constraint(arctic_maritime_control, antarctic_treaty_system).
narrative_ontology:affects_constraint(arctic_maritime_control, exclusive_economic_zones).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
