% ============================================================================
% CONSTRAINT STORY: rare_earth_seabed_mining
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_seabed_mining, []).

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
 *   constraint_id: rare_earth_seabed_mining
 *   human_readable: Deep-Sea Rare Earth Mining
 *   domain: economic/technological/environmental
 *
 * SUMMARY:
 *   Deep-sea rare earth mining presents a complex interplay between economic
 *   benefits and environmental costs. Japan's deep-sea test near
 *   Minamitorishima Island underscores the growing interest in exploiting
 *   seabed resources. This activity is driven by the need for rare earth
 *   elements, essential for various technologies, but carries significant
 *   risks to poorly understood deep-sea ecosystems. The constraint is
 *   actively enforced through the pursuit of profit by mining companies and
 *   incentivized by the demand for resources.
 *
 * KEY AGENTS:
 *   - Mining Companies: Primary beneficiary (institutional/arbitrage) - They gain access to resources and profit from extraction.
 *   - Deep Sea Ecosystems: Primary victim (powerless/trapped) - They are vulnerable to irreversible destruction with no exit.
 *   - Coastal Communities: Secondary actor (moderate/constrained) - They face potential for both economic benefits and ecological risks.
 *   - Rare Earth Consumers: Secondary beneficiary (moderate/mobile) - They benefit from the increased availability of resources.
 *   - Future Generations: Secondary victim (powerless/trapped) - Depleted resources and degraded environments limit their options.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_seabed_mining, 0.6).
domain_priors:suppression_score(rare_earth_seabed_mining, 0.7).
domain_priors:theater_ratio(rare_earth_seabed_mining, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_seabed_mining, extractiveness, 0.6).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_seabed_mining, tangled_rope).
narrative_ontology:human_readable(rare_earth_seabed_mining, "Deep-Sea Rare Earth Mining").
narrative_ontology:topic_domain(rare_earth_seabed_mining, "economic/technological/environmental").

domain_priors:requires_active_enforcement(rare_earth_seabed_mining).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, mining_companies).
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, rare_earth_consumers).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, deep_sea_ecosystems).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, future_generations).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, coastal_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Deep sea ecosystems are highly vulnerable and have no capacity to exit or resist the destructive impacts of seabed mining. The extraction is largely irreversible and harms a complex, poorly understood ecosystem.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Mining companies benefit from access to rare earth resources, which are vital for many technologies. They arbitrage regulatory loopholes and international waters to minimize extraction costs and maximize profits. Their extraction benefits consumer and investors.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Coastal communities may experience both benefits and costs. They may receive short-term economic gains through employment and infrastructure development, but face longer-term risks of environmental degradation, displacement, and resource depletion that constrained their future options.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Deep-sea rare earth mining presents a tangled rope. It provides access to important resources, but extracts heavily from the environment and future generations. The long-term ecological impacts and fairness of intergenerational resource allocation require careful analysis. It is an actively enforced constraint due to the incentives of mining companies and the demand for resources.
constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_seabed_mining_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_seabed_mining, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rare_earth_seabed_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Moderate-high. Mining extracts significant resources from the deep sea and can cause damage. Suppression (0.7): High. Strong enforcement through economic incentives. The barriers to exit are high due to limited alternative resources. Theater Ratio (0.3): Low. Current theater is low because much of activity is exploratory, but could increase as awareness and regulation grow.
 *
 * PERSPECTIVAL GAP:
 *   Mining companies view this as a rope, accessing needed resources through arbitrage. Deep sea ecosystems view this as a snare with no exit. Coastal communities see a tangled rope because there are both positive and negative impacts. The Analytical Observer recognizes the mixed nature of the impacts over the time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the beneficiary and victim relationship. Mining companies are able to arbitrage and are beneficiaries with a low directionality. The deep sea ecosystems are victims who cannot escape the consequences so have a high directionality. Coastal communities are both beneficiaries and victims and are more moderate. An analytical observer takes into account both aspects of the problem and assigns extractiveness based on this analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a tangled rope because mining provides benefits and harms, and requires active enforcement to occur. It is not simply extraction because there is utility to the output, and it is not pure coordination because there is a lot of extraction. The long-term sustainability of these practices is currently uncertain, as the benefits and harms have not been fully quantified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_ecological_impact,
    'What are the long-term ecological consequences of deep-sea mining?',
    'Conduct extensive long-term monitoring studies of deep-sea ecosystems before, during, and after mining operations. Implement adaptive management strategies.',
    'If impacts are severe and irreversible, the activity should be classified as a pure snare. If impacts are manageable, the activity can be a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_ecological_impact, empirical, 'Assesses the long-term ecological effects.').

omega_variable(
    resource_sustainability,
    'Are deep-sea rare earth resources used sustainably, or are they depleted rapidly without consideration for future generations?',
    'Implement strong resource management policies and promote the development of circular economy approaches to reduce demand for virgin materials.',
    'If resources are depleted rapidly, the activity is a snare. If resource use is sustainable, the activity is a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_sustainability, preference, 'Assesses the sustainability of resource use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_seabed_mining, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rare_tr_t0, rare_earth_seabed_mining, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rare_tr_t5, rare_earth_seabed_mining, theater_ratio, 5, 0.3).
narrative_ontology:measurement(rare_tr_t10, rare_earth_seabed_mining, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(rare_be_t0, rare_earth_seabed_mining, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(rare_be_t5, rare_earth_seabed_mining, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(rare_be_t10, rare_earth_seabed_mining, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_seabed_mining, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_seabed_mining, rare_earth_supply_chains).
narrative_ontology:affects_constraint(rare_earth_seabed_mining, deep_sea_conservation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
