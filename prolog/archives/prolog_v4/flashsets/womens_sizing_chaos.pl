% ============================================================================
% CONSTRAINT STORY: womens_sizing_chaos
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_womens_sizing_chaos, []).

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
 *   constraint_id: womens_sizing_chaos
 *   human_readable: The chaotic and non-standardized system of US women's clothing sizes.
 *   domain: consumer_economics/retail
 *
 * SUMMARY:
 *   The US women's clothing sizing system is a complex and chaotic system
 *   characterized by a lack of standardization. This results in consumers
 *   facing frustration and wasted time, while manufacturers and sizing
 *   consultants benefit from the confusion by catering to specific body types
 *   and creating artificial scarcity. This also impacts the Body Positivity
 *   Movement's goals.
 *
 * KEY AGENTS:
 *   - Female Consumers: Primary target (powerless/trapped) - constantly facing inconsistent sizing
 *   - Clothing Manufacturers: Primary beneficiary (institutional/arbitrage) - benefit from the lack of standardization
 *   - Sizing Consultants: Secondary beneficiary (moderate/mobile) - provide sizing expertise and guidance to consumers
 *   - Body Positivity Movement: Secondary actor (organized/constrained) - advocating for standardized sizing and challenging current practices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(womens_sizing_chaos, 0.6).
domain_priors:suppression_score(womens_sizing_chaos, 0.7).
domain_priors:theater_ratio(womens_sizing_chaos, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(womens_sizing_chaos, extractiveness, 0.6).
narrative_ontology:constraint_metric(womens_sizing_chaos, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(womens_sizing_chaos, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(womens_sizing_chaos, tangled_rope).
narrative_ontology:human_readable(womens_sizing_chaos, "The chaotic and non-standardized system of US women's clothing sizes.").
narrative_ontology:topic_domain(womens_sizing_chaos, "consumer_economics/retail").

domain_priors:requires_active_enforcement(womens_sizing_chaos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(womens_sizing_chaos, clothing_manufacturers).
narrative_ontology:constraint_beneficiary(womens_sizing_chaos, sizing_consultants).
narrative_ontology:constraint_victim(womens_sizing_chaos, female_consumers).
narrative_ontology:constraint_victim(womens_sizing_chaos, body_positivity_movement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AVERAGE FEMALE CONSUMER (SNARE) - Trapped by the need to wear clothing, and constantly facing inconsistent sizing, leading to frustration and wasted time/money. Cannot exit the system.
constraint_indexing:constraint_classification(womens_sizing_chaos, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BODY POSITIVITY MOVEMENT (TANGLED ROPE) - Advocates for standardized sizing and challenges the industry's current practices. Constrained by the power of major corporations, but also benefits by raising awareness and pushing for change, occasionally resulting in updated sizing models for some clothing brands.
constraint_indexing:constraint_classification(womens_sizing_chaos, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLOTHING MANUFACTURERS (ROPE) - Benefit from the lack of standardization by creating artificial scarcity and catering to specific body types. Allows them to manage inventory and maximize profits. The sizing chaos acts as a coordinating mechanism that lets them target specific demographics.
constraint_indexing:constraint_classification(womens_sizing_chaos, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADITIONAL RETAILERS (PITON) - Once benefited from consumers having to physically go to stores to try on items, and could use sizing as a way of creating customer loyalty by offering consistent sizing within their stores. However, the rise of online retail has lessened the benefit of physical stores, and many retailers now rely on the same chaotic sizing systems as everyone else. The historical benefit is now degraded, but they are stuck due to system inertia.
constraint_indexing:constraint_classification(womens_sizing_chaos, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - Sees the entire system as a tangled mess of coordination and extraction, where manufacturers benefit at the expense of consumers. The lack of standardization serves as a barrier to entry for new businesses and perpetuates a system that is ultimately inefficient.
constraint_indexing:constraint_classification(womens_sizing_chaos, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(womens_sizing_chaos_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(womens_sizing_chaos, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(womens_sizing_chaos, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(womens_sizing_chaos, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(womens_sizing_chaos, TR),
    TR >= 0.70.

:- end_tests(womens_sizing_chaos_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Consumers face direct costs such as wasted time, money and emotional stress. Suppression (0.70): High. Limited alternatives for consumers to avoid sizing issues. Theater Ratio (0.30): The system is less about theatrical performance and more about the concrete challenges faced by consumers and the benefits to manufacturers.
 *
 * PERSPECTIVAL GAP:
 *   Consumers see a system of extraction, where they are constantly facing issues with finding clothes that fit. Manufacturers see it as coordination, as they can control their inventory. The Body Positivity Movement sees an opportunity to challenge this system.
 *
 * DIRECTIONALITY LOGIC:
 *   Clothing manufacturers benefit from the lack of standardization and can engage in price discrimination. Female consumers bear the costs in time, money, and emotional well-being due to inconsistent sizing practices.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by acknowledging that some coordination occurs, like controlling inventory, but the main effect is extraction from consumers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_awareness,
    'To what extent is the consumer aware of the sizing issue and its implications?',
    'Survey consumers and conduct market research.',
    'If consumers are highly aware, pressure may increase on companies to adopt standardized sizing. If low awareness, the current system can persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_awareness, empirical, 'Consumer awareness of sizing issues').

omega_variable(
    technological_feasibility,
    'How feasible is it to implement standardized sizing using current technology?',
    'Consult with sizing experts and apparel engineers.',
    'If highly feasible, the costs for switching to standardized sizing are low and manufacturers might be willing to adopt it. If not feasible, the system could persist due to technological limitations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_feasibility, empirical, 'Feasibility of standardized sizing using current tech').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(womens_sizing_chaos, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wome_tr_t0, womens_sizing_chaos, theater_ratio, 0, 0.2).
narrative_ontology:measurement(wome_tr_t10, womens_sizing_chaos, theater_ratio, 10, 0.3).
narrative_ontology:measurement(wome_tr_t20, womens_sizing_chaos, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(wome_be_t0, womens_sizing_chaos, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(wome_be_t10, womens_sizing_chaos, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(wome_be_t20, womens_sizing_chaos, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(womens_sizing_chaos, resource_allocation).
narrative_ontology:affects_constraint(womens_sizing_chaos, body_image_standards).
narrative_ontology:affects_constraint(womens_sizing_chaos, fast_fashion_waste).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
