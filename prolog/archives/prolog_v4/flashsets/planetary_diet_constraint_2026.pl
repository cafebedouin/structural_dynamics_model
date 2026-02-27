% ============================================================================
% CONSTRAINT STORY: planetary_diet_constraint_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planetary_diet_constraint_2026, []).

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
 *   constraint_id: planetary_diet_constraint_2026
 *   human_readable: Planetary Boundary Dietary Alignment
 *   domain: ecological/economic/social
 *
 * SUMMARY:
 *   The "Planetary Boundary Dietary Alignment" constraint aims to shift
 *   global diets towards patterns that reduce environmental impact and
 *   promote sustainability. This involves reducing meat consumption,
 *   increasing plant-based food intake, and promoting food production
 *   practices that minimize resource depletion and greenhouse gas emissions.
 *   The constraint balances global ecological needs with individual consumer
 *   preferences and economic interests of food producers.
 *
 * KEY AGENTS:
 *   - Global Ecosystem Health: Primary beneficiary (institutional/arbitrage) - benefits from reduced environmental impact.
 *   - Future Generations: Secondary beneficiary (institutional/analytical) - benefits from a more sustainable planet.
 *   - Meat Producers: Primary victim (powerless/trapped) - faces economic hardship due to reduced demand.
 *   - Consumers Unwilling to Change Diet: Secondary victim (powerless/trapped) - faces limited food choices and increased costs.
 *   - Plant-Based Food Industry: Organized agent (organized/mobile) - benefits in the short term with a sunset clause if dietary trend shifts.
 *   - Average Global Citizen: The average global citizen is both constrained and benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planetary_diet_constraint_2026, 0.55).
domain_priors:suppression_score(planetary_diet_constraint_2026, 0.6).
domain_priors:theater_ratio(planetary_diet_constraint_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planetary_diet_constraint_2026, tangled_rope).
narrative_ontology:human_readable(planetary_diet_constraint_2026, "Planetary Boundary Dietary Alignment").
narrative_ontology:topic_domain(planetary_diet_constraint_2026, "ecological/economic/social").

domain_priors:requires_active_enforcement(planetary_diet_constraint_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, global_ecosystem_health).
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, future_generations).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, meat_producers).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, consumers_unwilling_to_change_diet).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The consumer who is unwilling or unable to change their diet due to habit, taste, or affordability is trapped by the constraint.  They experience extraction as limited food choices and increased costs.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Local meat producers face immediate economic hardship due to decreased demand for their products, without viable alternatives for income (trapped).
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% The average global citizen is both constrained (by limited options and social pressure) and benefits (from a healthier planet).  They have limited ability to arbitrage, but are also not totally trapped.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The constraint promotes the long-term health of the global ecosystem.  The ecosystem has an 'arbitrage' option in that it will eventually find a new equilibrium, but benefits from dietary changes that accelerate healing.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The plant-based food industry benefits in the short term but may lose relevance if future dietary trends shift away from current plant-based alternatives. Therefore, the plant-based food industry is a scaffold, offering temporary support for the shift to planetary diets.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the constraint as a tangled rope, balancing planetary health with individual freedoms and economic interests. The observer recognizes the complexities and trade-offs involved in promoting planetary diets.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_diet_constraint_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planetary_diet_constraint_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(planetary_diet_constraint_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Moderate-high. The constraint extracts from meat producers and consumers unwilling to change their diets. It imposes costs in terms of economic disruption and limited food choices. Suppression: Moderate-high. There is significant social and economic pressure to adopt planetary diets. Theater Ratio: Low. The focus is on real dietary changes, rather than performative actions. The claimed type is Tangled Rope because the constraint combines the coordination function of achieving planetary health with the asymmetric extraction from specific groups.
 *
 * PERSPECTIVAL GAP:
 *   The unwilling consumer and meat producers see a Snare, as they bear the brunt of the costs. The global ecosystem benefits (Rope), while plant-based food industry see it as a scaffold. The average global citizen faces a tangled rope and the analytical observer also sees tangled rope with the complexities and trade-offs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's relationship to the constraint. Beneficiaries (global ecosystem) experience negative directionality. Victims (meat producers, unwilling consumers) experience positive directionality. The analytical observer sees a balanced view. The derived D values affect the perceived extractiveness (chi).
 *
 * MANDATROPHY ANALYSIS:
 *   This analysis identifies the classification for the planetary diet as a Tangled Rope due to active enforcement and having both beneficiaries and victims. The classification resolves the mandatrophy with evidence and rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_willingness_to_change,
    'To what extent are consumers willing to alter their diets for environmental reasons?',
    'Surveys, market research, and behavioral studies to assess consumer preferences and adoption rates of planetary diets.',
    'High willingness: smoother transition, less extraction. Low willingness: requires stronger interventions, increased extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_willingness_to_change, empirical, 'Consumer willingness to change diet').

omega_variable(
    meat_producer_adaptation,
    'Can meat producers successfully adapt to producing plant-based alternatives or sustainable meat products?',
    'Track meat producer investments in plant-based alternatives or regenerative agriculture practices.',
    'Successful adaptation: less economic disruption, reduced extraction. Limited adaptation: greater economic hardship, increased extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meat_producer_adaptation, empirical, 'Meat producer adaptation to plant-based alternatives').

omega_variable(
    dietary_shift_equity,
    'How can the dietary shift be implemented equitably across different socioeconomic groups and regions?',
    'Analyze policy proposals for ensuring affordability and access to sustainable diets for all.',
    'Equitable implementation: reduced social inequality, increased coordination. Inequitable implementation: exacerbates inequalities, increased extraction from vulnerable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dietary_shift_equity, preference, 'Equitable dietary shift across socioeconomic groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planetary_diet_constraint_2026, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plan_tr_t0, planetary_diet_constraint_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(plan_tr_t15, planetary_diet_constraint_2026, theater_ratio, 15, 0.3).
narrative_ontology:measurement(plan_tr_t30, planetary_diet_constraint_2026, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(plan_be_t0, planetary_diet_constraint_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(plan_be_t15, planetary_diet_constraint_2026, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(plan_be_t30, planetary_diet_constraint_2026, base_extractiveness, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planetary_diet_constraint_2026, resource_allocation).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, sustainable_agriculture_practices).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, global_climate_agreement).

% DUAL FORMULATION NOTE:
% The Planetary Boundary Dietary Alignment constraint is linked to Sustainable Agriculture Practices and the Global Climate Agreement as it contributes to achieving their objectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
