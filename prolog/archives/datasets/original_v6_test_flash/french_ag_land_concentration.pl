% ============================================================================
% CONSTRAINT STORY: french_ag_land_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_ag_land_concentration, []).

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
 *   constraint_id: french_ag_land_concentration
 *   human_readable: French Agricultural Land Concentration
 *   domain: economic
 *
 * SUMMARY:
 *   Based on INSEE data, this constraint models the decades-long trend of
 *   agricultural land consolidation in France. It captures the shift of land
 *   ownership from small farmers to larger agricultural corporations, and its
 *   impacts on rural communities, agricultural practices, and biodiversity.
 *
 * KEY AGENTS:
 *   - Small Farmers: Primary target (powerless/trapped) - experience displacement and loss of livelihood.
 *   - Large Agricultural Corporations: Primary beneficiary (institutional/arbitrage) - accumulate land and increase profits.
 *   - Rural Communities: Secondary target (moderate/constrained) - face economic and social changes due to land concentration.
 *   - Agribusiness Sector: Secondary beneficiary (powerful/arbitrage) - benefits from increased scale of production and efficiency.
 *   - Agricultural Biodiversity: Primary target (powerless/trapped) - affected by monoculture practices and loss of traditional farming methods.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_ag_land_concentration, 0.6).
domain_priors:suppression_score(french_ag_land_concentration, 0.7).
domain_priors:theater_ratio(french_ag_land_concentration, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_ag_land_concentration, extractiveness, 0.6).
narrative_ontology:constraint_metric(french_ag_land_concentration, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(french_ag_land_concentration, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_ag_land_concentration, tangled_rope).
narrative_ontology:human_readable(french_ag_land_concentration, "French Agricultural Land Concentration").
narrative_ontology:topic_domain(french_ag_land_concentration, "economic").

domain_priors:requires_active_enforcement(french_ag_land_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, large_agricultural_corporations).
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, agribusiness_sector).
narrative_ontology:constraint_victim(french_ag_land_concentration, small_farmers).
narrative_ontology:constraint_victim(french_ag_land_concentration, rural_communities).
narrative_ontology:constraint_victim(french_ag_land_concentration, agricultural_biodiversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of small farmers who are increasingly trapped due to rising land prices, economies of scale favoring large farms, and limited access to capital.
constraint_indexing:constraint_classification(french_ag_land_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of large agricultural corporations that benefit from economies of scale, access to capital, and government subsidies, enabling them to acquire more land and increase productivity.
constraint_indexing:constraint_classification(french_ag_land_concentration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the land concentration represents a tangled rope due to its complex and contradictory effects. There are benefits such as increased agricultural output and efficiency, but also costs such as displacement of small farmers and environmental degradation.
constraint_indexing:constraint_classification(french_ag_land_concentration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective of rural communities that are constrained by the economic and social changes associated with agricultural land concentration. While some benefit from new jobs created by larger farms, others suffer from the loss of small farms and the decline of rural economies.
constraint_indexing:constraint_classification(french_ag_land_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_ag_land_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_ag_land_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_ag_land_concentration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_ag_land_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(french_ag_land_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score is based on the transfer of wealth and control from small farmers to large corporations. Suppression relates to the limited options available to small farmers, who face economic pressures and regulatory constraints. Theater ratio reflects that some government policies aimed at supporting small farmers have limited effectiveness, acting as performative measures.
 *
 * PERSPECTIVAL GAP:
 *   Small farmers perceive this as a snare, trapped by rising land prices and competition. Large corporations see it as a rope, facilitating efficient production. Rural communities experience it as a tangled rope, with mixed benefits and drawbacks. The analytical observer recognizes the inherent trade-offs and potential for negative externalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Large agricultural corporations and agribusiness are beneficiaries, given access to capital and arbitrage options, their directionality is low. Small farmers are victims, constrained and trapped, their directionality is high. Rural communities are moderately affected. The analytical observer takes a neutral stance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_social_efficiency,
    'What is the relative importance of economic efficiency versus social equity in assessing agricultural land concentration?',
    'Cost-benefit analysis that considers both economic and social impacts of agricultural land concentration.',
    'Different policy interventions are needed depending on the relative importance of economic efficiency versus social equity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_social_efficiency, preference, 'Relative importance of economic versus social factors').

omega_variable(
    environmental_impact_assessment,
    'What is the long-term environmental impact of agricultural land concentration?',
    'Environmental impact assessment that considers factors such as soil degradation, water pollution, and biodiversity loss.',
    'Different policy interventions are needed depending on the severity of the environmental impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_impact_assessment, empirical, 'Environmental impact assessment').

omega_variable(
    policy_intervention_effectiveness,
    'What policy interventions are most effective in mitigating the negative impacts of agricultural land concentration?',
    'Comparative analysis of different policy interventions implemented in France and other countries.',
    'Selection of appropriate policy interventions to address the negative impacts of agricultural land concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intervention_effectiveness, empirical, 'Policy intervention effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_ag_land_concentration, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fren_tr_t0, french_ag_land_concentration, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fren_tr_t20, french_ag_land_concentration, theater_ratio, 20, 0.3).
narrative_ontology:measurement(fren_tr_t40, french_ag_land_concentration, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(fren_be_t0, french_ag_land_concentration, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fren_be_t20, french_ag_land_concentration, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(fren_be_t40, french_ag_land_concentration, base_extractiveness, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_ag_land_concentration, resource_allocation).
narrative_ontology:affects_constraint(french_ag_land_concentration, european_agricultural_policy).
narrative_ontology:affects_constraint(french_ag_land_concentration, global_food_prices).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
