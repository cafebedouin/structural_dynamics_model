% ============================================================================
% CONSTRAINT STORY: trump_epa_greenhouse_gas_reversal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_epa_greenhouse_gas_reversal, []).

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
 *   constraint_id: trump_epa_greenhouse_gas_reversal
 *   human_readable: Trump EPA Reversal of Greenhouse Gas Finding
 *   domain: political
 *
 * SUMMARY:
 *   The Trump administration's EPA reversed the 2009 finding that greenhouse
 *   gases endanger public health, weakening regulations on emissions. This
 *   decision benefited the fossil fuel industry and political supporters
 *   while harming vulnerable communities, future generations, and the climate
 *   science community. The reversal exemplifies a tangled rope, where
 *   short-term economic gains are prioritized over long-term environmental
 *   sustainability.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Industry: Primary beneficiary (institutional/arbitrage) - benefits from reduced regulation.
 *   - Political Supporters: Secondary beneficiary (moderate/mobile) - gains political capital.
 *   - Vulnerable Communities: Primary victim (moderate/constrained) - suffers health and environmental consequences.
 *   - Future Generations: Secondary victim (powerless/trapped) - will bear the long-term costs of climate change.
 *   - Climate Science Community: Victim (analytical/mobile) - undermines scientific findings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_epa_greenhouse_gas_reversal, 0.65).
domain_priors:suppression_score(trump_epa_greenhouse_gas_reversal, 0.7).
domain_priors:theater_ratio(trump_epa_greenhouse_gas_reversal, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, extractiveness, 0.65).
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_epa_greenhouse_gas_reversal, tangled_rope).
narrative_ontology:human_readable(trump_epa_greenhouse_gas_reversal, "Trump EPA Reversal of Greenhouse Gas Finding").
narrative_ontology:topic_domain(trump_epa_greenhouse_gas_reversal, "political").

domain_priors:requires_active_enforcement(trump_epa_greenhouse_gas_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_epa_greenhouse_gas_reversal, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(trump_epa_greenhouse_gas_reversal, political_supporters).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, vulnerable_communities).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, future_generations).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, climate_science_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations are trapped and powerless to reverse the effects of increased greenhouse gas emissions, bearing the full cost.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Vulnerable communities are constrained by existing infrastructure and political realities, making exit difficult. They experience both costs (health impacts, environmental degradation) and some benefits (economic activity from the fossil fuel industry).
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The fossil fuel industry benefits from reduced regulatory burdens and increased short-term profits, allowing them to arbitrage the regulatory environment.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The EPA, under the Trump administration, maintains the appearance of environmental protection while effectively weakening regulations. Its original mandate is degraded, leading to a piton classification.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees a tangled rope: short-term economic benefits for some are coupled with long-term environmental costs for all.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_epa_greenhouse_gas_reversal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_epa_greenhouse_gas_reversal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trump_epa_greenhouse_gas_reversal, TR),
    TR >= 0.70.

:- end_tests(trump_epa_greenhouse_gas_reversal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score is high (0.65) because the reversal allows for continued pollution and exploitation of resources, with significant long-term costs for the environment and public health. The suppression score is also high (0.70) because the decision weakens existing environmental protections and suppresses alternative energy development. The theater ratio is moderate (0.75) because the EPA still engages in some environmental protection activities, although their effectiveness is diminished.
 *
 * PERSPECTIVAL GAP:
 *   Future generations, with no power or exit options, experience the reversal as a pure snare. The fossil fuel industry sees it as a rope, enabling coordination and economic growth. Vulnerable communities, constrained by their circumstances, experience a tangled rope, with both benefits and costs. The EPA experiences this as a piton, as the institution has been degraded and is not fulfilling its intended goals. The analytical observer sees a tangled rope because the benefits for some are traded for costs for others.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the structural relationships between the agents and the constraint. The fossil fuel industry and political supporters benefit, resulting in low d values. Vulnerable communities and future generations are harmed, resulting in high d values. The EPA, due to its degraded function, has a moderate d value.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_economic_impact,
    'What is the long-term economic impact of weakening environmental regulations versus the short-term gains?',
    'Economic modeling that incorporates climate change impacts and mitigation costs.',
    'If long-term costs outweigh short-term gains, the reversal is a snare. If short-term gains are substantial and outweigh long-term costs (unlikely), it could be classified as a rope from some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_economic_impact, empirical, 'The long-term economic consequences of the reversal.').

omega_variable(
    political_influence_vs_scientific_consensus,
    'To what extent was the reversal driven by political influence rather than scientific consensus?',
    'Analysis of internal EPA documents and communications, as well as lobbying records.',
    'If primarily political, the suppression score increases and the classification tends towards a snare. If primarily based on scientific disagreement (unlikely given the established consensus), the extractiveness score decreases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_influence_vs_scientific_consensus, empirical, 'The degree to which the reversal was politically motivated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_epa_greenhouse_gas_reversal, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trum_tr_t0, trump_epa_greenhouse_gas_reversal, theater_ratio, 0, 0.5).
narrative_ontology:measurement(trum_tr_t2, trump_epa_greenhouse_gas_reversal, theater_ratio, 2, 0.7).
narrative_ontology:measurement(trum_tr_t4, trump_epa_greenhouse_gas_reversal, theater_ratio, 4, 0.75).

% Extraction over time
narrative_ontology:measurement(trum_be_t0, trump_epa_greenhouse_gas_reversal, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(trum_be_t2, trump_epa_greenhouse_gas_reversal, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(trum_be_t4, trump_epa_greenhouse_gas_reversal, base_extractiveness, 4, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_epa_greenhouse_gas_reversal, resource_allocation).
narrative_ontology:affects_constraint(trump_epa_greenhouse_gas_reversal, paris_agreement_withdrawal).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
