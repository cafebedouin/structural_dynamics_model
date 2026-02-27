% ============================================================================
% CONSTRAINT STORY: great_mongolian_road_economic_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_great_mongolian_road_economic_dependency, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: great_mongolian_road_economic_dependency
 *   human_readable: Economic Dependency via Infrastructure Development
 *   domain: economic
 *
 * SUMMARY:
 *   The Great Mongolian Road project, spearheaded by Japan, aims to improve
 *   Mongolia's infrastructure. However, concerns exist regarding Mongolia's
 *   increasing economic dependence on Japan due to loans and potential
 *   exploitation of resources. The project presents a complex dynamic of
 *   coordination and extraction.
 *
 * KEY AGENTS:
 *   - Japanese Construction Companies: Primary beneficiary (institutional/arbitrage)
 *   - Mongolian Domestic Industries: Primary victim (powerless/trapped)
 *   - Mongolian Government: Moderate actor (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(great_mongolian_road_economic_dependency, 0.6).
domain_priors:suppression_score(great_mongolian_road_economic_dependency, 0.5).
domain_priors:theater_ratio(great_mongolian_road_economic_dependency, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, extractiveness, 0.6).
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(great_mongolian_road_economic_dependency, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(great_mongolian_road_economic_dependency, tangled_rope).
narrative_ontology:human_readable(great_mongolian_road_economic_dependency, "Economic Dependency via Infrastructure Development").
narrative_ontology:topic_domain(great_mongolian_road_economic_dependency, "economic").

domain_priors:requires_active_enforcement(great_mongolian_road_economic_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(great_mongolian_road_economic_dependency, japanese_construction_companies).
narrative_ontology:constraint_beneficiary(great_mongolian_road_economic_dependency, japanese_export_sector).
narrative_ontology:constraint_victim(great_mongolian_road_economic_dependency, mongolian_domestic_industries).
narrative_ontology:constraint_victim(great_mongolian_road_economic_dependency, mongolian_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Mongolian domestic industries are trapped due to increased competition from foreign companies and reliance on foreign loans, facing a snare.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The Mongolian government is constrained by the need for infrastructure development but benefits from improved infrastructure, leading to a tangled rope scenario.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Japanese construction companies benefit from the project through increased revenue and market share, experiencing a rope dynamic.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees the situation as a tangled rope, with both coordination and extraction aspects evident.
constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(great_mongolian_road_economic_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(great_mongolian_road_economic_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(great_mongolian_road_economic_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(great_mongolian_road_economic_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.60) due to Mongolia's reliance on foreign loans and potential exploitation of resources. Suppression is moderate (0.50) because alternative development options are limited. The theater ratio is low (0.30) since there are actual infrastructure developments being made.
 *
 * PERSPECTIVAL GAP:
 *   Japanese companies see a rope due to benefits, while Mongolian industries face a snare. The Mongolian government experiences a tangled rope due to a mixed impact. The analytical observer sees both aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position. Japanese companies benefit, Mongolian industries are harmed, and the Mongolian government faces a mixed situation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_economic_impact,
    'What will be the long-term economic impact of the infrastructure development on Mongolia''s economy and sovereignty?',
    'Economic modeling and geopolitical analysis.',
    'Determine if Mongolia''s economy will be strengthened or further dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_economic_impact, empirical, 'Long-term economic impact on Mongolia''s economy').

omega_variable(
    debt_sustainability,
    'Is Mongolia''s debt sustainable given the loans taken for the infrastructure projects?',
    'Debt sustainability analysis and monitoring of economic growth.',
    'Assess the risk of debt distress for Mongolia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_sustainability, empirical, 'Assessment of Mongolia''s debt sustainability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(great_mongolian_road_economic_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grea_tr_t0, great_mongolian_road_economic_dependency, theater_ratio, 0, 0.2).
narrative_ontology:measurement(grea_tr_t5, great_mongolian_road_economic_dependency, theater_ratio, 5, 0.3).
narrative_ontology:measurement(grea_tr_t10, great_mongolian_road_economic_dependency, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(grea_be_t0, great_mongolian_road_economic_dependency, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(grea_be_t5, great_mongolian_road_economic_dependency, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(grea_be_t10, great_mongolian_road_economic_dependency, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(great_mongolian_road_economic_dependency, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
