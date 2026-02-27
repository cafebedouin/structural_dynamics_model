% ============================================================================
% CONSTRAINT STORY: project_vault_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_project_vault_2026, []).

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
 *   constraint_id: project_vault_2026
 *   human_readable: Project Vault Strategic Mineral Reserve
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Project Vault is a strategic mineral reserve designed to protect the US
 *   economy from supply chain disruptions. It aims to ensure a reliable
 *   supply of critical minerals for defense and industry, but it also
 *   introduces potential distortions into the global mineral market,
 *   affecting both domestic and foreign actors. The reserve represents a
 *   complex interplay of coordination and extraction.
 *
 * KEY AGENTS:
 *   - Domestic Mining Companies: Primary beneficiaries (institutional/arbitrage) — gain guaranteed market and potentially higher prices.
 *   - Defense Industry: Beneficiary (institutional/arbitrage) - access secured supply.
 *   - Foreign Mineral Exporters: Primary victims (powerless/trapped) — lose market share and pricing power.
 *   - Downstream Consumers: Victims (moderate/constrained) — potentially higher prices but also benefit from enhanced security of supply.
 *   - Analytical Observer: Assesses the overall balance between coordination and extraction (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(project_vault_2026, 0.55).
domain_priors:suppression_score(project_vault_2026, 0.45).
domain_priors:theater_ratio(project_vault_2026, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(project_vault_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(project_vault_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(project_vault_2026, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(project_vault_2026, tangled_rope).
narrative_ontology:human_readable(project_vault_2026, "Project Vault Strategic Mineral Reserve").
narrative_ontology:topic_domain(project_vault_2026, "economic/geopolitical").

domain_priors:requires_active_enforcement(project_vault_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(project_vault_2026, domestic_mining_companies).
narrative_ontology:constraint_beneficiary(project_vault_2026, defense_industry).
narrative_ontology:constraint_victim(project_vault_2026, foreign_mineral_exporters).
narrative_ontology:constraint_victim(project_vault_2026, downstream_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Foreign mineral exporters, particularly those in countries with less favorable trade relations, are trapped as they lose market share and pricing power. They have limited options to exit this constraint due to geopolitical dependencies.
constraint_indexing:constraint_classification(project_vault_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Downstream consumers face potentially higher prices due to reduced competition but also benefit from the enhanced security of supply. Their exit options are constrained by the limited availability of alternatives.
constraint_indexing:constraint_classification(project_vault_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Domestic mining companies are primary beneficiaries. They gain a guaranteed market and potentially higher prices. They can arbitrage the situation to maximize profits within the parameters of the reserve.
constraint_indexing:constraint_classification(project_vault_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The defense industry benefits from a secure and reliable supply of critical minerals, enabling them to fulfill production requirements without geopolitical supply chain risks. The arbitrage consists of the guaranteed availability.
constraint_indexing:constraint_classification(project_vault_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees a tangled rope. The mineral reserve aims to secure the supply chain (coordination) but also creates market distortions and potential for rent-seeking (extraction). The balance between these two aspects determines its overall impact.
constraint_indexing:constraint_classification(project_vault_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(project_vault_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(project_vault_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(project_vault_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Project Vault extracts from foreign exporters by limiting their access to the U.S. market. It also extracts from domestic consumers who may face higher prices. The extractiveness is mitigated by the potential for increased domestic production and greater supply chain security. Suppression (0.45): Moderate. The reserve suppresses competition from foreign suppliers and limits the flexibility of downstream consumers to choose alternative sources. However, it does not completely eliminate these options. Theater ratio (0.20): Low. Project Vault is primarily driven by strategic and economic considerations, not by public relations or performative gestures. The focus is on building a tangible reserve of minerals.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives highlight the different experiences of actors in relation to Project Vault. Domestic mining companies and the defense industry benefit from increased security and market access, while foreign exporters face reduced opportunities. Downstream consumers experience a trade-off between higher prices and greater supply reliability. The analytical observer sees a complex situation with both coordination and extraction aspects. The divergent exit options drive the differing classifications. Foreign mineral exporters are structurally trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values (d) are derived from the agent's power, exit options, and relationship to the extraction flow. Domestic mining companies, with high power and arbitrage options, experience low extraction. Foreign mineral exporters, with low power and trapped exit options, experience high extraction. Downstream consumers, with moderate power and constrained exit options, experience moderate extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_retaliation,
    'Will other nations retaliate with their own mineral reserve strategies, disrupting global trade?',
    'Track policy changes and trade flows among key mineral-producing nations.',
    'If retaliation occurs, Project Vault''s effectiveness could be undermined, leading to a snare for the US economy. If not, it remains a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_retaliation, empirical, 'The likelihood and impact of geopolitical retaliation.').

omega_variable(
    market_distortion,
    'To what extent will Project Vault distort mineral markets and create opportunities for rent-seeking?',
    'Monitor mineral prices, production levels, and industry profits.',
    'High distortion could shift the classification towards a snare, as benefits are concentrated among a few, while costs are borne by many. Low distortion maintains the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_distortion, empirical, 'The degree of market distortion caused by the reserve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_vault_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proj_tr_t0, project_vault_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(proj_tr_t5, project_vault_2026, theater_ratio, 5, 0.15).
narrative_ontology:measurement(proj_tr_t10, project_vault_2026, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(proj_be_t0, project_vault_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(proj_be_t5, project_vault_2026, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(proj_be_t10, project_vault_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(project_vault_2026, resource_allocation).
narrative_ontology:affects_constraint(project_vault_2026, global_mineral_supply_chains).
narrative_ontology:affects_constraint(project_vault_2026, rare_earth_dependency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
