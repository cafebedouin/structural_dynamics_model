% ============================================================================
% CONSTRAINT STORY: global_trade_externalities_neobiota
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_trade_externalities_neobiota, []).

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
 *   constraint_id: global_trade_externalities_neobiota
 *   human_readable: Global Trade's Externalization of Neobiota Costs
 *   domain: economic/environmental
 *
 * SUMMARY:
 *   The global trade system coordinates international commerce but fails to
 *   price in the massive ecological and economic costs of invasive alien
 *   species (neobiota), which are spread through its networks.
 *
 * KEY AGENTS:
 *   - Global Shipping Industry: Primary beneficiary (institutional/arbitrage) — benefits from unpriced externalities.
 *   - Importing Nations (Short Term): Secondary beneficiary (institutional/constrained) — benefits from cheaper goods in the short term.
 *   - Global Biodiversity: Primary victim (powerless/trapped) — trapped by the spread of neobiota.
 *   - Local Ecosystems: Secondary victim (moderate/constrained) — constrained by existing neobiota and new invasions.
 *   - Analytical Observer: Sees full structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_trade_externalities_neobiota, 0.65).
domain_priors:suppression_score(global_trade_externalities_neobiota, 0.7).
domain_priors:theater_ratio(global_trade_externalities_neobiota, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, extractiveness, 0.65).
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_trade_externalities_neobiota, tangled_rope).
narrative_ontology:human_readable(global_trade_externalities_neobiota, "Global Trade's Externalization of Neobiota Costs").
narrative_ontology:topic_domain(global_trade_externalities_neobiota, "economic/environmental").

domain_priors:requires_active_enforcement(global_trade_externalities_neobiota).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_trade_externalities_neobiota, global_shipping_industry).
narrative_ontology:constraint_beneficiary(global_trade_externalities_neobiota, importing_nations_short_term).
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, global_biodiversity).
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, local_ecosystems).
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, long_term_economic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Global biodiversity is trapped by the spread of neobiota, with no exit. It suffers the full cost of the externality.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Local ecosystems are constrained by existing neobiota and new invasions, but can sometimes adapt or be restored at great cost. This perspective reflects the mixed coordination/extraction.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The shipping industry benefits from unpriced externalities, experiencing the constraint as a facilitator of trade (coordination).
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Importing nations benefit from cheaper goods in the short term but are constrained by the potential long-term costs of invasive species.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees a tangled rope: a global trade system that coordinates commerce but externalizes the ecological and economic costs of invasive species.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_trade_externalities_neobiota_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_trade_externalities_neobiota, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_trade_externalities_neobiota, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_trade_externalities_neobiota, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(global_trade_externalities_neobiota_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the global trade system transfers significant ecological and economic costs onto third parties. Suppression is also high (0.70) because alternatives are suppressed through lack of effective biosecurity regulations. Theater ratio is relatively low (0.30) as the constraint itself has direct real effects on the system. The claimed_type is Tangled Rope because while there is clear coordination through the global trade system, it is not efficient due to the cost of the unpriced externality
 *
 * PERSPECTIVAL GAP:
 *   The global shipping industry sees a rope, the importing nation in the short run sees a tangle rope, but global biodiversity sees a snare, as it bears the cost without reciprocal benefit. The analytical observer sees a tangled rope, a system with some coordination and heavy extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The global shipping industry is a beneficiary (d close to 0) while global biodiversity is a victim (d close to 1). Importing nations in the short run are both, but in the long run would become victims too. The directionality reflects the structural position of each agent within the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a tangled rope, not a piton, because the global trade system is actively used, and not a degraded or vestigial system. It's a tangled rope, not simply a rope, because of the unpriced externality which leads to high extraction from certain agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_impact_quantification,
    'What is the full economic cost of invasive species spread through global trade?',
    'Comprehensive economic modeling incorporating biodiversity loss, agricultural damage, and healthcare costs.',
    'Higher economic cost would justify more stringent regulations and change the classification to a snare from the ''importing nations'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_quantification, empirical, 'The uncertainty in quantifying the full economic impact of invasive species.').

omega_variable(
    global_enforcement_feasibility,
    'How feasible is global enforcement of stricter biosecurity measures?',
    'Political and logistical analysis of implementing and enforcing global biosecurity standards.',
    'Low feasibility would mean continued extraction; high feasibility could shift classification to a rope by reducing the externality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_enforcement_feasibility, conceptual, 'The political and logistical challenges of global enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_trade_externalities_neobiota, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glob_tr_t0, global_trade_externalities_neobiota, theater_ratio, 0, 0.4).
narrative_ontology:measurement(glob_tr_t5, global_trade_externalities_neobiota, theater_ratio, 5, 0.35).
narrative_ontology:measurement(glob_tr_t10, global_trade_externalities_neobiota, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(glob_be_t0, global_trade_externalities_neobiota, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(glob_be_t5, global_trade_externalities_neobiota, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(glob_be_t10, global_trade_externalities_neobiota, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_trade_externalities_neobiota, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
