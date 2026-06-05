% ============================================================================
% CONSTRAINT STORY: ulysses_chp08
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp08, []).

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
 *   constraint_id: ulysses_chp08
 *   human_readable: The Lestrygonian Food Chain (Lunchtime Dublin)
 *   domain: social/economic/biological
 *
 * SUMMARY:
 *   Leopold Bloom wanders Dublin at lunchtime, contemplating the "predatory"
 *   nature of existence. The Lestrygonian episode in Ulysses examines the
 *   food chain as a social, economic, and biological constraint, where some
 *   benefit at the expense of others. This constraint story analyzes the
 *   perspectives of different actors within that food chain.
 *
 * KEY AGENTS:
 *   - Restaurant Owners: Primary beneficiary (institutional/arbitrage)
 *   - Food Suppliers: Secondary beneficiary (institutional/constrained)
 *   - Hungry Poor: Primary victim (powerless/trapped)
 *   - Animals: Secondary victim (powerless/trapped)
 *   - Average Dubliner: Moderate participant (moderate/constrained)
 *   - Analytical Observer: Global Perspective (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp08, 0.55).
domain_priors:suppression_score(ulysses_chp08, 0.3).
domain_priors:theater_ratio(ulysses_chp08, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp08, extractiveness, 0.55).
narrative_ontology:constraint_metric(ulysses_chp08, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ulysses_chp08, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp08, tangled_rope).
narrative_ontology:human_readable(ulysses_chp08, "The Lestrygonian Food Chain (Lunchtime Dublin)").
narrative_ontology:topic_domain(ulysses_chp08, "social/economic/biological").

domain_priors:requires_active_enforcement(ulysses_chp08).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp08, restaurant_owners).
narrative_ontology:constraint_beneficiary(ulysses_chp08, food_suppliers).
narrative_ontology:constraint_victim(ulysses_chp08, hungry_poor).
narrative_ontology:constraint_victim(ulysses_chp08, animals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Hungry Poor (Snare) - Trapped by poverty, they have limited access to food and are victims of the economic system. They experience direct extraction from the food chain. They have no real exit option and are powerless.
constraint_indexing:constraint_classification(ulysses_chp08, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: The Average Dubliner (Tangled Rope) - They are constrained by their income and food prices, but can still participate in the food chain. They experience mixed coordination and extraction, benefiting from the availability of food but also paying for it. Limited mobility due to financial constraints.
constraint_indexing:constraint_classification(ulysses_chp08, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective 3: Restaurant Owners (Rope) - They benefit from the food chain by selling food and making a profit. They experience the system as coordination, as they provide a service and receive payment. They can change their menu or prices (arbitrage) to increase profits.
constraint_indexing:constraint_classification(ulysses_chp08, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Perspective 4: The Analytical Observer (Tangled Rope) - This perspective acknowledges the entire cycle, understanding the necessary extraction from some to facilitate sustenance for others.
constraint_indexing:constraint_classification(ulysses_chp08, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp08_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp08, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp08, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp08, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ulysses_chp08_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The food chain extracts from animals and the poor to sustain the wealthy. Suppression (0.30): Low to moderate. There are some alternatives (vegetarianism, charity), but they are not readily available to all. Theater Ratio (0.20): Low. There is limited performative aspect to the actual sustenance requirement.
 *
 * PERSPECTIVAL GAP:
 *   The poor experience the system as pure extraction (snare), as they are trapped with little access to food. Restaurant owners see it as coordination (rope), as they facilitate the exchange of food for money. The analytical observer sees the mixed nature of the system (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are restaurant owners and food suppliers (low directionality); the victims are the hungry poor and animals (high directionality); and the average Dubliner experiences a mix of benefit and cost (moderate directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The system prevents mislabeling coordination as pure extraction by understanding that the food chain, while extractive to some, provides necessary sustenance to others. The entanglement is that without the extraction, the entire system cannot operate. Therefore, the analysis must assess what kind of changes can exist which still benefit the collective while minimising the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_sourcing_vs_profit,
    'To what degree is the extraction from animals justified by the benefit to humans?',
    'Philosophical debate, economic analysis of alternative food sources.',
    'If extraction is deemed unethical, then significant changes to the food chain would be necessary. If seen as justified, then the current system continues.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_sourcing_vs_profit, preference, 'Ethical implications of animal exploitation').

omega_variable(
    distribution_efficiency,
    'How efficiently is food distributed within Dublin?',
    'Economic analysis of food prices, access, and waste.',
    'If distribution is inefficient, then interventions may be needed to improve access for the poor. If efficient, then other solutions are needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distribution_efficiency, empirical, 'Efficiency of food distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp08, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp08, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ulys_tr_t5, ulysses_chp08, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp08, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp08, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ulys_be_t5, ulysses_chp08, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp08, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp08, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
