% ============================================================================
% CONSTRAINT STORY: global_stimulus_spree
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_stimulus_spree, []).

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
 *   constraint_id: global_stimulus_spree
 *   human_readable: The 2026 Global Fiscal Stimulus Surge
 *   domain: economic/political
 *
 * SUMMARY:
 *   Governments are deploying multitrillion-dollar stimulus packages to fuel
 *   AI, green energy, and rearmament. This surge in fiscal spending aims to
 *   stimulate economic growth, but raises concerns about long-term debt
 *   sustainability and the equitable distribution of benefits. The stimulus
 *   represents a complex interplay of coordination and extraction, with
 *   potential consequences for future generations.
 *
 * KEY AGENTS:
 *   - AI Sector: Primary beneficiary (institutional/arbitrage) - Receives funding and resources to advance research and development.
 *   - Green Energy Sector: Primary beneficiary (institutional/arbitrage) - Receives subsidies and incentives for renewable energy projects.
 *   - Defense Contractors: Primary beneficiary (institutional/arbitrage) - Awarded contracts for military equipment and services.
 *   - Future Taxpayers: Primary victim (powerless/trapped) - Bear the burden of increased government debt.
 *   - Countries with Low Fiscal Capacity: Secondary victim (moderate/constrained) - Face competitive disadvantages and potential debt crises.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_stimulus_spree, 0.55).
domain_priors:suppression_score(global_stimulus_spree, 0.4).
domain_priors:theater_ratio(global_stimulus_spree, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_stimulus_spree, extractiveness, 0.55).
narrative_ontology:constraint_metric(global_stimulus_spree, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(global_stimulus_spree, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_stimulus_spree, tangled_rope).
narrative_ontology:human_readable(global_stimulus_spree, "The 2026 Global Fiscal Stimulus Surge").
narrative_ontology:topic_domain(global_stimulus_spree, "economic/political").

domain_priors:requires_active_enforcement(global_stimulus_spree).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_stimulus_spree, ai_sector).
narrative_ontology:constraint_beneficiary(global_stimulus_spree, green_energy_sector).
narrative_ontology:constraint_beneficiary(global_stimulus_spree, defense_contractors).
narrative_ontology:constraint_victim(global_stimulus_spree, future_taxpayers).
narrative_ontology:constraint_victim(global_stimulus_spree, countries_with_low_fiscal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future taxpayers are burdened with increased debt due to the stimulus packages, with limited ability to influence current fiscal policy.
constraint_indexing:constraint_classification(global_stimulus_spree, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Countries lacking robust fiscal capacity are constrained by the global stimulus, facing competitive disadvantages and potential debt crises.
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% These sectors benefit from increased government funding and investment, creating opportunities for growth and innovation.
constraint_indexing:constraint_classification(global_stimulus_spree, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Governments may see the stimulus as a temporary measure to boost economic growth, with the intention of reducing spending in the future. However, political pressures may make it difficult to reduce spending once it has been implemented.
constraint_indexing:constraint_classification(global_stimulus_spree, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer sees a tangled rope: short term benefits versus potential long term consequences and uneven distribution of benefits.
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_stimulus_spree_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_stimulus_spree, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_stimulus_spree, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_stimulus_spree, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(global_stimulus_spree_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The stimulus extracts resources from future taxpayers to fund current projects. Suppression (0.40): The stimulus may suppress alternative economic policies and divert resources from other sectors. Theater ratio (0.30): The stimulus involves some performative spending and symbolic gestures, but is primarily focused on practical outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The AI, green energy and defense sectors benefit from the stimulus (rope), while future taxpayers are burdened with increased debt (snare). Countries with low fiscal capacity are constrained by the stimulus (tangled rope). Governments see a temporary boost (scaffold) and the analytical observer sees a mixed bag (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (AI, Green Energy, and Defense Sectors) experience low directionality, while victims (Future Taxpayers and Countries with Low Fiscal Capacity) experience high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification reflects the mixed nature of the stimulus, which involves both coordination (economic growth) and extraction (future debt).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_economic_impact,
    'What will be the long-term economic impact of the global stimulus surge?',
    'Economic modeling and analysis of historical stimulus packages.',
    'Positive economic growth or increased debt and inflation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_economic_impact, empirical, 'Uncertainty surrounding long-term economic impact.').

omega_variable(
    debt_sustainability,
    'Will increased government debt be sustainable in the long run?',
    'Analysis of debt-to-GDP ratios and interest rate trends.',
    'Sustainable debt levels or potential debt crises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability, empirical, 'Uncertainty surrounding debt sustainability.').

omega_variable(
    distribution_of_benefits,
    'How will the benefits of the stimulus be distributed across different sectors and countries?',
    'Analysis of investment patterns and economic indicators.',
    'Equitable distribution or increased inequality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distribution_of_benefits, empirical, 'Uncertainty surrounding distribution of benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_stimulus_spree, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glob_tr_t0, global_stimulus_spree, theater_ratio, 0, 0.2).
narrative_ontology:measurement(glob_tr_t3, global_stimulus_spree, theater_ratio, 3, 0.3).
narrative_ontology:measurement(glob_tr_t5, global_stimulus_spree, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(glob_be_t0, global_stimulus_spree, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(glob_be_t3, global_stimulus_spree, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(glob_be_t5, global_stimulus_spree, base_extractiveness, 5, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_stimulus_spree, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
