% ============================================================================
% CONSTRAINT STORY: indo_russian_submarine_lease_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_russian_submarine_lease_2025, []).

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
 *   constraint_id: indo_russian_submarine_lease_2025
 *   human_readable: Indo-Russian Nuclear Submarine Lease Agreement (Chakra III)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   This constraint represents the agreement for India to lease an
 *   Akula-class nuclear-powered attack submarine from Russia. The lease has
 *   implications for regional power dynamics, Indian military capabilities,
 *   and the Russian defense industry.
 *
 * KEY AGENTS:
 *   - Indian Navy: Primary beneficiary (institutional/arbitrage) - Gains enhanced capabilities.
 *   - Russian Defense Industry: Secondary beneficiary (institutional/arbitrage) - Gains financially and strategically.
 *   - Indian Taxpayers: Primary victim (moderate/constrained) - Bear the financial burden.
 *   - Regional Rivals: Secondary victim (powerless/trapped) - Face increased threat and limited options.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_russian_submarine_lease_2025, 0.6).
domain_priors:suppression_score(indo_russian_submarine_lease_2025, 0.4).
domain_priors:theater_ratio(indo_russian_submarine_lease_2025, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, extractiveness, 0.6).
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_russian_submarine_lease_2025, tangled_rope).
narrative_ontology:human_readable(indo_russian_submarine_lease_2025, "Indo-Russian Nuclear Submarine Lease Agreement (Chakra III)").
narrative_ontology:topic_domain(indo_russian_submarine_lease_2025, "geopolitical").

domain_priors:requires_active_enforcement(indo_russian_submarine_lease_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_russian_submarine_lease_2025, russian_defense_industry).
narrative_ontology:constraint_beneficiary(indo_russian_submarine_lease_2025, indian_navy).
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, indian_taxpayers).
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, regional_rivals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Regional rivals perceive the lease as a significant threat, limiting their strategic options and increasing military spending. They are essentially trapped within the regional power dynamic influenced by this lease.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% The Indian Navy benefits from enhanced capabilities and strategic depth, enabling a more assertive role in the Indian Ocean region. They can leverage this asset for power projection and influence.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Russian defense industry benefits financially and strategically from the lease, maintaining a key export market and reinforcing its geopolitical influence. They benefit from the arrangement, but also incur maintenance obligations.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% Indian taxpayers bear the financial burden of the lease, potentially diverting resources from other developmental priorities. They are constrained by the government's decision and have limited direct influence.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees the agreement as a complex interplay of geopolitical strategy, economic interests, and regional power dynamics, creating both benefits and risks.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_russian_submarine_lease_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_russian_submarine_lease_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(indo_russian_submarine_lease_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The lease extracts significant financial resources from India. Suppression (0.4): Regional rivals face limited strategic options. The theater ratio is low as the submarine provides genuine military capability.
 *
 * PERSPECTIVAL GAP:
 *   Regional rivals view the lease as a threat (Snare), while the Indian Navy benefits from increased capabilities (Rope). The analytical observer sees the complex interplay of benefits and risks (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries with arbitrage options experience lower or negative effective extraction; trapped agents with no exit bear maximum extraction; organized agents with exit paths experience moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint classification helps to distinguish between legitimate strategic alliances and extractive arrangements. The tangled rope classification accounts for the combination of benefits and costs involved in the lease.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regional_arms_race,
    'Will the submarine lease trigger a regional arms race, increasing instability?',
    'Monitoring regional military spending and procurement patterns.',
    'Increased regional instability would shift the classification towards a stronger snare for all regional actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_arms_race, empirical, 'Potential for increased regional arms race').

omega_variable(
    technology_transfer_dependence,
    'To what extent does the lease perpetuate Indian dependence on Russian military technology?',
    'Assessing the level of technology transfer and indigenous capability development.',
    'High dependence would classify the Indian Navy as ''constrained'', reducing their perceived ''rope'' power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_dependence, empirical, 'Technology transfer and dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_russian_submarine_lease_2025, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indo_tr_t0, indo_russian_submarine_lease_2025, theater_ratio, 0, 0.2).
narrative_ontology:measurement(indo_tr_t5, indo_russian_submarine_lease_2025, theater_ratio, 5, 0.3).
narrative_ontology:measurement(indo_tr_t10, indo_russian_submarine_lease_2025, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(indo_be_t0, indo_russian_submarine_lease_2025, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(indo_be_t5, indo_russian_submarine_lease_2025, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(indo_be_t10, indo_russian_submarine_lease_2025, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_russian_submarine_lease_2025, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
