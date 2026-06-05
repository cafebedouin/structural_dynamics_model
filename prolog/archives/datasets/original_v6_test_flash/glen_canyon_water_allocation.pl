% ============================================================================
% CONSTRAINT STORY: glen_canyon_water_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_glen_canyon_water_allocation, []).

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
 *   constraint_id: glen_canyon_water_allocation
 *   human_readable: Colorado River Water Allocation under the Colorado River Compact
 *   domain: political, economic, environmental
 *
 * SUMMARY:
 *   The Colorado River Compact, along with subsequent agreements and legal
 *   precedents, defines the allocation of water resources from the Colorado
 *   River among the Upper and Lower Basin states. This allocation system
 *   creates a complex interplay of coordination and extraction, where states
 *   benefit from reliable water access but also face constraints due to the
 *   compact's terms. Downstream ecosystems and Native American tribes bear a
 *   disproportionate burden due to reduced water flows and historical
 *   inequities.
 *
 * KEY AGENTS:
 *   - Upper Basin States Agriculture: Benefits from reliable water allocation for agriculture (institutional/constrained)
 *   - Lower Basin States Urban: Benefits from reliable water allocation for urban use (institutional/constrained)
 *   - Downstream Ecosystems: Bear the ecological cost of reduced flows (powerless/trapped)
 *   - Native American Tribes: Constrained by legal limitations but have some agency through negotiation (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(glen_canyon_water_allocation, 0.6).
domain_priors:suppression_score(glen_canyon_water_allocation, 0.7).
domain_priors:theater_ratio(glen_canyon_water_allocation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(glen_canyon_water_allocation, extractiveness, 0.6).
narrative_ontology:constraint_metric(glen_canyon_water_allocation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(glen_canyon_water_allocation, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(glen_canyon_water_allocation, tangled_rope).
narrative_ontology:human_readable(glen_canyon_water_allocation, "Colorado River Water Allocation under the Colorado River Compact").
narrative_ontology:topic_domain(glen_canyon_water_allocation, "political, economic, environmental").

domain_priors:requires_active_enforcement(glen_canyon_water_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(glen_canyon_water_allocation, upper_basin_states_agriculture).
narrative_ontology:constraint_beneficiary(glen_canyon_water_allocation, lower_basin_states_urban).
narrative_ontology:constraint_victim(glen_canyon_water_allocation, downstream_ecosystems).
narrative_ontology:constraint_victim(glen_canyon_water_allocation, native_american_tribes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Downstream ecosystems are trapped and bear the full ecological cost of reduced flows. They cannot exit the system and have no power to change the allocation.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Native American tribes are constrained by legal limitations but have some agency through negotiation and litigation, gaining some benefits and suffering losses.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Upper basin states benefit from reliable water allocation for agriculture, but are constrained by compact terms. Coordination role is emphasized.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Lower basin states benefit from reliable water allocation for urban use, but are constrained by compact terms. Coordination role is emphasized.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The analytical observer sees the compact as a tangled rope due to the mixed coordination and extraction dynamics. The long-term sustainability and ecological impacts reveal the extraction aspect.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(glen_canyon_water_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(glen_canyon_water_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(glen_canyon_water_allocation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(glen_canyon_water_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(glen_canyon_water_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): The compact extracts value from downstream ecosystems and Native American tribes by prioritizing water allocation to upper and lower basin states. Suppression (0.7): The legal framework of the compact and subsequent agreements suppresses alternative allocation schemes that might prioritize ecosystem health or tribal water rights. Theater Ratio (0.4): The theater ratio is moderate, as there is some genuine function in coordinating water allocation, but also performative aspects related to political negotiations and legal maneuvering.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions within the water allocation system. The upper and lower basin states perceive the compact as a coordination mechanism that ensures water availability. However, downstream ecosystems and Native American tribes experience the compact as a constraint that deprives them of essential resources.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic follows the flow of benefits and costs within the system. The upper and lower basin states are beneficiaries, experiencing the compact as a positive force. Downstream ecosystems and Native American tribes are victims, bearing the negative consequences of reduced water flows.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a tangled rope because it blends coordination and extraction. The allocation system provides a framework for managing water resources among states, but it also leads to ecological damage and historical inequities. Mislabeling it as pure coordination would ignore the negative impacts on downstream ecosystems and tribal communities. Mislabeling as pure extraction would ignore coordination among upper and lower basin states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_change_impact,
    'How will climate change affect the Colorado River''s water supply and the enforceability of the compact?',
    'Climate modeling and hydrological studies',
    'Reduced water availability may force renegotiation of the compact or lead to increased conflict among states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_change_impact, empirical, 'The impact of climate change on water supply').

omega_variable(
    tribal_water_rights,
    'How will the full recognition and implementation of tribal water rights impact water allocation under the compact?',
    'Legal rulings and negotiated settlements',
    'Increased tribal water rights may reduce the amount of water available for other users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribal_water_rights, conceptual, 'The impact of tribal water rights').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(glen_canyon_water_allocation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glen_tr_t0, glen_canyon_water_allocation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(glen_tr_t50, glen_canyon_water_allocation, theater_ratio, 50, 0.4).
narrative_ontology:measurement(glen_tr_t100, glen_canyon_water_allocation, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(glen_be_t0, glen_canyon_water_allocation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(glen_be_t50, glen_canyon_water_allocation, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(glen_be_t100, glen_canyon_water_allocation, base_extractiveness, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(glen_canyon_water_allocation, resource_allocation).
narrative_ontology:affects_constraint(glen_canyon_water_allocation, colorado_river_endangered_species).
narrative_ontology:affects_constraint(glen_canyon_water_allocation, colorado_river_delta_restoration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
