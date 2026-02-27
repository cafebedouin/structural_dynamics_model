% ============================================================================
% CONSTRAINT STORY: trajans_bridge_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trajans_bridge_lifecycle, []).

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
 *   constraint_id: trajans_bridge_lifecycle
 *   human_readable: Trajan's Bridge over the Danube (Lifecycle)
 *   domain: technological/military
 *
 * SUMMARY:
 *   Trajan's Bridge, built circa 105 AD, was a monumental Roman engineering
 *   feat to facilitate the conquest of Dacia. While initially a source of
 *   Roman power, its lifecycle shows a shift from a 'rope' for the Romans to
 *   a 'snare' for the conquered Dacian tribes and eventually a costly
 *   structure to maintain, demonstrating the complex interplay of
 *   coordination and extraction.
 *
 * KEY AGENTS:
 *   - Roman Empire: Beneficiary (institutional/arbitrage) - Gains military and economic advantages
 *   - Dacian Tribes: Victim (powerless/trapped) - Suffers loss of autonomy and resources
 *   - Military Engineers: Beneficiary (powerful/mobile) - Improves social status and career prospect
 *   - Local Ecosystem: Victim (powerless/trapped) - Suffers environmental stress.
 *   - Historical Observer: Analytical observer (analytical/analytical) - sees the full lifecycle and consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trajans_bridge_lifecycle, 0.6).
domain_priors:suppression_score(trajans_bridge_lifecycle, 0.7).
domain_priors:theater_ratio(trajans_bridge_lifecycle, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, extractiveness, 0.6).
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trajans_bridge_lifecycle, tangled_rope).
narrative_ontology:human_readable(trajans_bridge_lifecycle, "Trajan's Bridge over the Danube (Lifecycle)").
narrative_ontology:topic_domain(trajans_bridge_lifecycle, "technological/military").

domain_priors:requires_active_enforcement(trajans_bridge_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trajans_bridge_lifecycle, roman_empire).
narrative_ontology:constraint_beneficiary(trajans_bridge_lifecycle, military_engineers).
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, dacian_tribes).
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, local_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The bridge facilitates Roman military control and resource extraction, severely limiting Dacian autonomy and access to resources. They are trapped geographically and politically.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% The bridge is initially a pure coordination mechanism for troop movement, resource transport, and projecting power. The Roman Empire benefits significantly from this infrastructure.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Analyzes the bridge as a tangled rope. Coordination for the Roman military combined with extraction from Dacia. Over time, the bridge's maintenance becomes difficult and is eventually abandoned, yet the historical impact remains significant.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

% The engineers involved in building the bridge benefitted from career advancement and renown for their skills. They have mobility and power within the Empire.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% The construction and use of the bridge extracts from the local ecology. The ecosystem is unable to exit the region and suffers degradation.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trajans_bridge_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trajans_bridge_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trajans_bridge_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.6) due to the Roman exploitation of Dacian resources and the environmental impact. Suppression is also high (0.7) as the bridge facilitated the Roman military presence, suppressing Dacian resistance and self-determination. Theater ratio is lower (0.3) as the bridge serves an obvious primary function in military/resource transport.
 *
 * PERSPECTIVAL GAP:
 *   The Roman Empire viewed the bridge as a rope, aiding their expansion and control. The Dacian tribes experienced it as a snare, limiting their freedom. The analytical observer understands it as a tangled rope, showcasing the dual nature of its impact over time. The engineers see a opportunity of progress and skill. The local ecosystem is trapped and degraded.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (Roman Empire and engineers) experience low directionality, while the victims (Dacian tribes, ecosystem) experience high directionality. This accurately reflects the power dynamics and consequences of the bridge's existence. The analytical perspective considers both the Roman and Dacian viewpoints, recognizing the bridge's mixed coordination and extraction effects.
 *
 * MANDATROPHY ANALYSIS:
 *   Without considering the varying perspectives and time horizon, one might misclassify this as a pure 'rope' from the Roman perspective, ignoring its extractive consequences for the Dacians. Or one might only see it as a ‘snare’ for the Dacians, neglecting the Roman Empire’s initial coordination benefits. The 'tangled rope' classification with varying perspectives prevents such mischaracterizations, showing how initial coordination can create future extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_integrity_vs_maintenance,
    'To what extent did the bridge''s structural weaknesses lead to its abandonment vs. intentional dismantling by the Romans?',
    'Archaeological investigation and historical document analysis',
    'If structural integrity was the main issue, the constraint shifts toward a Mountain. If dismantled due to cost/strategy, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_integrity_vs_maintenance, empirical, 'The primary driver behind the bridge''s ultimate demise.').

omega_variable(
    dacian_resistance_impact,
    'How much did Dacian resistance affect the cost-benefit analysis of maintaining the bridge?',
    'Historical analysis of Roman military expenditures and Dacian resistance activities',
    'Higher Dacian resistance would raise the extractiveness felt by the Romans, potentially shifting their perspective towards a Snare. Lower resistance would shift it toward a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dacian_resistance_impact, empirical, 'Influence of Dacian resistance on the Roman maintenance decision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trajans_bridge_lifecycle, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(traj_tr_t0, trajans_bridge_lifecycle, theater_ratio, 0, 0.1).
narrative_ontology:measurement(traj_tr_t20, trajans_bridge_lifecycle, theater_ratio, 20, 0.3).
narrative_ontology:measurement(traj_tr_t60, trajans_bridge_lifecycle, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(traj_be_t0, trajans_bridge_lifecycle, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(traj_be_t20, trajans_bridge_lifecycle, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(traj_be_t60, trajans_bridge_lifecycle, base_extractiveness, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trajans_bridge_lifecycle, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
