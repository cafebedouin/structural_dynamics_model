% ============================================================================
% CONSTRAINT STORY: silent_dependency_activation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silent_dependency_activation, []).

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
 *   constraint_id: silent_dependency_activation
 *   human_readable: The Invisible Supply Chain Trap
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The invisible supply chain trap describes a situation where a critical
 *   component or raw material becomes essential to a complex system but
 *   remains largely unnoticed until a disruption occurs. This dependency is
 *   often exacerbated by a lack of transparency, limited supplier
 *   diversification, and high switching costs. When market conditions shift,
 *   or geopolitical tensions rise, this hidden dependency can rapidly
 *   transform into a significant bottleneck, impacting downstream
 *   manufacturers and end consumers.
 *
 * KEY AGENTS:
 *   - Downstream Manufacturers: Primary victims (powerless/trapped) – face supply disruptions and increased costs.
 *   - Component Monopolist: Primary beneficiary (institutional/arbitrage) – benefits from increased demand and pricing power.
 *   - National Governments: Secondary actor (moderate/constrained) – seeks to maintain supply chain stability but faces limited direct control.
 *   - First Movers: Secondary beneficiaries (powerful/mobile) - Initially benefit from implementing the critical component, but are also vulnerable to its supply chain risks.
 *   - End Consumers: Indirect victims (powerless/constrained) - Face higher prices and limited product availability due to supply disruptions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silent_dependency_activation, 0.6).
domain_priors:suppression_score(silent_dependency_activation, 0.7).
domain_priors:theater_ratio(silent_dependency_activation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silent_dependency_activation, extractiveness, 0.6).
narrative_ontology:constraint_metric(silent_dependency_activation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(silent_dependency_activation, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silent_dependency_activation, tangled_rope).
narrative_ontology:human_readable(silent_dependency_activation, "The Invisible Supply Chain Trap").
narrative_ontology:topic_domain(silent_dependency_activation, "technological/economic").

domain_priors:requires_active_enforcement(silent_dependency_activation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silent_dependency_activation, component_monopolist).
narrative_ontology:constraint_beneficiary(silent_dependency_activation, first_movers).
narrative_ontology:constraint_victim(silent_dependency_activation, downstream_manufacturers).
narrative_ontology:constraint_victim(silent_dependency_activation, end_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Downstream Manufacturers (Snare). They are trapped by the dependency and face significant costs due to supply disruptions or price hikes. Limited exit options due to specialized manufacturing processes and high switching costs.
constraint_indexing:constraint_classification(silent_dependency_activation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: National Governments (Tangled Rope). They benefit from a functioning supply chain but are constrained in their ability to address the dependency directly. They might attempt to diversify supply or subsidize domestic production, but these efforts are often slow and costly.
constraint_indexing:constraint_classification(silent_dependency_activation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Component Monopolist (Rope). Benefits from increased demand and pricing power due to the dependency. Experiences the constraint as coordination—they are simply responding to market signals. Has arbitrage exit options due to global demand.
constraint_indexing:constraint_classification(silent_dependency_activation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Analytical Observer (Tangled Rope). Sees the full structure of the dependency, including the potential for disruption and the distribution of costs and benefits across different actors. Can analyze the systemic risk and potential mitigation strategies.
constraint_indexing:constraint_classification(silent_dependency_activation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silent_dependency_activation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(silent_dependency_activation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silent_dependency_activation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(silent_dependency_activation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(silent_dependency_activation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The monopolist can extract significant rents due to the lack of alternatives. Suppression (0.70): High. Alternatives are actively suppressed through patents, trade secrets, and high initial capital costs. Theater Ratio (0.20): Low. Minimal theater; the monopolist provides a genuine service, but is structurally positioned to exploit its dominance.
 *
 * PERSPECTIVAL GAP:
 *   The downstream manufacturers experience this as a Snare, as they are highly dependent and have limited exit options. The component monopolist sees it as a Rope, a mutually beneficial coordination mechanism. National governments see it as a Tangled Rope, where they benefit from economic activity but are also exposed to systemic risk. The analytical observer sees the full picture as a Tangled Rope: a system with a coordination function that has become exploitative due to the power imbalance.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value is determined by the agent's power, exit options, and relationship to the extraction flow. Downstream manufacturers are victims with few exit options, leading to a high directionality value and snare classification. The component monopolist is a beneficiary with arbitrage options, leading to a low directionality value and rope classification. Governments are moderate actors with constrained exit options, leading to a moderate directionality value and tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the same supply chain structure can be perceived differently depending on the perspective. The monopolist may genuinely believe they are providing a valuable service, while downstream manufacturers experience exploitation. The tangled rope classification recognizes both aspects of the relationship. It acknowledges the initial coordination benefit of having a specialized component supplier, but also highlights the potential for that supplier to extract excessive rents once a dependency is established. The presence of victims and beneficiaries along with active enforcement justifies the tangled rope rather than a simple rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_detection_horizon,
    'What is the typical time horizon for detecting these types of hidden dependencies before they become critical bottlenecks?',
    'Historical analysis of past supply chain disruptions and their root causes; development of early warning indicators based on market concentration and technological trends.',
    'If short: Mitigation strategies can be implemented proactively. If long: Supply chains remain vulnerable to unforeseen disruptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_detection_horizon, empirical, 'Time horizon for dependency detection').

omega_variable(
    switching_cost_sensitivity,
    'How sensitive are downstream manufacturers to switching costs associated with diversifying their supply base?',
    'Economic modeling of manufacturing processes; surveys of downstream manufacturers regarding their perceived switching costs and willingness to invest in alternative supply sources.',
    'If highly sensitive: Manufacturers remain locked into existing dependencies. If less sensitive: Manufacturers are more likely to diversify their supply base, reducing the overall risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_sensitivity, empirical, 'Sensitivity of downstream manufacturers to switching costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silent_dependency_activation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sile_tr_t0, silent_dependency_activation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sile_tr_t5, silent_dependency_activation, theater_ratio, 5, 0.15).
narrative_ontology:measurement(sile_tr_t10, silent_dependency_activation, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(sile_be_t0, silent_dependency_activation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sile_be_t5, silent_dependency_activation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sile_be_t10, silent_dependency_activation, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silent_dependency_activation, resource_allocation).
narrative_ontology:affects_constraint(silent_dependency_activation, rare_earth_dependency).
narrative_ontology:affects_constraint(silent_dependency_activation, semiconductor_shortage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
