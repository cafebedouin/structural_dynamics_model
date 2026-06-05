% ============================================================================
% CONSTRAINT STORY: rational_inertia_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rational_inertia_trap, []).

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
 *   constraint_id: rational_inertia_trap
 *   human_readable: The Legacy Protocol Lock-in
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The legacy protocol lock-in describes a scenario where a technologically
 *   or socially superior protocol exists, but individual agents find it
 *   rational to remain in the inferior legacy system. This occurs because the
 *   local cost of switching exceeds the immediate, local benefit, even though
 *   the collective or long-term gain from switching would be massive. This is
 *   a classic collective action problem exacerbated by network effects, path
 *   dependency, and switching costs.
 *
 * KEY AGENTS:
 *   - Potential New Protocol Adopters: Primary target (powerless/trapped) - Bear the cost of the extraction.
 *   - Legacy Protocol Vendors: Primary beneficiary (institutional/arbitrage) - Benefit from continued lock-in and vendor loyalty.
 *   - Early Adopters of Legacy Protocol: Moderate adopters (moderate/constrained) - Initially benefited but now are constrained by their prior investments.
 *   - Overall System Efficiency: Affected (powerless/trapped) - Abstract victim representing the overall system performance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rational_inertia_trap, 0.55).
domain_priors:suppression_score(rational_inertia_trap, 0.65).
domain_priors:theater_ratio(rational_inertia_trap, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rational_inertia_trap, extractiveness, 0.55).
narrative_ontology:constraint_metric(rational_inertia_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rational_inertia_trap, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rational_inertia_trap, tangled_rope).
narrative_ontology:human_readable(rational_inertia_trap, "The Legacy Protocol Lock-in").
narrative_ontology:topic_domain(rational_inertia_trap, "technological/economic").

domain_priors:requires_active_enforcement(rational_inertia_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rational_inertia_trap, legacy_protocol_vendors).
narrative_ontology:constraint_beneficiary(rational_inertia_trap, early_adopters_legacy_protocol).
narrative_ontology:constraint_victim(rational_inertia_trap, potential_new_protocol_adopters).
narrative_ontology:constraint_victim(rational_inertia_trap, overall_system_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Potential New Protocol Adopter (Snare) - Feels the immediate cost of switching but cannot realize the collective benefit alone. Trapped by network effects and switching costs.
constraint_indexing:constraint_classification(rational_inertia_trap, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Legacy Protocol Vendors (Rope) - Benefit from continued lock-in; their products and services are already integrated and widely used. They can charge rents and influence standards.
constraint_indexing:constraint_classification(rational_inertia_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: Analytical Observer (Tangled Rope) - Sees both the coordination and extraction aspects of the lock-in. Acknowledges both the benefits to vendors and early adopters, and the cost to the overall system.
constraint_indexing:constraint_classification(rational_inertia_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 4: Early Adopters of the Legacy Protocol (Piton) - Already invested in the legacy system, so they're constrained to stay due to switching costs and established networks, even if a superior protocol is available. Their prior investment acts as a form of lock-in. They may perform rituals of maintaining the old system despite its inefficiency.
constraint_indexing:constraint_classification(rational_inertia_trap, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_inertia_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rational_inertia_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_inertia_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rational_inertia_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rational_inertia_trap, TR),
    TR >= 0.70.

:- end_tests(rational_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate to High. Reflects the cost imposed on the potential adopters of the superior protocol as they are forced to use the inferior legacy system. Suppression (0.65): Moderate to High. The network effects and switching costs suppress alternatives. The early adopters are also constrained by their past decisions. The theater ratio is now high, reflecting some actual continued functional usage alongside performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The potential new adopters experience a snare, feeling trapped by the costs of switching. Legacy vendors experience a rope because the network effects coordinate action to maintain their profits. Early adopters experience something akin to a piton because they're constrained into a path-dependent outcome with a theater that keeps them bound to the old ways. Analytical observers see the tangled rope because there is a coordination failure with an exploitative extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the relationship between each agent and the protocol adoption. Potential adopters are victims and constrained, yielding a high directionality value. Legacy vendors are beneficiaries and can arbitrage new developments, which drives a low directionality value. Early adopters are more constrained than the vendors, and benefit less than the new protocol adopters would, yielding a moderate directionality value. Analytical observers view the whole scenario, which yields a balanced extraction level.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_strength,
    'How strong are the network effects that reinforce the legacy protocol?',
    'Empirical analysis of adoption rates and switching costs',
    'Stronger network effects make the lock-in more severe, shifting the classification toward snare. Weaker network effects allow for easier transitions to new protocols, shifting towards scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_strength, empirical, 'The strength of network effects affects lock-in severity.').

omega_variable(
    switching_cost_assessment,
    'What is the true cost of switching to the superior protocol, including both direct and indirect expenses?',
    'Cost-benefit analysis of implementing the new protocol, considering all relevant factors.',
    'Higher switching costs make the lock-in more entrenched, increasing the extraction. Lower switching costs create opportunities for escape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_assessment, empirical, 'Determining true cost of switching to the new protocol.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rational_inertia_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rati_tr_t0, rational_inertia_trap, theater_ratio, 0, 0.55).
narrative_ontology:measurement(rati_tr_t5, rational_inertia_trap, theater_ratio, 5, 0.65).
narrative_ontology:measurement(rati_tr_t10, rational_inertia_trap, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(rati_be_t0, rational_inertia_trap, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(rati_be_t5, rational_inertia_trap, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(rati_be_t10, rational_inertia_trap, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rational_inertia_trap, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
