% ============================================================================
% CONSTRAINT STORY: global_protocol_entrenchment
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_protocol_entrenchment, []).

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
 *   constraint_id: global_protocol_entrenchment
 *   human_readable: Entrenchment of a Universal Standard Lock Protocol
 *   domain: technological/economic
 *
 * SUMMARY:
 *   A universal standard lock, while offering benefits of interoperability
 *   and ease of use, can create an entrenched system that extracts value from
 *   late adopters and stifles innovation. Early adopters and protocol
 *   developers benefit, while others bear the costs.
 *
 * KEY AGENTS:
 *   - Early Adopters: Beneficiaries (institutional/arbitrage) - Benefit from network effects.
 *   - Late Adopters: Victims (powerless/trapped) - Forced to comply with the standard.
 *   - Competing Innovations: Victims (moderate/constrained) - Face difficulty in displacing the standard.
 *   - Protocol Developers: Beneficiaries (powerful/mobile) - Control and extract rents.
 *   - Analytical Observer: (analytical/analytical) - Sees the mixed nature of the standard.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_protocol_entrenchment, 0.6).
domain_priors:suppression_score(global_protocol_entrenchment, 0.7).
domain_priors:theater_ratio(global_protocol_entrenchment, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_protocol_entrenchment, extractiveness, 0.6).
narrative_ontology:constraint_metric(global_protocol_entrenchment, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(global_protocol_entrenchment, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_protocol_entrenchment, tangled_rope).
narrative_ontology:human_readable(global_protocol_entrenchment, "Entrenchment of a Universal Standard Lock Protocol").
narrative_ontology:topic_domain(global_protocol_entrenchment, "technological/economic").

domain_priors:requires_active_enforcement(global_protocol_entrenchment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_protocol_entrenchment, early_adopters).
narrative_ontology:constraint_beneficiary(global_protocol_entrenchment, protocol_developers).
narrative_ontology:constraint_victim(global_protocol_entrenchment, late_adopters).
narrative_ontology:constraint_victim(global_protocol_entrenchment, competing_innovations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Late adopters are forced to comply with the established standard, incurring costs to retrofit or adapt their systems.
constraint_indexing:constraint_classification(global_protocol_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Newer, possibly superior, locking protocols face an uphill battle to displace the entrenched standard.
constraint_indexing:constraint_classification(global_protocol_entrenchment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Early adopters benefit from the network effects and economies of scale afforded by the dominant standard.
constraint_indexing:constraint_classification(global_protocol_entrenchment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The developers of the entrenched standard maintain control and extract rents through licensing, maintenance, and upgrades.
constraint_indexing:constraint_classification(global_protocol_entrenchment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the mixed coordination and extraction inherent in the global standard.
constraint_indexing:constraint_classification(global_protocol_entrenchment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_protocol_entrenchment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_protocol_entrenchment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_protocol_entrenchment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_protocol_entrenchment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(global_protocol_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate as the standard lock does provide benefits of interoperability, but also extracts value from those who must comply. Suppression is high due to the network effects and established infrastructure surrounding the standard. Theater ratio is low as the lock is primarily functional.
 *
 * PERSPECTIVAL GAP:
 *   The various agents view the universal standard lock differently based on their position. Early adopters see a beneficial system of interoperability, while late adopters are stuck with a system that they are forced to use. Innovations see a barrier to entry for improvements on the standard.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries and victims are clearly defined. Early adopters benefit. Late adopters, competing innovations are hurt. Early adopter get benefit. Innovation and Late adopters must adapt and pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The entanglement of rope and snare elements comes from the coordination benefits of standardization vs the extraction from less powerful actors who get locked in and cannot easily innovate or switch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_threshold,
    'What level of switching cost is required to shift from the old to the new universal standard lock?',
    'Study of adoption rates of previous universal standard locks and determine what level of incentive or penalty for not switching caused change.',
    'Understanding the level of stickiness for universal standard locks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Quantifying the difficulty to switch from an old standard to a newer standard.').

omega_variable(
    backwards_compatibility,
    'How important is backwards compatibility for a universal standard lock?',
    'Analysis of user complaints and system failures from lack of backwards compatibility.',
    'Whether to incorporate old universal standard lock features in new lock. ',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(backwards_compatibility, conceptual, 'How important is it for a new universal standard lock to work with older systems?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_protocol_entrenchment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glob_tr_t0, global_protocol_entrenchment, theater_ratio, 0, 0.2).
narrative_ontology:measurement(glob_tr_t5, global_protocol_entrenchment, theater_ratio, 5, 0.3).
narrative_ontology:measurement(glob_tr_t10, global_protocol_entrenchment, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(glob_be_t0, global_protocol_entrenchment, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(glob_be_t5, global_protocol_entrenchment, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(glob_be_t10, global_protocol_entrenchment, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_protocol_entrenchment, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
