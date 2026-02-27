% ============================================================================
% CONSTRAINT STORY: swift_legacy_piton
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_swift_legacy_piton, []).

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
 *   constraint_id: swift_legacy_piton
 *   human_readable: The SWIFT Financial Messaging Inertia
 *   domain: technological/economic
 *
 * SUMMARY:
 *   SWIFT (Society for Worldwide Interbank Financial Telecommunication) is a
 *   global financial messaging network that enables financial institutions
 *   worldwide to send and receive information about financial transactions in
 *   a secure, standardized and reliable environment. While it once
 *   represented a significant technological advancement, it has become a
 *   legacy system burdened by technical debt, security vulnerabilities, and
 *   high costs. However, due to its entrenched position and the immense
 *   coordination challenges involved in transitioning to a new system, SWIFT
 *   persists as the dominant standard, effectively functioning as a piton in
 *   the global financial landscape.
 *
 * KEY AGENTS:
 *   - SWIFT Organization: Beneficiary (institutional/arbitrage) - Maintains revenue and relevance
 *   - Incumbent Banks: Beneficiary/Victim (institutional/constrained) - Heavily invested but constrained by the system's limitations
 *   - Smaller Banks: Victim (powerless/trapped) - Forced to use SWIFT despite high costs
 *   - Fintech Innovators: Victim (moderate/mobile) - Hindered by SWIFT's dominance; seek new solutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(swift_legacy_piton, 0.15).
domain_priors:suppression_score(swift_legacy_piton, 0.8).
domain_priors:theater_ratio(swift_legacy_piton, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(swift_legacy_piton, extractiveness, 0.15).
narrative_ontology:constraint_metric(swift_legacy_piton, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(swift_legacy_piton, theater_ratio, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(swift_legacy_piton, piton).
narrative_ontology:human_readable(swift_legacy_piton, "The SWIFT Financial Messaging Inertia").
narrative_ontology:topic_domain(swift_legacy_piton, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(swift_legacy_piton, swift_organization).
narrative_ontology:constraint_beneficiary(swift_legacy_piton, incumbent_banks).
narrative_ontology:constraint_victim(swift_legacy_piton, smaller_banks).
narrative_ontology:constraint_victim(swift_legacy_piton, fintech_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Smaller banks are trapped by the SWIFT system as it is the only accepted standard for international transactions, despite high costs and outdated technology.
constraint_indexing:constraint_classification(swift_legacy_piton, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Incumbent banks are heavily invested in the SWIFT infrastructure, making a transition costly and difficult, despite acknowledging its inefficiencies. The system is largely performative.
constraint_indexing:constraint_classification(swift_legacy_piton, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The SWIFT organization benefits from the continued use of its system, maintaining its relevance and revenue stream, even as alternative technologies emerge.
constraint_indexing:constraint_classification(swift_legacy_piton, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees SWIFT as a piton, a system that has outlived its technical usefulness but persists due to institutional inertia and the difficulty of coordinating a global transition.
constraint_indexing:constraint_classification(swift_legacy_piton, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swift_legacy_piton_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(swift_legacy_piton, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(swift_legacy_piton, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(swift_legacy_piton, TR),
    TR >= 0.70.

:- end_tests(swift_legacy_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: Low (0.15). SWIFT extracts fees from transactions, but these are relatively small compared to the overall value of the transactions. The cost of compliance and upgrades is also a factor. Suppression: High (0.80). The lack of viable alternatives and the network effect lock-in make it difficult for institutions to exit the SWIFT system. The high suppression, combined with the relatively low extractiveness and high theater, point to a piton classification. Theater Ratio: High (0.90). Much of the activity surrounding SWIFT involves maintaining the existing infrastructure and complying with regulatory requirements, rather than providing genuine value or innovation.
 *
 * PERSPECTIVAL GAP:
 *   Smaller banks and fintech innovators experience SWIFT as a snare due to the high costs and limitations of the system, while the SWIFT organization and incumbent banks see it as a rope or scaffold, providing a necessary coordination function. However, the analytical observer recognizes the system's degraded state and its persistence due to inertia and lock-in, leading to a piton classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The SWIFT organization benefits from the continued use of its system and therefore has a low directionality value. Smaller banks, on the other hand, are forced to use SWIFT despite its limitations and high costs, resulting in a high directionality value. Incumbent banks are in a mixed position, having invested heavily in the SWIFT infrastructure but also recognizing its inefficiencies, leading to a moderate directionality value.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_cost,
    'What is the true cost of transitioning to a new global financial messaging system?',
    'Comprehensive cost-benefit analysis, including infrastructure upgrades, training, and potential security risks.',
    'Determines the feasibility of alternative systems and the likelihood of SWIFT being replaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost, empirical, 'True cost of transitioning to a new global financial messaging system.').

omega_variable(
    network_effect_lock_in,
    'How strong is the network effect lock-in that prevents a transition away from SWIFT?',
    'Analysis of network effects and the difficulty of coordinating a global transition.',
    'Determines the extent to which SWIFT''s dominance is self-perpetuating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_lock_in, conceptual, 'The strength of the network effect preventing transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(swift_legacy_piton, 1973, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swif_tr_t1980, swift_legacy_piton, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(swif_tr_t2000, swift_legacy_piton, theater_ratio, 2000, 0.6).
narrative_ontology:measurement(swif_tr_t2020, swift_legacy_piton, theater_ratio, 2020, 0.9).

% Extraction over time
narrative_ontology:measurement(swif_be_t1980, swift_legacy_piton, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(swif_be_t2000, swift_legacy_piton, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(swif_be_t2020, swift_legacy_piton, base_extractiveness, 2020, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(swift_legacy_piton, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
