% ============================================================================
% CONSTRAINT STORY: asymmetric_computational_difficulty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_computational_difficulty, []).

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
 *   constraint_id: asymmetric_computational_difficulty
 *   human_readable: Proof-of-Work Zero-Day
 *   domain: technological
 *
 * SUMMARY:
 *   This scenario describes a cryptographic primitive used for coordination
 *   (Rope) that has been secretly compromised by a single actor. The
 *   asymmetric computational difficulty resulting from a zero-day exploit
 *   creates a Snare for network participants, who are unknowingly being
 *   extracted from. The exploiter, on the other hand, benefits from this
 *   asymmetry.
 *
 * KEY AGENTS:
 *   - Network Participants: Victims (powerless/trapped)
 *   - Zero-Day Exploiter: Beneficiary (powerful/arbitrage)
 *   - Analytical Observer: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_computational_difficulty, 0.75).
domain_priors:suppression_score(asymmetric_computational_difficulty, 0.85).
domain_priors:theater_ratio(asymmetric_computational_difficulty, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, extractiveness, 0.75).
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_computational_difficulty, snare).
narrative_ontology:human_readable(asymmetric_computational_difficulty, "Proof-of-Work Zero-Day").
narrative_ontology:topic_domain(asymmetric_computational_difficulty, "technological").

domain_priors:requires_active_enforcement(asymmetric_computational_difficulty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_computational_difficulty, zero_day_exploiter).
narrative_ontology:constraint_victim(asymmetric_computational_difficulty, network_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A network participant who is unaware of the zero-day vulnerability is trapped and powerless, experiencing the constraint as a snare. They are extracted from without realizing the presence of the attack.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% The zero-day exploiter benefits from the asymmetric computational difficulty, allowing them to extract value from the network with relative ease. They see this as a coordination tool.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, the situation presents a tangled rope where there is both coordination (proof of work) and asymmetric extraction (zero-day exploit).
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_computational_difficulty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_computational_difficulty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_computational_difficulty, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_computational_difficulty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asymmetric_computational_difficulty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the zero-day exploit allows the attacker to extract significant value from the network. Suppression is high because network participants are unaware of the vulnerability and cannot easily defend against it. Theater ratio is low because there is little performative activity associated with the exploit.
 *
 * PERSPECTIVAL GAP:
 *   The network participant sees a snare because they are trapped and powerless. The zero-day exploiter sees it as a rope because it allows them to coordinate their extraction. The analytical observer sees it as a tangled rope because there is both coordination (proof of work) and asymmetric extraction (zero-day exploit).
 *
 * DIRECTIONALITY LOGIC:
 *   The network participants are the victims and are trapped. The zero-day exploiter is the beneficiary and has arbitrage. The analytical observer is neutral.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exploit_detection_time,
    'How long will it take for the zero-day exploit to be detected by the network?',
    'Security audits, network monitoring, and community analysis.',
    'Shorter detection time reduces the extractiveness. Longer detection time increases the extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploit_detection_time, empirical, 'Time until zero-day exploit is detected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_computational_difficulty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asym_tr_t0, asymmetric_computational_difficulty, theater_ratio, 0, 0.3).
narrative_ontology:measurement(asym_tr_t5, asymmetric_computational_difficulty, theater_ratio, 5, 0.3).
narrative_ontology:measurement(asym_tr_t10, asymmetric_computational_difficulty, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(asym_be_t0, asymmetric_computational_difficulty, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(asym_be_t5, asymmetric_computational_difficulty, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(asym_be_t10, asymmetric_computational_difficulty, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_computational_difficulty, enforcement_mechanism).
narrative_ontology:affects_constraint(asymmetric_computational_difficulty, blockchain_security).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
