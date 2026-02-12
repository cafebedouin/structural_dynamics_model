% ============================================================================
% CONSTRAINT STORY: unrequited_love_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-10
% ============================================================================

:- module(unrequited_love_protocol, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: unrequited_love_protocol
 * human_readable: The Limerence Negotiation Protocol
 * domain: Social/Psychological
 *
 * SUMMARY:
 * Unrequited love is modeled here as a failed protocol negotiation where one 
 * party (the Suitor) continuously sends "connection requests" (emotional 
 * labor, validation, time) that are neither accepted nor fully rejected 
 * by the second party (the Object). This "pending" state creates a high-extraction 
 * environment where the social script of "politeness" or "friendship" acts 
 * as a suppression mechanism against exit.
 *
 * KEY AGENTS (by structural relationship):
 * - unrequited_suitor: Primary target (powerless/trapped) — bears the cost of 
 * non-reciprocal emotional subsidy.
 * - validation_recipient: Primary beneficiary (institutional/arbitrage) — 
 * receives ego-subsidies with zero return obligation.
 * - romantic_market_observer: Analytical observer — views the transaction 
 * as a misallocation of emotional capital.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unrequited_love_protocol, 0.68).
domain_priors:suppression_score(unrequited_love_protocol, 0.72).   % Normative shame & "Hope"
domain_priors:theater_ratio(unrequited_love_protocol, 0.45).       % High functional pain, moderate performance.

% --- Constraint metric facts ---
narrative_ontology:constraint_metric(unrequited_love_protocol, extractiveness, 0.68).
narrative_ontology:constraint_metric(unrequited_love_protocol, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unrequited_love_protocol, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unrequited_love_protocol, snare).

% --- Structural relationships ---
% The recipient benefits from the "Status Quo" of the protocol.
narrative_ontology:constraint_beneficiary(unrequited_love_protocol, validation_recipient).

% The suitor bears the cost of the asymmetric extraction.
narrative_ontology:constraint_victim(unrequited_love_protocol, unrequited_suitor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUITOR (SNARE)
% Trapped by the "Hope" variable, they experience the protocol as a pure drain.
constraint_indexing:constraint_classification(unrequited_love_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE RECIPIENT (ROPE)
% From the beneficiary's side, it's a "Rope" of social coordination (maintaining 
% a friendship/social circle) with a negative chi (net benefit).
constraint_indexing:constraint_classification(unrequited_love_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Computation: eps(0.68) * f(d=0.72) * sigma(local=0.8) => High Extraction.
constraint_indexing:constraint_classification(unrequited_love_protocol, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unrequited_love_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unrequited_love_protocol, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unrequited_love_protocol, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    narrative_ontology:constraint_metric(unrequited_love_protocol, extractiveness, E),
    E >= 0.46.

:- end_tests(unrequited_love_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extractiveness (0.68) reflects the total time/emotional energy diverted 
 * from productive self-actualization. Suppression (0.72) is high because the 
 * protocol is enforced by internal cognitive biases (limerence) and external 
 * social scripts ("don't make it weird").
 *
 * PERSPECTIVAL GAP:
 * The Suitor sees a Snare (trapped extraction); the Recipient sees a Rope 
 * (a stable, coordinated social relationship they can 'arbitrage' for 
 * validation without paying the 'cost' of commitment).
 *
 * DIRECTIONALITY LOGIC:
 * The validation_recipient is the structural beneficiary. They are 
 * assigned 'institutional' power here because they hold the 'keys' to the 
 * protocol state machine (Accept/Reject).
 *
 * MANDATROPHY ANALYSIS:
 * This is not a Piton because the emotional extraction is active and 
 * volatile, not merely inertial. It is a Snare because it relies on 
 * suppressing the Suitor's exit via the "Hope" theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_limerence_utility,
    'Is the unrequited state a biological "waiting room" for mate selection?',
    'Analysis of long-term pair-bonding success rates following limerence.',
    'If True: Scaffold (Temporary filter). If False: Snare (Dead-end extraction).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unrequited_love_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the decay of functional hope and the rise of theatrical performance.
narrative_ontology:measurement(unreq_tr_t0, unrequited_love_protocol, theater_ratio, 0, 0.20).
narrative_ontology:measurement(unreq_tr_t5, unrequited_love_protocol, theater_ratio, 5, 0.35).
narrative_ontology:measurement(unreq_tr_t10, unrequited_love_protocol, theater_ratio, 10, 0.45).

% Extraction intensifies as the suitor "doubles down" to resolve the negotiation.
narrative_ontology:measurement(unreq_ex_t0, unrequited_love_protocol, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(unreq_ex_t5, unrequited_love_protocol, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(unreq_ex_t10, unrequited_love_protocol, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It is an information standard for "Friendship."
narrative_ontology:coordination_type(unrequited_love_protocol, information_standard).

% Influences the broader constraint of social monogamy.
narrative_ontology:affects_constraint(unrequited_love_protocol, social_monogamy_norms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0)
   ========================================================================== */

% Differentiating the 'Beloved' from a generic institutional actor.
% They are a full beneficiary (0.05).
constraint_indexing:directionality_override(unrequited_love_protocol, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
