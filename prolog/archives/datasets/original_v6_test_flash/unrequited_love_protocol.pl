% ============================================================================
% CONSTRAINT STORY: unrequited_love_protocol
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unrequited_love_protocol, []).

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
 *   constraint_id: unrequited_love_protocol
 *   human_readable: The Limerence Negotiation Protocol
 *   domain: Social/Psychological
 *
 * SUMMARY:
 *   Unrequited love is modeled here as a failed protocol negotiation where
 *   one party (the Suitor) continuously sends "connection requests"
 *   (emotional labor, validation, time) that are neither accepted nor fully
 *   rejected by the second party (the Object). The Suitor's emotional labor
 *   and time investment are consistently extracted without any reciprocation,
 *   creating a parasitic imbalance. Because the object doesn't explicitly
 *   deny the requests, the suitor gets snared.
 *
 * KEY AGENTS:
 *   - suitor: powerlessness, trapped in emotional investment
 *   - object: moderate power, mobile or constrained depending on clarity of rejection
 *   - analytical observer: analytical power to see system structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unrequited_love_protocol, 0.65).
domain_priors:suppression_score(unrequited_love_protocol, 0.75).
domain_priors:theater_ratio(unrequited_love_protocol, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unrequited_love_protocol, extractiveness, 0.65).
narrative_ontology:constraint_metric(unrequited_love_protocol, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unrequited_love_protocol, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unrequited_love_protocol, snare).
narrative_ontology:human_readable(unrequited_love_protocol, "The Limerence Negotiation Protocol").
narrative_ontology:topic_domain(unrequited_love_protocol, "Social/Psychological").

% --- Structural relationships ---
narrative_ontology:constraint_victim(unrequited_love_protocol, suitor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The suitor is trapped in a cycle of unreciprocated effort and hope, with limited ability to exit the situation due to emotional investment. They experience the protocol as pure extraction, receiving little to no return on their investment of emotional labor.
constraint_indexing:constraint_classification(unrequited_love_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The object of affection may be aware of the suitor's feelings but does not reciprocate them. They may find the attention flattering or burdensome, but ultimately maintain a distance. Their engagement is largely theatrical, performing civility without genuine connection.
constraint_indexing:constraint_classification(unrequited_love_protocol, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% From an analytical perspective, the limerence negotiation protocol is a tangled rope involving a coordination failure with asymmetrical extraction. The suitor provides emotional labor and validation (coordination), but receives no reciprocity, bearing the cost of the unrequited connection (asymmetric extraction). Requires active enforcement, manifested in social pressure and internal cognitive biases.
constraint_indexing:constraint_classification(unrequited_love_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unrequited_love_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unrequited_love_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unrequited_love_protocol, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unrequited_love_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unrequited_love_protocol, TR),
    TR >= 0.70.

:- end_tests(unrequited_love_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.65 due to high emotional and time investment by Suitor with no return. Suppression: 0.75 reflecting the Suitor's limited exit options and potential for social or internal pressure to continue the protocol. Theater ratio: 0.30 reflects the Object not giving much in return, but giving enough to maintain a facade.
 *
 * PERSPECTIVAL GAP:
 *   The suitor experiences the protocol as a snare due to lack of reciprocity, while the object views it as a piton or potentially a rope if they derive some benefits from the attention but do not reciprocate. The analytical observer sees a tangled rope due to the asymmetry and the coordination failure.
 *
 * DIRECTIONALITY LOGIC:
 *   The suitor is a victim because of the continuous exertion of emotional labor without reciprocation. The object is not explicitly a beneficiary but does maintain some power through not fully rejecting the suitor's requests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_threshold,
    'What level of reciprocal engagement is required to transition this from a Snare to a Rope or Tangled Rope?',
    'Quantifiable metrics of engagement and reciprocation (time investment, communication frequency, emotional disclosure).',
    'The definition of ''sufficient'' reciprocation determines whether the Suitor is merely exploited or participating in a mutually beneficial exchange.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_threshold, empirical, 'Identifies the level of return investment necessary to prevent extraction.').

omega_variable(
    clarity_of_rejection,
    'To what extent is the Object''s lack of reciprocation clearly communicated?',
    'Analysis of the Object''s communication patterns, explicit statements, and implicit signals.',
    'If rejection is ambiguous, the Suitor can maintain hope and continue extractive pursuit. If rejection is unambiguous, the protocol may dissolve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clarity_of_rejection, empirical, 'The level of clarity is essential for an exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unrequited_love_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unre_tr_t0, unrequited_love_protocol, theater_ratio, 0, 0.1).
narrative_ontology:measurement(unre_tr_t5, unrequited_love_protocol, theater_ratio, 5, 0.2).
narrative_ontology:measurement(unre_tr_t10, unrequited_love_protocol, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(unre_be_t0, unrequited_love_protocol, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(unre_be_t5, unrequited_love_protocol, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(unre_be_t10, unrequited_love_protocol, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unrequited_love_protocol, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
