% ============================================================================
% CONSTRAINT STORY: digital_identity_tether
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_identity_tether, []).

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
 *   constraint_id: digital_identity_tether
 *   human_readable: The Centralized Identity Anchor
 *   domain: technological
 *
 * SUMMARY:
 *   The Centralized Identity Anchor describes the inability of a digital
 *   subject to decouple their reputation, social graph, and authentication
 *   from a primary provider (the 'Identity Anchor'). This creates a lock-in
 *   effect where users are penalized for switching providers due to the loss
 *   of accumulated digital capital. The constraint extracts from users by
 *   limiting their mobility and control over their digital identities.
 *
 * KEY AGENTS:
 *   - Digital Subject: The primary target (powerless/trapped) - bears the cost of lock-in and limited mobility.
 *   - Identity Provider: The primary beneficiary (institutional/arbitrage) - benefits from user lock-in and network effects.
 *   - Analytical Observer: Sees the mixed system (analytical/analytical) - understands the coordination and extraction aspects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_identity_tether, 0.6).
domain_priors:suppression_score(digital_identity_tether, 0.7).
domain_priors:theater_ratio(digital_identity_tether, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_identity_tether, extractiveness, 0.6).
narrative_ontology:constraint_metric(digital_identity_tether, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(digital_identity_tether, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_identity_tether, tangled_rope).
narrative_ontology:human_readable(digital_identity_tether, "The Centralized Identity Anchor").
narrative_ontology:topic_domain(digital_identity_tether, "technological").

domain_priors:requires_active_enforcement(digital_identity_tether).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_identity_tether, identity_provider).
narrative_ontology:constraint_victim(digital_identity_tether, digital_subject).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The digital subject is trapped within the identity ecosystem and bears the full cost of lock-in.
constraint_indexing:constraint_classification(digital_identity_tether, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% The identity provider benefits from user lock-in and network effects, viewing the anchor as a necessary coordination mechanism.
constraint_indexing:constraint_classification(digital_identity_tether, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees a mixed system where the identity anchor coordinates authentication but also extracts from users by limiting their mobility and control.
constraint_indexing:constraint_classification(digital_identity_tether, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_identity_tether_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_identity_tether, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_identity_tether, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_identity_tether, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_identity_tether_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. Users are significantly penalized for switching providers due to the loss of accumulated digital capital (reputation, social graph, etc.). Suppression (0.7): High. Limited alternatives exist, and users are often unaware of the implications of centralized identity.
 *
 * PERSPECTIVAL GAP:
 *   The digital subject experiences a snare due to lock-in. The identity provider views it as a rope, enabling efficient authentication. The analytical observer sees a tangled rope, recognizing both the coordination and extraction aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic follows from the beneficiary/victim declarations. The identity provider benefits and has arbitrage exit options, leading to a low/negative chi. The digital subject bears the costs and is trapped, leading to a high chi. The analytical observer is neutral and sees both aspects.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by understanding that different agents experience the same system differently based on their structural position. The identity provider's perspective is genuine but incomplete; the digital subject's perspective reveals the extraction that is not visible from the provider's view. The high extractiveness is justified because the digital subject is significantly penalized for switching providers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralized_identity_viability,
    'To what extent can decentralized identity solutions (e.g., blockchain-based identity) effectively replace centralized identity anchors?',
    'Analysis of adoption rates, security audits, and scalability tests of decentralized identity systems.',
    'If viable: the snare becomes a scaffold as users gain exit options. If not viable: the snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_identity_viability, empirical, 'Viability of decentralized identity solutions').

omega_variable(
    user_awareness_of_lockin,
    'How aware are users of the limitations and risks associated with being tethered to a centralized identity anchor?',
    'User surveys and behavioral analysis to assess understanding of identity management and data privacy.',
    'If aware: users may demand better exit options, potentially shifting the constraint. If unaware: the snare persists due to lack of pressure for change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_awareness_of_lockin, empirical, 'User awareness of centralized identity lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_identity_tether, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_identity_tether, theater_ratio, 0, 0.2).
narrative_ontology:measurement(digi_tr_t5, digital_identity_tether, theater_ratio, 5, 0.3).
narrative_ontology:measurement(digi_tr_t10, digital_identity_tether, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_identity_tether, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(digi_be_t5, digital_identity_tether, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(digi_be_t10, digital_identity_tether, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_identity_tether, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
