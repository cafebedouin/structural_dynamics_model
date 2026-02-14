% ============================================================================
% CONSTRAINT STORY: postman_survival_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_postman_survival_protocol, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: postman_survival_protocol
 *   human_readable: Postman's Protocol for Informational Hygiene
 *   domain: social/technological
 *
 * SUMMARY:
 *   This constraint is Neil Postman's defensive protocol for protecting individual
 *   agency against the informational overload and depersonalization of large
 *   technological bureaucracies. It is a set of personal rules for self-coordination,
 *   such as regularizing trivial routines, distrusting large groups, and rejecting
 *   the "magical power" of quantification. The constraint is the protocol itself,
 *   not the societal conditions it responds to.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individual Adherents: Primary beneficiary (powerless/mobile) — uses the protocol to maintain agency and humor.
 *   - Technological Bureaucracies: Adversary (institutional/constrained) — the protocol is a tool for resisting its influence.
 *   - Analytical Observer: Sees the full structure of the protocol as a coordination tool.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(postman_survival_protocol, 0.15).
domain_priors:suppression_score(postman_survival_protocol, 0.30).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(postman_survival_protocol, 0.08).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(postman_survival_protocol, extractiveness, 0.15).
narrative_ontology:constraint_metric(postman_survival_protocol, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(postman_survival_protocol, theater_ratio, 0.08).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(postman_survival_protocol, rope).

% --- Binary flags ---
% The protocol requires active individual enforcement to maintain.
domain_priors:requires_active_enforcement(postman_survival_protocol).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(postman_survival_protocol, individual_adherents).
%
% Who bears disproportionate cost?
% No direct victim. The protocol is a defensive tool; it doesn't extract from
% an adversary, but rather enables exit from extraction.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL ADHERENT (ROPE)
% For the person adopting the protocol, it is a pure coordination tool (Rope).
% It helps regularize personal behavior to gain agency and cope with external pressures.
% The low extraction represents the cognitive overhead of adherence.
constraint_indexing:constraint_classification(postman_survival_protocol, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: THE TECHNOLOGICAL BUREAUCRACY (ROPE)
% From the perspective of the bureaucracy whose influence is being resisted, the
% protocol is a Rope used by individuals to coordinate their defense. It is a
% nuisance that reduces the institution's ability to extract attention and
% compliance, but it is not a direct attack.
constraint_indexing:constraint_classification(postman_survival_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% The analytical observer sees the protocol for its true structure: a low-extraction,
% self-coordination mechanism designed to improve an individual's resilience.
% It is a classic Rope.
constraint_indexing:constraint_classification(postman_survival_protocol, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(postman_survival_protocol_tests).

test(uniform_classification) :-
    % Verify this is a uniform-type constraint (Rope from all perspectives).
    constraint_indexing:constraint_classification(postman_survival_protocol, rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(postman_survival_protocol, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(postman_survival_protocol, rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify metrics are consistent with Rope classification.
    narrative_ontology:constraint_metric(postman_survival_protocol, extractiveness, E),
    E =< 0.45.

:- end_tests(postman_survival_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original story conflated the protocol (the solution) with the societal
 *   problems it addresses (the problem). This version corrects that by focusing
 *   strictly on the protocol itself as the constraint.
 *
 *   - Base Extractiveness (ε=0.15): The protocol is a tool for personal agency.
 *     It does not asymmetrically extract value for a third party. The low ε
 *     represents the cognitive cost and effort required for adherence.
 *   - Suppression Score (0.30): Postman's advice explicitly suppresses certain
 *     behaviors (e.g., "plain speaking," reliance on quantification) in favor
 *     of others (e.g., routine, tact). This is a core function of the protocol.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap. This is a uniform-type constraint,
 *   classifying as a Rope from all major perspectives. For the individual, it's
 *   a tool for self-coordination. For the bureaucracy, it's a coordination
 *   tool used by others to resist its influence. For the analyst, its structure
 *   is clearly that of a Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiary is the `individual_adherents` who use the protocol.
 *   There is no structural victim; the bureaucracy is an adversary whose influence
 *   is evaded, not a party from whom value is extracted by the protocol.
 *
 * MANDATROPHY ANALYSIS:
 *   By correctly identifying the protocol as a low-extraction Rope, we avoid
 *   mislabeling it as a Snare or Mountain based on the environment it operates
 *   in. The high extraction and suppression of the bureaucratic world are
 *   external factors, not properties of this specific constraint. This story
 *   is about a tool to navigate a Snare, it is not the Snare itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_postman_protocol,
    'Does long-term adherence to the protocol lead to genuine agency (Rope), or does it degrade into a theatrical performance of resistance that changes nothing, becoming an inertial Piton?',
    'Longitudinal study comparing life outcomes and self-reported agency of adherents vs. a control group.',
    'If genuine agency -> Rope. If theatrical -> Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(postman_survival_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required, as base_extractiveness (0.15) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The protocol is a standard for processing information and social interaction.
narrative_ontology:coordination_type(postman_survival_protocol, information_standard).

% Network relationships (structural influence edges)
% This protocol is a response to a background condition of bureaucratic opacity.
% The existence of that (hypothetical) constraint affects this one.
narrative_ontology:affects_constraint(bureaucratic_opacity, postman_survival_protocol).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary status
% correctly models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */