% ============================================================================
% CONSTRAINT STORY: hygiene_disposal_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-19
% ============================================================================

:- module(constraint_hygiene_disposal_protocol, []).

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hygiene_disposal_protocol
 *   human_readable: Institutional Hygiene Protocol (Incineration)
 *   domain: clinical/institutional
 *
 * SUMMARY:
 *   A terminal safety rule triggered by Scarlet Fever. The protocol demands
 *   the destruction of all contaminated objects to prevent the spread of
 *   disease. This creates a total perspectival gap: a life-saving
 *   coordination mechanism for the human household, but a fatal, purely
 *   extractive event for the Velveteen Rabbit.
 *
 * KEY AGENTS (by structural relationship):
 *   - Velveteen Rabbit (as proxy for all contaminated objects): Primary target (powerless/trapped) — faces total destruction.
 *   - The Doctor (as proxy for public health institutions): Primary beneficiary (institutional/arbitrage) — enacts the protocol to mitigate risk.
 *   - Household Residents: Secondary beneficiaries (moderate/constrained) — benefit from the removal of a biological hazard.
 *   - Analytical Observer: Sees the full structure of the perspectival gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hygiene_disposal_protocol, 0.92).
domain_priors:suppression_score(hygiene_disposal_protocol, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(hygiene_disposal_protocol, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hygiene_disposal_protocol, extractiveness, 0.92).
narrative_ontology:constraint_metric(hygiene_disposal_protocol, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(hygiene_disposal_protocol, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hygiene_disposal_protocol, snare).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hygiene_disposal_protocol, household_residents).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hygiene_disposal_protocol, contaminated_objects).
%
% Gate requirements:
%   Snare: victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% For the Rabbit, trapped in a sack with no exit, the extraction is total.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(hygiene_disposal_protocol, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For the Doctor, this is a standard, non-extractive coordination mechanism for public health.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(hygiene_disposal_protocol, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical observer sees the high base extraction and suppression,
% classifying it as a Snare despite its coordination function for one group.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(hygiene_disposal_protocol, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hygiene_disposal_protocol_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(hygiene_disposal_protocol, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hygiene_disposal_protocol, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(threshold_validation) :-
    % Verify high extraction and suppression for a snare.
    narrative_ontology:constraint_metric(hygiene_disposal_protocol, extractiveness, E),
    narrative_ontology:constraint_metric(hygiene_disposal_protocol, suppression_requirement, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(hygiene_disposal_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε) is 0.92 because the protocol requires the
 *   literal physical destruction of the target entity. Suppression is 0.95
 *   because the protocol, backed by medical authority, permits no alternatives
 *   like sterilization; destruction is the only option. Theater is very low
 *   (0.10) as this is a purely functional, non-performative act.
 *
 * PERSPECTIVAL GAP:
 *   The gap is absolute. For the Rabbit (target), the protocol is a Snare
 *   leading to its end. For the Doctor (beneficiary), it is a Rope—a standard
 *   public health coordination tool with no perceived extraction. This is a
 *   canonical example of how a single constraint can be both a coordination
 *   mechanism and a pure extraction mechanism, depending entirely on the
 *   observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The `contaminated_objects` group is the victim, leading to a high
 *   directionality (d ≈ 1.0) and thus high effective extraction (χ) for the
 *   powerless agent. The `household_residents` are beneficiaries, leading to
 *   a low directionality (d ≈ 0.0) and negative χ for the institutional agent
 *   who is enforcing the protocol on their behalf.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] The high extraction score (0.92) is not a
 *   misclassification of a complex coordination problem. The framework
 *   correctly identifies that while the protocol *has* a coordination function
 *   (public health), its effect on the target is purely extractive and
 *   terminal. The system avoids mislabeling this as a Tangled Rope by
 *   recognizing that from the target's trapped perspective, there is no
 *   coordination benefit whatsoever, only total loss. The classification
 *   correctly resolves to Snare for the target and Rope for the beneficiary,
 *   capturing the irreconcilable nature of their experiences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_hazard_misclassification,
    'Is the Rabbit truly a biological hazard if he has become "Real"?',
    'Laboratory testing of metaphysical vs. biological contagion.',
    'If magic purifies germs, the Snare is an error of institutional bias against non-clinical ontologies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omega_hazard_misclassification, conceptual, 'Conflict between clinical and magical ontologies.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hygiene_disposal_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% This models the protocol's potential danger escalating as the diagnosis is confirmed.

% Theater ratio over time (consistently low and functional):
narrative_ontology:measurement(disposal_tr_t0, hygiene_disposal_protocol, theater_ratio, 0, 0.10).
narrative_ontology:measurement(disposal_tr_t5, hygiene_disposal_protocol, theater_ratio, 5, 0.10).
narrative_ontology:measurement(disposal_tr_t10, hygiene_disposal_protocol, theater_ratio, 10, 0.10).

% Extraction over time (spikes as the fever is diagnosed and protocol enacted):
narrative_ontology:measurement(disposal_ex_t0, hygiene_disposal_protocol, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(disposal_ex_t5, hygiene_disposal_protocol, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(disposal_ex_t10, hygiene_disposal_protocol, base_extractiveness, 10, 0.92).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% This Snare is triggered by the network contamination of the boy's illness,
% which in turn was enabled by the Rabbit's transformation into a "Real" object
% worthy of devotion.
narrative_ontology:affects_constraint(devotional_transformation, hygiene_disposal_protocol).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */