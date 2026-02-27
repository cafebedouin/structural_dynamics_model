% ============================================================================
% CONSTRAINT STORY: birthday_paradox_collison
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birthday_paradox_collison, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: birthday_paradox_collison
 *   human_readable: The Birthday Paradox (Collision Probability)
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   The Birthday Paradox is a mathematical principle, not a social or
 *   institutional constraint in the traditional sense. It demonstrates that
 *   in any finite domain of N possibilities, the probability of a collision
 *   reaches 50% with only sqrt(N) items — far fewer than most people
 *   intuitively expect. This constraint is genuinely invariant across all
 *   observables and measurement methodologies because it derives from
 *   elementary probability theory. No actor, institution, or perspective can
 *   negotiate its mathematical structure. The constraint exhibits zero
 *   degrees of freedom: it is immutable, irreducible, and universal. It
 *   emerges naturally from combinatorial mathematics and cannot be suppressed
 *   or engineered around, only accommodated through design choices like
 *   larger hash spaces. This makes it a canonical mountain constraint — a
 *   natural law of discrete mathematics.
 *
 * KEY AGENTS:
 *   - Mathematical Analyst: Observer of the constraint (analytical/analytical) — perceives the constraint as fundamental combinatorics with no extraction
 *   - Cryptographic System Designer: Powerful agent constrained by the mathematics (powerful/mobile) — must accommodate the birthday bound through key size selection; cannot bypass it
 *   - Standards Body: Organized institutional agent (organized/constrained) — sets cryptographic standards reflecting birthday collision bounds; cannot override the constraint
 *   - System Administrator: Powerless agent trapped by inherited design (powerless/trapped) — experiences the constraint operationally when systems lack sufficient key space; cannot change the mathematics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birthday_paradox_collison, 0.08).
domain_priors:suppression_score(birthday_paradox_collison, 0.02).
domain_priors:theater_ratio(birthday_paradox_collison, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birthday_paradox_collison, extractiveness, 0.08).
narrative_ontology:constraint_metric(birthday_paradox_collison, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(birthday_paradox_collison, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(birthday_paradox_collison, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(birthday_paradox_collison, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birthday_paradox_collison, mountain).
narrative_ontology:human_readable(birthday_paradox_collison, "The Birthday Paradox (Collision Probability)").
narrative_ontology:topic_domain(birthday_paradox_collison, "mathematical/technological").

domain_priors:emerges_naturally(birthday_paradox_collison).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL OBSERVER (MOUNTAIN) — The birthday paradox is a strict mathematical consequence of probability theory. For any finite domain N, the expected collision threshold at ~0.5 probability occurs at sqrt(N) items. This derives from the birthday problem solution: P(collision) = 1 - (N!/((N-k)! * N^k)). The constraint is universal, timeless, and independent of institutional context. No agent experiences extraction because the constraint is not extracting — it is a natural law of combinatorics. Zero degrees of freedom: the mathematics cannot be negotiated, engineered around, or suppressed. Accessibility collapse is maximal — the proof is accessible to any analyst with undergraduate probability; resistance is minimal — no alternative mathematics contradicts it.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: CRYPTOGRAPHIC SYSTEM DESIGNER (MOUNTAIN) — Even with significant power and mobility (ability to choose hash functions, key sizes, collision detection mechanisms), the birthday paradox constraint remains immutable. Designers cannot reduce the sqrt(N) threshold — they can only acknowledge it and size their systems accordingly. A 256-bit hash output has ~2^128 collision resistance by birthday bounds; a 128-bit output has ~2^64. These are not negotiable design choices but mathematical facts. The designer's power lies in selecting N large enough, not in bypassing the constraint. From this perspective too, the constraint is mountain: zero degrees of freedom once domain size is chosen.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: STANDARDS BODY (MOUNTAIN) — Organizations like NIST that set cryptographic standards cannot negotiate with the birthday paradox. When recommending SHA-256 over SHA-1 (due to birthday collision concerns as computers scaled), the constraint was not being overcome — it was being accommodated. The organization's power is constrained by mathematics: standards must reflect the birthday bound or be deprecated. The constraint is immutable and universal across all standards bodies, making it mountain from this institutional view as well.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SYSTEM ADMINISTRATOR (OPERATIONAL VICTIM) — A sysadmin managing a database with insufficient key space may experience the birthday paradox as a operational constraint: collisions are inevitable given the system's design. However, the constraint itself remains mountain — no power, exit, or organizing capacity changes the mathematics. The administrator is trapped not by extraction but by their system's inherited design. The birthday paradox operates at the same mathematical depth from this powerless perspective as from the analyst's perspective. The constraint is universal and immutable even when experienced as a limiting burden.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birthday_paradox_collison_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(birthday_paradox_collison, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birthday_paradox_collison, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(birthday_paradox_collison, ExtMetricName, E),
    domain_priors:suppression_score(birthday_paradox_collison, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(birthday_paradox_collison),
    narrative_ontology:constraint_metric(birthday_paradox_collison, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(birthday_paradox_collison, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(birthday_paradox_collison_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The birthday paradox does not extract value from any agent because it is not an institutional arrangement or power structure — it is a mathematical law. No beneficiary or victim relationship exists. The value reflects only the minimal 'cost' of acknowledgment and accommodation in system design. Suppression (0.02): Near-zero. The constraint cannot be suppressed; it is a direct mathematical fact. The small non-zero value reflects only measurement uncertainty, not actual suppression capacity. Theater ratio (0.15): Minimal. The constraint has almost no performative component. Cryptographic standards either satisfy the birthday bound or they do not — there is no theater in mathematics. The small value accounts for possible pedagogical framing but not functional disguise. All three measurements are stable across the interval (0 to 100 time units) because the mathematical constraint itself does not degrade, strengthen, or shift over time.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is exceptional: there is no perspectival gap. All four perspectives (analytical, powerful, organized, powerless) classify the constraint identically as Mountain. Even the powerless system administrator, who may experience the birthday paradox as an operational burden, recognizes it as an immutable mathematical fact, not extraction by a beneficiary. The constraint's classification is invariant across all indexical positions because the underlying mathematical structure is invariant. This uniform classification is a marker of a genuine natural law: the same observer conclusions hold regardless of power, exit options, time horizon, or spatial scope.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation applies because there are no beneficiaries or victims. The birthday paradox is not an extraction mechanism — it is a mathematical property that applies equally to all observers and all applications. The constraint does not preferentially benefit one agent while burdening another. System designers and administrators who must accommodate the constraint are not being extracted from; they are dealing with a mathematical fact that affects system design uniformly. Even the weakest agent (the powerless administrator) cannot experience worse treatment from the birthday paradox than the most powerful designer — the mathematics applies identically. This absence of differential extraction is a defining feature of mountain constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_collision_resistance,
    'Does quantum computing (Grover''s algorithm) reduce the effective birthday collision threshold from sqrt(N) to a lower polynomial bound, thereby changing the structural constraint?',
    'Demonstration of quantum hardware achieving Grover-accelerated collision-finding in controlled setting; validation of collision-finding cost reduction relative to classical birthday bound predictions',
    'If quantum advantage is realized: the constraint shifts from classical birthday paradox to quantum-accelerated collision finding — a different mathematical structure (not a Mountain in classical cryptography). If quantum advantage does not materialize: classical birthday paradox remains immutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_collision_resistance, empirical, 'Whether quantum algorithms fundamentally change the collision threshold').

omega_variable(
    domain_size_as_parameter,
    'Is the birthday paradox a constraint on collision probability, or merely a parameter-dependent property of any finite domain?',
    'Philosophical analysis of what constitutes a ''constraint'' versus a ''property.'' If domain size is always a choice variable (you can always use N large enough), is the paradox constraining or merely informative?',
    'If the paradox is a mere property: classification might downgrade to Rope (coordination mechanism for choosing N appropriately). If it is a genuine structural constraint: Mountain is correct. This is a conceptual omega, not empirical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_size_as_parameter, conceptual, 'Whether the paradox is a constraint or merely a parametric property').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birthday_paradox_collison, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bday_tr_t0, birthday_paradox_collison, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bday_tr_t50, birthday_paradox_collison, theater_ratio, 50, 0.15).
narrative_ontology:measurement(bday_tr_t100, birthday_paradox_collison, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(bday_be_t0, birthday_paradox_collison, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(bday_be_t50, birthday_paradox_collison, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(bday_be_t100, birthday_paradox_collison, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(birthday_paradox_collison, information_standard).
narrative_ontology:affects_constraint(birthday_paradox_collison, hash_collision_resistance).
narrative_ontology:affects_constraint(birthday_paradox_collison, cryptographic_key_selection).
narrative_ontology:affects_constraint(birthday_paradox_collison, random_number_generator_security).

% DUAL FORMULATION NOTE:
% The birthday paradox is a foundational principle that structures multiple downstream constraints in cryptography and random number generation. It is not itself part of a decomposition (unlike the BGS cluster) but rather a natural law that constrains the feasible parameters of other constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
