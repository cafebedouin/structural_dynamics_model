% ============================================================================
% CONSTRAINT STORY: thermodynamics_entropy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-16
% ============================================================================

:- module(constraint_thermodynamics_entropy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: thermodynamics_entropy
 *   human_readable: The Second Law of Thermodynamics (Entropy)
 *   domain: technological
 *
 * SUMMARY:
 *   The Second Law of Thermodynamics dictates that in any isolated system, the
 *   total entropy (a measure of disorder) must increase or stay the same. This
 *   imposes a fundamental "arrow of time" and a hard limit on the efficiency
 *   of any engine or biological process, representing the inevitable decay of
 *   concentrated energy into unusable heat. It is a universal, unchangeable
 *   feature of the physical world.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Biological Organism: Subject (powerless/trapped) — experiences the law's effects as decay and limitation.
 *   - The Industrial Engineer: User (institutional/arbitrage) — uses the law's predictability to design systems.
 *   - The Cosmologist: Observer (analytical) — studies the law's universal implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% A physical law has no beneficiary to whom value is extracted. Its "cost" is a
% universal condition, not a transfer. Thus, base extractiveness is near zero.
domain_priors:base_extractiveness(thermodynamics_entropy, 0.05).
% Suppression is low because alternatives (like perpetual motion) are logically
% incoherent, not coercively suppressed. The law doesn't fight alternatives;
% it defines the space where no alternative is possible.
domain_priors:suppression_score(thermodynamics_entropy, 0.05).
domain_priors:theater_ratio(thermodynamics_entropy, 0.0).       % A physical law has no performative aspect.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thermodynamics_entropy, extractiveness, 0.05).
narrative_ontology:constraint_metric(thermodynamics_entropy, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(thermodynamics_entropy, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain certification) ---
% Accessibility collapse: entropy increase is inescapable in any isolated system.
% No conceivable alternative exists within the axioms of statistical mechanics.
narrative_ontology:constraint_metric(thermodynamics_entropy, accessibility_collapse, 0.98).
% Resistance: resisting the second law is physically incoherent.
narrative_ontology:constraint_metric(thermodynamics_entropy, resistance, 0.0).

% --- Emergence flag (required for mountain metric gate) ---
domain_priors:emerges_naturally(thermodynamics_entropy).

% --- Constraint claim (must match analytical perspective type) ---
% As a fundamental, unchangeable law of physics, its structural type is Mountain.
narrative_ontology:constraint_claim(thermodynamics_entropy, mountain).
narrative_ontology:human_readable(thermodynamics_entropy, "The Second Law of Thermodynamics (Entropy)").
narrative_ontology:topic_domain(thermodynamics_entropy, "technological").

% --- Binary flags ---
% No active enforcement is required; it is an emergent property of statistical mechanics.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain (natural law), this constraint has no beneficiaries or victims
% in the structural sense of value transfer. No enrichment needed.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because the base metrics (ε ≤ 0.25,
% suppression ≤ 0.05) force a Mountain classification regardless of context.

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% For a biological organism, life is a constant struggle against decay. While
% this *feels* like a Snare, its structural reality is that of an unchangeable
% physical limit—a Mountain.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ENGINEER (MOUNTAIN)
% For an engineer, the law's predictability is a tool. While this utility
% *feels* like a Rope, the law itself is not a coordination mechanism. It is a
% fixed feature of the landscape—a Mountain—that enables coordination.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a universal, analytical view, the Second Law is a quintessential
% Mountain. Its low extraction and suppression scores reflect its status as a
% fundamental, non-coercive, non-extractive feature of reality.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thermodynamics_entropy_tests).

test(classification_invariance) :-
    % Verify the classification is Mountain from all perspectives.
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain,
    TypeAnalytical == mountain.

test(claim_is_mountain) :-
    % The constraint's structural claim must be mountain.
    narrative_ontology:constraint_claim(thermodynamics_entropy, mountain).

test(threshold_validation) :-
    % Verify the constraint's metrics fall within the Mountain thresholds.
    narrative_ontology:constraint_metric(thermodynamics_entropy, extractiveness, E),
    narrative_ontology:constraint_metric(thermodynamics_entropy, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(thermodynamics_entropy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story models a fundamental physical law. The key was to distinguish
 *   between the law's *impact* and its structural *type*. While the impact of
 *   entropy on an ordered system is severe (feels like high extraction), the
 *   law itself does not transfer value to a beneficiary. Therefore, its base
 *   extractiveness (ε) is correctly set to a low value (0.05). Similarly, its
 *   suppression score is low (0.05) because alternatives like perpetual motion
 *   are logically impossible, not coercively suppressed. These metrics firmly
 *   place the constraint in the Mountain category.
 *
 * PERSPECTIVAL INVARIANCE:
 *   There is no perspectival gap. As a Mountain-only constraint, the Second Law
 *   is classified identically from all viewpoints. The *experience* of the law
 *   differs—an organism feels its effects as decay (like a Snare), and an
 *   engineer uses its predictability (like a Rope)—but the underlying
 *   structure is invariant. The framework correctly identifies the objective
 *   structure as a Mountain, while the narrative context captures the subjective
 *   experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   This model resolves the Mandatrophy problem by correctly identifying the
 *   constraint's type. An earlier, incorrect model used a high ε (0.80) to
 *   represent the law's negative impact, which incorrectly triggered Mandatrophy
 *   checks and required a `tangled_rope` or `snare` classification. This was a
 *   category error, conflating universal cost with social extraction. By setting
 *   ε to a structurally correct low value, the constraint is properly
 *   classified as a Mountain, and the Mandatrophy question becomes moot.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_thermodynamics_entropy,
    'Is the universe a truly isolated system subject to heat death, or could unknown physics (e.g., multiverse interactions, vacuum energy renewal) provide an escape, making the law contingent rather than absolute?',
    'Empirical validation of cosmological models beyond the Standard Model.',
    'If absolute (Mountain), all agency is ultimately futile against heat death. If contingent, long-term survival strategies may exist.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(thermodynamics_entropy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% For a physical law, the metrics are constant over any interval, representing
% its unchanging nature. While not strictly required for low-extraction
% constraints, this data demonstrates its stability.
%
% Theater ratio over time (flat at zero):
narrative_ontology:measurement(thermodynamics_entropy_tr_t0, thermodynamics_entropy, theater_ratio, 0, 0.0).
narrative_ontology:measurement(thermodynamics_entropy_tr_t5, thermodynamics_entropy, theater_ratio, 5, 0.0).
narrative_ontology:measurement(thermodynamics_entropy_tr_t10, thermodynamics_entropy, theater_ratio, 10, 0.0).

% Extraction over time (flat at 0.05):
narrative_ontology:measurement(thermodynamics_entropy_ex_t0, thermodynamics_entropy, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(thermodynamics_entropy_ex_t5, thermodynamics_entropy, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(thermodynamics_entropy_ex_t10, thermodynamics_entropy, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or Network data is applicable for a fundamental physical law.
% It does not have a coordination function, nor is it coupled to other social
% constraints in the typical sense.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a Mountain, the classification is invariant,
% and directionality does not alter the outcome.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */