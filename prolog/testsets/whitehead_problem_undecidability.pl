% ============================================================================
% CONSTRAINT STORY: whitehead_problem_undecidability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_whitehead_problem_undecidability, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: whitehead_problem_undecidability
 *   human_readable: The Whitehead Problem (Group Theory Undecidability)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Whitehead Problem asks whether every abelian group A such that every
 *   extension of the integers by A is split (a "Whitehead group") must be a free
 *   abelian group. Saharon Shelah proved in 1974 that this is undecidable in
 *   standard Zermelo-Fraenkel set theory with the Axiom of Choice (ZFC). This
 *   undecidability is a fixed, unchangeable feature of the ZFC axiomatic system,
 *   functioning as a Mountain of logic. It establishes a hard limit on what can
 *   be proven about certain infinite algebraic structures within that system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Classical Algebraist: Seeker of a definitive "yes" or "no" answer, who
 *     encounters the logical limit (powerless/trapped).
 *   - Set Theorist/Logician: Uses the undecidability to explore different models
 *     of set theory (institutional/arbitrage).
 *   - Analytical Observer: Recognizes the undecidability as a structural property
 *     of the ZFC system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: A proven mathematical theorem has near-zero extraction. It is a
% statement of fact about a logical system.
domain_priors:base_extractiveness(whitehead_problem_undecidability, 0.05).
% Rationale: The theorem does not suppress alternatives; it reveals the
% existence of alternative models of set theory where the answer is different.
domain_priors:suppression_score(whitehead_problem_undecidability, 0.01).
% Rationale: Formal mathematical truth has no performative component.
domain_priors:theater_ratio(whitehead_problem_undecidability, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(whitehead_problem_undecidability, extractiveness, 0.05).
narrative_ontology:constraint_metric(whitehead_problem_undecidability, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(whitehead_problem_undecidability, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Rationale: The logical conclusion is inescapable within ZFC.
narrative_ontology:constraint_metric(whitehead_problem_undecidability, accessibility_collapse, 1.0).
% Rationale: One cannot "resist" a mathematical proof, only change the axioms.
narrative_ontology:constraint_metric(whitehead_problem_undecidability, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(whitehead_problem_undecidability, mountain).
narrative_ontology:human_readable(whitehead_problem_undecidability, "The Whitehead Problem (Group Theory Undecidability)").
narrative_ontology:topic_domain(whitehead_problem_undecidability, "mathematical/logical").

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the axioms of ZFC and the rules of logic.
domain_priors:emerges_naturally(whitehead_problem_undecidability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (a logical fact), the constraint has no
% inherent beneficiaries or victims. The impact on different groups is a
% downstream effect of their relationship to the axiomatic system itself.

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

% PERSPECTIVE 1: THE CLASSICAL ALGEBRAIST (MOUNTAIN)
% For an algebraist seeking a single, definitive answer within ZFC, the
% undecidability is an unyielding logical barrier.
constraint_indexing:constraint_classification(whitehead_problem_undecidability, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE SET THEORIST / LOGICIAN (MOUNTAIN)
% For the logician, the undecidability is also a fixed fact, but one that
% enables the exploration of different models (e.g., V=L vs. MA+¬CH). The
% fact itself remains a Mountain.
constraint_indexing:constraint_classification(whitehead_problem_undecidability, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% Analytically, the undecidability is a fundamental, structural property of
% the ZFC axiomatic system.
constraint_indexing:constraint_classification(whitehead_problem_undecidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(whitehead_problem_undecidability_tests).

test(classification_invariance) :-
    % Verify that as a Mountain, the classification is invariant.
    constraint_indexing:constraint_classification(whitehead_problem_undecidability, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(whitehead_problem_undecidability, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(whitehead_problem_undecidability, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(mountain_metric_thresholds) :-
    % Verify metrics are within Mountain thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(whitehead_problem_undecidability, ExtMetricName, E),
    narrative_ontology:constraint_metric(whitehead_problem_undecidability, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_present) :-
    % Verify the NL profile metrics required for certification are present.
    domain_priors:emerges_naturally(whitehead_problem_undecidability),
    narrative_ontology:constraint_metric(whitehead_problem_undecidability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(whitehead_problem_undecidability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(whitehead_problem_undecidability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is re-classified as a pure Mountain. Shelah's proof
 *   establishes a logical fact about the ZFC system. Such facts are the
 *   canonical example of Mountain constraints: fixed, unchangeable, and with
 *   near-zero intrinsic extraction or suppression. The original file's attempt
 *   to classify it as a Tangled Rope/Snare based on the "metaphysical tax" on
 *   researchers conflates the constraint itself with the downstream reaction to
 *   it. The framework requires classifying the structural fact, which is a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a Mountain of logic, the classification is
 *   invariant across all observers. The *implications* of the mountain differ
 *   (an obstacle for one, a tool for another), but the constraint's type does not.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. As a Mountain, there are no beneficiaries or victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Mountain prevents the mislabeling of a fundamental
 *   logical limit as a form of social or economic extraction. It correctly
 *   locates the constraint in the realm of logical necessity rather than
 *   contingent social arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_whitehead_problem_undecidability,
    'Does a "natural" or "standard" model of set theory exist that is universally accepted and resolves the Whitehead problem?',
    'Broad consensus among mathematicians and logicians on a canonical extension to ZFC (e.g., accepting large cardinal axioms as standard).',
    'If consensus emerges, the problem becomes decidable (a resolved Mountain). If not, it remains a permanent feature of axiomatic pluralism.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_whitehead_problem_undecidability, conceptual, 'Whether a canonical "true" model of set theory exists to resolve the undecidability.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(whitehead_problem_undecidability, 1974, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness is below the 0.46 threshold for mandatory
% temporal tracking. As a mathematical theorem, its properties are static.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable. As a pure Mountain, it has no coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */