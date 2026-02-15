% ============================================================================
% CONSTRAINT STORY: liar_paradox
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_liar_paradox, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
- % in both files' narrative context sections.
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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liar_paradox
 *   human_readable: The Liar Paradox (Self-Referential Inconsistency)
 *   domain: logic/epistemology
 *
 * SUMMARY:
 *   The constraint emerges from the sentence "This statement is false." If the
 *   statement is true, then it must be false; if it is false, then it must be
 *   true. This creates a logical contradiction that functions as a structural
 *   limit on any formal system that allows unrestricted self-reference. It is
 *   a foundational "no-go zone" in classical logic, closely related to
 *   Gödel's incompleteness theorems and the Halting Problem.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Logician (institutional/trapped): Encounters the paradox as an
 *     unbreakable law of their domain, a fundamental boundary condition.
 *   - The Programmer (powerless/mobile): Experiences the paradox as a
 *     recursive error or infinite loop; can exit by rewriting the code.
 *   - The Philosopher (analytical/analytical): Studies the paradox as an
 *     object, mapping its implications for theories of truth and language.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liar_paradox, 0.05).
domain_priors:suppression_score(liar_paradox, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(liar_paradox, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liar_paradox, extractiveness, 0.05).
narrative_ontology:constraint_metric(liar_paradox, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(liar_paradox, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(liar_paradox, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(liar_paradox, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(liar_paradox, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the structure of logic and language
% without human design or enforcement. Required for the mountain metric gate.
domain_priors:emerges_naturally(liar_paradox).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), the Liar Paradox does not
% have structural beneficiaries or victims in the sense of asymmetric
% extraction. Its effects are universal and symmetric for any system
% operating under its rules.

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

% UNIFORM-TYPE CONSTRAINT: MOUNTAIN
% As a natural law of logic, the Liar Paradox classifies as a Mountain from
% all perspectives. Its status as an unchangeable feature of reality is not
% dependent on the observer's power, time horizon, or exit options. The
% following perspectives demonstrate this invariance.

% PERSPECTIVE 1: THE INSTITUTIONAL LOGICIAN
% For the logician working within a formal system, the paradox is an
% unchangeable feature of the landscape. They are trapped by the rules of
% their system and cannot wish the paradox away.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE POWERLESS PROGRAMMER
% For a programmer, the paradox manifests as an infinite loop or a type error
% they cannot resolve. While they can exit by rewriting the specific piece of
% code (mobile exit), the underlying logical constraint remains a Mountain
% that they must code around.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The philosopher or meta-mathematician observes the paradox as a fundamental
% law. Their analytical stance does not change its nature. This is the default
% analytical context.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liar_paradox_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain across all tested perspectives.
    constraint_indexing:constraint_classification(liar_paradox, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(liar_paradox, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liar_paradox, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_metric_thresholds) :-
    % Verify that base metrics adhere to the Mountain classification thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    config:param(mountain_extractiveness_max, MountainEpsMax),
    config:param(mountain_suppression_ceiling, MountainSuppMax),
    narrative_ontology:constraint_metric(liar_paradox, ExtMetricName, E),
    narrative_ontology:constraint_metric(liar_paradox, SuppMetricName, S),
    E =< MountainEpsMax,
    S =< MountainSuppMax.

test(natural_law_profile_present) :-
    % Verify that the required NL profile data is present for certification.
    domain_priors:emerges_naturally(liar_paradox),
    narrative_ontology:constraint_metric(liar_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(liar_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(liar_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Liar Paradox is a canonical example of a Mountain constraint. It is a
 *   natural law of any logical system sufficiently expressive to allow
 *   self-reference.
 *   - Extractiveness (0.05): Represents the minimal cognitive or computational
 *     overhead required to navigate around the paradox. It doesn't extract
 *     value, but it forecloses certain direct logical paths.
 *   - Suppression (0.01): The paradox cannot be suppressed. Resistance is
 *     incoherent. Alternatives like paraconsistent logics do not eliminate
 *     the paradox within classical systems; they create new systems with
 *     different rules.
 *   - NL Profile: Accessibility collapse is 1.0 because no alternative is
 *     conceivable within its logical frame. Resistance is near-zero. It
 *     emerges naturally from the structure of language.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a natural law, its classification is
 *   invariant. A programmer might have 'mobile' exit from a specific instance
 *   (rewriting code), but this doesn't change the nature of the underlying
 *   logical law, which remains a Mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear Mountain, preventing misclassification.
 *   Previous attempts to frame it as a Snare (e.g., used by mainstream
 *   logicians to suppress dialetheism) commit a category error. The social
 *   enforcement of a particular logical paradigm is a separate, human-created
 *   constraint (likely a Tangled Rope), not a property of the paradox itself.
 *   The ε-invariance principle requires separating the logical law (liar_paradox,
 *   ε=0.05) from the academic normativity (e.g., peer_review_orthodoxy, ε=0.55).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_liar_paradox,
    'Is logic a feature of the universe (Platonism) or a construct of human cognition?',
    'Discovery of non-human intelligence with a fundamentally different, yet effective, logical framework.',
    'If universal, the paradox is a physical limit like gravity. If cognitive, it is a bug in our specific mental "operating system" that another intelligence might not share.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_liar_paradox, conceptual, 'Whether logic is discovered (Platonic) or invented (Cognitive construct).').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(liar_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a Mountain with base_extractiveness < 0.46, this
% constraint does not exhibit lifecycle drift. Its properties are static.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this fundamental logical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. As a Mountain, there are no beneficiaries or victims, so
% directionality derivation is not used.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */