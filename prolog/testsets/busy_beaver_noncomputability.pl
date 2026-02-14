% ============================================================================
% CONSTRAINT STORY: busy_beaver_noncomputability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_busy_beaver_noncomputability, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: busy_beaver_noncomputability
 *   human_readable: The Non-Computability of the Busy Beaver Function (Σ)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   The Busy Beaver function, Σ(n), defines the maximum number of steps a
 *   halting Turing machine with n states can take. It is a well-defined but
 *   non-computable function that grows faster than any computable function.
 *   This constraint represents an absolute, unchangeable limit on algorithmic
 *   prediction and optimization, stemming directly from the Halting Problem.
 *   It is a fundamental boundary of what is knowable through computation.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Turing Machine (Subject): A powerless agent whose behavior is
 *     absolutely bounded by this logical limit.
 *   - The Theoretical Computer Scientist (Observer): An institutional agent
 *     who observes the limit as a fixed feature of the logical landscape.
 *   - The Brute-Force Searcher (External Agent): An agent whose *activity*
 *     of trying to compute Σ(n) is highly extractive, but this activity is a
 *     separate constraint from the mathematical fact of non-computability itself.
 *   - Analytical Observer: Sees the full structure as a pure logical limit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a pure mathematical fact, the constraint has no inherent
% extractiveness or suppression. It is a feature of the logical universe.
% The low, non-zero values represent the minimal cognitive overhead to grasp
% the concept. It does not extract resources or suppress alternatives; it
% defines a space where no computational alternatives exist.
domain_priors:base_extractiveness(busy_beaver_noncomputability, 0.05).
domain_priors:suppression_score(busy_beaver_noncomputability, 0.05).
domain_priors:theater_ratio(busy_beaver_noncomputability, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, extractiveness, 0.05).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain. Without them,
% the NL signature defaults to 0.5 and fails certification.
% Accessibility collapse is 1.0 because no alternative is conceivable.
% Resistance is 0.0 because active opposition is logically incoherent.
narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(busy_beaver_noncomputability, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the axioms of computation without
% human design or enforcement. Required for the mountain metric gate.
domain_priors:emerges_naturally(busy_beaver_noncomputability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a pure Mountain (natural law), this constraint has no structural
% beneficiaries or victims. The concepts do not apply. No enrichment needed.

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

% PERSPECTIVE 1: THE TURING MACHINE (MOUNTAIN)
% For a specific machine, its maximum step count is bounded by an absolute
% logical limit. It cannot "choose" to run longer. The limit is a fixed
% feature of its mathematical universe, unyielding and absolute.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE THEORETICAL COMPUTER SCIENTIST (MOUNTAIN)
% For the researcher, the function is an unchangeable feature of the
% computational landscape. It is a fixed point of reference used to prove
% other theorems about computability. It cannot be altered or negotiated.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical perspective confirms the constraint is a fundamental,
% unchangeable limit of logic, classifying it as a Mountain.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(busy_beaver_noncomputability_tests).

test(is_uniform_mountain) :-
    % Verify it is classified as a Mountain from all key perspectives.
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_threshold_adherence) :-
    % Verify metrics are within the allowed range for a Mountain classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, ExtMetricName, E),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(busy_beaver_noncomputability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The non-computability of the Busy Beaver function is a pure Mountain, a
 *   fundamental law of computation with near-zero intrinsic extraction and
 *   suppression. The metrics (ε=0.05, S=0.05) reflect its status as a feature
 *   of the logical universe, not a human-imposed rule. The Natural Law profile
 *   metrics (accessibility_collapse=1.0, resistance=0.0) and the
 *   emerges_naturally flag confirm this, allowing it to pass the engine's
 *   strict certification for Mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a true logical limit, the constraint is a
 *   Mountain from all perspectives. Any attempt to show a 'Snare' perspective
 *   for a brute-force searcher would be a misapplication of the framework. The
 *   mathematical fact of non-computability (the Mountain) is distinct from the
 *   human activity of trying to defy it. That activity, 'brute_force_bb_search',
 *   would be a separate constraint story with a very high ε, correctly
 *   classified as a Snare. This follows the ε-invariance principle: if changing
 *   the observable (from the mathematical theorem to the human search activity)
 *   changes ε, you have two different constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   As a pure Mountain, the concepts of beneficiary and victim do not apply.
 *   The constraint is a symmetric, unchangeable fact of the logical universe.
 *   Therefore, no beneficiary/victim declarations are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This regeneration prevents a "false natural law" error. By enforcing the
 *   low-extraction, low-suppression signature of a Mountain, we avoid
 *   mislabeling a fundamental limit as an extractive or coercive mechanism.
 *   The perceived "extraction" felt by a searcher is not a property of the
 *   limit itself, but of the searcher's choice to expend resources against an
 *   immovable object.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_busy_beaver_unprovability,
    'At what value of n does the specific value of Σ(n) become independent of ZFC, making it unknowable within standard mathematics?',
    'Investigation into the smallest Turing machine that simulates a search for a contradiction in ZFC.',
    'If n is small (e.g., < 1000), it implies that the boundary of mathematical provability is much closer than anticipated.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(busy_beaver_noncomputability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a pure Mountain with base_extractiveness <= 0.46, temporal data is not required.
% The constraint is static and does not drift over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% This constraint is a direct consequence of the Halting Problem.
narrative_ontology:affects_constraint(halting_problem_undecidability, busy_beaver_noncomputability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. As a Mountain, directionality is not a factor.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */