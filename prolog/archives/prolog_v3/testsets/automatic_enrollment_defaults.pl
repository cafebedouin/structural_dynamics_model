% ============================================================================
% CONSTRAINT STORY: automatic_enrollment_defaults
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_automatic_enrollment_defaults, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: automatic_enrollment_defaults
 *   human_readable: Automatic Enrollment Defaults in Retirement Plans
 *   domain: economic/social
 *
 * SUMMARY:
 *   Automatic enrollment is a choice architecture where individuals are placed
 *   into a program (like a 401k retirement plan) by default, rather than
 *   requiring them to actively opt-in. It utilizes the cognitive "status quo
 *   bias" to dramatically increase participation rates, aiming to improve
 *   long-term financial outcomes for employees. This file models the policy
 *   itself, which is a human-designed system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Employees with inertia: Primary beneficiary (powerless/trapped) — benefits from overcoming cognitive friction to save for retirement.
 *   - Choice Architects (Employers/Policymakers): Primary beneficiary (institutional/arbitrage) — benefits by fulfilling fiduciary duties and improving workforce financial health.
 *   - Employees preferring active choice: Primary target (powerless/trapped) — bears the cost of having a decision made for them, even if easily reversible.
 *   - Analytical Observer: Sees the full structure as a low-extraction coordination mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extremely low; the policy is designed to retain value for the individual
% by overcoming their own cognitive friction. The "extraction" is of pure
% autonomy, not financial value.
domain_priors:base_extractiveness(automatic_enrollment_defaults, 0.05).
% Low; while the default is passive, the opt-out mechanism is usually simple
% and clearly communicated, imposing a low barrier to exit.
domain_priors:suppression_score(automatic_enrollment_defaults, 0.10).
% Very low; the system is functional, not performative. Its success is measured
% by participation rates and savings balances, not theatrical compliance.
domain_priors:theater_ratio(automatic_enrollment_defaults, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(automatic_enrollment_defaults, extractiveness, 0.05).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, suppression_requirement, 0.10).
narrative_ontology:constraint_metric(automatic_enrollment_defaults, theater_ratio, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(automatic_enrollment_defaults, rope).
narrative_ontology:human_readable(automatic_enrollment_defaults, "Automatic Enrollment Defaults in Retirement Plans").
narrative_ontology:topic_domain(automatic_enrollment_defaults, "economic/social").

% --- Binary flags ---
% The policy requires active, ongoing implementation by payroll systems for each
% new employee. This constitutes active enforcement, which prevents the engine
% from misclassifying this Rope as a Scaffold (see Mandatrophy Analysis).
domain_priors:requires_active_enforcement(automatic_enrollment_defaults).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, employees_with_inertia).
narrative_ontology:constraint_beneficiary(automatic_enrollment_defaults, employers).

% Who bears disproportionate cost? (The cost is loss of active choice, not money)
narrative_ontology:constraint_victim(automatic_enrollment_defaults, employees_preferring_active_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% This is a uniform-type constraint (Rope-only). The classification is stable
% across all perspectives because the base extractiveness is extremely low.
% The perspectival gap is reflected in the sign of effective extraction (χ):
% negative for beneficiaries (a subsidy), positive but tiny for victims.

% PERSPECTIVE 1: THE EMPLOYEE (TARGET/BENEFICIARY)
% For an employee with inertia, this is a helpful Rope that coordinates their
% future self's interest. For one who prefers active choice, it's still a Rope
% due to low extraction and easy exit, but one with a small positive χ.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE EMPLOYER / CHOICE ARCHITECT (BENEFICIARY)
% For the employer, this is a classic Rope. It is a powerful, low-cost tool
% to coordinate employee behavior toward a beneficial outcome (higher retirement
% savings), increasing employee well-being and fulfilling fiduciary duties.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The observer sees a pure coordination mechanism with negligible extraction,
% classifying it as a Rope. The underlying cognitive bias is a Mountain, but
% the policy built upon it is a Rope.
constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(automatic_enrollment_defaults_tests).

test(uniform_rope_classification) :-
    % Verify that this is a uniform-type constraint, classifying as Rope
    % from multiple key perspectives.
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(automatic_enrollment_defaults, rope, context(agent_power(analytical), _, _, _)).

test(low_extraction_and_suppression) :-
    narrative_ontology:constraint_metric(automatic_enrollment_defaults, extractiveness, E),
    narrative_ontology:constraint_metric(automatic_enrollment_defaults, suppression_requirement, S),
    E < 0.1,
    S < 0.2.

:- end_tests(automatic_enrollment_defaults_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics (ε=0.05, suppression=0.10) reflect a "libertarian paternalistic"
 *   constraint designed to be beneficial while preserving choice (via opt-out).
 *   The core function is coordination—aligning present actions with future
 *   well-being—making it a canonical Rope.
 *
 * PERSPECTIVAL GAP:
 *   This is a uniform-type constraint (Rope-only). The perspectival gap is not
 *   in the classification type but in the computed value of effective extraction (χ).
 *   - For beneficiaries (employees with inertia), d is low, f(d) is negative, and χ is negative, representing a subsidy against their own cognitive friction.
 *   - For victims (employees preferring active choice), d is high, f(d) is positive, and χ is positive but extremely small.
 *   Both scenarios fall well within the Rope classification thresholds.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are employees who would not have saved otherwise, and employers
 *   who fulfill their fiduciary duty. The "victim" is the abstract principle of
 *   active choice, or any employee who is defaulted into a suboptimal fund and
 *   fails to opt-out. The structural data reflects this by naming both groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The extremely low base extractiveness (ε=0.05) and clear coordination function
 *   (increasing retirement savings) robustly defend against misclassification as
 *   a Snare. Even from the perspective of a 'trapped' powerless agent, the
 *   effective extraction χ is far too low to meet the Snare threshold (χ ≥ 0.66).
 *   This demonstrates how the framework's quantitative approach distinguishes
 *   between benevolent nudges (Ropes) and coercive traps (Snares).
 *
 *   This constraint is a canonical example of the "Scaffold Danger Zone": its
 *   low extraction, lack of a sunset clause, and coordination function could
 *   lead the engine to misclassify it as a Scaffold. The declaration of
 *   `requires_active_enforcement/1` resolves this ambiguity. The ongoing
 *   action by payroll systems to enroll new employees is a form of active
 *   enforcement, which correctly disables the engine's Scaffold classification
 *   gate, ensuring it is classified as a Rope.
 *
 *   NOTE ON DECOMPOSITION: This policy (a Rope) is constructed upon the
 *   `cognitive_status_quo_bias` (a Mountain). A complete analysis would involve
 *   a separate story for the cognitive bias, linked via `affects_constraint`.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_auto_enroll,
    'Is the default option chosen for the user''s best interest (Rope) or for the benefit of the provider (e.g., defaulting into a high-fee fund, making it a Tangled Rope)?',
    'Auditing the financial incentives of the choice architect vs. the long-term outcomes for the user across different default funds.',
    'If for user: A benevolent Rope. If for provider: An extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_auto_enroll, empirical, 'Is the default option truly in the employee''s best interest, or does it benefit the provider via high fees?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(automatic_enrollment_defaults, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a stable Rope, so metrics are flat. Data is included for completeness
% as extraction is below the 0.46 threshold.
% Theater ratio over time:
narrative_ontology:measurement(auto_enroll_tr_t0, automatic_enrollment_defaults, theater_ratio, 0, 0.05).
narrative_ontology:measurement(auto_enroll_tr_t5, automatic_enrollment_defaults, theater_ratio, 5, 0.05).
narrative_ontology:measurement(auto_enroll_tr_t10, automatic_enrollment_defaults, theater_ratio, 10, 0.05).

% Extraction over time:
narrative_ontology:measurement(auto_enroll_ex_t0, automatic_enrollment_defaults, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(auto_enroll_ex_t5, automatic_enrollment_defaults, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(auto_enroll_ex_t10, automatic_enrollment_defaults, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy coordinates the allocation of future resources (income) to savings.
narrative_ontology:coordination_type(automatic_enrollment_defaults, resource_allocation).

% Network relationships (structural influence edges)
% This policy is built upon a fundamental aspect of human psychology.
% narrative_ontology:affects_constraint(cognitive_status_quo_bias, automatic_enrollment_defaults).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */