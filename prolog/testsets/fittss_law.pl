% ============================================================================
% CONSTRAINT STORY: fitts_law_industrial_application
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_fitts_law_industrial_application, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fitts_law_industrial_application
 *   human_readable: Fitts’s Law (Industrial Application)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint models the industrial and economic application of Fitts's
 *   Law, a model of human movement. While the underlying biological law is a
 *   Mountain, its application in workplace design to maximize throughput
 *   functions as a Tangled Rope. It provides a genuine coordination function
 *   for UI/UX designers and industrial engineers, while simultaneously enabling
 *   the severe extraction of motor vitality from repetitive task workers,
 *   leading to conditions like RSI.
 *
 * KEY AGENTS (by structural relationship):
 *   - Repetitive Task Workers: Primary target (powerless/trapped) — bears extraction of physical health and motor vitality through high-speed, repetitive tasks.
 *   - Industrial Engineers & UI Designers: Primary beneficiary (institutional/arbitrage) — uses the law as a coordination tool to optimize workflows and system efficiency.
 *   - Analytical Observer: Sees the full structure, recognizing both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: High (0.6). The application of the law in high-throughput
% environments extracts the long-term physical health (motor vitality) of
% workers in exchange for marginal gains in speed, leading to RSI.
domain_priors:base_extractiveness(fitts_law_industrial_application, 0.6).

% Rationale: High (0.7). The economic pressure for maximum efficiency
% suppresses alternatives like slower work paces, more frequent breaks, or
% capital-intensive automation, making the high-extraction work model the
% only viable option for workers.
domain_priors:suppression_score(fitts_law_industrial_application, 0.7).

% Rationale: Low (0.05). The constraint is highly functional; its application
% directly relates to measured output, with little performative theater.
domain_priors:theater_ratio(fitts_law_industrial_application, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fitts_law_industrial_application, extractiveness, 0.6).
narrative_ontology:constraint_metric(fitts_law_industrial_application, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fitts_law_industrial_application, theater_ratio, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fitts_law_industrial_application, tangled_rope).
narrative_ontology:human_readable(fitts_law_industrial_application, "Fitts’s Law (Industrial Application)").
narrative_ontology:topic_domain(fitts_law_industrial_application, "technological/economic").

% --- Binary flags ---
% Rationale: The economic benefits are enforced through quotas, performance
% monitoring systems, and the threat of termination for underperformance.
domain_priors:requires_active_enforcement(fitts_law_industrial_application).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fitts_law_industrial_application, industrial_engineers_and_ui_designers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fitts_law_industrial_application, repetitive_task_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE REPETITIVE TASK WORKER (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(fitts_law_industrial_application, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE UI DESIGNER / INDUSTRIAL ENGINEER (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(fitts_law_industrial_application, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the coordination function (Rope for
% designers) and the severe extraction (Snare for workers).
constraint_indexing:constraint_classification(fitts_law_industrial_application, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fitts_law_industrial_application_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the worker sees a Snare while the designer sees a Rope.
    constraint_indexing:constraint_classification(fitts_law_industrial_application, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fitts_law_industrial_application, rope,
        context(agent_power(institutional), _, _, _)).

test(analytical_claim_consistency) :-
    % Verify the analytical perspective matches the declared constraint claim.
    narrative_ontology:constraint_claim(fitts_law_industrial_application, ClaimType),
    constraint_indexing:constraint_classification(fitts_law_industrial_application, ClaimType,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify all structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(fitts_law_industrial_application, _), % has_coordination_function
    narrative_ontology:constraint_victim(fitts_law_industrial_application, _),     % has_asymmetric_extraction
    domain_priors:requires_active_enforcement(fitts_law_industrial_application).

:- end_tests(fitts_law_industrial_application_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file conflated the biological law (a Mountain, ε≈0) with its
 *   industrial application, creating a linter conflict. This version resolves
 *   the conflict by modeling only the application. The base extractiveness (ε)
 *   is set to 0.6 to reflect the severe extraction of long-term health (RSI)
 *   from workers in optimized, high-throughput environments. Suppression is
 *   high (0.7) because economic pressures and system design make alternatives
 *   (slower pace, automation) inaccessible to the worker.
 *
 * PERSPECTIVAL GAP:
 *   - The worker (powerless/trapped) experiences the system as a Snare. The
 *     "efficiency" of the system is a trap that extracts their physical
 *     well-being for productivity metrics they don't control.
 *   - The designer (institutional/arbitrage) sees it as a Rope. For them,
 *     Fitts's Law is a pure coordination tool to create usable, efficient
 *     interfaces, with the extractive consequences externalized onto the user.
 *   - The analyst sees the complete picture: a Tangled Rope, where a genuine
 *     coordination function is inextricably linked to asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'industrial_engineers_and_ui_designers' gain efficiency, predictability, and control over system throughput.
 *   - Victims: 'repetitive_task_workers' bear the costs through physical strain, injury, and burnout. The system extracts value from their motor vitality.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the application as a Tangled Rope, we avoid two errors.
 *   First, we avoid mislabeling a system with a clear coordination function
 *   as a pure Snare. Second, and more importantly, we avoid the "false natural
 *   law" error of calling the entire system a Mountain, which would obscure
 *   the severe, policy-driven extraction imposed on workers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_fitts_law_neuroplasticity,
    'Can neural interfaces or advanced haptic feedback fundamentally alter the speed-accuracy tradeoff, effectively bypassing the biological limit that this industrial application exploits?',
    'Long-term study of Brain-Machine Interface (BMI) performance in target acquisition tasks vs. traditional motor input.',
    'If the limit can be bypassed, this constraint degrades from a Tangled Rope leveraging a Mountain to a pure Snare of technological inequality. If not, the biological limit remains the hard floor for extraction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fitts_law_industrial_application, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the intensification of extraction over time, from early
% Taylorism to modern algorithmic management, while the theatrical component
% remains negligible.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(fitts_law_app_tr_t0, fitts_law_industrial_application, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fitts_law_app_tr_t5, fitts_law_industrial_application, theater_ratio, 5, 0.05).
narrative_ontology:measurement(fitts_law_app_tr_t10, fitts_law_industrial_application, theater_ratio, 10, 0.05).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(fitts_law_app_ex_t0, fitts_law_industrial_application, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(fitts_law_app_ex_t5, fitts_law_industrial_application, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(fitts_law_app_ex_t10, fitts_law_industrial_application, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The law provides a standard for predicting human motor performance, used in design.
narrative_ontology:coordination_type(fitts_law_industrial_application, information_standard).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint.
%
% DUAL FORMULATION NOTE:
% This constraint is one of two stories decomposed from "Fitts's Law".
% Decomposed because ε differs across observables (ε-invariance principle).
% The biological law is a Mountain; its industrial application is a Tangled Rope.
% Related stories:
%   - fitts_law_biological_limit (ε=0.05, Mountain)
%   - fitts_law_industrial_application (ε=0.60, Tangled Rope) - This file.
%
narrative_ontology:affects_constraint(fitts_law_biological_limit, fitts_law_industrial_application).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */