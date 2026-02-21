% ============================================================================
% CONSTRAINT STORY: requirement_invasive_diabetes_testing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_invasive_diabetes_testing, []).

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
 *   constraint_id: requirement_invasive_diabetes_testing
 *   human_readable: The Requirement for Invasive Blood Testing to Diagnose and Monitor Diabetes
 *   domain: technological/economic
 *
 * SUMMARY:
 *   For decades, the standard of care for diagnosing and monitoring diabetes
 *   has required invasive blood tests (e.g., finger-prick, venous draw).
 *   This constraint forces patients into a recurring cycle of physical
 *   discomfort and financial cost for test strips and devices, while creating
 *   a stable, lucrative market for medical device manufacturers. The emergence
 *   of non-invasive technologies highlights the extractive nature of this long-standing
 *   requirement.
 *
 * KEY AGENTS (by structural relationship):
 *   - People with Diabetes: Primary target (powerless/trapped) — bear the physical, psychological, and financial costs of mandatory testing.
 *   - Medical Device Manufacturers: Primary beneficiary (institutional/arbitrage) — benefit from a captive market with recurring revenue from disposable test strips.
 *   - Healthcare Providers: Secondary actors (organized/constrained) — operate within the system, using the tests as a standard diagnostic tool.
 *   - Public Health Analysts: Analytical observer — sees the full system, including both its coordination function and its extractive cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(requirement_invasive_diabetes_testing, 0.75).
domain_priors:suppression_score(requirement_invasive_diabetes_testing, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(requirement_invasive_diabetes_testing, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, extractiveness, 0.75).
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(requirement_invasive_diabetes_testing, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(requirement_invasive_diabetes_testing). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(requirement_invasive_diabetes_testing, medical_device_manufacturers).
narrative_ontology:constraint_beneficiary(requirement_invasive_diabetes_testing, health_insurance_providers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(requirement_invasive_diabetes_testing, people_with_diabetes).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Reconciles the coordination function with the asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: HEALTHCARE PROVIDERS (TANGLED ROPE)
% They are part of the system, benefiting from a clear standard but also
% dealing with the consequences of patient non-compliance due to the
% invasive nature of the test.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(requirement_invasive_diabetes_testing_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the patient and the manufacturer.
    constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    % The analytical view must classify as Tangled Rope, reconciling the two extremes.
    constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A constraint can only be a Tangled Rope if it has a beneficiary (coordination),
    % a victim (extraction), and requires enforcement.
    narrative_ontology:constraint_beneficiary(requirement_invasive_diabetes_testing, _),
    narrative_ontology:constraint_victim(requirement_invasive_diabetes_testing, _),
    domain_priors:requires_active_enforcement(requirement_invasive_diabetes_testing).

:- end_tests(requirement_invasive_diabetes_testing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): This score reflects the high, recurring financial cost of disposable test strips and devices, combined with the non-monetary costs of physical pain, scarring, and psychological aversion borne by patients. The market is captive and the product is non-optional for effective disease management.
 *   - Suppression Score (0.80): For decades, the high barriers to entry for new diagnostic technologies (regulatory approval, clinical validation, integration into medical workflows) have effectively suppressed non-invasive alternatives, locking in the existing business model.
 *   - Theater Ratio (0.15): The tests are highly functional and provide essential medical data. Theatricality is low.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a person with diabetes (powerless, trapped), the constraint is a Snare—a painful, costly, and unavoidable requirement. For a medical device manufacturer (institutional, arbitrage), it is a perfect Rope—a brilliant coordination mechanism that standardizes medical practice and guarantees a perpetual, high-margin revenue stream. The system that coordinates the industry is the same system that extracts from the patient.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `medical_device_manufacturers` directly profit from the sale of mandatory, consumable products. Their institutional power and arbitrage exit options yield a low/negative effective extraction (χ), classifying the constraint as a Rope from their perspective.
 *   - Victim: `people_with_diabetes` are the targets. They are structurally trapped, bearing all the costs. This drives their directionality `d` towards 1.0, yielding a very high χ and a Snare classification.
 *   The `analytical` perspective sees both sides, hence the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This model correctly avoids mislabeling the constraint. A naive analysis might see only the patient's plight and call it a pure Snare, ignoring the vast, complex system of industrial and medical coordination it enables. Conversely, an industry-focused view might see only the coordination (a Rope) and ignore the immense, asymmetric extraction. The Tangled Rope classification, derived from the analytical perspective, correctly identifies that it is simultaneously a coordination mechanism and an extractive one. This is crucial for understanding why it's so resilient: it serves a powerful constituency's interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_invasive_diabetes_testing,
    'Is the historical dominance of invasive testing a result of fundamental biophysical difficulty (a Mountain), or a result of market/regulatory capture and suppressed innovation (a Tangled Rope/Snare)?',
    'Successful, widespread deployment of a non-invasive alternative with equivalent or superior accuracy. If this occurs, it resolves the question in favor of the Tangled Rope/Snare interpretation.',
    'If it was a Mountain, the old constraint was an unchangeable fact of biology. If it is a Tangled Rope, then decades of patient suffering and cost were socially constructed and avoidable.',
    confidence_without_resolution(high) % Confidence is high that it's a Tangled Rope, given emerging tech.
).

% /3 form: typed classification for reporting engine (REQUIRED)
% The reporting engine reads narrative_ontology:omega_variable/3 with structure
% (ID, TypeClass, Description) where TypeClass is one of:
%   empirical   — resolvable by gathering more data
%   conceptual  — depends on definitional or theoretical framing
%   preference  — depends on value judgments or policy choices
% The /3 form is what the engine reads; /5 provides narrative context.
narrative_ontology:omega_variable(omega_invasive_diabetes_testing, empirical, 'Whether non-invasive tech can match invasive accuracy, determining if the constraint is technological or economic.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(requirement_invasive_diabetes_testing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε > 0.46), requiring temporal data.
% The timeline represents the era of widespread home glucose monitoring.

% Theater ratio has remained low and stable; the test is functional.
narrative_ontology:measurement(ridt_tr_t0, requirement_invasive_diabetes_testing, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ridt_tr_t5, requirement_invasive_diabetes_testing, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ridt_tr_t10, requirement_invasive_diabetes_testing, theater_ratio, 10, 0.15).

% Extraction has increased as diabetes prevalence grew and home monitoring became a multi-billion dollar recurring market.
narrative_ontology:measurement(ridt_ex_t0, requirement_invasive_diabetes_testing, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ridt_ex_t5, requirement_invasive_diabetes_testing, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ridt_ex_t10, requirement_invasive_diabetes_testing, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The blood glucose level is a standardized piece of
% information that coordinates all subsequent medical and behavioral actions.
narrative_ontology:coordination_type(requirement_invasive_diabetes_testing, information_standard).

% Network relationships: The requirement for testing directly influences
% pricing and prescription models for diabetes treatments.
narrative_ontology:affects_constraint(requirement_invasive_diabetes_testing, insulin_pricing_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately reflects
% the power dynamics of the scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */