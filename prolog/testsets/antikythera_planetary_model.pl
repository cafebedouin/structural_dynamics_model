% ============================================================================
% CONSTRAINT STORY: antikythera_planetary_model
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_antikythera_planetary_model, []).

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
 *   constraint_id: antikythera_planetary_model
 *   human_readable: "Antikythera Mechanism's Geocentric Planetary Model"
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   This constraint represents the complex, epicycle-based geocentric model
 *   of planetary motion encoded in the Antikythera Mechanism. While a
 *   remarkable feat of engineering and a powerful coordination tool for
 *   Hellenistic astronomers, it was built on a physically incorrect premise.
 *   Its success and complexity extracted cognitive resources from the
 *   scientific community, actively suppressing simpler but more accurate
 *   heliocentric models for centuries.
 *
 * KEY AGENTS (by structural relationship):
 *   - Heliocentric Proponents: Primary target (powerless/trapped) — The success of the geocentric model, embodied in this device, marginalized their simpler, more accurate theories, trapping them outside the scientific consensus.
 *   - Hellenistic Astronomers: Primary beneficiary (organized/mobile) — The mechanism standardized prediction and calculation, forming the basis of their professional practice.
 *   - Ptolemaic School: Institutional beneficiary (institutional/arbitrage) - The dominant scientific institution that controlled the paradigm, benefiting from its stability and authority.
 *   - Modern Researchers: Analytical observer — See the full structure: its coordination function, its inherent inaccuracies, and its suppressive historical role.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of two stories decomposed from "The Antikythera Mechanism".
 * Decomposed because ε differs across observables (ε-invariance principle, DP-001).
 * The mechanism's function is split into two distinct structural claims:
 *   - antikythera_eclipse_prediction (ε≈0.05, Mountain): The highly accurate Saros cycle model for predicting eclipses. Its empirical success lent credibility to the planetary model.
 *   - antikythera_planetary_model (ε=0.55, Tangled Rope): The complex, inaccurate geocentric model for planetary motion, which is the subject of this file.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antikythera_planetary_model, 0.55).
domain_priors:suppression_score(antikythera_planetary_model, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(antikythera_planetary_model, 0.95).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antikythera_planetary_model, extractiveness, 0.55).
narrative_ontology:constraint_metric(antikythera_planetary_model, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(antikythera_planetary_model, theater_ratio, 0.95).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(antikythera_planetary_model, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(antikythera_planetary_model). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(antikythera_planetary_model, hellenistic_astronomers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(antikythera_planetary_model, heliocentric_proponents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (HELIOCENTRIC PROPONENTS)
% The model acts as a cognitive snare, its complexity and apparent success
% making alternative (and ultimately correct) theories seem non-viable.
% Engine derives victim + trapped -> d≈0.95 -> f(d)≈1.42.
% χ = 0.55 * 1.42 * σ(regional=0.9) ≈ 0.70. This meets Snare criteria
% (χ≥0.66, ε≥0.46, suppression≥0.60).
constraint_indexing:constraint_classification(antikythera_planetary_model, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (HELLENISTIC ASTRONOMERS)
% For its users, the model was a powerful coordination device (Rope),
% standardizing calculations and creating a shared professional language.
% Engine derives beneficiary + mobile -> d≈0.15 -> f(d)≈-0.01.
% χ = 0.55 * -0.01 * σ(regional=0.9) ≈ -0.005. This is a clear Rope.
constraint_indexing:constraint_classification(antikythera_planetary_model, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE INSTITUTIONAL BENEFICIARY (PTOLEMAIC SCHOOL)
% From the perspective of the dominant scientific institution, the model is a
% pure coordination device (Rope) that solidifies their paradigm and authority.
% Engine derives beneficiary + arbitrage -> d≈0.05 -> f(d)≈-0.12.
% χ = 0.55 * -0.12 * σ(regional=0.9) ≈ -0.06. A clear Rope.
constraint_indexing:constraint_classification(antikythera_planetary_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MODERN RESEARCHERS)
% Sees both the coordination function and the extractive, suppressive nature.
% The system has a beneficiary, a victim, and requires enforcement, satisfying
% the structural requirements for a Tangled Rope.
% Engine derives analytical -> d≈0.72 -> f(d)≈1.15.
% χ = 0.55 * 1.15 * σ(global=1.2) ≈ 0.759. Despite meeting Snare thresholds
% numerically, the presence of a genuine coordination function classifies it as
% a Tangled Rope, resolving the Mandatrophy.
constraint_indexing:constraint_classification(antikythera_planetary_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antikythera_planetary_model_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(antikythera_planetary_model, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antikythera_planetary_model, rope, context(agent_power(institutional), _, _, _)).

test(tangled_rope_analytical_view) :-
    % Verify the analytical resolution is Tangled Rope.
    constraint_indexing:constraint_classification(antikythera_planetary_model, tangled_rope, context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression_thresholds) :-
    narrative_ontology:constraint_metric(antikythera_planetary_model, extractiveness, E), E >= 0.46,
    narrative_ontology:constraint_metric(antikythera_planetary_model, suppression_requirement, S), S >= 0.60.

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(antikythera_planetary_model, _),
    narrative_ontology:constraint_victim(antikythera_planetary_model, _),
    domain_priors:requires_active_enforcement(antikythera_planetary_model).

:- end_tests(antikythera_planetary_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. This value represents the significant cognitive and opportunity cost imposed by the model. It extracted intellectual energy, directing it towards refining a flawed paradigm (adding more epicycles) rather than exploring simpler, physically correct alternatives.
 *   - Suppression (0.75): High. The model's complexity and its success in certain predictions created a formidable intellectual moat, making it extremely difficult for alternative theories like heliocentrism to gain traction. It suppressed dissent via its sheer intricacy.
 *   - Theater Ratio (0.95): Extremely high from a modern viewpoint. Its original scientific function is gone; it now functions as a historical artifact for exhibition and study, a performance of ancient genius. Despite this, its high structural extraction (χ) prevents it from being a Piton, which must have low χ. It's a preserved Tangled Rope, not an inert one.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a Hellenistic astronomer (beneficiary), the mechanism is a pure Rope, a tool that coordinates their entire field and enables prediction. For a proponent of heliocentrism (victim), it is a Snare, a beautiful and complex trap that makes the truth inaccessible and their work irrelevant. The analytical observer sees both sides, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `hellenistic_astronomers`. They gain predictive power, professional status, and a common framework for their work.
 *   - Victim: `heliocentric_proponents`. They bear the cost of marginalization. Their simpler, more elegant model is suppressed by the dominant, complex, and "good enough" incumbent. This structural relationship drives the d-value high for them, leading to a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic case of Mandatrophy resolution. From the analytical perspective, the effective extraction (χ ≈ 0.76) is high enough to qualify as a Snare. However, the system correctly classifies it as a Tangled Rope because it possesses a genuine coordination function, declared via `constraint_beneficiary`. The framework recognizes that even highly extractive systems can have real utility for a specific group, preventing the error of labeling a complex socio-technical system as pure, malicious extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_antikythera_planetary_model,
    'Was the geocentric model''s dominance due to its genuine predictive superiority with available data, or due to institutional inertia and the suppression of equally viable heliocentric models?',
    'A robust simulation of Hellenistic-era observational data to quantitatively compare the predictive accuracy of the Antikythera''s model against a plausible contemporary heliocentric model.',
    'If geocentrism was superior, it was a functional Tangled Rope. If heliocentrism was equally or more accurate, it was a less functional and more suppressive Snare.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_antikythera_planetary_model, empirical, 'Was the geocentric model dominant due to predictive superiority or institutional suppression?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antikythera_planetary_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), requiring temporal data.
% The model's extraction increased as it required more complexity to fit
% observations, and its theatricality rose as its scientific function waned.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(antikythera_planetary_model_tr_t0, antikythera_planetary_model, theater_ratio, 0, 0.10).
narrative_ontology:measurement(antikythera_planetary_model_tr_t5, antikythera_planetary_model, theater_ratio, 5, 0.40).
narrative_ontology:measurement(antikythera_planetary_model_tr_t10, antikythera_planetary_model, theater_ratio, 10, 0.95).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(antikythera_planetary_model_ex_t0, antikythera_planetary_model, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(antikythera_planetary_model_ex_t5, antikythera_planetary_model, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(antikythera_planetary_model_ex_t10, antikythera_planetary_model, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(antikythera_planetary_model, information_standard).

% Network relationships (structural influence edges)
% The empirically verifiable success of the eclipse prediction model
% (a Mountain) lent unwarranted credibility to the flawed planetary model
% (this Tangled Rope), forming a classic network dependency.
narrative_ontology:affects_constraint(antikythera_eclipse_prediction, antikythera_planetary_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the power dynamics of the historical context.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */