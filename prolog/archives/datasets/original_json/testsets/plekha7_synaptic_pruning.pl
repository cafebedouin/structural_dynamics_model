% ============================================================================
% CONSTRAINT STORY: plekha7_synaptic_pruning
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_plekha7_synaptic_pruning, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: plekha7_synaptic_pruning
 *   human_readable: "PLEKHA7 Gene's Role in Synaptic Pruning and Schizophrenia Risk"
 *   domain: biological
 *
 * SUMMARY:
 *   The gene PLEKHA7 acts as a biological switch for synaptic pruning in the
 *   prefrontal cortex during adolescence. Certain genetic variants cause this
 *   switch to be less effective, leading to incomplete pruning and the retention
 *   of weak, "noisy" synaptic connections. This brain structure is a fixed,
 *   unchangeable biological predisposition that significantly increases an
 *   individual's risk of developing schizophrenia.
 *
 * KEY AGENTS (by structural relationship):
 *   - Individuals with risk-associated PLEKHA7 variants: Primary target (powerless/trapped) — they are subject to the adverse neurological outcome and cannot change their genetic makeup.
 *   - Individuals with typical PLEKHA7 variants: Not a beneficiary in an extractive sense, but their neurodevelopment follows the standard, functional pathway. They are subject to the same natural law, which works as intended.
 *   - Neuroscientists / Geneticists: Analytical observer — understands the mechanism as a fundamental law of neurobiology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plekha7_synaptic_pruning, 0.05).
domain_priors:suppression_score(plekha7_synaptic_pruning, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(plekha7_synaptic_pruning, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plekha7_synaptic_pruning, extractiveness, 0.05).
narrative_ontology:constraint_metric(plekha7_synaptic_pruning, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(plekha7_synaptic_pruning, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification, preventing a Mountain classification.
narrative_ontology:constraint_metric(plekha7_synaptic_pruning, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(plekha7_synaptic_pruning, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(plekha7_synaptic_pruning, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Declared because this constraint is a product of biological evolution, not
% human design. This is mandatory for the mountain metric gate to fire.
domain_priors:emerges_naturally(plekha7_synaptic_pruning).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a uniform-type Mountain constraint, beneficiary/victim declarations are
% not required. The constraint's classification is invariant.

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

% This is a UNIFORM-TYPE constraint (Mountain-only). The classification is
% the same from all perspectives, demonstrating invariance.

% PERSPECTIVE 1: THE GENETICALLY AT-RISK INDIVIDUAL
% An individual with the risk-associated variant experiences this as an
% unchangeable, fixed aspect of their biology. They are powerless to alter it
% and trapped within its consequences. It is a Mountain.
constraint_indexing:constraint_classification(plekha7_synaptic_pruning, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INDIVIDUAL WITH TYPICAL GENETICS
% An individual with the typical variant also experiences this as a natural law,
% just one that functions correctly for them. The underlying mechanism is still
% a fixed biological reality. It is a Mountain.
constraint_indexing:constraint_classification(plekha7_synaptic_pruning, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).


% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% A neuroscientist sees the genetic mechanism as a fundamental, non-negotiable
% law of neurodevelopment. The very low ε and suppression scores, combined with
% its natural emergence, confirm the Mountain classification.
constraint_indexing:constraint_classification(plekha7_synaptic_pruning, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plekha7_synaptic_pruning_tests).

test(classification_invariance) :-
    % For a Mountain, there is no perspectival gap. All see the same type.
    constraint_indexing:constraint_classification(plekha7_synaptic_pruning, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plekha7_synaptic_pruning, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == mountain,
    TypeTarget == TypeAnalytical.

test(mountain_threshold_adherence) :-
    domain_priors:base_extractiveness(plekha7_synaptic_pruning, E),
    domain_priors:suppression_score(plekha7_synaptic_pruning, S),
    E =< 0.25,
    S =< 0.05.

test(mountain_nl_profile_certification) :-
    % Verify the constraint passes the Natural Law certification metrics.
    narrative_ontology:constraint_metric(plekha7_synaptic_pruning, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(plekha7_synaptic_pruning, resistance, R),
    config:param(natural_law_collapse_min, MinCollapse),
    config:param(natural_law_resistance_max, MaxResistance),
    AC >= MinCollapse,
    R =< MaxResistance.

:- end_tests(plekha7_synaptic_pruning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it represents a
 *   fundamental, non-negotiable biological process.
 *   - Base Extractiveness (ε=0.05): The gene doesn't "extract" value in an
 *     economic or social sense; it imposes a fixed biological risk. The value
 *     is near zero to reflect its non-extractive nature.
 *   - Suppression (0.02): The constraint doesn't suppress alternatives through
 *     coercion; it *is* the only available reality for an individual's neuro-
 *     development.
 *   - NL Profile: The `accessibility_collapse` is 1.0 because an individual
 *     with the genetic variant cannot access the alternative (a brain with
 *     typical pruning). The `resistance` is 0.0 because active opposition to
 *     one's own genetic code is incoherent. The `emerges_naturally` flag is
 *     true. These three facts are required for a Mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a key feature of a Mountain. The
 *   constraint's classification is invariant across all observers, whether
 *   they are subject to its adverse effects (powerless/trapped) or not.
 *   While the *consequences* are vastly different for individuals with different
 *   gene variants, the underlying *constraint* (the biological law itself)
 *   is perceived by all as a fixed, unchangeable reality.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint does not have beneficiaries or victims in
 *   the typical sense of social or economic extraction. It is a natural law.
 *   Directionality `d` will fall back to canonical values based on power, but
 *   because `ε` is extremely low, the effective extraction `χ` remains near
 *   zero for all indices, reinforcing the Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a fixed, natural limitation. It
 *   prevents the risk of schizophrenia from being mislabeled as a social
-  *   Snare (e.g., blaming individuals for their condition) or a simple Rope
 *   (e.g., assuming a behavioral fix is sufficient). It correctly frames the
 *   problem at the biological level, highlighting that any potential
 *   intervention would need to operate at a similarly fundamental (e.g.,
 *   genetic, pharmacological) level, rather than through social coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_plekha7,
    'Is the PLEKHA7 variant''s effect on schizophrenia risk deterministic enough to be a pure Mountain, or are there significant environmental/epigenetic co-factors that could mitigate it?',
    'Longitudinal studies tracking large cohorts with the risk variant across diverse socioeconomic and environmental conditions to identify outcome variance.',
    'If strong mitigating co-factors exist, the constraint may be better modeled as part of a larger Tangled Rope involving societal support systems. If the effect is highly penetrant regardless of environment, the Mountain classification holds.',
    confidence_without_resolution(high) % Current literature suggests a very strong, primary genetic link.
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(plekha7_synaptic_pruning, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as ε < 0.46. However, for a biological
% constraint, metrics are stable over human history. These facts demonstrate
% the constraint's non-drifting, static nature.

% Theater ratio is always zero.
narrative_ontology:measurement(plekha7_tr_t0, plekha7_synaptic_pruning, theater_ratio, 0, 0.0).
narrative_ontology:measurement(plekha7_tr_t5, plekha7_synaptic_pruning, theater_ratio, 5, 0.0).
narrative_ontology:measurement(plekha7_tr_t10, plekha7_synaptic_pruning, theater_ratio, 10, 0.0).

% Base extractiveness is constant and low.
narrative_ontology:measurement(plekha7_ex_t0, plekha7_synaptic_pruning, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(plekha7_ex_t5, plekha7_synaptic_pruning, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(plekha7_ex_t10, plekha7_synaptic_pruning, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint has no coordination function.

% Network relationships: This genetic predisposition likely influences other
% cognitive functions that could be modeled as their own constraints.
narrative_ontology:affects_constraint(plekha7_synaptic_pruning, cognitive_executive_function).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. As a Mountain with ε near zero, the
% canonical directionality derivations produce the correct near-zero effective
% extraction (χ) for all perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */