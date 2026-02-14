% ============================================================================
% CONSTRAINT STORY: van_der_waerden_theorem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_van_der_waerden_theorem, []).

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
 *   constraint_id: van_der_waerden_theorem
 *   human_readable: Van der Waerden's Theorem
 *   domain: mathematical
 *
 * SUMMARY:
 *   Van der Waerden's theorem is a fundamental result in Ramsey theory. It states that for any given positive integers r and k, there exists some number N such that if the integers {1, 2, ..., N} are colored with r different colors, then there necessarily exists a monochromatic arithmetic progression of length k.  This constraint reflects a fundamental structural limit on the possibility of avoiding monochromatic progressions under certain coloring conditions.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematicians: Analytical observer (analytical/analytical) — understands theorem implications
 *   - Random Coloring Algorithm: Primary target (powerless/trapped) — subject to the theorem's constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(van_der_waerden_theorem, 0.05). % Very low extractiveness, inherent mathematical property
domain_priors:suppression_score(van_der_waerden_theorem, 0.01).   % Very low suppression - no real alternative exists, its math
domain_priors:theater_ratio(van_der_waerden_theorem, 0.0).       % No performative aspect

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(van_der_waerden_theorem, extractiveness, 0.05).
narrative_ontology:constraint_metric(van_der_waerden_theorem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(van_der_waerden_theorem, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.

narrative_ontology:constraint_metric(van_der_waerden_theorem, accessibility_collapse, 0.95). % No alternative coloring can fully avoid progressions.
narrative_ontology:constraint_metric(van_der_waerden_theorem, resistance, 0.01). % Minimal resistance, it's a provable theorem.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(van_der_waerden_theorem, mountain).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(van_der_waerden_theorem).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(van_der_waerden_theorem). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.

domain_priors:emerges_naturally(van_der_waerden_theorem).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
% narrative_ontology:constraint_beneficiary(van_der_waerden_theorem, [beneficiary_group]).
%
% Who bears disproportionate cost?
% narrative_ontology:constraint_victim(van_der_waerden_theorem, [victim_group]).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(van_der_waerden_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE ANALYTICAL OBSERVER (MOUNTAIN)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(van_der_waerden_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: A COMPUTER IMPLEMENTING A COLORING ALGORITHM
constraint_indexing:constraint_classification(van_der_waerden_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(van_der_waerden_theorem_tests).

test(perspectival_consistency) :-
    % Verify type consistency across perspectives.
    constraint_indexing:constraint_classification(van_der_waerden_theorem, Type1, context(_, _, _, _)),
    constraint_indexing:constraint_classification(van_der_waerden_theorem, Type2, context(_, _, _, _)),
    Type1 == Type2.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(van_der_waerden_theorem, ExtMetricName, E),
    E =< 0.25. % Mountain

test(natural_law_signature) :-
    narrative_ontology:constraint_metric(van_der_waerden_theorem, accessibility_collapse, Collapse),
    Collapse >= 0.85,
    narrative_ontology:constraint_metric(van_der_waerden_theorem, resistance, Resistance),
    Resistance =< 0.15,
    domain_priors:emerges_naturally(van_der_waerden_theorem).

:- end_tests(van_der_waerden_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain due to its nature as a proven mathematical theorem. Its extractiveness and suppression scores are extremely low, reflecting that it's a fundamental property of numbers and colorings rather than an actively enforced rule. The accessibility collapse is high because it's difficult to avoid monochromatic progressions given the theorem's conditions, and resistance is low because it's a mathematically provable result.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap, as the theorem is a fundamental mathematical truth and is experienced as such from all viewpoints, whether mathematical, computational, or abstractly analytical. All perspectives result in a 'mountain' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no explicit beneficiaries or victims in the traditional sense for a mathematical theorem. We conceptualize coloring algorithms as being 'constrained' by the theorem. However, since its a mountain constraint and doesn't require beneficiaries or victims, this conceptualization is more of a reflection of the theorem's behavior rather than a traditional beneficial/victim relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This theorem is not mislabeled as coordination or extraction because it's a foundational principle that dictates an inescapable structural constraint. Its function is not about coordinating action but fundamentally limiting what is possible. Therefore, any attempt to classify this as a form of social arrangement would be incorrect. The low extractiveness score prevents this misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_van_der_waerden,
    'To what extent can variations of the theorem be considered structurally different constraints?',
    'Further exploration of related theorems in Ramsey theory and their variations.',
    'If true, more separate constraint stories are needed; if false, a single encompassing story is sufficient.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(van_der_waerden_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Since the base extractiveness is very low (0.05) temporal measurements are not strictly required but included for consistency.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(van_der_waerden_theorem_tr_t0, van_der_waerden_theorem, theater_ratio, 0, 0.0).
narrative_ontology:measurement(van_der_waerden_theorem_tr_t5, van_der_waerden_theorem, theater_ratio, 5, 0.0).
narrative_ontology:measurement(van_der_waerden_theorem_tr_t10, van_der_waerden_theorem, theater_ratio, 10, 0.0).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(van_der_waerden_theorem_ex_t0, van_der_waerden_theorem, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(van_der_waerden_theorem_ex_t5, van_der_waerden_theorem, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(van_der_waerden_theorem_ex_t10, van_der_waerden_theorem, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(van_der_waerden_theorem, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(van_der_waerden_theorem, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */