% ============================================================================
% CONSTRAINT STORY: ivt_accessibility_barrier
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_ivt_accessibility_barrier, []).

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
 *   constraint_id: ivt_accessibility_barrier
 *   human_readable: Accessibility Barrier to the Intermediate Value Theorem
 *   domain: technological
 *
 * SUMMARY:
 *   The Intermediate Value Theorem (IVT) states that for a continuous function f on a closed interval [a, b], if k is any number between f(a) and f(b), then there exists at least one number c in [a, b] such that f(c) = k.  However, accessing and understanding this theorem requires overcoming significant accessibility barriers related to mathematical knowledge, education, and resources.  This story models that accessibility barrier.
 *
 * KEY AGENTS (by structural relationship):
 *   - Students: Primary target (powerless/trapped) — bears extraction (effort to learn)
 *   - Educators/Institutions: Primary beneficiary (institutional/arbitrage) — benefits from teaching and research funding related to math.
 *   - Textbook Publishers: Secondary actor (institutional/arbitrage)
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ivt_accessibility_barrier, 0.55).  % Substantial effort and resources needed to learn and apply.
domain_priors:suppression_score(ivt_accessibility_barrier, 0.40).    % Alternative routes to problem-solving exist, but formal math often preferred.
domain_priors:theater_ratio(ivt_accessibility_barrier, 0.20).        % Low theater ratio because mathematical rigor is paramount.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ivt_accessibility_barrier, extractiveness, 0.55).
narrative_ontology:constraint_metric(ivt_accessibility_barrier, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(ivt_accessibility_barrier, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(ivt_accessibility_barrier, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(ivt_accessibility_barrier, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ivt_accessibility_barrier, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(ivt_accessibility_barrier).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(ivt_accessibility_barrier). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(ivt_accessibility_barrier).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ivt_accessibility_barrier, educators_institutions).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ivt_accessibility_barrier, students).
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
constraint_indexing:constraint_classification(ivt_accessibility_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(ivt_accessibility_barrier, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(ivt_accessibility_barrier, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.
%
% Example — Regulatory capture:
%
% % Perspective 4A: Captured regulator (institutional, constrained exit)
% constraint_indexing:constraint_classification(ivt_accessibility_barrier, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(national))).
%
% % Perspective 4B: Regulated company (institutional, arbitrage exit)
% constraint_indexing:constraint_classification(ivt_accessibility_barrier, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(arbitrage),
%             spatial_scope(national))).

% PERSPECTIVE 5: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, χ ≤ 0.30, theater ≤ 0.70.
% constraint_indexing:constraint_classification(ivt_accessibility_barrier, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(ivt_accessibility_barrier).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ivt_accessibility_barrier_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(ivt_accessibility_barrier, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ivt_accessibility_barrier, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ivt_accessibility_barrier, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(ivt_accessibility_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is scored at 0.55 because learning the IVT requires significant effort in terms of time, prerequisite knowledge, and access to educational resources.  The suppression score is 0.40 because while there are alternative ways to approach certain problems the IVT solves, the theorem provides a formalized and widely accepted method.  The theater ratio is low (0.20) because mathematical integrity is more important than symbolic performance in understanding the IVT. A piton classification is inappropriate as the constraint is functional, not inertial.
 *
 * PERSPECTIVAL GAP:
 *   Students perceive the accessibility barrier as a snare because they are often forced to learn the IVT for academic success, and the effort required can be significant, while Educators/Institutions view it as a rope because it's part of a larger coordinated system of education and research that benefits them through funding and recognition.
 *
 * DIRECTIONALITY LOGIC:
 *   Educators and institutions benefit by teaching the IVT because it enhances their credibility and is a required component of degree programs, leading to funding opportunities. Students, however, bear the cost in terms of time, effort, and financial resources needed to learn and apply the theorem.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   N/A
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the IVT accessibility barrier as a tangled rope prevents mislabeling the overall educational system as a pure extraction scheme. While there is some degree of extraction in requiring students to learn abstract math, the system also provides a framework for understanding and solving problems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ivt_accessibility_barrier,
    'To what extent are alternative problem-solving methods suppressed by the emphasis on formal mathematical training?',
    'A comparative study of problem-solving outcomes using alternative methods versus the IVT.',
    'If True: higher suppression score, moves closer to a snare. If False: lower suppression score, more of a rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ivt_accessibility_barrier, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ivt_accessibility_barrier_tr_t0, ivt_accessibility_barrier, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ivt_accessibility_barrier_tr_t5, ivt_accessibility_barrier, theater_ratio, 5, 0.20).
narrative_ontology:measurement(ivt_accessibility_barrier_tr_t10, ivt_accessibility_barrier, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ivt_accessibility_barrier_ex_t0, ivt_accessibility_barrier, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ivt_accessibility_barrier_ex_t5, ivt_accessibility_barrier, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ivt_accessibility_barrier_ex_t10, ivt_accessibility_barrier, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(ivt_accessibility_barrier, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(ivt_accessibility_barrier, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(ivt_accessibility_barrier, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(ivt_accessibility_barrier, [sibling_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(ivt_accessibility_barrier, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */