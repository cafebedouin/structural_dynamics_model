% ============================================================================
% CONSTRAINT STORY: constraint_sylow
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_sylow, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constraint_sylow
 *   human_readable: Sylow Theorems
 *   domain: technological
 *
 * SUMMARY:
 *   Sylow Theorems provide structural constraints on the subgroups of finite groups.
 *   They guarantee the existence of subgroups of prime power order and provide information
 *   about the number of such subgroups. This can be viewed as a constraint on the possible
 *   group structures, which simplifies the search space for group-theoretic proofs.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematicians: Primary target (powerless/trapped) — must adhere to theorems
 *   - Theorem Provers: Primary beneficiary (institutional/arbitrage) — automated reasoning benefits
 *   - Mathematical Structure: Emergent system (N/A/N/A) — Constraint arises from the rules of mathematics
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constraint_sylow, 0.10).
domain_priors:suppression_score(constraint_sylow, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(constraint_sylow, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constraint_sylow, extractiveness, 0.10).
narrative_ontology:constraint_metric(constraint_sylow, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(constraint_sylow, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
narrative_ontology:constraint_metric(constraint_sylow, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(constraint_sylow, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(constraint_sylow, mountain).
narrative_ontology:human_readable(constraint_sylow, "Sylow Theorems").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(constraint_sylow).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(constraint_sylow). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
domain_priors:emerges_naturally(constraint_sylow).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(constraint_sylow, theorem_provers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(constraint_sylow, mathematicians).
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
constraint_indexing:constraint_classification(constraint_sylow, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(constraint_sylow, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(constraint_sylow, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_sylow_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(constraint_sylow, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constraint_sylow, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget = TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constraint_sylow, ExtMetricName, E),
    E =< 0.25. % Mountain or low-extraction Rope.

:- end_tests(constraint_sylow_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Sylow Theorems are classified as a Mountain because they represent
 *   fundamental constraints on the structure of finite groups, arising from the
 *   axioms of group theory itself. The base extractiveness is low because the
 *   theorems primarily reduce search space rather than actively extracting effort.
 *   Suppression is minimal because mathematicians can still explore any group
 *   structure, but the theorems provide guidance and limitations.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap because the theorems are viewed as a
 *   fundamental truth, regardless of the power or exit options of the observer.
 *   Both mathematicians and theorem provers benefit from the constraints
 *   provided by the Sylow Theorems.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematicians are considered the 'victims' in the sense that they must
 *   adhere to the theorems' constraints, limiting the possible group structures
 *   they explore. Theorem provers are the 'beneficiaries' because the theorems
 *   simplify automated reasoning. The derived directionality captures this
 *   structural relationship.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Not applicable in this scenario.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain prevents mislabeling this constraint as
 *   pure extraction. While the theorem does limit possibilities, it does so by
 *   unveiling inherent mathematical structures and does not impose restrictions
 *   for any agent’s benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sylow,
    'To what extent do further group-theoretic theorems increase or decrease the search space for mathematicians?',
    'Empirical study of theorem proving time before and after theorem',
    'Increased difficulty if search space increases, easier if it decreases',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(constraint_sylow, 0, 10).

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
narrative_ontology:measurement(constraint_sylow_tr_t0, constraint_sylow, theater_ratio, 0, 0.0).
narrative_ontology:measurement(constraint_sylow_tr_t5, constraint_sylow, theater_ratio, 5, 0.0).
narrative_ontology:measurement(constraint_sylow_tr_t10, constraint_sylow, theater_ratio, 10, 0.0).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(constraint_sylow_ex_t0, constraint_sylow, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(constraint_sylow_ex_t5, constraint_sylow, base_extractiveness, 5, 0.10).
narrative_ontology:measurement(constraint_sylow_ex_t10, constraint_sylow, base_extractiveness, 10, 0.10).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(constraint_sylow, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(constraint_sylow, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(constraint_sylow, [other_constraint_id]).

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
% constraint_indexing:directionality_override(constraint_sylow, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */