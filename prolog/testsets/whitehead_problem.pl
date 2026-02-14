% ============================================================================
% CONSTRAINT STORY: whitehead_incompleteness
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_whitehead_incompleteness, []).

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
 *   constraint_id: whitehead_incompleteness
 *   human_readable: Whitehead Problem and Large Cardinals
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Whitehead Problem, specifically its resolution's dependence on the set-theoretic axioms beyond ZFC (Zermelo-Fraenkel set theory with the axiom of choice), highlights the incompleteness of ZFC for settling certain mathematical questions. The existence of Whitehead groups that are not free depends on the assumption of large cardinals or related axioms. This demonstrates that the ZFC axiom system, while sufficient for much of mathematics, is incomplete.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematicians: Primary target (analytical/constrained) — limited by ZFC.
 *   - Set Theorists: Primary beneficiary (analytical/arbitrage) — benefit from developing new axioms.
 *   - The ZFC Axiom System: Imposes the constraint (None/None) — the constraint itself.
 *   - Analytical observer: Sees the complete structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(whitehead_incompleteness, 0.05).
domain_priors:suppression_score(whitehead_incompleteness, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(whitehead_incompleteness, 0.00).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(whitehead_incompleteness, extractiveness, 0.05).
narrative_ontology:constraint_metric(whitehead_incompleteness, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(whitehead_incompleteness, theater_ratio, 0.00).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.

narrative_ontology:constraint_metric(whitehead_incompleteness, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(whitehead_incompleteness, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(whitehead_incompleteness, mountain).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(whitehead_incompleteness).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(whitehead_incompleteness). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.

domain_priors:emerges_naturally(whitehead_incompleteness).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
% NOTE: For mountain constraints, this is not strictly required but provides
% additional context. No enrichment needed for linter compliance.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(whitehead_incompleteness, set_theorists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(whitehead_incompleteness, mathematicians).
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
constraint_indexing:constraint_classification(whitehead_incompleteness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(whitehead_incompleteness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(whitehead_incompleteness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(whitehead_incompleteness_tests).

test(perspectival_consistency) :-
    % Verify all perspectives classify as the same type (Mountain).
    findall(Type,
            constraint_indexing:constraint_classification(whitehead_incompleteness, Type, _),
            Types),
    sort(Types, UniqueTypes),
    UniqueTypes = [mountain].

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(whitehead_incompleteness, ExtMetricName, E),
    E =< 0.25. % Mountain: base extractiveness <= 0.25

test(natural_law_metrics) :-
	narrative_ontology:constraint_metric(whitehead_incompleteness, accessibility_collapse, Collapse),
	narrative_ontology:constraint_metric(whitehead_incompleteness, resistance, Resistance),
	Collapse >= 0.85,
	Resistance =< 0.15.

:- end_tests(whitehead_incompleteness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Whitehead Problem, viewed as a limit on provability within ZFC, exhibits low extractiveness and suppression. It is a fundamental limitation of a formal system, akin to Gödel's incompleteness theorems, making it a 'Mountain' constraint. The low extractiveness reflects the system's internal consistency, and the minimal suppression stems from the freedom to explore alternative axiomatic systems.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives (mathematicians, set theorists, analytical observers) classify this as a 'Mountain' because the limitation is inherent in the formal system, not a social or economic construct where different actors might perceive different incentives or power dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Set theorists benefit from the existence of the limitation because it provides a justification for developing new axiomatic systems. Mathematicians are 'victims' in that the problem limits what they can prove *within* ZFC but are far from trapped. Both groups, however, recognize it as a limitation of ZFC. The impact on each is mild. The primary structural impact is the limitation ZFC imposes.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Not applicable, as this is not primarily an inter-institutional constraint. It is a fundamental limit of a formal system.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling a fundamental mathematical limitation as a system of pure extraction. This is not a 'Snare' where some actors exploit others, but a fundamental incompleteness of a formal system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_whitehead,
    'Will further developments in set theory provide axioms that resolve the Whitehead problem in a manner more widely accepted than current large cardinal assumptions?',
    'Historical analysis of acceptance rates of new set-theoretic axioms within the mathematical community.',
    'If True: The significance of large cardinals might diminish. If False: Large cardinals will remain central to resolving the problem.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(whitehead_incompleteness, 0, 10).

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
narrative_ontology:measurement(whitehead_incompleteness_tr_t0, whitehead_incompleteness, theater_ratio, 0, 0.00).
narrative_ontology:measurement(whitehead_incompleteness_tr_t5, whitehead_incompleteness, theater_ratio, 5, 0.00).
narrative_ontology:measurement(whitehead_incompleteness_tr_t10, whitehead_incompleteness, theater_ratio, 10, 0.00).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(whitehead_incompleteness_ex_t0, whitehead_incompleteness, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(whitehead_incompleteness_ex_t5, whitehead_incompleteness, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(whitehead_incompleteness_ex_t10, whitehead_incompleteness, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(whitehead_incompleteness, information_standard). % This is a standard of mathematical provability

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
narrative_ontology:boltzmann_floor_override(whitehead_incompleteness, 0.05). % Override to reflect the fundamental nature of the limit.

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(whitehead_incompleteness, another_constraint_id).

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
% narrative_ontology:affects_constraint(whitehead_incompleteness, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(whitehead_incompleteness, analytical, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */