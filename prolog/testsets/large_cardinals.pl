% ============================================================================
% CONSTRAINT STORY: large_cardinals_inaccessibility
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_large_cardinals_inaccessibility, []).

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
 *   constraint_id: large_cardinals_inaccessibility
 *   human_readable: Inaccessibility of Large Cardinals
 *   domain: technological
 *
 * SUMMARY:
 *   Large cardinals are a specific class of infinite sets that are 'large' in the sense that their existence cannot be proven from the standard axioms of set theory (ZFC). The inaccessibility of large cardinals acts as a theoretical barrier to provability, where assuming their existence allows mathematicians to prove theorems about 'smaller' sets, but the large cardinals themselves remain inaccessible within the ZFC framework. This story focuses on the constraint that Large Cardinals cannot be proven by ZFC.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematicians attempting to prove large cardinal existence: Primary target (moderate/constrained) — bears extraction (intellectual effort).
 *   - Set theorists postulating new axioms: Primary beneficiary (powerful/arbitrage) — benefits from assuming the existence.
 *   - The mathematical community: Observer (analytical/analytical) — sees the overall structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(large_cardinals_inaccessibility, 0.15). % Low, as the effort is inherent to the field.
domain_priors:suppression_score(large_cardinals_inaccessibility, 0.05).   % Low coercion as mathematicians can work on other problems.
domain_priors:theater_ratio(large_cardinals_inaccessibility, 0.10).       % Very little theatrical maintenance

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(large_cardinals_inaccessibility, extractiveness, 0.15).
narrative_ontology:constraint_metric(large_cardinals_inaccessibility, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(large_cardinals_inaccessibility, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.

narrative_ontology:constraint_metric(large_cardinals_inaccessibility, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(large_cardinals_inaccessibility, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(large_cardinals_inaccessibility, mountain).
narrative_ontology:human_readable(large_cardinals_inaccessibility, "Inaccessibility of Large Cardinals").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause([id]).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement([id]). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.

domain_priors:emerges_naturally(large_cardinals_inaccessibility).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(large_cardinals_inaccessibility, set_theorists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(large_cardinals_inaccessibility, mathematicians).
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
constraint_indexing:constraint_classification(large_cardinals_inaccessibility, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(large_cardinals_inaccessibility, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(large_cardinals_inaccessibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(large_cardinals_inaccessibility_tests).

test(perspectival_agreement) :-
    % Verify perspectival agreement that Large Cardinals are inaccessible.
    constraint_indexing:constraint_classification(large_cardinals_inaccessibility, TypeTarget, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(large_cardinals_inaccessibility, TypeBeneficiary, context(agent_power(powerful), _, _, _)),
    TypeTarget = TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(large_cardinals_inaccessibility, ExtMetricName, E),
    E =< 0.25. % Mountain (low extractiveness).

:- end_tests(large_cardinals_inaccessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores are assigned based on the inherent difficulty of proving the existence of large cardinals within ZFC. Extractiveness is low because the intellectual effort is inherent to mathematical research. Suppression is also low, as mathematicians are free to explore other areas. The high accessibility_collapse and low resistance reflect the well-established nature of this limitation.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap. Both mathematicians and set theorists recognize that the existence of large cardinals cannot be proven by ZFC alone. Set theorists embrace this limitation by postulating new axioms.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematicians are considered victims because their efforts to prove large cardinals within ZFC are constrained by this inaccessibility. Set theorists benefit by using the unprovability as justification for new axioms.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain prevents mislabeling the inaccessibility as a pure extraction. The inaccessibility is not actively enforced but rather a consequence of the established axiomatic system. It represents a fundamental limitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_large_cardinals,
    'Will a new axiomatic system be developed which can prove the existence of large cardinals?',
    'Development of a new, more powerful axiomatic system.',
    'If True: ZFC loses dominance, If False: Current limitations remain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(large_cardinals_inaccessibility, 0, 10).

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
narrative_ontology:measurement(large_cardinals_inaccessibility_tr_t0, large_cardinals_inaccessibility, theater_ratio, 0, 0.10).
narrative_ontology:measurement(large_cardinals_inaccessibility_tr_t5, large_cardinals_inaccessibility, theater_ratio, 5, 0.10).
narrative_ontology:measurement(large_cardinals_inaccessibility_tr_t10, large_cardinals_inaccessibility, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(large_cardinals_inaccessibility_ex_t0, large_cardinals_inaccessibility, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(large_cardinals_inaccessibility_ex_t5, large_cardinals_inaccessibility, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(large_cardinals_inaccessibility_ex_t10, large_cardinals_inaccessibility, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(large_cardinals_inaccessibility, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override([id], [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint([id], [other_constraint_id]).

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
% constraint_indexing:directionality_override([id], institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */