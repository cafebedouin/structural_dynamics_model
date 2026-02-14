% ============================================================================
% CONSTRAINT STORY: arctic_maritime_control
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-31
% ============================================================================

:- module(constraint_arctic_maritime_control, []).

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
 *   constraint_id: arctic_maritime_control
 *   human_readable: Arctic Maritime Control Regime
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The Arctic Maritime Control Regime represents the set of international
 *   laws, treaties, and national claims that govern access to and resource
 *   extraction from the Arctic Ocean and its surrounding territories. This
 *   constraint determines which nations can operate in the Arctic and under
 *   what conditions.
 *
 * KEY AGENTS (by structural relationship):
 *   - Non-Arctic Nations: Primary target (powerless/constrained) — bears extraction (limited access, resource denial)
 *   - Arctic Council Nations: Primary beneficiary (institutional/arbitrage) — benefits from preferential access and resource claims
 *   - Russia: Secondary actor (institutional/constrained) - benefits from the regime but also constrained by it
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_maritime_control, 0.55).
domain_priors:suppression_score(arctic_maritime_control, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(arctic_maritime_control, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_maritime_control, extractiveness, 0.55).
narrative_ontology:constraint_metric(arctic_maritime_control, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(arctic_maritime_control, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(arctic_maritime_control, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(arctic_maritime_control, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(arctic_maritime_control, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(arctic_maritime_control).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(arctic_maritime_control). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(arctic_maritime_control).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(arctic_maritime_control, arctic_council_nations).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(arctic_maritime_control, non_arctic_nations).
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
constraint_indexing:constraint_classification(arctic_maritime_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(arctic_maritime_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(arctic_maritime_control, tangled_rope,
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
% Perspective 4A: A major Arctic nation (e.g. Russia), institutional but constrained by the regime
constraint_indexing:constraint_classification(arctic_maritime_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_maritime_control_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(arctic_maritime_control, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_maritime_control, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(arctic_maritime_control, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(arctic_maritime_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Arctic Maritime Control Regime has a base extractiveness of 0.55
 *   because while it facilitates some resource extraction and navigation,
 *   it also restricts access to non-Arctic nations and imposes environmental
 *   regulations. The suppression score of 0.70 reflects the limited avenues
 *   for challenging the existing legal framework and the power dynamics
 *   among the Arctic Council nations. The low theater ratio (0.20) reflects
 *   that it is largely functional with less performative activity.
 *
 * PERSPECTIVAL GAP:
 *   Non-Arctic nations perceive the regime as a Snare, as it restricts their
 *   access and resource extraction opportunities. Arctic Council nations, on
 *   the other hand, view it as a Rope, facilitating coordinated resource
 *   management and navigation within the region.
 *
 * DIRECTIONALITY LOGIC:
 *   Arctic Council nations benefit from preferential access and resource
 *   claims within the Arctic region. Non-Arctic nations bear the cost of
 *   limited access and potential denial of resource extraction opportunities.
 *   The beneficiary declaration identifies Arctic Council nations, while the
 *   victim declaration identifies non-Arctic nations. These declarations map
 *   to real structural relationships as determined by international law and
 *   treaties.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Russia benefits significantly from the Arctic Maritime Control Regime due
 *   to its extensive Arctic coastline and resource holdings. However, it
 *   is also constrained by international laws and treaties. This dual
 *   relationship is represented by the inter-institutional perspective
 *   (perspective 4A) with constrained exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction
 *   by acknowledging the regime's role in coordinating resource management
 *   and navigation (Rope aspect). However, it also recognizes the asymmetric
 *   extraction imposed on non-Arctic nations (Snare aspect).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_arctic_control,
    'To what extent will climate change undermine the current regime?',
    'Monitor ice melt rates, new shipping routes, and international disputes',
    'If climate change continues to melt the Arctic, resource extraction will be more readily available, potentially undermining the existing control regime and shifting the balance of power among nations.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(arctic_maritime_control, 0, 10).

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
narrative_ontology:measurement(arctic_tr_t0, arctic_maritime_control, theater_ratio, 0, 0.10).
narrative_ontology:measurement(arctic_tr_t5, arctic_maritime_control, theater_ratio, 5, 0.15).
narrative_ontology:measurement(arctic_tr_t10, arctic_maritime_control, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(arctic_ex_t0, arctic_maritime_control, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(arctic_ex_t5, arctic_maritime_control, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(arctic_ex_t10, arctic_maritime_control, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(arctic_maritime_control, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(arctic_maritime_control, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(arctic_maritime_control, [other_constraint_id]).

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
% narrative_ontology:affects_constraint(arctic_maritime_control, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(arctic_maritime_control, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */