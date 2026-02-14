% ============================================================================
% CONSTRAINT STORY: fraser_river_salmon_regulation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_fraser_river_salmon_regulation, []).

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
 *   constraint_id: fraser_river_salmon_regulation
 *   human_readable: Fraser River Salmon Regulation
 *   domain: economic
 *
 * SUMMARY:
 *   Regulation of Fraser River salmon fishing, initially aimed at conservation,
 *   has become a complex system that extracts value from indigenous communities
 *   while benefiting commercial fishing interests and government revenue. The
 *   regulations, enforced through quotas and restrictions, have disproportionately
 *   impacted First Nations who rely on salmon for subsistence and cultural practices.
 *
 * KEY AGENTS (by structural relationship):
 *   - First Nations: Primary target (powerless/trapped) — bears extraction through limited access
 *   - Commercial Fishers: Primary beneficiary (organized/arbitrage) — benefits from increased share
 *   - Fisheries and Oceans Canada (DFO): Secondary actor (institutional/constrained)
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fraser_river_salmon_regulation, 0.55).
domain_priors:suppression_score(fraser_river_salmon_regulation, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(fraser_river_salmon_regulation, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, extractiveness, 0.55).
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(fraser_river_salmon_regulation, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(fraser_river_salmon_regulation, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fraser_river_salmon_regulation, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(fraser_river_salmon_regulation).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(fraser_river_salmon_regulation). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(fraser_river_salmon_regulation).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fraser_river_salmon_regulation, commercial_fishers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fraser_river_salmon_regulation, first_nations).
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
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, tangled_rope,
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
% Perspective 4: Captured regulator (institutional, constrained exit)
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, χ ≤ 0.30, theater ≤ 0.70.
% constraint_indexing:constraint_classification(fraser_river_salmon_regulation, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(fraser_river_salmon_regulation).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fraser_river_salmon_regulation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(fraser_river_salmon_regulation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fraser_river_salmon_regulation, TypeBeneficiary, context(agent_power(organized), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fraser_river_salmon_regulation, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(fraser_river_salmon_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Fraser River salmon regulation is classified as a Tangled Rope
 *   because it combines a coordination function (managing salmon stocks) with
 *   asymmetric extraction (disproportionately affecting First Nations).
 *   The base extractiveness is set to 0.55, reflecting the significant
 *   impact on First Nations' access to a vital resource. The suppression score
 *   is 0.70, indicating limited alternative options for First Nations. The
 *   theater ratio is 0.40, suggesting that while there are performative
 *   aspects to the regulation (e.g., consultations), the core function
 *   remains managing the fishery. This low theater ratio is inconsistent with
 *   a Piton classification, as the constraint is actively functional, not inertial.
 *
 * PERSPECTIVAL GAP:
 *   First Nations perceive the regulation as a Snare, restricting their
 *   access and cultural practices. Commercial fishers see it as a Rope,
 *   providing a framework for sustainable fishing and a fair share of the
 *   resource.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial fishers benefit from the regulation through increased
 *   access and reduced competition (d ≈ 0.05). First Nations bear the costs
 *   through restricted access and cultural disruption (d ≈ 0.95). DFO
 *   experiences a more neutral relationship, balancing conservation and
 *   economic interests (d ≈ 0.50).
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   DFO's role is complex. While mandated to manage the fishery sustainably,
 *   its policies have historically favored commercial interests, leading to
 *   a constrained position. This is reflected in the inter-institutional
 *   perspective, where DFO experiences the regulation as a Rope but with
 *   a lower degree of benefit compared to commercial fishers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this system as
 *   pure coordination (Rope). While the regulation has a conservation aspect,
 *   the asymmetric extraction and high suppression score demonstrate that it
 *   is not a purely beneficial system. It also prevents mislabeling it as pure
 *   extraction (Snare), as the conservation efforts do provide some coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fraser_salmon,
    'Is the primary intent of the regulation conservation or economic benefit?',
    'Historical analysis of policy decisions and stakeholder influence',
    'If conservation, classify as Rope; if economic benefit, classify as Snare',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fraser_river_salmon_regulation, 0, 10).

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
narrative_ontology:measurement(fraser_river_salmon_regulation_tr_t0, fraser_river_salmon_regulation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fraser_river_salmon_regulation_tr_t5, fraser_river_salmon_regulation, theater_ratio, 5, 0.35).
narrative_ontology:measurement(fraser_river_salmon_regulation_tr_t10, fraser_river_salmon_regulation, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(fraser_river_salmon_regulation_ex_t0, fraser_river_salmon_regulation, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(fraser_river_salmon_regulation_ex_t5, fraser_river_salmon_regulation, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(fraser_river_salmon_regulation_ex_t10, fraser_river_salmon_regulation, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(fraser_river_salmon_regulation, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(fraser_river_salmon_regulation, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(fraser_river_salmon_regulation, [other_constraint_id]).

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
% narrative_ontology:affects_constraint(fraser_river_salmon_regulation, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(fraser_river_salmon_regulation, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */