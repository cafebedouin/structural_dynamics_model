% ============================================================================
% CONSTRAINT STORY: israeli_settlement_policy_authority_restriction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_israeli_settlement_policy_authority_restriction, []).

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
 *   constraint_id: israeli_settlement_policy_authority_restriction
 *   human_readable: Israeli Settlement Policy Restricting Palestinian Authority
 *   domain: political
 *
 * SUMMARY:
 *   Israeli policy restricts the Palestinian Authority's (PA) ability to operate in Area C of the West Bank. This involves the PA being denied permission to build, operate infrastructure, or otherwise govern in the area. The policy benefits Israeli settlements and associated interests by preventing competing claims of governance and resource control, while directly impacting Palestinian residents by denying them services and development.
 *
 * KEY AGENTS (by structural relationship):
 *   - Palestinian Residents in Area C: Primary target (powerless/trapped) — bears direct extraction
 *   - Palestinian Authority: Institutional target (moderate/trapped) — bears extraction of authority
 *   - Israeli Settlers: Primary beneficiary (organized/arbitrage) — benefits from constraint
 *   - Israeli Government: Enforcing institution (institutional/constrained)
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israeli_settlement_policy_authority_restriction, 0.55).
domain_priors:suppression_score(israeli_settlement_policy_authority_restriction, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(israeli_settlement_policy_authority_restriction, 0.75).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, extractiveness, 0.55).
narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(israeli_settlement_policy_authority_restriction, tangled_rope).
narrative_ontology:human_readable(israeli_settlement_policy_authority_restriction, "Israeli Settlement Policy Restricting Palestinian Authority").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(israeli_settlement_policy_authority_restriction).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(israeli_settlement_policy_authority_restriction). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(israeli_settlement_policy_authority_restriction).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(israeli_settlement_policy_authority_restriction, israeli_settlers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(israeli_settlement_policy_authority_restriction, palestinian_authority).
narrative_ontology:constraint_victim(israeli_settlement_policy_authority_restriction, palestinian_residents_area_c).
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

% PERSPECTIVE 1A: THE POWERLESS TARGET (Palestinian Residents)
% Agent who bears the most direct extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 1B: THE INSTITUTIONAL TARGET (Palestinian Authority)
% A more powerful but still trapped actor, experiencing extraction of authority.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (Israeli Settlers)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ENFORCING INSTITUTION (Israeli Government)
% Experiences the policy as a Piton due to high maintenance costs (diplomatic,
% legal, financial) for a policy whose original function has degraded, now
% primarily serving a narrow interest group. High theater ratio reflects this.
constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israeli_settlement_policy_authority_restriction_tests).

test(perspectival_gap_moderate) :-
    % Verify perspectival gap between institutional target and beneficiary.
    constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, TypeTarget, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, TypeBeneficiary, context(agent_power(organized), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(perspectival_gap_powerless) :-
    % Verify perspectival gap between powerless target and beneficiary.
    constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israeli_settlement_policy_authority_restriction, TypeBeneficiary, context(agent_power(organized), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(israeli_settlement_policy_authority_restriction, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

test(piton_theater_ratio) :-
    domain_priors:theater_ratio(israeli_settlement_policy_authority_restriction, TR),
    TR >= 0.70.

:- end_tests(israeli_settlement_policy_authority_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high suppression score (0.70) reflects the active prevention of Palestinian development and governance in Area C. The extractiveness score (0.55) reflects the transfer of resources and control to Israeli settlements. The high theater ratio (0.75) reflects the significant diplomatic, legal, and administrative overhead required by the Israeli state to maintain a policy whose original strategic function has arguably degraded, making it a Piton from the state's perspective.
 *
 * PERSPECTIVAL GAP:
 *   Palestinian residents and the Palestinian Authority view the constraint as a Snare, as they are trapped with limited options and bear the costs. Israeli settlers view it as a Rope, providing a stable, beneficial framework for their activities. The Israeli government experiences it as a Piton, a costly and inertial policy. This multi-way gap is characteristic of a Tangled Rope at the analytical level.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli settlers benefit through increased land access, resource control, and reduced competition from Palestinian development. The Palestinian Authority bears costs through restricted development opportunities and loss of governance control. Palestinian residents of Area C bear the most direct costs through lack of services, building restrictions, and displacement risk.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The Israeli government, while an institutional actor and enforcer, experiences the policy as a Piton. Initially intended to secure broad territorial control, the policy now primarily serves the narrower interests of the settlers, while the government incurs significant international criticism and internal costs for enforcement. The high theater ratio (0.75) represents the large proportion of activity dedicated to performative justification (legal arguments, diplomatic statements) versus the policy's diminished function for the state's overall strategic interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by acknowledging the coordination benefit for Israeli settlers (Rope perspective), while highlighting the coercive extraction experienced by Palestinians (Snare perspective) and the institutional decay seen by the enforcing state (Piton perspective). The analytical Tangled Rope classification correctly synthesizes these conflicting realities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_israeli_settlement_policy_authority_restriction,
    'To what extent is the policy actively enforced for strategic goals versus passively maintained due to political inertia?',
    'Analysis of internal government communications vs. public statements regarding Area C policy.',
    'Higher strategic intent -> more Tangled Rope-like, higher inertia -> more Piton-like from the state perspective.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(israeli_settlement_policy_authority_restriction, 0, 10).

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
narrative_ontology:measurement(israeli_settlement_policy_authority_restriction_tr_t0, israeli_settlement_policy_authority_restriction, theater_ratio, 0, 0.40).
narrative_ontology:measurement(israeli_settlement_policy_authority_restriction_tr_t5, israeli_settlement_policy_authority_restriction, theater_ratio, 5, 0.60).
narrative_ontology:measurement(israeli_settlement_policy_authority_restriction_tr_t10, israeli_settlement_policy_authority_restriction, theater_ratio, 10, 0.75).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(israeli_settlement_policy_authority_restriction_ex_t0, israeli_settlement_policy_authority_restriction, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(israeli_settlement_policy_authority_restriction_ex_t5, israeli_settlement_policy_authority_restriction, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(israeli_settlement_policy_authority_restriction_ex_t10, israeli_settlement_policy_authority_restriction, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(israeli_settlement_policy_authority_restriction, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(israeli_settlement_policy_authority_restriction, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(israeli_settlement_policy_authority_restriction, [other_constraint_id]).

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
% narrative_ontology:affects_constraint(israeli_settlement_policy_authority_restriction, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(israeli_settlement_policy_authority_restriction, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */