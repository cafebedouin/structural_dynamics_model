% ============================================================================
% CONSTRAINT STORY: germany_tennet_takeover
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_germany_tennet_takeover, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: germany_tennet_takeover
 * human_readable: German Government Stake in TenneT Germany
 * domain: economic/political
 * * SUMMARY:
 * The German government is buying a significant stake in TenneT Germany, a crucial electricity grid operator, to prevent a foreign takeover and ensure energy security during the green energy transition. This acquisition is framed as vital infrastructure investment but is funded by taxpayers.
 * * KEY AGENTS:
 * - German Citizens: Subject (Powerless)
 * - German Government: Beneficiary (Institutional)
 * - Analytical Observer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(germany_tennet_takeover, 0.35). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(germany_tennet_takeover, 0.45).   % Structural property (raw, unscaled). Increased to reflect mandatory taxpayer funding.
domain_priors:theater_ratio(germany_tennet_takeover, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(germany_tennet_takeover, extractiveness, 0.35).
narrative_ontology:constraint_metric(germany_tennet_takeover, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(germany_tennet_takeover, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(germany_tennet_takeover, tangled_rope).

% Binary flags
% narrative_ontology:has_sunset_clause(germany_tennet_takeover).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(germany_tennet_takeover). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, german_government).
narrative_ontology:constraint_victim(germany_tennet_takeover, german_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (TANGLED ROPE)
% The use of taxpayer funds is felt as extraction, but the benefit of a stable
% energy grid is also present. χ = 0.35 * 1.5 * 1.0 = 0.525. This is below the
% Snare threshold (0.66) but well above the Rope threshold (0.35).
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination. χ = 0.35 * -0.2 * 1.0 = -0.07.
% The negative extraction indicates a net benefit from this perspective.
constraint_indexing:constraint_classification(germany_tennet_takeover, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. χ = 0.35 * 1.15 * 1.2 = 0.483.
% The observer sees both the coordination function (grid security) and the
% asymmetric extraction (taxpayer funding), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% This is not a temporary measure with a sunset clause.
% constraint_indexing:constraint_classification(germany_tennet_takeover, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(germany_tennet_takeover).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germany_tennet_takeover_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == tangled_rope,
    TypeInstitutional == rope.

test(analytical_classification) :-
    % Verify the analytical observer correctly identifies it as a Tangled Rope.
    constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_requirements) :-
    % A Tangled Rope requires enforcement, a beneficiary, and a victim.
    domain_priors:requires_active_enforcement(germany_tennet_takeover),
    narrative_ontology:constraint_beneficiary(germany_tennet_takeover, _),
    narrative_ontology:constraint_victim(germany_tennet_takeover, _).

:- end_tests(germany_tennet_takeover_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The German government's stake in TenneT is a classic Tangled Rope. From the perspective of taxpayers (powerless, trapped), it feels extractive (χ=0.525), but not predatory enough to be a Snare, as they also benefit from grid stability. The government (institutional) sees it as a pure Rope (χ=-0.07), a necessary coordination tool for national energy security with a net positive outcome.
 * The Analytical Observer confirms the Tangled Rope classification, recognizing the dual nature of the constraint: it has a genuine coordination function (securing critical infrastructure) but is funded through coercive, asymmetric extraction (taxation). The suppression score of 0.45 reflects the mandatory nature of taxpayer funding, and `requires_active_enforcement` is asserted because this policy is backed by the state's fiscal and regulatory power.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is crucial here. A simpler model might classify this as a Snare from the citizen's view and a Rope from the government's, creating an unbridgeable gap. Tangled Rope correctly identifies that even for the victim, there is a coordination benefit, preventing the system from mischaracterizing a complex public policy as pure predation. It acknowledges the legitimacy of the coordination claim while still quantifying the extractive cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_germany_tennet_takeover,
    'Will the state ownership lead to greater efficiency and security, or will it introduce political inefficiencies that negate the benefits?',
    'Long-term comparative analysis of grid performance, costs, and investment cycles against privately-managed European grid operators.',
    'If it leads to greater efficiency, the Rope classification becomes more dominant. If it leads to inefficiency, the Snare-like qualities are amplified.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(germany_tennet_takeover, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not strictly required as base_extractiveness (0.35) is below
% the 0.46 threshold, but is included to model the initial state and
% stabilization of the policy.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(germany_tennet_takeover_tr_t0, germany_tennet_takeover, theater_ratio, 0, 0.05).
narrative_ontology:measurement(germany_tennet_takeover_tr_t5, germany_tennet_takeover, theater_ratio, 5, 0.10).
narrative_ontology:measurement(germany_tennet_takeover_tr_t10, germany_tennet_takeover, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(germany_tennet_takeover_ex_t0, germany_tennet_takeover, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(germany_tennet_takeover_ex_t5, germany_tennet_takeover, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(germany_tennet_takeover_ex_t10, germany_tennet_takeover, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a state intervention to manage a critical resource (the energy grid).
narrative_ontology:coordination_type(germany_tennet_takeover, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% No override is justified; default for resource_allocation will be used.
% narrative_ontology:boltzmann_floor_override(germany_tennet_takeover, 0.1).

% Network relationships (structural influence edges)
% This act of partial nationalization sets a precedent that could influence
% policy regarding other critical infrastructure.
narrative_ontology:affects_constraint(germany_tennet_takeover, german_port_logistics).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */