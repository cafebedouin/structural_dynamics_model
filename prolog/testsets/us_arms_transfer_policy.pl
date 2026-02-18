% ============================================================================
% CONSTRAINT STORY: us_arms_transfer_policy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_us_arms_transfer_policy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_arms_transfer_policy
 *   human_readable: US Arms Transfer Policy
 *   domain: political
 *
 * SUMMARY:
 *   The US Arms Transfer Policy governs the sale and distribution of weapons and military technology by the United States to other nations. This policy is intended to balance economic and strategic interests with human rights concerns, but often results in the prioritization of strategic alliances and economic gains, even at the expense of civilian populations in recipient countries.
 *
 * KEY AGENTS (by structural relationship):
 *   - Civilian Populations in Recipient Countries: Primary target (powerless/trapped) — bears extraction (increased conflict, instability)
 *   - US Defense Industry: Primary beneficiary (institutional/arbitrage) — benefits from arms sales
 *   - US Government: Secondary actor (institutional/constrained) — balances strategic interests, economic gains, and human rights (often with strategic interests dominating)
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_arms_transfer_policy, 0.55).
domain_priors:suppression_score(us_arms_transfer_policy, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_arms_transfer_policy, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_arms_transfer_policy, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_arms_transfer_policy, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(us_arms_transfer_policy, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(us_arms_transfer_policy, accessibility_collapse, [0.85-1.0]).
% narrative_ontology:constraint_metric(us_arms_transfer_policy, resistance, [0.0-0.15]).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_arms_transfer_policy, tangled_rope).
narrative_ontology:human_readable(us_arms_transfer_policy, "US Arms Transfer Policy").
narrative_ontology:topic_domain(us_arms_transfer_policy, "political").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(us_arms_transfer_policy).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(us_arms_transfer_policy). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(us_arms_transfer_policy).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_arms_transfer_policy, us_defense_industry).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_arms_transfer_policy, civilian_populations_in_recipient_countries).
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
constraint_indexing:constraint_classification(us_arms_transfer_policy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(us_arms_transfer_policy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(us_arms_transfer_policy, tangled_rope,
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
% constraint_indexing:constraint_classification(us_arms_transfer_policy, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(national))).
%
% % Perspective 4B: Regulated company (institutional, arbitrage exit)
% constraint_indexing:constraint_classification(us_arms_transfer_policy, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(arbitrage),
%             spatial_scope(national))).

% PERSPECTIVE 5: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, χ ≤ 0.30, theater ≤ 0.70.
% constraint_indexing:constraint_classification(us_arms_transfer_policy, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(us_arms_transfer_policy).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_arms_transfer_policy_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(us_arms_transfer_policy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_arms_transfer_policy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_arms_transfer_policy, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(us_arms_transfer_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.55, reflecting the significant negative externalities borne by civilian populations in recipient countries (increased violence, displacement, and human rights abuses). The suppression score is 0.70, reflecting the active efforts to suppress alternative policies, such as stricter human rights conditionality or a complete ban on arms sales to certain regimes, due to lobbying by the defense industry and strategic considerations. The theater ratio is low (0.30), indicating that the policy primarily serves its intended economic and strategic functions, rather than being primarily for show.
 *
 * PERSPECTIVAL GAP:
 *   Civilian populations in recipient countries experience the policy as a Snare due to the increased violence and instability resulting from arms transfers, while the US defense industry perceives it as a Rope, facilitating their economic activity and growth.
 *
 * DIRECTIONALITY LOGIC:
 *   The US defense industry benefits economically from arms sales, leading to a low d value (0.05 after exit option adjustment), and is therefore classified as a Rope. Civilian populations in recipient countries bear the costs in terms of violence and instability, leading to a high d value (0.95 after exit option adjustment), and is therefore classified as a Snare.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   The US government faces competing pressures: strategic alliances, economic gains for the defense industry, and human rights considerations. This creates a complex dynamic where the policy outcomes are often a compromise that falls short of fully addressing human rights concerns.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as pure coordination (Rope) by recognizing the asymmetric extraction experienced by civilian populations. It also prevents mislabeling as a Snare by acknowledging the coordination function of supporting strategic alliances and economic benefits for the US defense industry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_arms_transfer_policy,
    'To what extent does the US government genuinely prioritize human rights considerations in its arms transfer decisions?',
    'Detailed analysis of internal policy memos and external impact assessments.',
    'If human rights are genuinely prioritized, the extractiveness would be lower and the classification might shift towards a more balanced Tangled Rope or even a less extractive system. If not, the Snare classification for affected populations is more accurate.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_arms_transfer_policy, 0, 10).

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
narrative_ontology:measurement(us_arms_transfer_policy_tr_t0, us_arms_transfer_policy, theater_ratio, 0, 0.20).
narrative_ontology:measurement(us_arms_transfer_policy_tr_t5, us_arms_transfer_policy, theater_ratio, 5, 0.25).
narrative_ontology:measurement(us_arms_transfer_policy_tr_t10, us_arms_transfer_policy, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(us_arms_transfer_policy_ex_t0, us_arms_transfer_policy, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(us_arms_transfer_policy_ex_t5, us_arms_transfer_policy, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(us_arms_transfer_policy_ex_t10, us_arms_transfer_policy, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(us_arms_transfer_policy, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(us_arms_transfer_policy, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(us_arms_transfer_policy, [other_constraint_id]).

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
% narrative_ontology:affects_constraint(us_arms_transfer_policy, [sibling_constraint_id]).

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
% constraint_indexing:directionality_override(us_arms_transfer_policy, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */