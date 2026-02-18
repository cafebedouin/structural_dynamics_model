% ============================================================================
% CONSTRAINT STORY: oscar_campaign_spending
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_oscar_campaign_spending, []).

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
 *   constraint_id: oscar_campaign_spending
 *   human_readable: Oscar Campaign Spending Limits
 *   domain: social
 *
 * SUMMARY:
 *   The informal limit on Oscar campaign spending, beyond which returns diminish and backlash increases. Studios attempt to influence Academy voters through various means, but excessive spending can be counterproductive. This constraint aims to maintain a degree of fairness and prevent the awards from being solely determined by marketing budgets.
 *
 * KEY AGENTS (by structural relationship):
 *   - Smaller Studios/Independent Filmmakers: Primary target (powerless/trapped) — faces disadvantage due to limited resources
 *   - Major Studios: Primary beneficiary (institutional/arbitrage) — benefits from the ability to spend on campaigns, but also risks overspending and backlash
 *   - Academy Voters: Secondary actor (powerful/mobile) — influenced by campaigns, but also resistant to blatant manipulation.
 *   - Analytical observer — sees full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oscar_campaign_spending, 0.40).
domain_priors:suppression_score(oscar_campaign_spending, 0.50).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(oscar_campaign_spending, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oscar_campaign_spending, extractiveness, 0.40).
narrative_ontology:constraint_metric(oscar_campaign_spending, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(oscar_campaign_spending, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(oscar_campaign_spending, tangled_rope).
narrative_ontology:human_readable(oscar_campaign_spending, "Oscar Campaign Spending Limits").
narrative_ontology:topic_domain(oscar_campaign_spending, "social").

% --- Binary flags ---
domain_priors:requires_active_enforcement(oscar_campaign_spending). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(oscar_campaign_spending, major_studios).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(oscar_campaign_spending, smaller_studios).
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
% Smaller Studios/Independent Filmmakers
constraint_indexing:constraint_classification(oscar_campaign_spending, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Major Studios
constraint_indexing:constraint_classification(oscar_campaign_spending, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
constraint_indexing:constraint_classification(oscar_campaign_spending, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oscar_campaign_spending_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(oscar_campaign_spending, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oscar_campaign_spending, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(oscar_campaign_spending, ExtMetricName, E),
    E >= 0.30, E =< 0.90.

:- end_tests(oscar_campaign_spending_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   I assigned these scores based on the observation that Oscar campaigns have a moderate extractive element, as smaller studios are at a disadvantage. However, excessive spending is also actively discouraged, implying a coordination function as well.
 *
 * PERSPECTIVAL GAP:
 *   The smaller studios perceive the spending as a snare, trapping them because they lack the resources to compete effectively. The larger studios see it as a rope, a necessary element for competition, but with a point of diminishing returns if they overspend, causing them to be seen as overly aggressive.
 *
 * DIRECTIONALITY LOGIC:
 *   Major studios benefit as they can afford to spend, but not without limits. Smaller studios bear costs due to their relative inability to compete financially. Academy voters are implicitly modeled via constraints on studio behavior—they are not explicitly beneficiaries or victims, but their preferences shape the effectiveness of campaign spending.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling coordination as pure extraction because the system recognizes there *is* some benefit to the system due to increased visibility of nominated films and that pure extraction might result in backlash from voters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_oscar_campaign_spending,
    'How much influence do campaign expenditures *actually* have on Academy voters?',
    'Statistical analysis of campaign spending vs. voting patterns.',
    'If High: More Snare-like; If Low: More Rope-like.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(oscar_campaign_spending, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(oscar_campaign_spending_ex_t0, oscar_campaign_spending, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oscar_campaign_spending_ex_t5, oscar_campaign_spending, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(oscar_campaign_spending_ex_t10, oscar_campaign_spending, base_extractiveness, 10, 0.40).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(oscar_campaign_spending_tr_t0, oscar_campaign_spending, theater_ratio, 0, 0.20).
narrative_ontology:measurement(oscar_campaign_spending_tr_t5, oscar_campaign_spending, theater_ratio, 5, 0.30).
narrative_ontology:measurement(oscar_campaign_spending_tr_t10, oscar_campaign_spending, theater_ratio, 10, 0.30).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(oscar_campaign_spending, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */