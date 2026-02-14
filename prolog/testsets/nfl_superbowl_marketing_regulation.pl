% ============================================================================
% CONSTRAINT STORY: nfl_superbowl_marketing_regulation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-22
% ============================================================================

:- module(constraint_nfl_superbowl_marketing_regulation, []).

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
 *   constraint_id: nfl_superbowl_marketing_regulation
 *   human_readable: NFL Super Bowl Advertising Regulations
 *   domain: economic
 *
 * SUMMARY:
 *   The NFL exerts strong control over advertising and marketing during the Super Bowl, limiting competitors and extracting rent from advertisers who wish to associate with the event. This includes restrictions on the use of NFL trademarks and imagery. This regulation is actively enforced to protect the NFL's branding and revenue.
 *
 * KEY AGENTS (by structural relationship):
 *   - Small Businesses & Non-Sponsors: Primary target (powerless/trapped) — bears extraction
 *   - NFL: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Major Advertisers & Large Competitors: Secondary actors (powerful/mobile or moderate/constrained) — partially benefit, partially bear cost
 *   - Analytical observer: Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nfl_superbowl_marketing_regulation, 0.55).
domain_priors:suppression_score(nfl_superbowl_marketing_regulation, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(nfl_superbowl_marketing_regulation, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, extractiveness, 0.55).
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nfl_superbowl_marketing_regulation, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(nfl_superbowl_marketing_regulation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, nfl).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, competing_brands).

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
% Small businesses and non-sponsors are completely excluded from the marketing ecosystem.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The NFL benefits from exclusivity and brand protection.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The constraint has elements of both coordination (brand protection) and extraction (rent-seeking)
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: SUPER BOWL ADVERTISERS
% These companies benefit from associating with the Super Bowl, but still face restrictions and costs
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nfl_superbowl_marketing_regulation_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, ExtMetricName, E),
    E >= 0.46. % High-extraction Tangled Rope

:- end_tests(nfl_superbowl_marketing_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The NFL exerts significant control over the Super Bowl's marketing landscape, restricting competitor access and generating substantial revenue. The base extractiveness is set to 0.55, reflecting the cost to competing brands wishing to leverage the Super Bowl's cultural prominence without directly paying the NFL. The suppression score is 0.70, reflecting the NFL's active enforcement of trademark regulations and limitations on ambush marketing.
 *
 * PERSPECTIVAL GAP:
 *   Small businesses and non-sponsoring brands perceive the regulation as a snare, as it completely excludes them from marketing opportunities. The NFL sees it as a rope, a coordination mechanism protecting their brand and revenue. Major advertisers see it as a tangled rope because they benefit from the association but are also subject to the NFL's control and high costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The NFL benefits directly through increased revenue and brand protection. Competing brands, particularly smaller ones, bear the cost of restricted marketing opportunities. Super Bowl Advertisers are somewhat symmetric: they benefit from the Super Bowl's popularity but are also subject to marketing restrictions and high advertising costs.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents mislabeling the NFL's actions as purely extractive. There is a genuine coordination function in protecting the Super Bowl brand, but this is coupled with significant rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_nfl_marketing,
    'To what extent does the NFL regulation genuinely protect the Super Bowl brand vs. simply extracting rent?',
    'Empirical study of brand impact from competitor advertising during the Super Bowl.',
    'If the primary function is brand protection, it may be closer to a rope; if primarily rent-seeking, a snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nfl_superbowl_marketing_regulation, 0, 10).

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
narrative_ontology:measurement(nfl_marketing_tr_t0, nfl_superbowl_marketing_regulation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nfl_marketing_tr_t5, nfl_superbowl_marketing_regulation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(nfl_marketing_tr_t10, nfl_superbowl_marketing_regulation, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(nfl_marketing_ex_t0, nfl_superbowl_marketing_regulation, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(nfl_marketing_ex_t5, nfl_superbowl_marketing_regulation, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(nfl_marketing_ex_t10, nfl_superbowl_marketing_regulation, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(nfl_superbowl_marketing_regulation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */