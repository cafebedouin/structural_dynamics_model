% ============================================================================
% CONSTRAINT STORY: news_paywall_inequality
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-30
% ============================================================================

:- module(constraint_news_paywall_inequality, []).

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
 *   constraint_id: news_paywall_inequality
 *   human_readable: Information Asymmetry due to News Paywalls
 *   domain: social
 *
 * SUMMARY:
 *   News paywalls create an information asymmetry where access to high-quality journalism is restricted to those who can afford it, while those who cannot afford it rely on freely available but potentially lower quality or biased sources. This creates a stratified information ecosystem.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-income individuals: Primary target (powerless/trapped) — bears extraction
 *   - News publishers: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Advertisers: Secondary beneficiary (powerful/arbitrage) - benefit from traffic
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(news_paywall_inequality, 0.48).
domain_priors:suppression_score(news_paywall_inequality, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(news_paywall_inequality, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(news_paywall_inequality, extractiveness, 0.48).
narrative_ontology:constraint_metric(news_paywall_inequality, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(news_paywall_inequality, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(news_paywall_inequality, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(news_paywall_inequality). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(news_paywall_inequality, news_publishers).
narrative_ontology:constraint_beneficiary(news_paywall_inequality, advertisers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(news_paywall_inequality, low_income_individuals).

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
constraint_indexing:constraint_classification(news_paywall_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(news_paywall_inequality, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(news_paywall_inequality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(news_paywall_inequality_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(news_paywall_inequality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(news_paywall_inequality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(news_paywall_inequality, ExtMetricName, E),
    E >= 0.46. % Expecting a high-extraction tangled rope or snare

:- end_tests(news_paywall_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is 0.48 because news paywalls do extract value from individuals who cannot afford to subscribe, as they are deprived of access to potentially critical information and in-depth reporting. The suppression score is 0.55 because while alternative sources exist, they may be of lower quality, less comprehensive, or biased. There is active enforcement as the paywalls are actively maintained. Theater ratio is low because paywalls primarily serve a direct revenue-generating function.
 *
 * PERSPECTIVAL GAP:
 *   Low-income individuals perceive the paywall as a Snare, trapping them in a lower-quality information environment. News publishers view it as a Rope, a necessary coordination mechanism to fund journalism in an era of declining ad revenue. The analytical observer sees the combined coordination and extraction and thus classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   News publishers benefit because the paywall generates revenue and potentially a more engaged subscriber base. Low-income individuals bear the cost because they are excluded from accessing the information. Advertisers benefit from the increased user traffic to freely available articles. Beneficiary/victim declarations map to the structural relationships because news publishers are structurally positioned to gain from the paywall, while low-income individuals are structurally disadvantaged.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this as pure extraction (Snare) because there *is* a coordination function (funding quality journalism). It also prevents mislabeling it as pure coordination (Rope) because there *is* demonstrably asymmetric extraction—a barrier to entry based on income.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_news_paywall,
    'To what extent does the information disparity created by paywalls impact civic engagement and democratic participation?',
    'Longitudinal studies on the correlation between news access, political knowledge, and voter turnout across income brackets.',
    'If true (high impact), stricter regulations or public funding for news may be warranted. If false (low impact), current paywall strategies may be considered less problematic.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(news_paywall_inequality, 0, 10).

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
narrative_ontology:measurement(news_paywall_inequality_tr_t0, news_paywall_inequality, theater_ratio, 0, 0.10).
narrative_ontology:measurement(news_paywall_inequality_tr_t5, news_paywall_inequality, theater_ratio, 5, 0.15).
narrative_ontology:measurement(news_paywall_inequality_tr_t10, news_paywall_inequality, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(news_paywall_inequality_ex_t0, news_paywall_inequality, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(news_paywall_inequality_ex_t5, news_paywall_inequality, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(news_paywall_inequality_ex_t10, news_paywall_inequality, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(news_paywall_inequality, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */