% ============================================================================
% CONSTRAINT STORY: g7_debt_trap
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_g7_debt_trap, []).

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
 *   constraint_id: g7_debt_trap
 *   human_readable: G7 Debt Trap for Developing Nations
 *   domain: economic
 *
 * SUMMARY:
 *   The G7 and associated institutions (IMF, World Bank) provide loans to developing nations, often with conditions that require structural adjustment policies. These policies can lead to increased extraction from the borrowing nation to service the debt, creating a long-term dependency and limiting economic sovereignty.
 *
 * KEY AGENTS (by structural relationship):
 *   - Developing Nations: Primary target (powerless/trapped) — bears extraction
 *   - G7 Nations/IMF/World Bank: Primary beneficiary (institutional/arbitrage) — benefits from debt repayment and policy influence.
 *   - Local Elites in Developing Nations: Secondary actor (moderate/mobile). Benefit from the initial influx of capital and often implement the policies that create long-term problems.
 *   - Analytical Observer: Sees full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(g7_debt_trap, 0.55). % Substantial extraction due to debt servicing and policy constraints
domain_priors:suppression_score(g7_debt_trap, 0.70).   % Limited alternatives due to reliance on external financing.
domain_priors:theater_ratio(g7_debt_trap, 0.30).       % Relatively low theater. The debt servicing is a real obligation, but structural adjustment policies are often promoted as "good governance".

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(g7_debt_trap, extractiveness, 0.55).
narrative_ontology:constraint_metric(g7_debt_trap, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(g7_debt_trap, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(g7_debt_trap, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(g7_debt_trap). %Active enforcement through repayment schedules and conditionality.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
narrative_ontology:constraint_beneficiary(g7_debt_trap, g7_nations).
narrative_ontology:constraint_victim(g7_debt_trap, developing_nations).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
constraint_indexing:constraint_classification(g7_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
constraint_indexing:constraint_classification(g7_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
constraint_indexing:constraint_classification(g7_debt_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: LOCAL ELITES IN DEVELOPING NATIONS
constraint_indexing:constraint_classification(g7_debt_trap, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(g7_debt_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(g7_debt_trap, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(g7_debt_trap, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(g7_debt_trap, ExtMetricName, E),
    E >= 0.46.

:- end_tests(g7_debt_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The G7 debt trap is assigned a base extractiveness of 0.55, reflecting the substantial burden placed on developing nations to service their debts. The suppression score of 0.70 acknowledges that reliance on external financing often limits the alternatives available to these nations, forcing them to comply with conditions set by the lending institutions. The theater ratio is relatively low at 0.30 because, while some of the promoted policies may have a performative aspect, the debt obligations themselves are very real.
 *
 * PERSPECTIVAL GAP:
 *   Developing nations perceive the constraint as a snare, due to their limited exit options and the long-term negative consequences of debt and structural adjustment. G7 nations, on the other hand, often view it as a form of coordination or development assistance (Rope), where they are providing needed capital.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiaries are identified as G7 nations and associated institutions like the IMF and World Bank, who benefit from debt repayment and increased influence over economic policy. The victims are the developing nations who bear the cost of debt servicing and the potentially harmful effects of structural adjustment policies. Local elites are identified as benefiting in the short term via access to investment capital, however they also can become victims as a result of the negative long-term impacts from policy implemnetation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as pure extraction (Snare) by acknowledging that there is some coordination function (capital provision). However, the significant extractiveness and suppression indicate that the coordination benefits are overshadowed by the asymmetric extraction imposed on developing nations. It also prevents labeling as pure coordination (Rope), which would ignore the exploitative components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_g7_debt,
    'To what degree do structural adjustment policies genuinely benefit developing nations in the long term?',
    'Empirical studies comparing economic growth and social outcomes in nations that have undergone structural adjustment versus those that haven\'t.',
    'If True: Reclassification toward Rope. If False: Stronger Snare classification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(g7_debt_trap, 0, 10).

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
narrative_ontology:measurement(g7_debt_trap_tr_t0, g7_debt_trap, theater_ratio, 0, 0.20).
narrative_ontology:measurement(g7_debt_trap_tr_t5, g7_debt_trap, theater_ratio, 5, 0.30).
narrative_ontology:measurement(g7_debt_trap_tr_t10, g7_debt_trap, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(g7_debt_trap_ex_t0, g7_debt_trap, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(g7_debt_trap_ex_t5, g7_debt_trap, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(g7_debt_trap_ex_t10, g7_debt_trap, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(g7_debt_trap, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed in this scenario

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */