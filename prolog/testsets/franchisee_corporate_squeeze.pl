% ============================================================================
% CONSTRAINT STORY: franchisee_corporate_squeeze
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-31
% ============================================================================

:- module(constraint_franchisee_corporate_squeeze, []).

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
 *   constraint_id: franchisee_corporate_squeeze
 *   human_readable: Franchise Agreement Squeeze
 *   domain: economic
 *
 * SUMMARY:
 *   Franchise agreements, while intended for mutual benefit, can become mechanisms for extracting wealth from franchisees by the corporate franchisor. This extraction occurs through mandatory purchases, marketing fees, and strict operational guidelines that favor the franchisor's profitability over the franchisee's, leading to reduced franchisee margins and increased risk. The closure of 250 Pizza Hut stores highlights the potential for this imbalance.
 *
 * KEY AGENTS (by structural relationship):
 *   - Franchisees: Primary target (moderate/constrained) — bears extraction.
 *   - Corporate Franchisor: Primary beneficiary (institutional/arbitrage) — benefits from constraint.
 *   - Consumers: Indirectly affected party (powerless/mobile) — experiences the coordination function (brand consistency).
 *   - Analytical Observer: Sees the full structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(franchisee_corporate_squeeze, 0.48).
domain_priors:suppression_score(franchisee_corporate_squeeze, 0.62).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(franchisee_corporate_squeeze, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, extractiveness, 0.48).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(franchisee_corporate_squeeze, tangled_rope).
narrative_ontology:human_readable(franchisee_corporate_squeeze, "Franchise Agreement Squeeze").

% --- Binary flags ---
domain_priors:requires_active_enforcement(franchisee_corporate_squeeze). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(franchisee_corporate_squeeze, corporate_franchisor).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(franchisee_corporate_squeeze, franchisees).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (FRANCHISEE)
% Agent who bears the most extraction. In this case, not fully powerless,
% but heavily constrained by the agreement.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (FRANCHISOR)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE END CONSUMER (ROPE)
% Indirectly affected by the constraint, experiencing the coordination function
% (brand consistency) without direct extraction. This satisfies the linter
% requirement for a powerless perspective.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(franchisee_corporate_squeeze_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypeTarget, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(franchisee_corporate_squeeze, ExtMetricName, E),
    E >= 0.46. % High-extraction Tangled Rope

:- end_tests(franchisee_corporate_squeeze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.48, reflecting that while there's a coordination function (brand recognition, supply chain), a significant portion of the value is being extracted to the franchisor. Suppression is set to 0.62, representing the limited alternatives available to franchisees due to contractual obligations and the high cost of exit. The theater ratio is 0.30, as there is some performative coordination but the primary function is extraction.
 *
 * PERSPECTIVAL GAP:
 *   The franchisee experiences the agreement as a Snare due to the high costs and limited exit options. The franchisor views it as a Rope, facilitating coordination and brand consistency. The analytical observer sees the Tangled Rope, acknowledging the coordination benefits alongside asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The corporate franchisor benefits through fees, royalties, and mandated purchases. Franchisees bear the costs in the form of reduced profitability, strict operational control, and limited ability to adapt to local market conditions. The directionality values are derived from these structural relationships and the associated exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a coordination mechanism as pure extraction by recognizing the coordination function provided by the franchise model (brand, supply chain, marketing). However, it also avoids mislabeling asymmetric extraction as pure coordination by acknowledging the power imbalance and the franchisor's ability to extract value at the expense of franchisees.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_franchisee_squeeze,
    'To what extent are the franchise agreements genuinely negotiated vs. imposed contracts?',
    'Analysis of franchise agreement negotiation records and legal precedents.',
    'If genuinely negotiated (more rope-like), franchisee risk is knowingly assumed. If imposed (more snare-like), franchisee exploitation is implied.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(franchisee_corporate_squeeze, 0, 10).

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
narrative_ontology:measurement(franchisee_corporate_squeeze_tr_t0, franchisee_corporate_squeeze, theater_ratio, 0, 0.20).
narrative_ontology:measurement(franchisee_corporate_squeeze_tr_t5, franchisee_corporate_squeeze, theater_ratio, 5, 0.25).
narrative_ontology:measurement(franchisee_corporate_squeeze_tr_t10, franchisee_corporate_squeeze, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(franchisee_corporate_squeeze_ex_t0, franchisee_corporate_squeeze, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(franchisee_corporate_squeeze_ex_t5, franchisee_corporate_squeeze, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(franchisee_corporate_squeeze_ex_t10, franchisee_corporate_squeeze, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(franchisee_corporate_squeeze, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */