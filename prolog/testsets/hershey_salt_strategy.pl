% ============================================================================
% CONSTRAINT STORY: hershey_salt_strategy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_hershey_salt_strategy, []).

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
 *   constraint_id: hershey_salt_strategy
 *   human_readable: Hershey's Salt Inclusion Strategy
 *   domain: economic
 *
 * SUMMARY:
 *   Hershey's strategy to include salt in more of its products to increase consumer demand.  This strategy creates a subtle "extraction" of value from consumers by manipulating their biological responses to crave more of the product.  This happens as Hershey also lobbies against regulations preventing this kind of flavor engineering.
 *
 * KEY AGENTS (by structural relationship):
 *   - Consumers: Primary target (powerless/trapped) — bears extraction
 *   - Hershey: Primary beneficiary (institutional/arbitrage) — benefits from strategy
 *   - Regulators: Secondary actor (institutional/constrained)
 *   - Analysts: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hershey_salt_strategy, 0.48).
domain_priors:suppression_score(hershey_salt_strategy, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(hershey_salt_strategy, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hershey_salt_strategy, extractiveness, 0.48).
narrative_ontology:constraint_metric(hershey_salt_strategy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hershey_salt_strategy, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hershey_salt_strategy, tangled_rope).
narrative_ontology:human_readable(hershey_salt_strategy, "Hershey's Salt Inclusion Strategy").
narrative_ontology:topic_domain(hershey_salt_strategy, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(hershey_salt_strategy). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
narrative_ontology:constraint_beneficiary(hershey_salt_strategy, hershey).
narrative_ontology:constraint_victim(hershey_salt_strategy, consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
constraint_indexing:constraint_classification(hershey_salt_strategy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
constraint_indexing:constraint_classification(hershey_salt_strategy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
constraint_indexing:constraint_classification(hershey_salt_strategy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% INTER-INSTITUTIONAL PERSPECTIVES: Regulators are constrained by lobbying
constraint_indexing:constraint_classification(hershey_salt_strategy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hershey_salt_strategy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hershey_salt_strategy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hershey_salt_strategy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hershey_salt_strategy, ExtMetricName, E),
    E >= 0.46. % High-extraction Snare/Tangled.

:- end_tests(hershey_salt_strategy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.48 reflecting the subtle manipulation
 *   of consumer behavior. The suppression score of 0.65 is high because the
 *   industry lobbies against regulations that would provide consumers with
 *   healthier alternatives or better information about the effects of added
 *   salt and sugar.
 *
 * PERSPECTIVAL GAP:
 *   Consumers perceive this as a snare because they are unaware of the
 *   engineered craving and find it difficult to resist the products.
 *   Hershey views this as a rope because it coordinates its product
 *   development and marketing strategy to increase profits.
 *
 * DIRECTIONALITY LOGIC:
 *   Hershey benefits through increased sales and profits, leading to a
 *   beneficiary status. Consumers bear the cost through potential health
 *   issues related to increased salt intake, justifying their victim status.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   Regulators are intended to protect consumers but are often subject to
 *   lobbying efforts from Hershey, constraining their ability to act
 *   decisively.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling this as pure coordination. While Hershey coordinates internally, the strategy has a strong extractive component toward consumers who are subtly manipulated into buying more product than they would otherwise. This prevents framing this as a socially benign business decision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hershey_salt,
    'To what extent are consumers aware of the engineered craving?',
    'Longitudinal studies on consumer awareness and behavior.',
    'If consumers are largely unaware, the constraint is more extractive. If aware, less so.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hershey_salt_strategy, 0, 10).

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
narrative_ontology:measurement(hershey_salt_tr_t0, hershey_salt_strategy, theater_ratio, 0, 0.20).
narrative_ontology:measurement(hershey_salt_tr_t5, hershey_salt_strategy, theater_ratio, 5, 0.25).
narrative_ontology:measurement(hershey_salt_tr_t10, hershey_salt_strategy, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(hershey_salt_ex_t0, hershey_salt_strategy, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(hershey_salt_ex_t5, hershey_salt_strategy, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(hershey_salt_ex_t10, hershey_salt_strategy, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(hershey_salt_strategy, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(hershey_salt_strategy, 0.2).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(hershey_salt_strategy, [other_constraint_id]).

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
% constraint_indexing:directionality_override(hershey_salt_strategy, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */