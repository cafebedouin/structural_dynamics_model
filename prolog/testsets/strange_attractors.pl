% ============================================================================
% CONSTRAINT STORY: strange_attractor_systemic_risk
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_strange_attractor_systemic_risk, []).

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
 *   constraint_id: strange_attractor_systemic_risk
 *   human_readable: Systemic Risk Amplification via Strange Attractor Dynamics
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint describes how market dynamics, characterized by interconnected feedback loops and nonlinear relationships, can lead to emergent "strange attractors." These attractors concentrate risk and amplify systemic vulnerabilities, making the system more susceptible to shocks and cascading failures. Individual actors, optimizing for their own short-term gains, unknowingly reinforce these attractors, leading to collective instability.
 *
 * KEY AGENTS (by structural relationship):
 *   - Retail Investors: Primary target (powerless/trapped) — bears extraction due to amplified losses
 *   - Financial Institutions: Primary beneficiary (institutional/arbitrage) — benefits from increased transaction volume and risk arbitrage
 *   - Regulators: Secondary actor (institutional/constrained)
 *   - Economists: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strange_attractor_systemic_risk, 0.55).
domain_priors:suppression_score(strange_attractor_systemic_risk, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(strange_attractor_systemic_risk, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strange_attractor_systemic_risk, extractiveness, 0.55).
narrative_ontology:constraint_metric(strange_attractor_systemic_risk, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(strange_attractor_systemic_risk, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(strange_attractor_systemic_risk, tangled_rope).
narrative_ontology:human_readable(strange_attractor_systemic_risk, "Systemic Risk Amplification via Strange Attractor Dynamics").
narrative_ontology:topic_domain(strange_attractor_systemic_risk, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(strange_attractor_systemic_risk). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(strange_attractor_systemic_risk, financial_institutions).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(strange_attractor_systemic_risk, retail_investors).
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
constraint_indexing:constraint_classification(strange_attractor_systemic_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(strange_attractor_systemic_risk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(strange_attractor_systemic_risk, tangled_rope,
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
% Perspective 4A: Regulators (institutional, constrained exit)
constraint_indexing:constraint_classification(strange_attractor_systemic_risk, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strange_attractor_systemic_risk_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(strange_attractor_systemic_risk, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strange_attractor_systemic_risk, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(strange_attractor_systemic_risk, ExtMetricName, E),
    E >= 0.46. % High-extraction Snare/Tangled.

:- end_tests(strange_attractor_systemic_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.55 reflects the real losses experienced by retail investors due to market volatility amplified by these dynamics. The suppression score of 0.70 indicates the difficulty of alternative investment strategies and regulatory interventions. The theater ratio is low (0.30) because the underlying systemic dynamics are often obscured by technical jargon and complex models, not outright performative displays.
 *
 * PERSPECTIVAL GAP:
 *   Retail investors perceive this as a snare because they are often trapped by their limited knowledge and resources, facing disproportionate losses. Financial institutions see it as a rope because they benefit from the increased transaction volume and can exploit arbitrage opportunities.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial institutions benefit from increased market activity, even during downturns, as they profit from transaction fees and hedging strategies. Retail investors bear the cost of amplified market volatility and potential losses due to complex investment products and strategies they may not fully understand.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Regulators, while institutions themselves, are constrained by political pressures and limited resources. Therefore, their perspective is classified as Tangled Rope, as they are tasked with mitigating the risks but often face challenges in effectively controlling the underlying dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling this systemic risk as pure extraction by recognizing the inherent coordination function of markets, which provides liquidity and price discovery. However, the entangled nature of the attractor concentrates risk, leading to asymmetric extraction, especially affecting vulnerable participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_strange_attractor,
    'Is the observed market behavior truly a strange attractor, or merely a temporary phase of heightened volatility?',
    'Longitudinal analysis of market data using advanced nonlinear dynamical systems analysis.',
    'If True: Requires more aggressive regulatory interventions. If False: Current regulatory policies may be sufficient.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(strange_attractor_systemic_risk, 0, 10).

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
narrative_ontology:measurement(strange_attractor_systemic_risk_tr_t0, strange_attractor_systemic_risk, theater_ratio, 0, 0.20).
narrative_ontology:measurement(strange_attractor_systemic_risk_tr_t5, strange_attractor_systemic_risk, theater_ratio, 5, 0.30).
narrative_ontology:measurement(strange_attractor_systemic_risk_tr_t10, strange_attractor_systemic_risk, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(strange_attractor_systemic_risk_ex_t0, strange_attractor_systemic_risk, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(strange_attractor_systemic_risk_ex_t5, strange_attractor_systemic_risk, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(strange_attractor_systemic_risk_ex_t10, strange_attractor_systemic_risk, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(strange_attractor_systemic_risk, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(strange_attractor_systemic_risk, 0.20).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(strange_attractor_systemic_risk, other_constraint_id).

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
% constraint_indexing:directionality_override(strange_attractor_systemic_risk, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */