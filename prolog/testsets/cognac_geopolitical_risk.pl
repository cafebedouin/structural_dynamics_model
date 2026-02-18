% ============================================================================
% CONSTRAINT STORY: cognac_geopolitical_risk
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_cognac_geopolitical_risk, []).

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
 *   constraint_id: cognac_geopolitical_risk
 *   human_readable: Geopolitical Risk to Cognac Sales
 *   domain: economic
 *
 * SUMMARY:
 *   The global demand for Cognac is susceptible to geopolitical tensions and economic sanctions, impacting producers' revenue. In 2024, the threat of retaliatory tariffs by China on EU goods, including Cognac, due to EU investigations into Chinese subsidies highlights this vulnerability. This risk asymmetrically affects actors within the supply chain.
 *
 * KEY AGENTS (by structural relationship):
 *   - Small-scale grape growers: Primary target (powerless/trapped) — bears extraction with no recourse.
 *   - Large Cognac Producers (LVMH, Pernod Ricard): Primary target (institutional/constrained) — bears extraction from geopolitical risk but has some institutional power.
 *   - Geopolitical Actors (China, EU): Primary beneficiaries (institutional/constrained) — benefit from leveraging trade for political leverage.
 *   - Analytical Observer: Sees full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognac_geopolitical_risk, 0.50).
domain_priors:suppression_score(cognac_geopolitical_risk, 0.60).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cognac_geopolitical_risk, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognac_geopolitical_risk, extractiveness, 0.50).
narrative_ontology:constraint_metric(cognac_geopolitical_risk, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(cognac_geopolitical_risk, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cognac_geopolitical_risk, tangled_rope).
narrative_ontology:human_readable(cognac_geopolitical_risk, "Geopolitical Risk to Cognac Sales").
narrative_ontology:topic_domain(cognac_geopolitical_risk, "economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(cognac_geopolitical_risk). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cognac_geopolitical_risk, geopolitical_actors).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cognac_geopolitical_risk, cognac_producers).
narrative_ontology:constraint_victim(cognac_geopolitical_risk, small_scale_grape_growers).

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

% PERSPECTIVE 1: THE POWERLESS VICTIM (TANGLED ROPE)
% Small-scale grape growers. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
%   Scope modifier σ(regional)=0.9 dampens χ, keeping it below snare threshold.
%   χ = 0.50 * 1.42 * 0.9 = 0.639 -> Tangled Rope
constraint_indexing:constraint_classification(cognac_geopolitical_risk, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE INSTITUTIONAL TARGET (SNARE)
% Large Cognac producers. Engine derives d from:
%   victim membership + constrained exit → d ≈ 0.85 → f(d) ≈ 1.15
%   Global scope σ(global)=1.2 amplifies χ over the snare threshold.
%   χ = 0.50 * 1.15 * 1.2 = 0.69 -> Snare
constraint_indexing:constraint_classification(cognac_geopolitical_risk, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (ROPE)
% Geopolitical actors. Engine derives d from:
%   beneficiary membership + constrained exit → d ≈ 0.15 → f(d) ≈ -0.01 → low/negative χ
constraint_indexing:constraint_classification(cognac_geopolitical_risk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(cognac_geopolitical_risk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognac_geopolitical_risk_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between an institutional target and beneficiary.
    % Both have the same power and exit, but different structural relationships
    % (victim vs beneficiary) lead to different classifications.
    findall(Type,
            constraint_indexing:constraint_classification(cognac_geopolitical_risk, Type, context(agent_power(institutional), _, _, _)),
            Types),
    msort(Types, SortedUniqueTypes),
    member(rope, SortedUniqueTypes),
    member(snare, SortedUniqueTypes).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cognac_geopolitical_risk, ExtMetricName, E),
    E >= 0.46. % High-extraction Snare/Tangled.

:- end_tests(cognac_geopolitical_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Geopolitical risk is a classic tangled rope: it has a genuine coordination function (managing international trade and diplomatic relations) but also enables asymmetric extraction (tariffs, sanctions used as leverage). Base extractiveness is 0.50, reflecting the significant potential for value transfer from targeted industries. Suppression is 0.60, as diversifying entire national markets is extremely difficult for specialized, geographically-indicated products like Cognac.
 *
 * PERSPECTIVAL GAP:
 *   The gap is rich. Small-scale grape growers (powerless, trapped, regional) see a tangled rope because the regional scope dampens effective extraction below the snare threshold. Large producers (institutional, constrained, global) see a snare because their global operations amplify the effective extraction. Geopolitical actors (institutional beneficiaries) see a rope, as for them it's a tool of statecraft with manageable costs. The analytical observer synthesizes these views and confirms the tangled_rope classification, acknowledging both the coordination and extraction elements.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the state actors (e.g., China, EU) who can use trade policy as a tool of leverage. Victims are the economic actors caught in the middle: the large, publicly-traded Cognac producers and, more acutely, the small-scale grape growers who supply them and have no power to influence policy or diversify.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The EU and China both possess institutional power. The EU's investigation into Chinese subsidies prompts a retaliatory threat from China against an EU luxury good. This demonstrates the reciprocal nature of the constraint among institutional actors. Both have 'constrained' exit options because decoupling from major trade partners carries immense economic and political costs, making them subject to the rules of this game even as they shape them.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a tangled rope correctly avoids two errors. It is not a pure snare, because the underlying system of international trade and diplomacy has a vital coordination function. It is not a pure rope, because there is clear, asymmetric extraction imposed on specific industries for political ends. The framework correctly identifies the dual nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cognac,
    'To what extent can large producers absorb or deflect tariff costs versus passing them down to small-scale growers?',
    'Analysis of producer supply chain contracts and grower profit margins during trade disputes.',
    'If costs are passed down, the extraction on the powerless is higher than modeled. If absorbed, the institutional actor is a more effective shield.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cognac_geopolitical_risk, 0, 10).

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
narrative_ontology:measurement(cognac_geopolitical_risk_tr_t0, cognac_geopolitical_risk, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cognac_geopolitical_risk_tr_t5, cognac_geopolitical_risk, theater_ratio, 5, 0.20).
narrative_ontology:measurement(cognac_geopolitical_risk_tr_t10, cognac_geopolitical_risk, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cognac_geopolitical_risk_ex_t0, cognac_geopolitical_risk, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(cognac_geopolitical_risk_ex_t5, cognac_geopolitical_risk, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cognac_geopolitical_risk_ex_t10, cognac_geopolitical_risk, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(cognac_geopolitical_risk, enforcement_mechanism).

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
% No overrides needed for this constraint; the structural derivation is accurate.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */