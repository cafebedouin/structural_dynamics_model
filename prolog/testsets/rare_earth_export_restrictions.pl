% ============================================================================
% CONSTRAINT STORY: rare_earth_export_restrictions
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-27
% ============================================================================

:- module(constraint_rare_earth_export_restrictions, []).

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
 *   constraint_id: rare_earth_export_restrictions
 *   human_readable: Rare Earth Export Restrictions
 *   domain: economic
 *
 * SUMMARY:
 *   China's control over the rare earth elements (REE) supply chain, coupled with its use of export restrictions, creates a significant constraint on global manufacturers. This policy allows China to favor domestic industries while potentially disadvantaging foreign competitors relying on REEs. The degree of constraint varies across perspectives, from snare for trapped manufacturers to rope for Chinese industrial policy.
 *
 * KEY AGENTS (by structural relationship):
 *   - Foreign Manufacturers: Primary target (powerless/trapped) — bears extraction due to reliance on REEs for production.
 *   - Chinese Government: Primary beneficiary (institutional/arbitrage) — benefits through industrial policy advantages and strategic leverage.
 *   - Other Nations: Secondary actors (moderate/constrained) — affected nations seeking supply chain diversification.
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees the full structure and strategic implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_export_restrictions, 0.55).
domain_priors:suppression_score(rare_earth_export_restrictions, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(rare_earth_export_restrictions, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_export_restrictions, extractiveness, 0.55).
narrative_ontology:constraint_metric(rare_earth_export_restrictions, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(rare_earth_export_restrictions, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(rare_earth_export_restrictions, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(rare_earth_export_restrictions). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(rare_earth_export_restrictions, chinese_government).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(rare_earth_export_restrictions, foreign_manufacturers).

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
constraint_indexing:constraint_classification(rare_earth_export_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(rare_earth_export_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% Perspective 4: Other Nations (moderate power, constrained exit)
% These nations seek supply chain diversification but face constraints.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_export_restrictions_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(rare_earth_export_restrictions, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_export_restrictions, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rare_earth_export_restrictions, ExtMetricName, E),
    E >= 0.46. % High-extraction Tangled Rope/Snare.

:- end_tests(rare_earth_export_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint has a moderately high base extractiveness (0.55) because foreign manufacturers are heavily reliant on REEs for their production. The suppression score is also high (0.70) due to the lack of readily available alternative sources and the strategic control exercised by China.
 *
 * PERSPECTIVAL GAP:
 *   Foreign manufacturers perceive the restrictions as a snare because they are trapped by their dependence on REEs. The Chinese government views the restrictions as a rope, facilitating industrial policy and economic growth. The analytical observer recognizes both elements, classifying it as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The Chinese government benefits from the export restrictions through increased competitiveness of its domestic industries and strategic leverage in international relations. Foreign manufacturers bear the costs of these restrictions through increased production costs and supply chain vulnerabilities.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Other nations experience this constraint as a complex problem. They are attempting to develop alternative sources of REEs and diversify their supply chains to reduce reliance on China. Their exit option is constrained by technological limitations and geopolitical factors.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling coordination as pure extraction because the Chinese government's actions do have a genuine coordination function for its domestic industries. However, it simultaneously creates asymmetric extraction for foreign entities, justifying the tangled rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_rare_earth,
    'Will alternative REE sources become economically viable?',
    'Technological advancements in mining and processing outside of China',
    'If True: Reduced snare for foreign manufacturers. If False: Continued high dependence and extraction',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rare_earth_export_restrictions, 0, 10).

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
narrative_ontology:measurement(rare_earth_export_restrictions_tr_t0, rare_earth_export_restrictions, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rare_earth_export_restrictions_tr_t5, rare_earth_export_restrictions, theater_ratio, 5, 0.20).
narrative_ontology:measurement(rare_earth_export_restrictions_tr_t10, rare_earth_export_restrictions, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rare_earth_export_restrictions_ex_t0, rare_earth_export_restrictions, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rare_earth_export_restrictions_ex_t5, rare_earth_export_restrictions, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(rare_earth_export_restrictions_ex_t10, rare_earth_export_restrictions, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(rare_earth_export_restrictions, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(rare_earth_export_restrictions, 0.20).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(rare_earth_export_restrictions, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Example (uncomment if needed):
% constraint_indexing:directionality_override(rare_earth_export_restrictions, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */