% ============================================================================
% CONSTRAINT STORY: olympic_legacy_curling_investment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-26
% ============================================================================

:- module(constraint_olympic_legacy_curling_investment, []).

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
 *   constraint_id: olympic_legacy_curling_investment
 *   human_readable: Olympic Games Legacy Investment in Curling Clubs
 *   domain: economic
 *
 * SUMMARY:
 *   Following the Winter Olympics, there is often an increase in funding for the sports featured. This story focuses on the potential economic constraint created when that funding flows primarily to established curling clubs, potentially excluding smaller, less resourced clubs, or creating a long-term dependency on funding. It also considers the possibility that some of this funding is used in theatrical, performative ways.
 *
 * KEY AGENTS (by structural relationship):
 *   - Smaller Curling Clubs: Primary target (powerless/trapped) — bears potential exclusion or dependency
 *   - Established Curling Clubs: Primary beneficiary (powerful/arbitrage) — benefits from increased funding
 *   - National Olympic Committees: Secondary actor (institutional/constrained) — allocates funding, subject to political pressures
 *   - Analytical observer — sees full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(olympic_legacy_curling_investment, 0.35).
domain_priors:suppression_score(olympic_legacy_curling_investment, 0.40).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(olympic_legacy_curling_investment, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, extractiveness, 0.35).
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(olympic_legacy_curling_investment, tangled_rope).
narrative_ontology:human_readable(olympic_legacy_curling_investment, "Olympic Games Legacy Investment in Curling Clubs").

% --- Binary flags ---
domain_priors:requires_active_enforcement(olympic_legacy_curling_investment). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(olympic_legacy_curling_investment, established_curling_clubs).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(olympic_legacy_curling_investment, smaller_curling_clubs).
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

% PERSPECTIVE 1: THE PRIMARY TARGET
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.
% Perspective 4A: National Olympic Committees (institutional, constrained exit)
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4B: Established Curling Clubs (institutional, arbitrage exit)
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(olympic_legacy_curling_investment_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(olympic_legacy_curling_investment, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(olympic_legacy_curling_investment, TypeBeneficiary, context(agent_power(powerful), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(olympic_legacy_curling_investment, ExtMetricName, E),
    E < 0.46. % Check extraction is below snare threshold.

:- end_tests(olympic_legacy_curling_investment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.35 reflects the degree to which resources can be diverted from broader community benefits towards established clubs. Suppression is 0.40 because this funding mechanism can crowd out alternative community sports programs. The theater ratio of 0.3 reflects that a significant portion of the investment can be tied to performative activities, instead of building lasting infrastructure. The system requires active enforcement through funding allocation rules and grant-making bodies that favor established entities.
 *
 * PERSPECTIVAL GAP:
 *   Smaller, trapped clubs experience the funding mechanism as a TANGLED ROPE due to structural exclusion and dependency, which drives up effective extraction (χ). Beneficiaries like established clubs, who can leverage the funding with arbitrage exit options, perceive it as a pure coordination ROPE. The analytical observer, seeing both the coordination function and the asymmetric extraction, also classifies it as a TANGLED ROPE, aligning with the powerless perspective on the structure, if not the felt cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Established curling clubs benefit directly through increased funding, leading to a low d value (directionality). Smaller curling clubs bear costs through exclusion or increased competition for resources, leading to a higher d value.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The National Olympic Committees are constrained by political pressures and internal regulations in how they allocate funding. This can lead to prioritization of established clubs with existing infrastructure and relationships over smaller, less established clubs. From their perspective, it is a ROPE that grows a valuable sport. However, their constrained exit option means they experience more of the system's friction than the established clubs, who can arbitrage their resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope correctly identifies that this is not pure extraction; there is a genuine coordination function (promoting a sport). However, it avoids mislabeling it as a pure public good (Rope) by capturing the asymmetric distribution of benefits that is enforced by the funding structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_olympic_legacy_curling_investment,
    'To what extent will funding allocation mechanisms favor established clubs over emerging ones?',
    'Detailed analysis of funding distribution and program participation rates post-Olympics.',
    'If True (high bias): Tangled Rope. If False (equitable): Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(olympic_legacy_curling_investment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Not required as base_extractiveness (0.35) is not > 0.46.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(olympic_legacy_curling_investment, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]

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

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */