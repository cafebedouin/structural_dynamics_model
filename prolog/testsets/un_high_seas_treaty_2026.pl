% ============================================================================
% CONSTRAINT STORY: un_high_seas_treaty_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_un_high_seas_treaty_2026, []).

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
 *   constraint_id: un_high_seas_treaty_2026
 *   human_readable: UN High Seas Treaty for Marine Biodiversity (BBNJ)
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The UN High Seas Treaty, effective in 2026, establishes a legal
 *   framework for governing biodiversity in areas beyond national jurisdiction
 *   (the "high seas"). It aims to create marine protected areas and regulate
 *   activities like deep-sea mining and fishing, while also creating a
 *   mechanism for sharing benefits from marine genetic resources. The
 *   constraint is the new regulatory regime itself, which coordinates
 *   conservation efforts but also imposes significant costs and restrictions
 *   on industries that previously operated with near-total freedom.
 *
 * KEY AGENTS (by structural relationship):
 *   - Global Conservation Community: Primary beneficiary (institutional/arbitrage) — benefits from protected ecosystems.
 *   - High-Seas Extractive Industries: Primary target (organized/constrained) — bears compliance costs and access restrictions.
 *   - Small-scale Fishing Operators: Secondary target (powerless/trapped) — potentially priced out by compliance burdens.
 *   - Developing Nations: Hybrid role (moderate/constrained) — benefit from shared resources but face implementation costs.
 *   - Analytical Observer: Sees the full hybrid structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(un_high_seas_treaty_2026, 0.40).
domain_priors:suppression_score(un_high_seas_treaty_2026, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(un_high_seas_treaty_2026, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, extractiveness, 0.40).
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(un_high_seas_treaty_2026, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(un_high_seas_treaty_2026, tangled_rope).
narrative_ontology:human_readable(un_high_seas_treaty_2026, "UN High Seas Treaty for Marine Biodiversity (BBNJ)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(un_high_seas_treaty_2026). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(un_high_seas_treaty_2026, global_conservation_community).
narrative_ontology:constraint_beneficiary(un_high_seas_treaty_2026, developing_nations).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(un_high_seas_treaty_2026, high_seas_extractive_industries).
narrative_ontology:constraint_victim(un_high_seas_treaty_2026, small_scale_fishing_operators).
narrative_ontology:constraint_victim(un_high_seas_treaty_2026, developing_nations).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SMALL OPERATOR'S VIEW)
% Small-scale operators who lack the capital for compliance or legal challenges.
% Engine derives d from victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ
% χ = 0.40 * 1.42 * 1.2 (global scope) ≈ 0.68. This meets the Snare threshold (>= 0.66).
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY
% Global conservation community (NGOs, UN bodies). Engine derives d from:
% beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ
% χ = 0.40 * (-0.12) * 1.2 ≈ -0.06. This is clearly a Rope.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Engine derives d ≈ 0.72 -> f(d) ≈ 1.15.
% χ = 0.40 * 1.15 * 1.2 ≈ 0.55. This is squarely in the Tangled Rope range.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: ORGANIZED EXTRACTIVE INDUSTRIES
% Large fishing conglomerates, deep-sea mining firms. They are victims, but powerful.
% Engine derives d from victim membership + organized power + constrained exit -> d ≈ 0.65 -> f(d) ≈ 1.0.
% χ = 0.40 * 1.0 * 1.2 ≈ 0.48. This is a Tangled Rope, reflecting their ability to mitigate some costs.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4B: DEVELOPING NATIONS
% A hybrid role as both beneficiary (benefit-sharing) and victim (implementation costs).
% Engine sees membership in both groups, deriving a moderate d ≈ 0.55 -> f(d) ≈ 0.75.
% χ = 0.40 * 0.75 * 1.2 ≈ 0.36. This is on the cusp between Rope and Tangled Rope,
% reflecting the deep ambiguity of their position. The system will classify it as Tangled Rope
% due to the presence of non-trivial extraction and suppression.
constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(un_high_seas_treaty_2026_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between the powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(un_high_seas_treaty_2026, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(un_high_seas_treaty_2026, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % The system's objective classification must be Tangled Rope.
    constraint_indexing:constraint_classification(un_high_seas_treaty_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify the structural predicates required for a Tangled Rope classification are present.
    narrative_ontology:constraint_beneficiary(un_high_seas_treaty_2026, _),
    narrative_ontology:constraint_victim(un_high_seas_treaty_2026, _),
    domain_priors:requires_active_enforcement(un_high_seas_treaty_2026).

:- end_tests(un_high_seas_treaty_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.40): This value was chosen to represent a significant
 *     hybrid constraint. It's high enough to impose real costs (extraction) on
 *     affected industries, but not so high as to be a pure Snare (ε >= 0.46). It
 *     acknowledges the genuine, non-trivial coordination function of protecting a
 *     global commons.
 *   - Suppression (0.55): The treaty actively suppresses the prior norm of
 *     "freedom of the high seas" for extractive purposes. It's coercive and
 *     backed by international law, justifying a score above 0.5.
 *   - Classification: The analytical classification is Tangled Rope because the
 *     treaty possesses both a genuine coordination function (beneficiaries exist)
 *     and asymmetric extraction (victims exist), supported by active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the institutional conservation community (beneficiary),
 *   the treaty is a pure Rope: a mechanism for solving a collective action problem
 *   with negative perceived extraction (χ < 0). For a small-scale fishing operator
 *   (powerless target), the compliance costs and access restrictions are so severe
 *   that it functions as a Snare, effectively trapping them and extracting their
 *   livelihood. This gap between Rope and Snare is characteristic of major
 *   regulatory shifts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the explicit beneficiary/victim declarations.
 *   - `global_conservation_community` as beneficiary + arbitrage exit drives their `d` value near zero.
 *   - `high_seas_extractive_industries` as victim + constrained exit drives their `d` value high.
 *   - `developing_nations` are uniquely listed as BOTH beneficiary and victim.
 *     The engine synthesizes this dual role into a moderate `d` value, correctly
 *     capturing their ambiguous structural position: they stand to gain from
 *     benefit-sharing but lose from implementation burdens.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents mislabeling. A naive analysis might call the treaty
 *   a pure Snare (focusing only on the costs to industry) or a pure Rope
 *   (focusing only on the conservation goals). The Tangled Rope classification,
 *   driven by the existence of both beneficiary and victim groups, forces a
 *   more nuanced view. It acknowledges that a constraint can simultaneously
 *   solve a real problem AND create winners and losers in a structurally
 *   asymmetric way.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_un_high_seas_treaty_2026,
    'Will the treaty''s enforcement and benefit-sharing mechanisms be sufficiently funded and empowered to be effective?',
    'Observation of funding levels for the treaty''s governing body and records of enforcement actions/revenue distribution over the first decade (2026-2036).',
    'If YES, the Tangled Rope classification holds. If NO, the constraint degrades into a Piton (high theater, low function) as it becomes a paper tiger, or its extractive elements intensify if only costs are enforced but not benefits.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(un_high_seas_treaty_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling a hypothetical 10-year lifecycle projection from the treaty's
% entry into force. While ε is not > 0.46, this data is included to model
% the expected evolution of a major new geopolitical constraint.

% Theater ratio over time (models initial functionality followed by bureaucracy creep):
narrative_ontology:measurement(un_high_seas_treaty_2026_tr_t0, un_high_seas_treaty_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(un_high_seas_treaty_2026_tr_t5, un_high_seas_treaty_2026, theater_ratio, 5, 0.15).
narrative_ontology:measurement(un_high_seas_treaty_2026_tr_t10, un_high_seas_treaty_2026, theater_ratio, 10, 0.20).

% Extraction over time (models stable initial extraction):
narrative_ontology:measurement(un_high_seas_treaty_2026_ex_t0, un_high_seas_treaty_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(un_high_seas_treaty_2026_ex_t5, un_high_seas_treaty_2026, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(un_high_seas_treaty_2026_ex_t10, un_high_seas_treaty_2026, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This treaty's primary function is to allocate rights and resources in a global common.
narrative_ontology:coordination_type(un_high_seas_treaty_2026, resource_allocation).

% Network relationships (structural influence edges)
% This treaty will have profound structural impacts on related domains.
narrative_ontology:affects_constraint(un_high_seas_treaty_2026, deep_sea_mining_viability).
narrative_ontology:affects_constraint(un_high_seas_treaty_2026, global_fisheries_management).
narrative_ontology:affects_constraint(un_high_seas_treaty_2026, pharmaceutical_bioprospecting).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the relationships between the key agents. The hybrid role of developing
% nations, in particular, is captured well by their inclusion in both lists.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */