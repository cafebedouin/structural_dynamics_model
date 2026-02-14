% ============================================================================
% CONSTRAINT STORY: ai_compute_capital_moat
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ai_compute_capital_moat, []).

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
 *   constraint_id: ai_compute_capital_moat
 *   human_readable: The AI Compute & Capital Moat
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The development of frontier AI models requires immense capital for two
 *   primary inputs: elite research talent and massive-scale GPU compute clusters.
 *   This creates a formidable barrier to entry, where only a handful of well-
 *   funded corporate labs can compete. This constraint coordinates resources at
 *   an unprecedented scale while simultaneously extracting opportunity from the
 *   rest of the ecosystem (startups, academia, open-source).
 *
 * KEY AGENTS (by structural relationship):
 *   - Unfunded AI Startups & Academia: Primary target (powerless/trapped) — priced out of frontier research, unable to access necessary compute.
 *   - Foundational Model Labs & Investors (e.g., xAI, Nvidia): Primary beneficiary (institutional/arbitrage) — their capital creates and reinforces the moat, protecting their market position and future returns.
 *   - Elite AI Researchers: Secondary beneficiary (powerful/mobile) — benefit from high salaries and access to resources, but are also a resource being competed for.
 *   - Analytical Observer: A systems analyst — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_compute_capital_moat, 0.62).
domain_priors:suppression_score(ai_compute_capital_moat, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(ai_compute_capital_moat, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_compute_capital_moat, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_compute_capital_moat, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_compute_capital_moat, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ai_compute_capital_moat, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(ai_compute_capital_moat). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ai_compute_capital_moat, foundational_model_labs).
narrative_ontology:constraint_beneficiary(ai_compute_capital_moat, venture_capital_investors).
narrative_ontology:constraint_beneficiary(ai_compute_capital_moat, gpu_manufacturers). % e.g., Nvidia
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ai_compute_capital_moat, unfunded_ai_startups).
narrative_ontology:constraint_victim(ai_compute_capital_moat, academic_research_labs).
narrative_ontology:constraint_victim(ai_compute_capital_moat, open_source_ai_developers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% An unfunded AI startup. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
%   χ = 0.62 * 1.42 * σ(global=1.2) = 1.06
%   High χ (≥ 0.66), high ε (≥ 0.46), high suppression (≥ 0.60) → Snare
constraint_indexing:constraint_classification(ai_compute_capital_moat, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% A foundational model lab investor (e.g., Nvidia). Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
%   χ = 0.62 * -0.12 * σ(global=1.2) = -0.09
%   Negative χ → Rope
constraint_indexing:constraint_classification(ai_compute_capital_moat, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A systems analyst seeing both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15
%   χ = 0.62 * 1.15 * σ(global=1.2) = 0.85
%   χ is in [0.40, 0.90], ε ≥ 0.30, suppression ≥ 0.40 → Tangled Rope
constraint_indexing:constraint_classification(ai_compute_capital_moat, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: ELITE AI RESEARCHER (TANGLED ROPE)
% An agent with high mobility and bargaining power within the system.
% Their position is more symmetric. Engine falls back to canonical power atom:
%   power(powerful) → canonical d = 0.48 → f(d) ≈ 0.60
%   χ = 0.62 * 0.60 * σ(global=1.2) = 0.45
%   This χ is on the Rope/Tangled Rope boundary, reflecting their dual role
%   as a beneficiary of the system's resources but also a component part of it.
constraint_indexing:constraint_classification(ai_compute_capital_moat, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_compute_capital_moat_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(ai_compute_capital_moat, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(ai_compute_capital_moat, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(ai_compute_capital_moat, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_met) :-
    narrative_ontology:constraint_beneficiary(ai_compute_capital_moat, _),
    narrative_ontology:constraint_victim(ai_compute_capital_moat, _),
    domain_priors:requires_active_enforcement(ai_compute_capital_moat).

:- end_tests(ai_compute_capital_moat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.62): High. The constraint extracts the *opportunity*
 *     to compete at the AI frontier from those without capital. The potential
 *     market share of would-be competitors is effectively captured by the incumbents.
 *   - Suppression (0.75): High. The constraint functions by suppressing alternatives.
 *     It is structurally impossible for a small team to acquire and operate a
 *     100,000+ GPU cluster, regardless of their technical ingenuity.
 *   - Theater (0.15): Low. The capital is being functionally deployed to acquire
 *     compute and talent. While there is a PR element, the core activity is not
 *     performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For an unfunded startup, the inability to access compute
 *   is an absolute barrier with no coordination benefit, appearing as a pure Snare.
 *   For an investor, this same barrier is a protective moat that de-risks their
 *   massive capital outlay, making it a valuable coordination mechanism (Rope) for
 *   pooling resources to solve a difficult technical challenge. The system is
 *   simultaneously a tool for progress and a machine for exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `foundational_model_labs` and their `venture_capital_investors`
 *     directly benefit from the moat reducing competition. `gpu_manufacturers`
 *     benefit from the arms race driving demand for their hardware.
 *   - Victims: `unfunded_ai_startups`, `academic_research_labs`, and `open_source`
 *     developers bear the cost. They are locked out of the race, their potential
 *     innovations suppressed by lack of access to the primary resource.
 *   This structure drives the directionality `d`, leading to the vast difference
 *   in calculated effective extraction (χ) between perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the constraint.
 *   A simplistic analysis might label it a pure Snare (focusing only on the
 *   startups) or a pure Rope (focusing only on the investors' coordination problem).
 *   The Tangled Rope classification from the analytical view correctly captures
 *   that it possesses BOTH a genuine, large-scale coordination function AND a
 *   highly extractive, asymmetric outcome. This prevents mischaracterizing
 *   market concentration as either pure innovation or pure predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ai_compute_capital_moat,
    'Is this level of capital concentration a necessary condition for achieving AGI (a "Mountain" of physics/scaling laws), or is it primarily a market-cornering strategy (a "Tangled Rope" of economics)?',
    'Demonstration of qualitatively new capabilities at scale that are impossible otherwise vs. emergence of competitive, smaller models that achieve similar performance with less data/compute.',
    'If necessary (Mountain), the extraction is an unavoidable byproduct of a natural law. If strategic (Tangled Rope), the extraction is a feature of a constructed economic system that could be regulated or altered.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ai_compute_capital_moat, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Since base_extractiveness > 0.46,
% this data is required. This models the intensification of the compute moat
% from the early 2020s to a projected future state.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(ai_compute_capital_moat_tr_t0, ai_compute_capital_moat, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ai_compute_capital_moat_tr_t5, ai_compute_capital_moat, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ai_compute_capital_moat_tr_t10, ai_compute_capital_moat, theater_ratio, 10, 0.25).

% Extraction over time (intensifying as capital requirements grow):
narrative_ontology:measurement(ai_compute_capital_moat_ex_t0, ai_compute_capital_moat, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(ai_compute_capital_moat_ex_t5, ai_compute_capital_moat, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(ai_compute_capital_moat_ex_t10, ai_compute_capital_moat, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint is fundamentally about allocating scarce
% resources (capital, compute, talent) to a specific goal.
narrative_ontology:coordination_type(ai_compute_capital_moat, resource_allocation).

% Network relationships: The compute moat is structurally linked to the
% semiconductor supply chain and impacts the viability of open source models.
narrative_ontology:affects_constraint(ai_compute_capital_moat, semiconductor_supply_chain).
narrative_ontology:affects_constraint(ai_compute_capital_moat, open_source_ai_development).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships between the agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */