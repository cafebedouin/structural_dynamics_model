% ============================================================================
% CONSTRAINT STORY: global_trade_externalities_neobiota
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-01
% ============================================================================

:- module(constraint_global_trade_externalities_neobiota, []).

:- use_module(library(plunit)).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).
:- use_module(config).

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
 *   constraint_id: global_trade_externalities_neobiota
 *   human_readable: "Global Trade's Externalization of Neobiota Costs"
 *   domain: economic/environmental
 *
 * SUMMARY:
 *   The global trade system coordinates international commerce but fails to
 *   price in the massive ecological and economic costs of invasive alien
 *   species (neobiota), which are spread through its networks. This structure
 *   externalizes the risk, damage, and remediation costs onto local ecosystems,
 *   industries, and taxpayers, while the profits from frictionless trade are
 *   privatized by global logistics actors.
 *
 * KEY AGENTS (by structural relationship):
 *   - Local Taxpayers & Primary Producers: Primary target (powerless/trapped) — bear the direct costs of damage and control measures.
 *   - Global Trade & Logistics Sector: Primary beneficiary (institutional/arbitrage) — profits from the efficiency of a system that does not account for these externalities.
 *   - National Governments: Secondary actor (institutional/constrained) — caught between facilitating trade and managing local ecological crises.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_trade_externalities_neobiota, 0.55). % The unpriced cost of invasive species, estimated at over 1.2 trillion euros since 1960.
domain_priors:suppression_score(global_trade_externalities_neobiota, 0.75).   % Structural property (raw, unscaled). Alternatives (e.g., a fully biosecure trade system) are suppressed by immense economic inertia and political difficulty.
domain_priors:theater_ratio(global_trade_externalities_neobiota, 0.20).       % Piton detection (< 0.70). While some biosecurity is performative, the core function of trade is real.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, extractiveness, 0.55).
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(global_trade_externalities_neobiota, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(global_trade_externalities_neobiota, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(global_trade_externalities_neobiota). % Required for Tangled Rope. The system is maintained by trade agreements and a persistent lack of sufficiently funded global biosecurity protocols.

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(global_trade_externalities_neobiota, global_trade_logistics_sector).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(global_trade_externalities_neobiota, local_taxpayers_and_primary_producers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met)
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.55 * 1.42 * 1.0 (national scope) ≈ 0.78. With suppression=0.75, this is a Snare.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
%   χ = 0.55 * -0.12 * 1.2 (global scope) ≈ -0.08. This is a pure coordination Rope.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * 1.2 (global scope) ≈ 0.76.
% The system recognizes the high χ, high suppression, AND the coordination function
% (via `constraint_beneficiary` declaration) and correctly classifies it as Tangled Rope.
constraint_indexing:constraint_classification(global_trade_externalities_neobiota, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_trade_externalities_neobiota_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(global_trade_externalities_neobiota, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(global_trade_externalities_neobiota, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(global_trade_externalities_neobiota, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_conditions_met) :-
    narrative_ontology:constraint_beneficiary(global_trade_externalities_neobiota, _),
    narrative_ontology:constraint_victim(global_trade_externalities_neobiota, _),
    domain_priors:requires_active_enforcement(global_trade_externalities_neobiota).

:- end_tests(global_trade_externalities_neobiota_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): This quantifies the immense, unpriced negative externality of invasive species spread via trade, which the source article estimates in the trillions of euros over decades. It is a direct, asymmetric transfer of cost.
 *   - Suppression (0.75): Establishing a truly biosecure global trade network is a monumental coordination problem. Any single nation or port that imposes strict controls risks becoming uncompetitive. The inertia of the current, less-secure system is therefore enormous, effectively suppressing alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the `global_trade_logistics_sector`, the system is a pure Rope: a highly efficient mechanism for coordinating the movement of goods that generates vast profits. The negative externalities are invisible to their balance sheets. For `local_taxpayers_and_primary_producers`, the system is a Snare: they are trapped into paying for the damages (crop loss, infrastructure repair, health costs, control measures) created by a system over which they have no control.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are mobile, global actors with high exit (`arbitrage`) who can optimize routes and operations to avoid costs and regulations. The victims are geographically fixed (`trapped`) and must bear the ecological and economic consequences that manifest locally. This spatial and structural asymmetry is the core driver of the directionality, leading to a huge difference in the derived `d` value and thus the perceived effective extraction (χ).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the constraint, avoiding two common errors. It does not label "global trade" as a pure Snare, which would ignore its profound and necessary coordination function. It also does not label it as a benign Rope, which would ignore the catastrophic, unpriced externalities it imposes. The Tangled Rope classification captures the reality that it is a system with a genuine coordination function that has been co-opted to facilitate massive, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_global_trade_neobiota,
    'Is the lack of biosecurity a deliberate cost-saving measure by the trade sector (extractive intent) or an unavoidable emergent property of a complex system (coordination failure)?',
    'Analysis of internal documents from shipping consortiums, lobbying records related to biosecurity regulations, and insurance risk models.',
    'If deliberate, the constraint is closer to a pure Snare. If emergent, it is a dysfunctional Tangled Rope that could potentially be fixed with better coordination mechanisms.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(global_trade_externalities_neobiota, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The source article notes that the costs of neobiota are rising exponentially,
% driven by increasing globalization. This models the 'extraction_accumulation'
% lifecycle drift.
% Time 0 = ~1960, Time 5 = ~1990, Time 10 = ~2020s.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(global_trade_tr_t0, global_trade_externalities_neobiota, theater_ratio, 0, 0.10).
narrative_ontology:measurement(global_trade_tr_t5, global_trade_externalities_neobiota, theater_ratio, 5, 0.15).
narrative_ontology:measurement(global_trade_tr_t10, global_trade_externalities_neobiota, theater_ratio, 10, 0.20).

% Extraction over time (shows significant accumulation):
narrative_ontology:measurement(global_trade_ex_t0, global_trade_externalities_neobiota, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(global_trade_ex_t5, global_trade_externalities_neobiota, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(global_trade_ex_t10, global_trade_externalities_neobiota, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(global_trade_externalities_neobiota, global_infrastructure).

% Network relationships (structural influence edges)
% This economic constraint enables and exacerbates a biological one.

narrative_ontology:affects_constraint(global_trade_externalities_neobiota, biological_invasion_dynamics).

% --- Network Decomposition (Constraint Families) ---
%
% DUAL FORMULATION NOTE:
% This constraint is one of two stories decomposed from the colloquial concept of "invasive species".
% Decomposed because ε differs across observables (ε-invariance principle).
% The socio-economic system (Tangled Rope) is distinct from the biological process (Mountain).
% Related stories:
%   - biological_invasion_dynamics (ε≈0.05, Mountain) - The inexorable spread of an established species according to ecological laws.
%
% This story (`global_trade_externalities_neobiota`) models the economic system that creates the initial conditions for the biological constraint to operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation chain,
% using the beneficiary/victim declarations and the agents' exit options,
% accurately captures the structural power dynamics of this system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */