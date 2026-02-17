% ============================================================================
% CONSTRAINT STORY: french_ag_land_concentration
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_french_ag_land_concentration, []).

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
 *   constraint_id: french_ag_land_concentration
 *   human_readable: French Agricultural Land Concentration
 *   domain: economic
 *
 * SUMMARY:
 *   Based on INSEE data, this constraint models the decades-long trend of
 *   agricultural land consolidation in France. The number of farms has halved
 *   while average farm size has nearly doubled. This system, driven by economic
 *   pressures and policy (like the EU's CAP), simultaneously coordinates national
 *   food production while asymmetrically benefiting large, capitalized operations
 *   and extracting opportunity and capital from small-scale or aspiring farmers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Small_and_aspiring_farmers: Primary target (powerless/trapped) — faces prohibitive land costs and barriers to entry/expansion.
 *   - Large_scale_agribusiness: Primary beneficiary (institutional/arbitrage) — benefits from economies of scale, market power, and appreciating land assets.
 *   - French_state_and_EU_CAP: Inter-institutional actor (institutional/constrained) — designs and enforces the rules, ostensibly for coordination, but is captured by path dependency and powerful lobbies.
 *   - INSEE_analyst: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_ag_land_concentration, 0.55).
domain_priors:suppression_score(french_ag_land_concentration, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(french_ag_land_concentration, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_ag_land_concentration, extractiveness, 0.55).
narrative_ontology:constraint_metric(french_ag_land_concentration, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(french_ag_land_concentration, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(french_ag_land_concentration, tangled_rope).
narrative_ontology:human_readable(french_ag_land_concentration, "French Agricultural Land Concentration").

% --- Binary flags ---
domain_priors:requires_active_enforcement(french_ag_land_concentration). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, large_scale_agribusiness).
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, french_state_and_EU_CAP).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(french_ag_land_concentration, small_and_aspiring_farmers).
narrative_ontology:constraint_victim(french_ag_land_concentration, french_state_and_EU_CAP). % Also a victim of capture

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
% For small/aspiring farmers, the system is purely extractive. The coordination
% function is irrelevant to their experience of being priced out.
% Engine derives d from: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
%   χ = 0.55 * 1.42 * 1.0 (national) ≈ 0.78 (Snare: χ >= 0.66)
constraint_indexing:constraint_classification(french_ag_land_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For large agribusiness, this is a coordination system that enables efficient
% operation and capital growth.
% Engine derives d from: beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
%   χ = 0.55 * -0.12 * 1.0 (national) ≈ -0.07 (Rope: χ <= 0.35)
constraint_indexing:constraint_classification(french_ag_land_concentration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The INSEE analyst sees both the coordination function and the severe
% asymmetric extraction it produces.
% Engine derives canonical d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(french_ag_land_concentration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INTER-INSTITUTIONAL ACTOR (TANGLED ROPE)
% The French state is both a beneficiary (maintaining food supply stability)
% and a victim (of capture by powerful lobbies). Its exit options are constrained
% by political path dependency.
% Engine derives d from: beneficiary + victim + constrained exit -> d ~ 0.45 -> f(d) ~ 0.55
%  χ = 0.55 * 0.55 * 1.1 (continental scope for EU) = 0.33 (Rope, close to boundary)
% This shows a Rope under tension, reflecting its captured state.
constraint_indexing:constraint_classification(french_ag_land_concentration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_ag_land_concentration_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(french_ag_land_concentration, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(french_ag_land_concentration, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap validated: Target sees Snare, Beneficiary sees Rope~n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(french_ag_land_concentration, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('... Analytical view validated: Tangled Rope~n').

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(french_ag_land_concentration, _),
    narrative_ontology:constraint_victim(french_ag_land_concentration, _),
    domain_priors:requires_active_enforcement(french_ag_land_concentration),
    format('... Structural gates for Tangled Rope passed~n').

:- end_tests(french_ag_land_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. Reflects the significant transfer of land control (asset) and opportunity from a broad base of potential farmers to a concentrated group. The halving of farms and doubling of size is a direct measure of this extraction.
 *   - Suppression (0.75): High. The capital requirements and land prices create formidable barriers to entry, effectively suppressing the alternative of a more distributed, small-scale agricultural model.
 *   - Theater (0.40): Moderate. There is significant political discourse about supporting young farmers and rural life, but the material outcomes of policy drive concentration. This gap between stated intent and actual function constitutes theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme and defines the constraint. Small farmers experience it as a pure Snare (χ≈0.78); they are trapped in a system that extracts their viability. Large agribusiness experiences it as a pure Rope (χ≈-0.07); a rational system of coordination that facilitates their business model. The analytical view must reconcile these, hence Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'large_scale_agribusiness' directly benefits from economies of scale and asset appreciation. The 'french_state_and_EU_CAP' is a beneficiary because the system ensures food security and is politically stable, even if inequitable.
 *   - Victims: 'small_and_aspiring_farmers' are the primary victims, losing access to land and livelihoods. The 'french_state_and_EU_CAP' is also declared a victim to model its capture; it is constrained by its own path-dependent policies and lobbying, unable to easily change course even if it wanted to. This dual role is key to the inter-institutional analysis.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model includes a specific perspective for the state actor with a 'constrained' exit. Unlike the agribusinesses which have 'arbitrage' options (sell land, move capital), the state cannot easily exit its role as regulator and subsidizer. This structural constraint, combined with its dual beneficiary/victim status, results in a different directionality `d` and a view of the system as a 'Rope under tension,' accurately reflecting the political reality of regulatory capture.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the system. A simplistic analysis might label it a Snare, ignoring the genuine (and politically necessary) coordination function of ensuring a national food supply. Conversely, a naive policy view might call it a Rope, ignoring the massive, structurally embedded extraction. The Tangled Rope classification captures both, preventing the mislabeling that enables mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_french_ag_land_concentration,
    'Is the concentration an intended policy goal for "efficiency," or an unintended emergent property of path-dependent subsidy mechanisms?',
    'Historical analysis of CAP design documents and minutes of agricultural lobbying meetings.',
    'If intended, the constraint is a more deliberate Snare with a coordination facade. If unintended, it is a classic Tangled Rope that has drifted from a more benign state.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(french_ag_land_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the constraint's intensification over ~30 years.
% The system started as a milder form of coordination and has become
% progressively more extractive. This triggers 'extraction_accumulation' alerts.

% Theater ratio over time:
narrative_ontology:measurement(falp_tr_t0, french_ag_land_concentration, theater_ratio, 0, 0.20).
narrative_ontology:measurement(falp_tr_t5, french_ag_land_concentration, theater_ratio, 5, 0.30).
narrative_ontology:measurement(falp_tr_t10, french_ag_land_concentration, theater_ratio, 10, 0.40).

% Extraction over time:
narrative_ontology:measurement(falp_ex_t0, french_ag_land_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(falp_ex_t5, french_ag_land_concentration, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(falp_ex_t10, french_ag_land_concentration, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The CAP and associated land laws are a form of resource allocation.
narrative_ontology:coordination_type(french_ag_land_concentration, resource_allocation).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on beneficiary/victim declarations and exit options (arbitrage vs.
% constrained vs. trapped) accurately models the directionality for each
% key agent and captures the essential inter-institutional dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */