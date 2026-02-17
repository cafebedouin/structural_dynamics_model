% ============================================================================
% CONSTRAINT STORY: us_usmca_china_leverage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_usmca_china_leverage, []).

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
 *   constraint_id: us_usmca_china_leverage
 *   human_readable: "US leveraging of USMCA ratification to constrain Canadian foreign policy on China"
 *   domain: geopolitical
 *
 * SUMMARY:
 *   During the Trump administration, the United States government exerted
 *   significant pressure on Canada to adopt a more confrontational stance
 *   towards China. This pressure was implicitly and sometimes explicitly
 *   linked to the successful ratification of the US-Mexico-Canada Agreement
 *   (USMCA), a critical trade deal for the Canadian economy. The constraint is
 *   not the trade deal itself, but the political linkage that extracts foreign
 *   policy alignment from Canada by leveraging its economic dependency.
 *
 * KEY AGENTS (by structural relationship):
 *   - United States Government: Primary beneficiary (institutional/arbitrage) — achieves a coordinated North American foreign policy front against China.
 *   - Canadian Government: Primary target (institutional/constrained) — experiences extraction of its foreign policy autonomy.
 *   - Canadian Exporters to China: Secondary victim (organized/constrained) — bears the economic risk of worsened Canada-China relations.
 *   - Canadian Export Sector Workers: Powerless victim (powerless/trapped) - bears downstream economic risk with no policy influence.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_usmca_china_leverage, 0.55).
domain_priors:suppression_score(us_usmca_china_leverage, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_usmca_china_leverage, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_usmca_china_leverage, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_usmca_china_leverage, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_usmca_china_leverage, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_usmca_china_leverage, tangled_rope).
narrative_ontology:human_readable(us_usmca_china_leverage, "US leveraging of USMCA ratification to constrain Canadian foreign policy on China").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_usmca_china_leverage). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_usmca_china_leverage, us_government).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_usmca_china_leverage, canadian_government).
narrative_ontology:constraint_victim(us_usmca_china_leverage, canadian_exporters_to_china).
narrative_ontology:constraint_victim(us_usmca_china_leverage, canadian_export_sector_workers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) - MET

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

% PERSPECTIVE 1: THE POWERLESS VICTIM (SNARE)
% Canadian workers in export-dependent sectors who have no say in policy but
% bear the risk of economic disruption.
% Engine derives d from: victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ
constraint_indexing:constraint_classification(us_usmca_china_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: US GOVERNMENT (ROPE)
% The primary beneficiary, who sees this as a tool for strategic coordination.
% Engine derives d from: beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ
constraint_indexing:constraint_classification(us_usmca_china_leverage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes both the coordination function and the
% asymmetric extraction, leading to a Tangled Rope classification.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15.
constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The core dynamic is between two institutional actors with different power.

% PERSPECTIVE 4A: CANADIAN GOVERNMENT (SNARE)
% The primary target. Though institutional, their constrained exit options
% due to economic dependency make the constraint feel highly coercive.
% Engine derives d from: victim membership + institutional power + constrained exit -> d ≈ 0.60 -> high f(d).
constraint_indexing:constraint_classification(us_usmca_china_leverage, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4B: CANADIAN EXPORTERS (SNARE)
% A victim group whose economic viability is threatened by the constraint.
% Engine derives d from: victim membership + constrained exit -> d ≈ 0.85 -> high f(d) -> high χ
constraint_indexing:constraint_classification(us_usmca_china_leverage, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_usmca_china_leverage_tests).

test(perspectival_gap_inter_institutional, [nondet]) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(us_usmca_china_leverage, TypeCanada,
        context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(us_usmca_china_leverage, TypeUS,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    TypeCanada == snare,
    TypeUS == rope,
    TypeCanada \= TypeUS.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(us_usmca_china_leverage, _),
    narrative_ontology:constraint_victim(us_usmca_china_leverage, _),
    domain_priors:requires_active_enforcement(us_usmca_china_leverage).

:- end_tests(us_usmca_china_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): The extraction of foreign policy autonomy is a significant cost. It's not monetary, but it represents a high degree of political capital being transferred.
 *   - Suppression Score (0.75): Canada's alternatives were severely limited. The US is its largest trading partner by an overwhelming margin, making the ratification of USMCA a near-existential economic imperative. This high dependency is the source of the coercive power.
 *   - Theater Ratio (0.15): The pressure was real and consequential, not merely performative. The threats to the trade deal had tangible economic weight.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The US government, as the beneficiary with arbitrage exit, perceives a useful coordination mechanism (Rope) that aligns its key ally. Canada, as the target with constrained exit, perceives a coercive trap (Snare) that forces it to compromise its sovereignty for economic stability. The difference in classification is driven entirely by their structural positions relative to the constraint, which the directionality 'd' value captures.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The `us_government` directly benefits by achieving its strategic goal of a unified front against China without deploying military or direct economic sanctions.
 *   - Victims: The `canadian_government` bears the cost of lost autonomy and diplomatic flexibility. `canadian_exporters_to_china` and the `canadian_export_sector_workers` they employ bear the downstream economic risk from a chilled relationship with a major trading partner. This mapping of costs and benefits is what informs the directionality derivation.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a canonical example of an inter-institutional Tangled Rope. Both the US and Canada are 'institutional' actors, but their power is asymmetric. This asymmetry is not captured by the 'power' atom alone, but by the combination of power and `exit_options`. The US's `arbitrage` exit (it has more economic leverage) versus Canada's `constrained` exit (high dependency) creates the power differential that allows the extraction to occur within what appears to be a peer-to-peer negotiation.
 *
 * MANDATROPHY ANALYSIS:
 *   A naive analysis might classify this as just "hardball negotiation" (a Rope). This would be mandatrophy, ignoring the severe, asymmetric extraction of political autonomy. Conversely, calling it pure aggression (a Snare from all views) would miss the genuine coordination function the US was seeking to build. The Tangled Rope classification, derived from the analytical perspective, correctly identifies the dual nature of the constraint: it simultaneously coordinates policy and extracts compliance coercively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_usmca_china_leverage,
    'Was the linkage between USMCA ratification and China policy an explicit quid-pro-quo or an implicit atmospheric pressure?',
    'Declassified diplomatic records, or memoirs from key negotiators (e.g., Robert Lighthizer, Chrystia Freeland).',
    'If explicit, ε trends higher (~0.65, Snare from analytical view). If implicit, ε is lower (~0.45), solidifying the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_usmca_china_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), requiring temporal data.
% The pressure campaign intensified as USMCA negotiations concluded.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(us_usmca_china_leverage_tr_t0, us_usmca_china_leverage, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_usmca_china_leverage_tr_t5, us_usmca_china_leverage, theater_ratio, 5, 0.15).
narrative_ontology:measurement(us_usmca_china_leverage_tr_t10, us_usmca_china_leverage, theater_ratio, 10, 0.15).

% Extraction over time (increasing pressure):
narrative_ontology:measurement(us_usmca_china_leverage_ex_t0, us_usmca_china_leverage, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(us_usmca_china_leverage_ex_t5, us_usmca_china_leverage, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(us_usmca_china_leverage_ex_t10, us_usmca_china_leverage, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint is a mechanism for enforcing a unified
% geopolitical stance.
narrative_ontology:coordination_type(us_usmca_china_leverage, enforcement_mechanism).

% Network relationships: This policy constraint directly impacts other decisions,
% such as Canada's policy on Huawei's 5G technology.
narrative_ontology:affects_constraint(us_usmca_china_leverage, canada_huawei_5g_decision).
narrative_ontology:affects_constraint(us_china_trade_war, us_usmca_china_leverage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation chain
% (beneficiary/victim + power + exit_options -> d) accurately models the
% asymmetric power dynamic between the two institutional actors.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */