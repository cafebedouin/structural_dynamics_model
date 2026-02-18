% ============================================================================
% CONSTRAINT STORY: evfta_trade_agreement
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_evfta_trade_agreement, []).

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
% See: epsilon_invariance_princi ple.md

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
 *   constraint_id: evfta_trade_agreement
 *   human_readable: EU-Vietnam Free Trade Agreement (EVFTA)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU-Vietnam Free Trade Agreement (EVFTA) is a comprehensive trade deal
 *   that eliminates nearly all tariffs between the two blocs. While presented
 *   as a coordination mechanism to boost mutual prosperity and uphold a
 *   "rules-based order" amidst global trade tensions, it also imposes
 *   asymmetric costs and standards, particularly on smaller Vietnamese producers
 *   and laborers who must adapt to a new competitive landscape.
 *
 * KEY AGENTS (by structural relationship):
 *   - Vietnamese small-scale producers: Primary target (powerless/trapped) — bears costs of market adjustment and increased competition.
 *   - EU multinational corporations: Primary beneficiary (institutional/arbitrage) — gains tariff-free access to a large, growing market.
 *   - Vietnamese state & exporters: Primary beneficiary (institutional/arbitrage) — gains market access, geopolitical leverage, and a framework for domestic reform.
 *   - Analytical observer: Analytical observer — sees the dual nature of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evfta_trade_agreement, 0.48).
domain_priors:suppression_score(evfta_trade_agreement, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(evfta_trade_agreement, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evfta_trade_agreement, extractiveness, 0.48).
narrative_ontology:constraint_metric(evfta_trade_agreement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(evfta_trade_agreement, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(evfta_trade_agreement, tangled_rope).
narrative_ontology:human_readable(evfta_trade_agreement, "EU-Vietnam Free Trade Agreement (EVFTA)").
narrative_ontology:topic_domain(evfta_trade_agreement, "economic/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(evfta_trade_agreement). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(evfta_trade_agreement, eu_multinational_corporations).
narrative_ontology:constraint_beneficiary(evfta_trade_agreement, vietnamese_state_and_exporters).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(evfta_trade_agreement, vietnamese_small_scale_producers).
narrative_ontology:constraint_victim(evfta_trade_agreement, eu_import_competing_sectors).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (VIETNAMESE SMALL PRODUCER)
% Faces intense competition from efficient EU firms and must comply with new,
% complex standards (sanitary, phytosanitary, IP). The agreement's structure
% is imposed with no ability to opt out.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(evfta_trade_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (EU CORPORATION)
% Gains tariff-free access to a market of 95 million people, with rules that
% favor its scale and existing standards. A pure coordination enabler for profit.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(evfta_trade_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the genuine coordination function (facilitating trade) and the
% asymmetric extraction inherent in the ruleset. This is the canonical
% view that identifies the Tangled Rope structure.
constraint_indexing:constraint_classification(evfta_trade_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE VIETNAMESE STATE (INSTITUTIONAL BENEFICIARY)
% Sees the agreement as a tool for geopolitical positioning, economic modernization,
% and a hedge against regional instability. From this perspective, the extractive
% elements are a necessary cost for strategic gains.
constraint_indexing:constraint_classification(evfta_trade_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evfta_trade_agreement_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between those who bear costs and those who benefit.
    constraint_indexing:constraint_classification(evfta_trade_agreement, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(evfta_trade_agreement, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: Snare (powerless) vs Rope (institutional)~n', []).

test(analytical_view_is_tangled_rope) :-
    % The analytical observer must see the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(evfta_trade_agreement, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_met) :-
    % A Tangled Rope must have beneficiaries, victims, and require enforcement.
    narrative_ontology:constraint_beneficiary(evfta_trade_agreement, _),
    narrative_ontology:constraint_victim(evfta_trade_agreement, _),
    domain_priors:requires_active_enforcement(evfta_trade_agreement).

:- end_tests(evfta_trade_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value reflects that the agreement, while
 *     coordinative, contains significant non-reciprocal elements. Clauses on
 *     intellectual property, investor-state dispute settlement (ISDS), and
 *     non-tariff barriers (NTBs) disproportionately benefit capital-intensive
 *     EU firms over Vietnamese small and medium enterprises (SMEs).
 *   - Suppression (0.65): High. As binding international law, the EVFTA
 *     suppresses all alternative bilateral trade arrangements and locks in
 *     domestic policy choices for Vietnam, limiting its regulatory sovereignty.
 *   - Theater (0.20): Low. The agreement is highly functional, with tangible
 *     economic consequences. Political rhetoric is secondary to the enforceable rules.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For an EU corporation, the EVFTA is a 'Rope'—a set of
 *   rules that simplifies market access and reduces costs. For a Vietnamese
 *   small-scale farmer or textile worker, it is a 'Snare'—an inescapable change
 *   to their economic environment that introduces powerful, subsidized
 *   competitors and imposes costly new standards. The corporation has arbitrage
 *   exit (it can choose to invest or not), while the worker is trapped in the
 *   newly liberalized local market.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction flows from actors with low capital and adaptability
 *   (Vietnamese SMEs, labor) to those with high capital and legal sophistication
 *   (EU multinationals, Vietnamese state-connected exporters). The `beneficiary`
 *   and `victim` declarations model this directly. EU corporations and the
 *   Vietnamese state benefit from the macro-level coordination and market access.
 *   Vietnamese small producers and EU sectors facing new import competition bear
 *   the costs of adjustment and displacement. This structural relationship drives
 *   the directionality `d`, leading to radically different classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids the common pitfall of labeling all "free trade"
 *   agreements as pure Ropes. The Tangled Rope designation correctly identifies
 *   that a genuine coordination function (reducing trade friction) can coexist with
 *   a significant, asymmetric extractive function. By requiring the declaration
 *   of both beneficiary and victim groups, and enforcing the metric thresholds for
 *   Tangled Rope (ε ≥ 0.30, suppression ≥ 0.40), the framework prevents the
 *   extractive component from being ignored or hand-waved away as "creative
 *   destruction."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_evfta_trade_agreement,
    'To what extent will the included labor and environmental protection clauses be meaningfully enforced?',
    'Longitudinal analysis of labor tribunal rulings, environmental compliance reports, and independent audits in Vietnam over a 5-10 year period.',
    'If strongly enforced, base_extractiveness (ε) could be revised downward, shifting the analytical view closer to a Rope. If unenforced, ε remains high or could increase as loopholes are exploited.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(evfta_trade_agreement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection for this high-extraction constraint (ε > 0.46).
% The model shows high initial theater during political ratification, which gives way
% to rising, sustained extraction as the agreement's rules take full effect.
% T=0: Negotiation/Ratification Phase
% T=5: Early Implementation Phase
% T=10: Mature Phase

% Theater ratio over time (declines as functional impact replaces political signaling):
narrative_ontology:measurement(evfta_tr_t0, evfta_trade_agreement, theater_ratio, 0, 0.40).
narrative_ontology:measurement(evfta_tr_t5, evfta_trade_agreement, theater_ratio, 5, 0.25).
narrative_ontology:measurement(evfta_tr_t10, evfta_trade_agreement, theater_ratio, 10, 0.20).

% Extraction over time (increases as rules become embedded and exploited):
narrative_ontology:measurement(evfta_ex_t0, evfta_trade_agreement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(evfta_ex_t5, evfta_trade_agreement, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(evfta_ex_t10, evfta_trade_agreement, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: A comprehensive trade agreement that sets standards, allocates
% market access, and provides enforcement is a form of global infrastructure.
narrative_ontology:coordination_type(evfta_trade_agreement, global_infrastructure).

% Network relationships: The agreement is explicitly framed as a response to
% global trade disruptions and regional power dynamics.
narrative_ontology:affects_constraint(us_china_trade_war, evfta_trade_agreement).
narrative_ontology:affects_constraint(evfta_trade_agreement, asean_regional_integration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations combined with exit options (trapped vs. arbitrage)
% accurately models the directionality dynamics of the trade agreement.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */