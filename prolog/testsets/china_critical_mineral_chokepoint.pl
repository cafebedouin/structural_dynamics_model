% ============================================================================
% CONSTRAINT STORY: china_critical_mineral_chokepoint
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_china_critical_mineral_chokepoint, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: china_critical_mineral_chokepoint
 * human_readable: China's Strategic Chokepoint in Critical Mineral Processing
 * domain: economic/political/technological
 * * SUMMARY:
 * China's strategic dominance over the processing and supply of critical minerals (e.g., rare earths, gallium, germanium) creates a global chokepoint. This constraint leverages geological advantage and state-led industrial policy to exert economic and geopolitical influence, functioning as both a coordination mechanism for domestic industry and an extractive tool against global competitors.
 * * KEY AGENTS:
 * - Downstream Tech Manufacturers: Subjects (Powerless), dependent on the supply chain.
 * - Chinese State Industrial Planners: Beneficiaries (Institutional), using the chokepoint for strategic leverage.
 * - Global Supply Chain Analyst: Auditor (Analytical), observing the hybrid nature of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(china_critical_mineral_chokepoint, 0.85). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46
domain_priors:suppression_score(china_critical_mineral_chokepoint, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(china_critical_mineral_chokepoint, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, extractiveness, 0.85).
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(china_critical_mineral_chokepoint, theater_ratio, 0.10).

% Constraint self-claim: It is framed as a tool of national industrial strategy and enforcement of trade rules.
narrative_ontology:constraint_claim(china_critical_mineral_chokepoint, tangled_rope).
narrative_ontology:human_readable(china_critical_mineral_chokepoint, "China's Strategic Chokepoint in Critical Mineral Processing").
narrative_ontology:topic_domain(china_critical_mineral_chokepoint, "economic/political/technological").

% Binary flags
domain_priors:requires_active_enforcement(china_critical_mineral_chokepoint). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(china_critical_mineral_chokepoint, chinese_state_industrial_planners).
narrative_ontology:constraint_victim(china_critical_mineral_chokepoint, global_downstream_tech_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% A downstream tech manufacturer is trapped by supply dependency and price volatility.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Chinese state planners see this as a powerful coordination tool for industrial policy and geopolitical leverage.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees a hybrid system: a genuine coordination function for one group
% that enables high, asymmetric extraction from another, requiring active enforcement.
constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_critical_mineral_chokepoint_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical perspective must resolve the conflict by identifying the hybrid nature.
    constraint_indexing:constraint_classification(china_critical_mineral_chokepoint, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify all three structural requirements for a Tangled Rope are present.
    domain_priors:requires_active_enforcement(china_critical_mineral_chokepoint),
    narrative_ontology:constraint_beneficiary(china_critical_mineral_chokepoint, _), % Derives has_coordination_function
    narrative_ontology:constraint_victim(china_critical_mineral_chokepoint, _).     % Derives has_asymmetric_extraction

:- end_tests(china_critical_mineral_chokepoint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a constraint that is highly functional and strategically managed.
 * - Base Extractiveness (0.85): Represents the significant economic and strategic value extracted from global markets due to supply chain control.
 * - Suppression Score (0.70): High barriers to entry (capital investment, environmental regulations elsewhere, economies of scale) actively suppress the development of viable alternative processing facilities.
 * - Perspectival Gap: The gap is stark. For Chinese planners, it's a 'Rope' for national development and security. For a trapped global manufacturer, it's a 'Snare' that dictates their costs and survival.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case of potential Mandatrophy, where a system could mistakenly classify it as either a pure Rope (coordination) or a pure Snare (extraction). The Tangled Rope classification resolves this by acknowledging its dual nature. The analytical perspective correctly identifies that the coordination function (the 'Rope' for beneficiaries) is precisely what enables the high asymmetric extraction (the 'Snare' for victims). It is not one or the other; it is both, structurally intertwined. The previous classification of 'Mountain' was incorrect as the high extraction is a result of policy and infrastructure, not just raw geology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_china_critical_mineral_chokepoint,
    'Is the chokepoint primarily a result of natural geological endowment (Mountain) or a constructed strategic advantage through state-directed industrial policy (Tangled Rope)?',
    'Analysis of state investment in processing infrastructure vs. global geological surveys of raw deposits. If investment in processing far outstrips geological advantage, it points to a constructed constraint.',
    'If primarily Mountain, the solution is long-term geological exploration and new extraction tech. If Tangled Rope, the solution involves trade policy, diplomatic pressure, and building redundant processing capacity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(china_critical_mineral_chokepoint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over the last decade as strategic policy consolidated control.
% Theater ratio over time (remains low as this is a functional, not performative, constraint):
narrative_ontology:measurement(ccmc_tr_t0, china_critical_mineral_chokepoint, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ccmc_tr_t5, china_critical_mineral_chokepoint, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ccmc_tr_t10, china_critical_mineral_chokepoint, theater_ratio, 10, 0.10).

% Extraction over time (shows the consolidation of strategic control):
narrative_ontology:measurement(ccmc_ex_t0, china_critical_mineral_chokepoint, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ccmc_ex_t5, china_critical_mineral_chokepoint, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ccmc_ex_t10, china_critical_mineral_chokepoint, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: This is a clear case of managing a resource for strategic allocation.
narrative_ontology:coordination_type(china_critical_mineral_chokepoint, resource_allocation).

% Network relationships: This chokepoint directly impacts other technologically-dependent constraints.
narrative_ontology:affects_constraint(china_critical_mineral_chokepoint, semiconductor_supply_chain).
narrative_ontology:affects_constraint(china_critical_mineral_chokepoint, ev_battery_production).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */