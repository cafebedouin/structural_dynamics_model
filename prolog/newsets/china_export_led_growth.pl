% ============================================================================
% CONSTRAINT STORY: china_export_led_growth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_export_led_growth, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: china_export_led_growth
 *   human_readable: China's State-Directed Export-Led Growth Model
 *   domain: economic/political
 *
 * SUMMARY:
 *   China's state-directed, export-led growth model is a form of industrial
 *   policy that systematically prioritizes manufacturing investment and
 *   exports over domestic consumption. It achieves this through a combination
 *   of direct subsidies, an undervalued currency, and financial repression
 *   (artificially low interest rates on household savings, which provides
 *   cheap capital for state-favored industries). This has been
 *   extraordinarily successful at driving GDP growth and industrialization
 *   but has created massive internal and external imbalances.
 *
 * KEY AGENTS:
 *   - Chinese State/CCP: Primary beneficiary (institutional/arbitrage) - Gains geopolitical power, revenue, and social stability.
 *   - Chinese Households/Labor: Primary victim (powerless/trapped) - Experiences wage suppression and wealth transfer via financial repression.
 *   - Foreign Competitors: Secondary victim (organized/constrained) - Loses market share to subsidized Chinese firms.
 *   - Global Consumers: Indirect beneficiary (moderate/mobile) - Benefits from access to low-cost manufactured goods.
 *   - Analytical Observer: Sees the full structure (analytical/analytical) - Recognizes both the coordination and extraction functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_export_led_growth, 0.65).
domain_priors:suppression_score(china_export_led_growth, 0.75).
domain_priors:theater_ratio(china_export_led_growth, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_export_led_growth, extractiveness, 0.65).
narrative_ontology:constraint_metric(china_export_led_growth, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(china_export_led_growth, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_export_led_growth, tangled_rope).
narrative_ontology:human_readable(china_export_led_growth, "China's State-Directed Export-Led Growth Model").
narrative_ontology:topic_domain(china_export_led_growth, "economic/political").

domain_priors:requires_active_enforcement(china_export_led_growth).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_export_led_growth, chinese_state_and_soes).
narrative_ontology:constraint_beneficiary(china_export_led_growth, global_consumers).
narrative_ontology:constraint_victim(china_export_led_growth, chinese_households_and_labor).
narrative_ontology:constraint_victim(china_export_led_growth, foreign_competitors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE HOUSEHOLD (SNARE) — Trapped by capital controls and the Hukou system, this agent experiences the model as pure extraction. Wage growth is suppressed relative to productivity, and low interest rates on savings transfer wealth to state-owned enterprises. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92. This is a clear Snare.
constraint_indexing:constraint_classification(china_export_led_growth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CHINESE STATE (ROPE) — As the architect and primary beneficiary, the state perceives the model as a pure coordination mechanism for national development, geopolitical influence, and social stability. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. The negative effective extraction indicates a net subsidy from the state's perspective.
constraint_indexing:constraint_classification(china_export_led_growth, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: FOREIGN COMPETITOR (TANGLED ROPE) — An organized industrial sector in another country experiences this as a hybrid. It recognizes the coordination function but is a direct target of the extraction (loss of market share to subsidized competition). Exit is constrained, as it cannot easily abandon global markets. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.59.
constraint_indexing:constraint_classification(china_export_led_growth, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL CONSUMER (ROPE) — This agent is an indirect beneficiary, experiencing the system as a pure coordination benefit that delivers low-cost goods. They have mobile exit options (can buy other products) but are incentivized to participate. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01. Effectively zero extraction.
constraint_indexing:constraint_classification(china_export_led_growth, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical view recognizes both the historically successful coordination function (lifting millions from poverty, rapid industrialization) and the severe, systemic extraction from domestic households and foreign industries required to fuel it. This matches the claimed_type. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(china_export_led_growth, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_export_led_growth_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_export_led_growth, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_export_led_growth, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_export_led_growth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_export_led_growth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high because the transfer of wealth from the household sector to the industrial/state sector is a core, intentional feature of the policy, not a side effect. Suppression (0.75) is high due to the state's active enforcement through capital controls, state-controlled labor unions, and industrial policy that picks winners and suppresses market-based alternatives. Theater Ratio (0.30) is moderate; while there is significant state propaganda, the underlying economic mechanism is highly functional and effective at achieving its goals.
 *
 * PERSPECTIVAL GAP:
 *   The profound perspectival gap is central to this constraint. The Chinese state views it as a Rope, a successful national coordination project. The Chinese worker, whose savings are devalued and wages suppressed, experiences it as a Snare. Foreign industries, outcompeted by subsidized players, see a Tangled Rope. Global consumers, enjoying cheap goods, see a beneficial Rope. This divergence highlights how an agent's structural position determines their classification of the same underlying economic reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural relationships. The State, as a beneficiary with arbitrage, has a low 'd' value, resulting in a negative effective extraction (Rope). The trapped household victim has a very high 'd', leading to high effective extraction (Snare). The constrained foreign competitor has a moderate-high 'd' (Tangled Rope). The mobile consumer beneficiary has a low 'd' (Rope). The analytical perspective's canonical 'd' value correctly identifies the hybrid Tangled Rope nature.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolution of mandatrophy. To label the model a pure 'Rope' (as its proponents do) is to ignore the systemic extraction from households. To label it a pure 'Snare' (as its victims experience it) is to ignore the genuine coordination function that lifted hundreds of millions from poverty. The analytical classification of Tangled Rope correctly holds both truths in tension, identifying the structure as a hybrid of coordination and asymmetric extraction, which is precisely what it is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internal_rebalancing_capacity,
    'Can the Chinese state successfully pivot from an export/investment-led model to a domestic consumption-led model?',
    'Tracking the ratio of household consumption to GDP, growth in real wages, and reforms to the financial system over the next decade.',
    'Successful pivot would lower base extractiveness (ε), shifting the constraint towards a Rope. Failure would risk stagnation, increasing theater and turning it into a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_rebalancing_capacity, empirical, 'The state''s capacity to shift from export-led to consumption-led growth.').

omega_variable(
    geopolitical_blowback_threshold,
    'At what point does international backlash (tariffs, trade blocs, sanctions) become severe enough to force a structural change in the model?',
    'Monitoring the scale and scope of trade restrictions imposed by major trading partners and their impact on Chinese export volumes and GDP.',
    'High blowback would reduce the effectiveness of the export subsidy, potentially lowering suppression and extractiveness. Low blowback allows the model to persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_blowback_threshold, empirical, 'The threshold of international trade pressure required to alter the model.').

omega_variable(
    demographic_drag,
    'Is the model structurally resilient to China''s shrinking working-age population and the erosion of its low-cost labor advantage?',
    'Analysis of productivity growth in high-tech manufacturing vs. the decline in labor supply and rising wage pressures.',
    'If productivity cannot outpace demographic decline, the model''s core advantage erodes, potentially making it a Mountain (an unavoidable economic limit) from a future perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_drag, empirical, 'The model''s resilience to demographic decline and shrinking labor surplus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_export_led_growth, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t1990, china_export_led_growth, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(chin_tr_t2005, china_export_led_growth, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(chin_tr_t2025, china_export_led_growth, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(chin_be_t1990, china_export_led_growth, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(chin_be_t2005, china_export_led_growth, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(chin_be_t2025, china_export_led_growth, base_extractiveness, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_export_led_growth, resource_allocation).
narrative_ontology:affects_constraint(china_export_led_growth, global_supply_chains).
narrative_ontology:affects_constraint(china_export_led_growth, us_deindustrialization).
narrative_ontology:affects_constraint(china_export_led_growth, rare_earth_monopoly).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
