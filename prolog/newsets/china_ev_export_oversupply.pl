% ============================================================================
% CONSTRAINT STORY: china_ev_export_oversupply
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_ev_export_oversupply, []).

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
 *   constraint_id: china_ev_export_oversupply
 *   human_readable: Chinese EV Export Oversupply and Market Distortion
 *   domain: economic/political
 *
 * SUMMARY:
 *   China's state-led industrial policy has created massive overcapacity in
 *   its electric vehicle (EV) sector. This oversupply is increasingly
 *   exported to global markets at prices that foreign competitors, operating
 *   without equivalent state backing, cannot match. The constraint is the
 *   market distortion created by this policy, which simultaneously provides a
 *   coordination benefit (lower-cost EVs for consumers) while enacting a
 *   powerful extractive mechanism (capturing market share and hollowing out
 *   the industrial base of competitor nations).
 *
 * KEY AGENTS:
 *   - Chinese State: Primary beneficiary (institutional/arbitrage) - Executes a long-term strategy for technological and industrial dominance.
 *   - Western Automakers: Primary victim (institutional/constrained) - Incumbent firms whose business models are directly threatened by subsidized competition.
 *   - Western Auto Workers: Secondary victim (organized/trapped) - Face job losses with few comparable alternatives.
 *   - Global Consumers: Incidental beneficiary (powerless/mobile) - Benefit from lower prices in the short term, unaware of the larger extractive dynamic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_ev_export_oversupply, 0.6).
domain_priors:suppression_score(china_ev_export_oversupply, 0.75).
domain_priors:theater_ratio(china_ev_export_oversupply, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_ev_export_oversupply, extractiveness, 0.6).
narrative_ontology:constraint_metric(china_ev_export_oversupply, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(china_ev_export_oversupply, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_ev_export_oversupply, tangled_rope).
narrative_ontology:human_readable(china_ev_export_oversupply, "Chinese EV Export Oversupply and Market Distortion").
narrative_ontology:topic_domain(china_ev_export_oversupply, "economic/political").

domain_priors:requires_active_enforcement(china_ev_export_oversupply).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_ev_export_oversupply, chinese_state).
narrative_ontology:constraint_beneficiary(china_ev_export_oversupply, chinese_ev_manufacturers).
narrative_ontology:constraint_beneficiary(china_ev_export_oversupply, global_consumers_short_term).
narrative_ontology:constraint_victim(china_ev_export_oversupply, western_automakers).
narrative_ontology:constraint_victim(china_ev_export_oversupply, western_auto_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WESTERN AUTOMAKER (SNARE) — Faces subsidized competition that erodes market share and profitability. Exit is constrained by massive capital investments and national industrial importance. From this view, the constraint is pure extraction with no redeeming coordination function. d≈0.8, f(d)≈1.2, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(china_ev_export_oversupply, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: WESTERN AUTO WORKER (SNARE) — Directly threatened by factory closures and production cuts with limited options to exit the industry or region. Experiences the constraint as a direct threat to livelihood. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.85.
constraint_indexing:constraint_classification(china_ev_export_oversupply, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CHINESE STATE (ROPE) — The architect of the policy. Experiences the constraint as a pure coordination mechanism to achieve strategic goals: technological dominance, export growth, and geopolitical influence. Can modify the terms at will (arbitrage). d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(china_ev_export_oversupply, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL CONSUMER (ROPE) — In the short term, benefits from a greater supply of lower-cost EVs. Experiences this as a pure coordination benefit, increasing market choice and affordability. The extractive nature is invisible at the point of sale. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.006.
constraint_indexing:constraint_classification(china_ev_export_oversupply, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (delivering cheap EVs to a global market) and the severe, asymmetric extraction (undermining foreign industrial bases via state-directed capital). The high extraction and suppression, combined with a real coordination element, define it as a Tangled Rope. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.83.
constraint_indexing:constraint_classification(china_ev_export_oversupply, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_ev_export_oversupply_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_ev_export_oversupply, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_ev_export_oversupply, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_ev_export_oversupply, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_ev_export_oversupply_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The value being extracted is not just profit, but entire markets, technological leadership, and geopolitical leverage. This is a strategic, not merely commercial, form of extraction. Suppression (0.75): High. Western firms are suppressed by a competitor that does not operate under market logic. Tariffs and trade barriers are the only recourse, but these are slow, politically fraught, and often ineffective against the scale of the oversupply. Theater (0.20): Low. The policy's intent is relatively transparent and aligns with China's stated goals for industrial leadership. The conflict is over the legitimacy of the methods, not their existence.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the Chinese state and the short-term global consumer, the system is a Rope, efficiently coordinating resources to produce a desired good. For Western automakers and their employees, it is a Snare, a coercive trap destroying their economic viability. The analytical observer, weighing both the real coordination benefit and the severe asymmetric extraction, must classify it as a Tangled Rope. This highlights how a single policy can be simultaneously functional and extractive, depending on one's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural relationships. The Chinese state, as the beneficiary with arbitrage power, has a d-value near zero, resulting in negative effective extraction (a net subsidy). Western automakers and workers, as victims with constrained or trapped exit, have d-values approaching 1.0, experiencing maximum effective extraction. Global consumers, as beneficiaries with mobile exit (they can choose any car), have a low d-value, experiencing the system as a coordination benefit. The model correctly captures these divergent experiences without needing overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic resolution of the mandatrophy. A simplistic analysis might label the policy as 'predatory dumping' (a Snare). Another might focus on 'consumer choice' (a Rope). Deferential Realism avoids this by requiring a multi-perspectival view. The Tangled Rope classification from the analytical perspective correctly identifies that BOTH are happening simultaneously: a genuine coordination function is being used to power a massive extractive engine. The policy's political resilience comes from its ability to present its Rope-like face to beneficiaries while its Snare-like face is felt by victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_vs_efficiency,
    'What portion of the Chinese EV price advantage is attributable to state subsidies versus genuine manufacturing efficiencies and economies of scale?',
    'Forensic accounting of state-owned enterprises, supply chain cost analysis, and comparison with non-subsidized manufacturing costs.',
    'If primarily efficiency, the constraint is closer to a Rope (legitimate competition). If primarily subsidies, it confirms the Snare/Tangled Rope classification (illegitimate extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_vs_efficiency, empirical, 'Distinguishing state subsidies from legitimate production efficiency').

omega_variable(
    consumer_welfare_vs_industrial_base,
    'Does the short-term consumer welfare gain from cheaper EVs outweigh the long-term strategic cost of losing domestic automotive industrial capacity?',
    'Economic modeling of long-term impacts on GDP, employment, and national security versus short-term consumer surplus.',
    'This is a core policy debate. Framing it as a consumer issue favors a Rope interpretation; framing it as a strategic/industrial issue favors a Snare interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_welfare_vs_industrial_base, preference, 'Trade-off between short-term consumer benefit and long-term industrial health').

omega_variable(
    inevitable_market_shift,
    'Is this policy merely accelerating an inevitable market shift towards Chinese EV dominance that would have occurred anyway?',
    'Counterfactual analysis of market trends absent Chinese industrial policy.',
    'If the shift is inevitable, the policy appears more like a Scaffold (hastening a transition). If not, it is a coercive Snare creating an artificial market outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitable_market_shift, conceptual, 'Whether the policy accelerates an inevitable or creates an artificial outcome').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_ev_export_oversupply, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_ev_export_oversupply, theater_ratio, 0, 0.15).
narrative_ontology:measurement(chin_tr_t5, china_ev_export_oversupply, theater_ratio, 5, 0.18).
narrative_ontology:measurement(chin_tr_t10, china_ev_export_oversupply, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_ev_export_oversupply, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(chin_be_t5, china_ev_export_oversupply, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(chin_be_t10, china_ev_export_oversupply, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_ev_export_oversupply, resource_allocation).
narrative_ontology:affects_constraint(china_ev_export_oversupply, global_supply_chain_resilience).
narrative_ontology:affects_constraint(china_ev_export_oversupply, western_deindustrialization).
narrative_ontology:affects_constraint(china_ev_export_oversupply, lithium_ion_battery_geopolitics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
