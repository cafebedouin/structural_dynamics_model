% ============================================================================
% CONSTRAINT STORY: us_iran_maximum_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_iran_maximum_pressure, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_iran_maximum_pressure
 *   human_readable: US Maximum Pressure Campaign Against Iran
 *   domain: geopolitical/economic_coercion
 *
 * SUMMARY:
 *   The US maximum pressure campaign on Iran, initiated in 2018 following
 *   withdrawal from the JCPOA, represents a coercive constraint that exhibits
 *   characteristics of both pure extraction (snare) and mixed
 *   coordination-extraction (tangled rope) depending on the observer's
 *   structural position. The campaign employs comprehensive economic
 *   sanctions including petroleum export embargoes, financial system
 *   isolation, secondary sanctions on third parties, and targeted sectoral
 *   restrictions. From the perspective of the Iranian civilian population and
 *   economy, the constraint appears as a snare: no genuine exit option exists
 *   except through capitulation on nuclear posture, suppression is total
 *   (currency collapse, import restrictions, international financial
 *   isolation), and extraction flows entirely toward the US strategic
 *   position. From the perspective of the Iranian government, it is a tangled
 *   rope: the state retains some coordination function (mobilizing
 *   resistance, negotiating through intermediaries) and some constrained
 *   mobility (alternative partnerships with China and Russia). From the US
 *   administration's perspective, it functions as a coordination mechanism
 *   (rope) for enforcing non-proliferation and regional realignment. The
 *   campaign's theater ratio (0.62) reflects that much of the enforcement
 *   infrastructure is performative: secondary sanctions compliance is
 *   selective, enforcement is often signaling rather than comprehensive, and
 *   the stated objective (preventing weaponization) remains contested. The
 *   constraint has not achieved its stated goal of forcing renegotiation —
 *   instead it has created a stable extraction regime where Iran bears costs
 *   while maintaining nuclear advancement.
 *
 * KEY AGENTS:
 *   - Iranian Civilian Population: Primary victim (powerless/trapped) — bears currency collapse, medicine shortages, inflation; no exit option
 *   - Iranian Government: Secondary victim (moderate/constrained) — subject to extraction but retains negotiation pathways and alternative partnerships (Russia, China)
 *   - US Administration: Primary beneficiary (institutional/arbitrage) — captures strategic leverage, geopolitical realignment, regional ally consolidation
 *   - Saudi Arabia and UAE: Secondary beneficiary (powerful/mobile) — benefit from reduced Iranian regional competition but constrained by retaliation risks
 *   - Multinational Corporations: Constrained actor (powerful/mobile) — experience secondary sanctions compliance costs but can divest or reallocate
 *   - Humanitarian Organizations: Organized actor (organized/mobile) — build parallel corridors and negotiate carve-outs; see sunset path through negotiation
 *   - Sanctions Bureaucracy: Institutional actor (institutional/arbitrage) — maintains compliance theater; perpetuates constraint through administrative inertia
 *   - Analytical Observer: Civilizational context (analytical/analytical) — recognizes snare structure: designed coercion with no genuine off-ramp until capitulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_iran_maximum_pressure, 0.68).
domain_priors:suppression_score(us_iran_maximum_pressure, 0.78).
domain_priors:theater_ratio(us_iran_maximum_pressure, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_iran_maximum_pressure, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_iran_maximum_pressure, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_iran_maximum_pressure, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_iran_maximum_pressure, snare).
narrative_ontology:human_readable(us_iran_maximum_pressure, "US Maximum Pressure Campaign Against Iran").
narrative_ontology:topic_domain(us_iran_maximum_pressure, "geopolitical/economic_coercion").

domain_priors:requires_active_enforcement(us_iran_maximum_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_iran_maximum_pressure, us_administration).
narrative_ontology:constraint_beneficiary(us_iran_maximum_pressure, domestic_oil_producers).
narrative_ontology:constraint_beneficiary(us_iran_maximum_pressure, regional_allies).
narrative_ontology:constraint_victim(us_iran_maximum_pressure, iranian_economy).
narrative_ontology:constraint_victim(us_iran_maximum_pressure, iranian_civilians).
narrative_ontology:constraint_victim(us_iran_maximum_pressure, global_oil_market_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN CIVILIANS (SNARE) — Trapped within a national economy subjected to comprehensive secondary sanctions with no exit option. Bears the full cost through currency collapse, medicine shortages, inflation, and reduced public services. No meaningful alternatives and maximum vulnerability to extraction mechanism.
constraint_indexing:constraint_classification(us_iran_maximum_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IRANIAN GOVERNMENT (TANGLED ROPE) — Constrained by sanctions but retains negotiation pathways and resistance coordination functions. Experiences extraction (economic pressure) alongside a genuine coordination problem (how to sustain state function under blockade). Not fully trapped — has constrained mobility and some agency in negotiation or defection to alternative partnerships.
constraint_indexing:constraint_classification(us_iran_maximum_pressure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US ADMINISTRATION (ROPE) — Experiences the maximum pressure campaign as a coordination mechanism for realigning regional geopolitics and nuclear posture. Net beneficiary with arbitrage options — can escalate, negotiate, or pivot to other adversaries. Sees the constraint as a tool for solving coordination problems (deterrence, leverage for negotiation).
constraint_indexing:constraint_classification(us_iran_maximum_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL CORPORATIONS AND BANKS (TANGLED ROPE) — Face secondary sanctions compliance requirements that extract regulatory costs and operational restrictions, but retain mobility to shift operations, divest, or comply selectively. Experience both coercion (compliance burden) and coordination benefit (clarity on exposure management). Can exit at cost but choose constrained participation.
constraint_indexing:constraint_classification(us_iran_maximum_pressure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANITARIAN & MULTILATERAL ACTORS (SCAFFOLD) — See the maximum pressure campaign as a temporary enforcement mechanism with a negotiated sunset (JCPOA, successor agreements, sanctions relief). Organized actors (UN, Red Crescent, EU) are building parallel humanitarian access and economic corridors (carve-outs for medicine, food) that reduce extraction's severity and build pathways to constraint removal. Low effective extraction because the coalition has capacity to negotiate relief and sees a transition path.
constraint_indexing:constraint_classification(us_iran_maximum_pressure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: SANCTIONS ENFORCEMENT INFRASTRUCTURE (PITON) — The technical apparatus of secondary sanctions compliance has become substantially performative. Treasury OFAC lists, compliance certifications, and audit requirements proliferate beyond their enforcement capacity — institutions maintain costly compliance theater while actual enforcement is selective. The constraint persists through institutional inertia and career incentives within the bureaucracy, not primarily through functional extraction.
constraint_indexing:constraint_classification(us_iran_maximum_pressure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global scope, maximum pressure is a snare: it systematically extracts negotiation leverage from Iran while suppressing alternatives (regional coalitions, sanctions-busting networks, counter-coercion) and offering no genuine off-ramps until capitulation on nuclear posture. The extraction is structural, not natural — designed specifically to remove options until one choice becomes inevitable.
constraint_indexing:constraint_classification(us_iran_maximum_pressure, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_iran_maximum_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_iran_maximum_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_iran_maximum_pressure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_iran_maximum_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_iran_maximum_pressure, TR),
    TR >= 0.70.

:- end_tests(us_iran_maximum_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The campaign imposes severe economic costs on Iran with asymmetric burden distribution — Iranian civilians and economy bear the full weight while the US administration captures strategic leverage. The extractiveness has increased from 0.42 (at campaign onset with partial sanctions) to 0.68 (at comprehensive enforcement) as secondary sanctions expanded and alternative trade routes were closed. Suppression (0.78): Very high. Iran's exit options are severely constrained through multiple mechanisms: financial system isolation prevents alternative trade financing, petroleum export restrictions block primary revenue source, secondary sanctions threaten third-party partners, and international political isolation limits negotiation pathways. Suppression reflects the comprehensive nature of the coercion — not a single lever but a systemic closure of alternatives. Theater ratio (0.62): Moderate-high. The enforcement infrastructure includes significant performative elements: OFAC compliance certifications that exceed actual enforcement capacity, selective secondary sanctions enforcement, humanitarian carve-outs that reduce credibility of the 'maximum pressure' framing, and periodic signaling adjustments that suggest negotiability. However, the theater is not dominant — real economic harm is being inflicted, distinguishing this from a pure piton. The theater increased from 0.48 to 0.62 as the campaign matured and the gap between stated enforcement and actual compliance widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural asymmetry in power and exit options produces radically different classifications from mathematically identical base metrics. The snare classification from the powerless perspective captures the zero-exit condition and maximum suppression. The rope classification from the institutional beneficiary perspective captures the coordination function (solving deterrence and leverage problems). These are not contradictory — they reflect genuine structural differences in how the constraint operates across positions. The analytical observer's snare classification from the civilizational scope captures that the campaign has no designed exit except capitulation, making the rope reading from the US perspective analytically contestable (it assumes the coordination problem is genuinely shared, not unilaterally imposed).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Iranian civilians are full targets (d ≈ 0.95): they have no exit and bear maximum costs. Iranian government is partial target with some agency (d ≈ 0.70): constrained but retains alternatives and negotiation capacity. US administration is full beneficiary (d ≈ 0.05): captures strategic leverage, faces no material costs, has arbitrage options (escalate, negotiate, shift focus). Saudi/UAE are mixed (d ≈ 0.40): benefit from reduced Iranian competition but bear indirect costs of regional instability and retaliation risk. The directionality computation produces high effective extractiveness (χ) from the beneficiary perspective because they have low d-values that map to negative f(d), while the victim perspectives have high d-values producing high f(d), amplifying the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The high extractiveness (0.68) required explicit mandatrophy resolution through multiple omega variables. The resolution strategy is to decompose the apparent ambiguity between 'coercive enforcement' (snare) and 'deterrence coordination' (rope) into observable-dependent classifications. From the Iranian perspective, the constraint unambiguously satisfies snare criteria: ε ≥ 0.46 (confirmed), suppression ≥ 0.60 (0.78 confirmed), χ ≥ 0.66 (confirmed for powerless/trapped agent). From the US perspective, rope classification requires that the coordination problem is genuine (Iran's nuclear program genuinely threatens regional stability or US security) and that the constraint solves it (maximum pressure creates incentives for negotiation). The mandatrophy resolves by acknowledging both readings are structurally valid — they reflect different perspectives on whether the coordination problem is shared or unilaterally imposed. The analytical perspective interprets the absence of credible off-ramps (omega variable 3) as evidence that the rope reading is aspirational — the US frames deterrence as coordination, but without exit paths available to Iran, the structure is coercive rather than coordinative. The mandatrophy_resolved flag is set to true with the understanding that resolution consists of perspectival differentiation, not logical reconciliation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_intention_uncertainty,
    'What is Iran''s actual nuclear weapons program intent versus capability development?',
    'Classified intelligence assessment, IAEA inspection data, Iranian technical declarations, or post-sanctions technical capability analysis',
    'If weaponization intent is confirmed: maximum pressure is coordination enforcing non-proliferation (Rope/Scaffold from some perspectives). If intent is capability development for deterrence only: maximum pressure is pure coercion (Snare confirmed). If intent is ambiguous: maximum pressure embeds fundamental uncertainty about justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_intention_uncertainty, empirical, 'Iran''s actual nuclear weapons program intent versus capability development').

omega_variable(
    humanitarian_cost_threshold,
    'At what level of civilian suffering does a coercive constraint cross from ''extraction'' into ''structural violence''?',
    'Documented mortality/morbidity rates from sanctions-induced healthcare collapse, inflation-driven malnutrition, currency collapse suicides; longitudinal health metrics; WHO/UN assessments',
    'If threshold < observed suffering: snare classification is understated — the constraint is worse than snare metrics indicate. If threshold > observed suffering: humanitarian cost is externalized rather than counted as extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_cost_threshold, conceptual, 'Humanitarian cost threshold for structural violence classification').

omega_variable(
    negotiation_path_existence,
    'Do credible off-ramps exist where Iran can exit the constraint without complete capitulation on nuclear posture?',
    'Analysis of JCPOA negotiation history, current administration public statements on negotiation conditions, intelligence assessments of Iranian leadership preferences, EU/UN mediation proposals',
    'If credible off-ramps exist: snare is reclassified as Tangled Rope (mixed extraction and coordination). If off-ramps require complete surrender: snare classification confirmed — constraint has no genuine exit except through capitulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_path_existence, empirical, 'Whether credible negotiation pathways exist for constraint exit').

omega_variable(
    alternative_sanctions_coalition_viability,
    'Can Iran sustain economic function through alternative trade partnerships (China, Russia, Gulf smuggling networks) that effectively bypass secondary sanctions?',
    'Trade flow analysis (Chinese oil imports, Russian technology transfer, informal networks); Iranian foreign exchange reserves trajectory; comparison to North Korea and Venezuela sanction-busting models',
    'If viable: suppression is lower than 0.78 (Iran has structural workarounds). If not viable: suppression is understated — Iran has fewer alternatives than the metric suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_sanctions_coalition_viability, empirical, 'Viability of sanctions bypass through alternative partnerships').

omega_variable(
    theater_ratio_measurement_basis,
    'What proportion of maximum pressure enforcement is genuine economic consequence versus performative compliance theater?',
    'Comparison of stated versus actual enforcement actions (OFAC fines, blocked transactions, identified secondary violations); sanctions evasion success rates; compliance cost versus actual impact on Iranian state capacity',
    'If enforcement is selective/theatrical: theater_ratio should be higher (>0.75), degrading the snare classification toward piton. If enforcement is comprehensive: theater_ratio confirmed, snare is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_basis, empirical, 'Genuine versus performative enforcement in maximum pressure campaign').

omega_variable(
    directionality_ambiguity_saudi_emirati,
    'Are Saudi Arabia and UAE primarily beneficiaries of US maximum pressure on Iran, or are they constrained agents caught between US enforcement demands and Iranian retaliation threats?',
    'Analysis of regional business flows, energy sector impacts, military procurement dependencies, UAV/drone attack patterns; interview data on state perception of constraint; corporate filings showing margin impacts',
    'If beneficiaries: directionality is clear (extraction flows from Iran toward regional allies). If constrained: regional allies experience tangled rope — they benefit from reduced Iranian competition but bear costs of regional instability and retaliation risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(directionality_ambiguity_saudi_emirati, empirical, 'Regional ally structural position: beneficiary or constrained participant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_iran_maximum_pressure, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_i_tr_t0, us_iran_maximum_pressure, theater_ratio, 0, 0.48).
narrative_ontology:measurement(us_i_tr_t2, us_iran_maximum_pressure, theater_ratio, 2, 0.55).
narrative_ontology:measurement(us_i_tr_t4, us_iran_maximum_pressure, theater_ratio, 4, 0.62).

% Extraction over time
narrative_ontology:measurement(us_i_be_t0, us_iran_maximum_pressure, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(us_i_be_t2, us_iran_maximum_pressure, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(us_i_be_t4, us_iran_maximum_pressure, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_iran_maximum_pressure, enforcement_mechanism).
narrative_ontology:affects_constraint(us_iran_maximum_pressure, iranian_regional_proxy_network).
narrative_ontology:affects_constraint(us_iran_maximum_pressure, gulf_state_military_competition).
narrative_ontology:affects_constraint(us_iran_maximum_pressure, global_energy_market_volatility).

% DUAL FORMULATION NOTE:
% Maximum pressure should be decomposed into at least two constraint stories: (1) economic sanctions as extraction mechanism (ε=0.68, snare/tangled_rope), and (2) nuclear deterrence as coordination mechanism (ε=0.35, rope). These are structurally distinct — one measures coercive cost flow, the other measures shared security problems. The same empirical intervention produces different ε values depending on the observable chosen. The stories are linked by affects_constraints to show that economic extraction mechanisms support deterrence coordination mechanisms and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_iran_maximum_pressure, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
