% ============================================================================
% CONSTRAINT STORY: decapitation_as_regime_change
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decapitation_as_regime_change, []).

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
 *   constraint_id: decapitation_as_regime_change
 *   human_readable: Decapitation Strike as Regime Change Mechanism
 *   domain: international_relations/intelligence_operations/regime_change
 *
 * SUMMARY:
 *   The operational shift from nuclear facility targeting (deterrence logic:
 *   degrade capability, preserve regime) to leadership compound targeting
 *   (regime change logic: eliminate decision-maker, accept instability)
 *   represents a measurable policy escalation in intelligence-driven military
 *   operations. This constraint exhibits the structural signature of a
 *   tangled rope: genuine coordination functions (intelligence sharing,
 *   operational planning, alliance management) coexist with significant
 *   extraction concentrated on trapped populations and eroded collective
 *   goods. The theater ratio (0.58) reflects increasing performative
 *   justification: stated objectives (nuclear program rollback, WMD
 *   prevention) diverge from operational reality (regime elimination,
 *   regional realignment). The constraint is downstream of
 *   intelligence_as_sovereignty_transfer — the intelligence apparatus that
 *   enables decapitation strikes is itself a tangled rope constraint where
 *   host states trade sovereignty for security guarantees, creating the
 *   operational infrastructure that makes leadership targeting feasible.
 *
 * KEY AGENTS:
 *   - Targeted State Population: Primary victim (powerless/trapped) — bears maximum extraction through regime instability, infrastructure collapse, humanitarian crisis with no exit option
 *   - Deterrence Stability Framework: Abstract victim (powerless/trapped) — collective good of nuclear stability eroded by first-strike incentives and deterrence firewall collapse
 *   - Regional Realignment Advocates: Primary beneficiary (institutional/arbitrage) — intelligence apparatus, defense contractors, and geopolitical strategists capture operational budgets and influence
 *   - Regional Neighboring States: Secondary victim (moderate/constrained) — face mixed extraction (refugee flows, economic disruption) and coordination (intelligence sharing, alliance benefits)
 *   - International Law Framework: Institutional victim (institutional/constrained) — sovereignty norms violated with no enforcement mechanism against powerful actors
 *   - Arms Control Advocacy Coalition: Organized agents (organized/mobile) — see temporary escalation triggering counter-mobilization and doctrine reform (scaffold logic)
 *   - UN Security Council: Institutional observer (institutional/constrained) — maintains authorization theater despite veto-driven irrelevance (piton logic)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies structural asymmetry between beneficiaries with exit options and victims without
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decapitation_as_regime_change, 0.68).
domain_priors:suppression_score(decapitation_as_regime_change, 0.75).
domain_priors:theater_ratio(decapitation_as_regime_change, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decapitation_as_regime_change, extractiveness, 0.68).
narrative_ontology:constraint_metric(decapitation_as_regime_change, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(decapitation_as_regime_change, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decapitation_as_regime_change, tangled_rope).
narrative_ontology:human_readable(decapitation_as_regime_change, "Decapitation Strike as Regime Change Mechanism").
narrative_ontology:topic_domain(decapitation_as_regime_change, "international_relations/intelligence_operations/regime_change").

domain_priors:requires_active_enforcement(decapitation_as_regime_change).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decapitation_as_regime_change, regional_realignment_advocates).
narrative_ontology:constraint_beneficiary(decapitation_as_regime_change, intelligence_apparatus).
narrative_ontology:constraint_beneficiary(decapitation_as_regime_change, defense_contractors).
narrative_ontology:constraint_victim(decapitation_as_regime_change, deterrence_stability_framework).
narrative_ontology:constraint_victim(decapitation_as_regime_change, targeted_state_population).
narrative_ontology:constraint_victim(decapitation_as_regime_change, international_law_norms).
narrative_ontology:constraint_victim(decapitation_as_regime_change, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED STATE POPULATION (SNARE) — Civilians in the targeted state bear maximum extraction: regime instability, infrastructure collapse, humanitarian crisis, and loss of sovereignty with no exit option. The operational shift from nuclear facility strikes (deterrence logic) to leadership compound strikes (regime change logic) removes any pretense of limited objectives. Population cannot exit national borders en masse, cannot influence targeting decisions, and bears full cost of post-regime chaos.
constraint_indexing:constraint_classification(decapitation_as_regime_change, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DETERRENCE STABILITY FRAMEWORK (SNARE) — The abstract collective good of nuclear deterrence stability is trapped and powerless. Decapitation strikes undermine the mutual assured destruction equilibrium by suggesting that regime survival rather than capability degradation is the true objective. This creates first-strike incentives for targeted states (use-it-or-lose-it logic) and erodes the firewall between conventional and nuclear conflict. The framework has no advocate and cannot exit the security dilemma.
constraint_indexing:constraint_classification(decapitation_as_regime_change, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL NEIGHBORING STATES (TANGLED ROPE) — States adjacent to the targeted regime face mixed extraction and coordination. They benefit from potential regime change if the targeted state was hostile, but bear costs of refugee flows, economic disruption, and spillover instability. Exit options are constrained by geography and alliance commitments. Some coordination function exists (intelligence sharing, airspace access) but asymmetric extraction is significant.
constraint_indexing:constraint_classification(decapitation_as_regime_change, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL REALIGNMENT ADVOCATES (ROPE) — Primary beneficiaries experience the constraint as coordination: decapitation enables regional power restructuring aligned with their strategic interests. Intelligence apparatus and defense contractors capture operational budgets, geopolitical influence, and post-conflict reconstruction contracts. Arbitrage exit options allow these actors to shift resources and narratives as needed. Net beneficiaries see low effective extraction.
constraint_indexing:constraint_classification(decapitation_as_regime_change, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LAW FRAMEWORK (TANGLED ROPE) — International legal institutions face genuine coordination needs (preventing aggressive war, protecting sovereignty) but are constrained by enforcement asymmetry. Decapitation strikes violate sovereignty norms and UN Charter Article 2(4), yet the legal framework cannot exit the enforcement dilemma: powerful states ignore rulings while weak states bear full legal constraints. Mixed extraction and coordination with significant suppression.
constraint_indexing:constraint_classification(decapitation_as_regime_change, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ARMS CONTROL ADVOCACY COALITION (SCAFFOLD) — Organized civil society actors (ICAN, Ploughshares Fund, academic networks) see decapitation doctrine as a temporary escalation that will trigger counter-mobilization. The operational shift from deterrence to regime change is so destabilizing that it creates political space for arms control treaties and doctrine reform. Sunset logic: the more extreme the policy, the stronger the backlash and institutional correction. Mobile exit options allow coalition members to shift advocacy strategies.
constraint_indexing:constraint_classification(decapitation_as_regime_change, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: UN SECURITY COUNCIL (PITON) — The Security Council's authorization process for use of force has degraded into theater. Veto power ensures that decapitation strikes by permanent members or their allies proceed without authorization, while the Council maintains the ritual of debate and resolutions. Theater ratio is high: the institutional process persists through inertia and legitimacy maintenance, not because it constrains powerful actors. The Council sees its own function as atrophied.
constraint_indexing:constraint_classification(decapitation_as_regime_change, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, decapitation doctrine exhibits both coordination (intelligence sharing, operational planning, alliance management) and extraction (sovereignty violation, deterrence erosion, humanitarian cost externalization). The operational shift from nuclear facility targeting to leadership targeting is a measurable policy escalation that concentrates extraction on trapped populations while benefiting institutional actors with arbitrage options. Structural asymmetry is clear: beneficiaries can exit or reframe; victims cannot.
constraint_indexing:constraint_classification(decapitation_as_regime_change, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decapitation_as_regime_change_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decapitation_as_regime_change, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decapitation_as_regime_change, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decapitation_as_regime_change, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decapitation_as_regime_change, TR),
    TR >= 0.70.

:- end_tests(decapitation_as_regime_change_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but below snare threshold. The operational shift from deterrence to regime change concentrates extraction on trapped populations (civilians, deterrence stability) while benefiting institutional actors with arbitrage options (intelligence apparatus, defense contractors, regional realignment advocates). The extraction is significant but not total because genuine coordination functions exist: intelligence sharing provides real security value to allies, operational planning coordinates multi-state responses, and some regional neighbors do benefit from hostile regime removal. The asymmetry is severe but the coordination function is real. Suppression (0.75): Very high. Alternatives to decapitation (diplomatic engagement, sanctions, deterrence-only targeting) are systematically suppressed through intelligence framing (regime is irrational, diplomacy has failed, deterrence is insufficient) and operational momentum (once intelligence infrastructure is in place, pressure to use it). Targeted states have no exit from the threat; neighboring states are locked into alliance commitments; international law has no enforcement mechanism. Theater ratio (0.58): High and rising but stabilizing below piton threshold. Stated objectives (nuclear program rollback, WMD prevention, counter-terrorism) diverge from operational reality (regime elimination, regional power restructuring), but the gap is not total — some strikes do target WMD infrastructure alongside leadership. The theater component reflects increasing performative justification as doctrine normalizes, but the constraint retains functional operational content.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Regional realignment advocates see coordination (Rope) — decapitation enables strategic restructuring aligned with their interests, and they experience low extraction due to arbitrage exit options. The arms control coalition sees a temporary problem with a sunset (Scaffold) — the operational escalation is so destabilizing that it will trigger counter-mobilization and doctrine reform. The UN Security Council sees its own degraded ritual (Piton) — authorization processes persist through inertia despite veto-driven irrelevance. Regional neighbors see mixed coordination and extraction (Tangled Rope) — intelligence sharing and alliance benefits coexist with refugee costs and instability spillover. But trapped victims see pure extraction (Snare): targeted populations bear regime instability and humanitarian crisis with no exit; the deterrence stability framework erodes with no self-correction mechanism; international law norms are violated with no enforcement. The analytical observer identifies this as a tangled rope with severe extraction asymmetry — genuine coordination functions (intelligence sharing, operational planning) coexist with concentrated extraction on powerless agents. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' Beneficiaries with arbitrage options see coordination; victims without exit see extraction; the analytical view synthesizes both.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reveals clear extraction asymmetry alongside genuine coordination. Regional realignment advocates (institutional/arbitrage, beneficiaries) experience low effective extraction — they capture operational budgets, geopolitical influence, and reconstruction contracts with full exit options. The intelligence apparatus benefits from mission expansion and budget justification. Defense contractors benefit from operational demand and post-conflict reconstruction. These actors can reframe operations, shift resources, or exit entirely if political costs rise. In contrast, targeted state populations (powerless/trapped, victims) experience maximum extraction — regime instability, infrastructure collapse, humanitarian crisis — with no exit option. Civilians cannot leave en masse, cannot influence targeting decisions, and bear full cost of post-regime chaos. The deterrence stability framework (powerless/trapped, victim) is an abstract collective good with no advocate: nuclear stability erodes through first-strike incentives and firewall collapse, but the framework cannot organize or exit the security dilemma. Regional neighboring states (moderate/constrained) face mixed extraction: they may benefit from hostile regime removal but bear costs of refugee flows and economic disruption, with exit constrained by geography and alliance commitments. International law institutions (institutional/constrained) face enforcement asymmetry: powerful states ignore sovereignty norms while weak states bear full legal constraints. The directionality gap between beneficiaries (low d, low chi) and victims (high d, high chi) coexists with genuine coordination functions (intelligence sharing, operational planning, alliance management).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that snare and tangled rope classifications are both structurally valid from different perspectives, with the analytical classification (tangled rope) acknowledging both the genuine coordination function (intelligence sharing, alliance management, operational planning) and the severe extraction asymmetry (concentrated costs on trapped populations, eroded deterrence stability, violated sovereignty norms). The snare classification from powerless/trapped perspectives is their structural reality — they experience maximum extraction with no exit. The rope classification from institutional/arbitrage perspectives is their genuine experience — they capture benefits with full exit options. The scaffold classification from organized/mobile perspectives reflects real counter-mobilization potential — extreme escalation creates political space for arms control reform. The piton classification from the UN Security Council reflects real institutional degradation — authorization theater persists despite irrelevance. The analytical tangled rope classification synthesizes these perspectives: the constraint has a genuine coordination function (regional security management, intelligence sharing) but exhibits severe extraction asymmetry (beneficiaries can exit, victims cannot). The operational shift from deterrence to regime change is measurable through target selection and stated objectives, and the extraction accumulation over time (0.58 → 0.68) reflects mission creep from exceptional contingency to routine operational option. The mandatrophy is resolved by showing that all classifications are legitimate perspectival readings of the same structural data, with the analytical view identifying the coordination-extraction hybrid and the extraction asymmetry that makes it a snare from trapped perspectives but a tangled rope when coordination functions are included in the analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_threshold,
    'At what threshold does leadership targeting cross from deterrence reinforcement (demonstrating resolve) to deterrence erosion (creating first-strike incentives)?',
    'Game-theoretic modeling of targeted state decision calculus; historical analysis of crisis stability before and after decapitation attempts; measurement of nuclear alert posture changes following leadership strikes',
    'If threshold is low (any leadership targeting destabilizes): snare classification strengthened. If threshold is high (only successful decapitation destabilizes): some tangled rope perspectives gain validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_threshold, empirical, 'Threshold at which leadership targeting destabilizes deterrence').

omega_variable(
    regime_change_success_rate,
    'What is the empirical success rate of decapitation strikes in achieving stable regime change vs triggering prolonged instability?',
    'Historical case analysis: Libya 2011, Iraq 2003, Afghanistan 2001, Syria 2017-present; measurement of post-strike governance stability, humanitarian outcomes, and regional spillover effects over 5-10 year horizons',
    'If success rate is high and stability follows: coordination function is real, reducing snare classification. If success rate is low or instability follows: extraction dominates, strengthening snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_change_success_rate, empirical, 'Empirical success rate of decapitation in achieving stable outcomes').

omega_variable(
    intelligence_reliability_bias,
    'Do intelligence assessments supporting decapitation strikes exhibit systematic bias toward overestimating regime fragility and underestimating post-strike chaos?',
    'Retrospective analysis of pre-strike intelligence estimates vs post-strike outcomes; identification of institutional incentives for optimistic assessments; comparison of intelligence community predictions to independent academic forecasts',
    'If systematic bias exists: the constraint is partly an intelligence apparatus self-justification mechanism (higher extraction). If assessments are unbiased: operational decisions reflect genuine strategic calculation (lower extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligence_reliability_bias, empirical, 'Whether intelligence assessments exhibit pro-strike bias').

omega_variable(
    humanitarian_cost_externalization,
    'Are post-decapitation humanitarian costs (civilian casualties, refugee flows, infrastructure collapse) systematically externalized to regional neighbors and international aid systems rather than borne by strike executors?',
    'Cost-benefit analysis comparing strike executor expenditures (operational costs, reconstruction aid) to regional neighbor costs (refugees, economic disruption) and international system costs (humanitarian response, peacekeeping); tracking of who pays for post-regime stabilization',
    'If costs are externalized: extraction is higher than operational budgets suggest (snare strengthened). If costs are internalized: coordination function is more genuine (tangled rope strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_cost_externalization, empirical, 'Whether humanitarian costs are externalized to third parties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decapitation_as_regime_change, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decap_theater_initial, decapitation_as_regime_change, theater_ratio, 0, 0.45).
narrative_ontology:measurement(decap_theater_early, decapitation_as_regime_change, theater_ratio, 3, 0.5).
narrative_ontology:measurement(decap_theater_mid, decapitation_as_regime_change, theater_ratio, 6, 0.54).
narrative_ontology:measurement(decap_theater_current, decapitation_as_regime_change, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(decap_extract_initial, decapitation_as_regime_change, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(decap_extract_early, decapitation_as_regime_change, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(decap_extract_mid, decapitation_as_regime_change, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(decap_extract_current, decapitation_as_regime_change, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decapitation_as_regime_change, enforcement_mechanism).
narrative_ontology:affects_constraint(decapitation_as_regime_change, intelligence_as_sovereignty_transfer).

% DUAL FORMULATION NOTE:
% This constraint is downstream of intelligence_as_sovereignty_transfer. The upstream constraint (intelligence apparatus as sovereignty trade) creates the operational infrastructure that enables decapitation strikes. The tangled rope classification of the upstream constraint (host states trade sovereignty for security guarantees) feeds into the tangled rope classification of this constraint (operational infrastructure is used for regime change with genuine coordination alongside extraction). The network relationship is causal: without the intelligence infrastructure from sovereignty transfer, leadership targeting would not be operationally feasible at scale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decapitation_as_regime_change, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
