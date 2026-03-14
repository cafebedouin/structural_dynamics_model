% ============================================================================
% CONSTRAINT STORY: north_korean_sanctions_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_north_korean_sanctions_regime, []).

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
 *   constraint_id: north_korean_sanctions_regime
 *   human_readable: North Korean Sanctions Regime
 *   domain: geopolitical/economic/security
 *
 * SUMMARY:
 *   The North Korean sanctions regime represents a complex geopolitical
 *   constraint that operates across multiple institutional and human levels
 *   simultaneously. Initiated following nuclear weapons development and
 *   escalated through multilateral UN Security Council resolutions
 *   (2006–present), the regime imposes comprehensive economic sanctions
 *   designed to deter weapons development through maximum economic pressure.
 *   The constraint exhibits structural tension: it serves a genuine
 *   coordination function for security establishment actors (deterring
 *   proliferation without direct military intervention) while simultaneously
 *   extracting severely from the civilian population that has no capacity to
 *   influence regime weapons policy. The regime's extractiveness has
 *   increased over time (0.35→0.71) as sanctions broadened in scope and
 *   enforcement tightened, while theater ratio has also increased (0.42→0.58)
 *   as enforcement effectiveness has plateaued despite intensified measures.
 *   The constraint exemplifies how a single policy structure can classify as
 *   Snare for powerless agents, Tangled Rope for constrained humanitarian
 *   actors, Rope for beneficiary security establishments, and risk
 *   false-summit Mountain classification at the civilizational analytical
 *   level where security imperatives naturalize contingent policy choices.
 *
 * KEY AGENTS:
 *   - North Korean Civilian Population: Primary victim (powerless/trapped) — bears extraction through economic collapse, medical supply shortages, malnutrition, currency instability with no exit mechanism
 *   - North Korean Government: Secondary actor (institutional/trapped by regime control) — subject to sanctions but insulated from extraction through state monopoly control and regime loyalty mechanisms
 *   - US Security Establishment: Primary beneficiary (institutional/arbitrage) — benefits from coercive deterrence mechanism and maintains policy flexibility through exemptions
 *   - China and Russia: Secondary beneficiary (powerful/mobile) — experience Tangled Rope: enforce rules while maintaining strategic trade relationships, with exit options through secondary markets
 *   - UN Security Council: Institutional coordinator (institutional/arbitrage) — benefits from collective action mechanism that avoids direct military intervention
 *   - Humanitarian Organizations: Constrained actor (moderate/constrained) — experience genuine coordination function (aid delivery) alongside extraction (licensing delays, monitoring burdens, legal liability)
 *   - Sanctions Enforcement Apparatus: Beneficiary institution (institutional/arbitrage) — benefits from expanded enforcement infrastructure, budget allocation, and organizational power
 *   - Dissident and Refugee Networks: Organized victims (organized/constrained) — trapped by regime control of borders, constrained by reduced aid resources flowing through humanitarian channels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_korean_sanctions_regime, 0.68).
domain_priors:suppression_score(north_korean_sanctions_regime, 0.72).
domain_priors:theater_ratio(north_korean_sanctions_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_korean_sanctions_regime, extractiveness, 0.68).
narrative_ontology:constraint_metric(north_korean_sanctions_regime, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(north_korean_sanctions_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(north_korean_sanctions_regime, snare).
narrative_ontology:human_readable(north_korean_sanctions_regime, "North Korean Sanctions Regime").
narrative_ontology:topic_domain(north_korean_sanctions_regime, "geopolitical/economic/security").

domain_priors:requires_active_enforcement(north_korean_sanctions_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(north_korean_sanctions_regime, security_establishment).
narrative_ontology:constraint_beneficiary(north_korean_sanctions_regime, sanctions_enforcement_apparatus).
narrative_ontology:constraint_victim(north_korean_sanctions_regime, north_korean_civilian_population).
narrative_ontology:constraint_victim(north_korean_sanctions_regime, north_korean_economy).
narrative_ontology:constraint_victim(north_korean_sanctions_regime, humanitarian_access_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NORTH KOREAN CIVILIAN POPULATION (SNARE) — Trapped by geographic containment and regime control. Bears extraction through economic collapse, medical supply restrictions, food scarcity, and currency instability. No exit mechanism available. Maximum experienced extraction with minimal coordination benefit. Suppression is total: regime prevents emigration, confiscates external aid, controls information flow. The population cannot organize resistance or exit.
constraint_indexing:constraint_classification(north_korean_sanctions_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NORTH KOREAN ECONOMY (SNARE) — Structurally trapped by sanctions enforcement. Cannot access international markets, foreign currency, technology, or investment. The constraint extracts through degradation: human capital flight, technological stagnation, infrastructure collapse. No coordination function present — the sanctions regime provides no alternative mechanism for economic coordination, only prohibition.
constraint_indexing:constraint_classification(north_korean_sanctions_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: HUMANITARIAN ACCESS ORGANIZATIONS (TANGLED ROPE) — Constrained by sanctions enforcement, licensing requirements, and regime restrictions. Experience genuine coordination function (delivering medical supplies, food aid) alongside extraction (limited access, legal liability, operational costs). They benefit from coordination mechanisms but face asymmetric constraints — sanctions impose monitoring burdens not faced by domestic aid organizations in other regions.
constraint_indexing:constraint_classification(north_korean_sanctions_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: UN SECURITY COUNCIL PERMANENT MEMBERS (ROPE) — Experience the sanctions regime as pure coordination mechanism. The constraint solves the collective action problem of deterring nuclear proliferation and coercive state behavior without requiring direct military intervention. Permanent members (especially US, China, Russia) have exit options through diplomatic channels and arbitrage opportunities through secondary markets. Net beneficiary perspective — the regime serves their security interests.
constraint_indexing:constraint_classification(north_korean_sanctions_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SANCTIONS ENFORCEMENT APPARATUS (ROPE) — Treasury departments, customs agencies, international financial institutions. Experience the regime as pure coordination: they are solving the problem of enforcing multilateral economic constraints. Extraction runs toward them (budgets, institutional power, analytic capacity), not away from them. They have arbitrage options: can adjust enforcement intensity, target selection, or negotiated exemptions. Net beneficiary with high organizational power.
constraint_indexing:constraint_classification(north_korean_sanctions_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL TRADE PARTNERS (TANGLED ROPE) — China and Russia face constraints from the sanctions regime: they have incentives to enforce multilateral rules (coordination function) but also economic interests in trade with North Korea (extraction). Their exit options are constrained but mobile — they can adjust enforcement intensity, exploit secondary markets, or renegotiate through diplomatic channels. Experience both benefits (maintaining rules-based order) and costs (forgoing trade revenue, managing US pressure).
constraint_indexing:constraint_classification(north_korean_sanctions_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: DISSIDENT AND REFUGEE NETWORKS (SNARE) — Organized actors with strategic capacity but constrained exit options. The sanctions regime both traps their countrymen and limits resources available for support networks. They experience extraction through reduced capacity to assist defectors, support underground resistance, or fund humanitarian operations. Unlike the general population, they have some exit options (legal status in asylum countries, fundraising capacity) but face maximum suppression from the regime.
constraint_indexing:constraint_classification(north_korean_sanctions_regime, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: INTERNATIONAL SANCTIONS FRAMEWORK (PITON) — The multilateral sanctions regime persists through institutional inertia despite degraded functionality. Theater is moderate (0.58): public messaging emphasizes coercive deterrence, but effectiveness is constrained by enforcement gaps, secondary market leakage, and regime adaptation. The framework continues because exit would require renegotiating security architecture, not because it functions optimally. The regime is maintained through diplomatic coordination and compliance theater, not through achieved coercive objectives.
constraint_indexing:constraint_classification(north_korean_sanctions_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — Risk of naturalizing contingent geopolitical arrangements as immutable constraints. The argument that 'proliferation must be stopped by any means' naturalizes what is actually a policy choice. From a civilizational analytical perspective, the constraint appears unchangeable — nuclear proliferation is treated as inherently intolerable. However, the structural data contradicts this: the regime is actively maintained through enforcement choices, not through physical/logical necessity. The mountain classification is a false summit, revealing how security framings naturalize policy.
constraint_indexing:constraint_classification(north_korean_sanctions_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_korean_sanctions_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(north_korean_sanctions_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_korean_sanctions_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(north_korean_sanctions_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(north_korean_sanctions_regime, TR),
    TR >= 0.70.

:- end_tests(north_korean_sanctions_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The regime extracts severely from the civilian population through economic isolation, supply chain disruption, currency collapse, and regime control of external aid. Extraction is asymmetric — the beneficiary institutions (security establishment, enforcement apparatus) are insulated from costs, while the targeted population bears maximum burden. The value reflects both direct extraction (sanctions-induced scarcity) and indirect extraction (regime's intensified internal control in response to external pressure). The upward trajectory (0.35→0.71) indicates that extraction mechanisms have intensified as the regime has adapted to contain sanctions impact through tighter internal suppression. Suppression (0.72): Very high. The regime maintains near-total suppression through border control, information monopoly, internal security apparatus, and elimination of exit options. Sanctions themselves add suppression mechanisms: external aid reduction, currency restrictions, and travel prohibitions. This suppression is not merely external (border closure) but internalized through regime propaganda and loyalty mechanisms. Theater ratio (0.58): Moderate-high. The public messaging of the sanctions regime emphasizes deterrence effectiveness and moral clarity (preventing weapons proliferation). However, enforcement has significant gaps: secondary market leakage through China and Russia, cryptocurrency adaptation by regime, smuggling networks, and diplomatic exemptions reduce actual enforcement coverage. The theater increases over time as enforcement gaps widen but public messaging remains unchanged. Enforcement authorities maintain the theater through compliance theater (statistics on prosecutions, sanctions designations) rather than actual constraint on regime weapons development. Beneficiaries (security_establishment, sanctions_enforcement_apparatus): The institutions that benefit are the US security establishment (maintains deterrence without military cost) and the sanctions enforcement infrastructure (Treasury, State Department, international financial institutions). Neither group experiences extraction — the regime enables their institutional power and budget allocation. Victims (north_korean_civilian_population, north_korean_economy, humanitarian_access_workers): The civilian population bears the maximum extraction through economic collapse and regime intensification. The broader economy is structurally trapped. Humanitarian workers experience mixed extraction (access delays, legal liability) alongside genuine coordination benefits (enabling aid delivery).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across the agent spectrum. The powerless victim sees Snare; the institutional beneficiary sees Rope; the organized victims see Snare with organized capacity; the regional powers see Tangled Rope; the analytical observer risks false-summit Mountain. The gap is not measurement ambiguity but genuinely different structural experiences. The civilian population has zero agency; the security establishment has full agency; humanitarian organizations have constrained agency. These are not different readings of the same structure but genuinely different structural realities as experienced from different positions. The constraint is not singular — it is a presheaf of different constraints, one for each structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the extraction flow. The civilian population (victim + trapped) has d approaching 1.0 — they are the full target of extraction with zero exit capacity. This produces maximum f(d) (sigmoid value ~1.42), driving high χ (effective extraction as experienced). The security establishment (beneficiary + arbitrage) has d approaching 0.0 — extraction flows toward them, not away. This produces negative f(d) (~-0.12), driving negative χ (they experience the constraint as advantageous). Humanitarian organizations (mixed + constrained) have d in the 0.55–0.70 range: they bear costs (licensing delays, legal liability, access restrictions) but benefit from coordination function (enabling aid delivery). This intermediate d produces moderate χ (~0.55–0.75) depending on exit_options and power level. Regional powers (beneficiary + mobile, but victims of enforcement asymmetry) have d in the 0.40–0.50 range: they benefit from rule structure but face costs from enforcement intensity. The powerful + mobile combination produces moderate χ despite beneficiary status, because their exit options are genuinely available. The analytical observer at civilizational scope with analytical exit options would have d around 0.73, producing χ approaching 1.15. However, the false-summit detector identifies this as naturalization of contingent policy, not immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH MISLABELING RISK: The constraint is classified as Snare at the primary victim perspective (powerless/trapped/global), meeting all gates: ε=0.68 ≥ 0.46, suppression=0.72 ≥ 0.60, χ ≥ 0.66 at that perspective. However, the security establishment and enforcement apparatus classify the same structural mechanism as pure Rope (coordination without extraction, net beneficiary). The mandatrophy here is the risk of misclassifying the beneficiary's perspective as justification for the classification. The constraint is not pure coordination — it is extraction with coordination function for some agents and pure extraction for others. The resolution is to maintain both perspectives in the analysis and recognize that the security establishment's 'Rope' experience coexists with the civilian population's 'Snare' experience. Neither is more true — both are structural realities. The mandatrophy is resolved by acknowledging asymmetry: the regime serves genuine deterrence functions AND extracts severely from the powerless. Both are true simultaneously. Mislabeling would occur if the constraint were called 'Rope' (correct for beneficiaries, wrong for victims) or if the beneficiary perspective were used to deny the extraction experienced by victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_deterrence_efficacy,
    'Does the sanctions regime''s extraction from the civilian population measurably increase deterrence against nuclear weapons development, or does extraction occur independently of deterrence outcome?',
    'Counterfactual analysis of deterrence scenarios; comparison of North Korean weapons development trajectory under sanctions vs hypothetical soft-power engagement models; assessment of whether economic damage correlates with weapons program slowdown or acceleration',
    'If deterrence efficacy is high: classification trends toward Tangled Rope (coordination + extraction justified by security outcome). If efficacy is low or negative: classification is pure Snare (extraction without coordination benefit). Current evidence suggests low efficacy — weapons development has accelerated despite sanctions intensity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_deterrence_efficacy, empirical, 'Whether sanctions extraction produces measurable deterrence effect').

omega_variable(
    civilian_harm_mechanism_intentionality,
    'Is civilian suffering a deliberate extraction mechanism within the sanctions design, or a regrettable side effect of security-focused policy?',
    'Policy document analysis: do architects explicitly model civilian harm as a coercive tool? Comparison with humanitarian exemptions offered in sanctions design. Assessment of regime response patterns: does targeting civilians increase pressure on regime, or does it consolidate regime control?',
    'If deliberate: confirms Snare classification (extraction is the mechanism). If unintended: reframes as Tangled Rope with collateral damage (coordination goal with extraction side effect). Current evidence suggests ambiguity — humanitarian exemptions are limited, and civilian harm is understood but not primary objective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_harm_mechanism_intentionality, conceptual, 'Whether civilian harm is deliberate mechanism or policy side effect').

omega_variable(
    regime_adaptation_and_constraint_decay,
    'Does the sanctions regime''s extractive capacity decay over time as the regime develops alternative economic pathways (smuggling, cryptocurrency, secondary markets), or does suppression remain constant?',
    'Longitudinal measurement of sanctions enforcement effectiveness: tracking secondary market leakage, black market currency valuations, regime innovation in circumvention. Comparison of extractiveness trajectory in measurements with regime adaptation timeline.',
    'If decay is rapid: theater_ratio rises (enforcement becomes increasingly performative), classification trends toward Piton (institutional inertia). If suppression remains high: classification remains Snare. Measurements show both dynamics: enforcement effectiveness decays, but suppression mechanisms intensify (regime tightens internal controls).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_adaptation_and_constraint_decay, empirical, 'Whether sanctions extraction capacity decays with regime adaptation').

omega_variable(
    humanitarian_access_coordination_function,
    'Do sanctions licensing mechanisms for humanitarian aid represent genuine coordination (solving the problem of delivering aid while managing security concerns) or pure extraction theater (security screening that provides no real security benefit)?',
    'Analysis of smuggling patterns: do licensed humanitarian channels leak to regime security apparatus? Comparison of aid delivery efficiency in North Korea vs other sanctioned regimes with different licensing structures. Assessment of whether security vetting prevents regime diversion or merely adds bureaucratic cost.',
    'If genuine coordination: Tangled Rope classification for humanitarian organizations is correct. If pure theater: humanitarian access is Snare (licensing apparatus extracts through delay and administrative burden without coordination function). Current evidence suggests mixed: some genuine security concerns, but also significant administrative extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_access_coordination_function, empirical, 'Whether humanitarian licensing represents coordination or extraction theater').

omega_variable(
    secondary_market_enforcement_asymmetry,
    'Do enforcement gaps in secondary markets and third-party sanctions evasion represent failure of the global sanctions regime or intentional tolerance of extraction through complicit enforcement partners?',
    'Analysis of enforcement patterns: which secondary market channels are actively prosecuted vs tolerated? Examination of bilateral relationships between enforcement authorities and trading partners. Assessment of whether enforcement intensity correlates with strategic alliance rather than sanctions coherence.',
    'If systemic failure: classification approaches Piton (theater-dominated, enforcement compromised). If intentional tolerance: reframes extraction mechanism — enforcement establishment extracts through selective prosecution and partnership rents. Current evidence suggests both: genuine enforcement limitations plus strategic tolerance of allies'' sanctions evasion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_market_enforcement_asymmetry, empirical, 'Whether secondary market leakage is enforcement failure or intentional tolerance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(north_korean_sanctions_regime, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nksr_tr_t0, north_korean_sanctions_regime, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nksr_tr_t5, north_korean_sanctions_regime, theater_ratio, 5, 0.48).
narrative_ontology:measurement(nksr_tr_t10, north_korean_sanctions_regime, theater_ratio, 10, 0.55).
narrative_ontology:measurement(nksr_tr_t15, north_korean_sanctions_regime, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(nksr_be_t0, north_korean_sanctions_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nksr_be_t5, north_korean_sanctions_regime, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(nksr_be_t10, north_korean_sanctions_regime, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(nksr_be_t15, north_korean_sanctions_regime, base_extractiveness, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(north_korean_sanctions_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(north_korean_sanctions_regime, nuclear_nonproliferation_treaty).
narrative_ontology:affects_constraint(north_korean_sanctions_regime, humanitarian_access_regimes).
narrative_ontology:affects_constraint(north_korean_sanctions_regime, secondary_market_circumvention_networks).
narrative_ontology:affects_constraint(north_korean_sanctions_regime, east_asia_security_architecture).

% DUAL FORMULATION NOTE:
% The sanctions regime is downstream of nuclear nonproliferation commitments and affects multiple secondary constraints: humanitarian access mechanisms experience the licensing bottleneck as extraction, regional security architecture maintains incentive misalignment between deterrence goals and collateral costs, and underground circumvention networks adapt to enforcement gaps. Network links represent structural dependencies and causal influence, not value judgments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(north_korean_sanctions_regime, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
