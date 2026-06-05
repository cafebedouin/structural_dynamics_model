% ============================================================================
% CONSTRAINT STORY: shadow_fleet_sanctions_evasion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shadow_fleet_sanctions_evasion, []).

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
 *   constraint_id: shadow_fleet_sanctions_evasion
 *   human_readable: Sanctions Evasion via Shadow Fleet
 *   domain: geopolitical/economic_coercion
 *
 * SUMMARY:
 *   The shadow fleet represents a structural tension between economic
 *   coercion (sanctions) and the incentives to circumvent coercion. Shadow
 *   fleets — networks of aging, poorly-identified tankers that transport
 *   sanctioned oil through intermediate ports and coordinated ship-to-ship
 *   transfers — enable Iran, Venezuela, and other sanctioned exporters to
 *   maintain global oil sales despite international prohibitions. The
 *   constraint exhibits the full spectrum of DR types from different
 *   institutional positions. The sanctioning coalition (US, EU allies)
 *   experiences the shadow fleet as a snare: their enforcement credibility
 *   erodes as evasion scales, and they face a choice between escalating
 *   enforcement (rising costs) or accepting evasion (credibility loss).
 *   Sanctioned exporters experience a tangled rope: shadow fleets enable
 *   revenue access but impose legal, operational, and financial risks. Shadow
 *   fleet operators experience pure coordination (rope): they solve the
 *   collective action problem of moving oil to buyers. Compliant exporters
 *   experience extraction (tangled rope): lower circumvention prices undercut
 *   legitimate trade. Maritime regulators experience degraded ritual (piton):
 *   flag state accountability and port state control persist through
 *   institutional inertia despite obvious evasion pathways. Emerging
 *   satellite verification coalitions experience a temporary coordination
 *   problem with a sunset (scaffold): as detection technology matures, the
 *   opacity advantage of shadow fleets declines. The analytical observer
 *   risks seeing an immutable economic law (mountain) — that economic
 *   coercion always spawns evasion — but the structural data reveals this as
 *   a false summit: the shadow fleet extraction is contingent on specific
 *   institutional failures (flag state accountability, maritime opacity,
 *   payment system vulnerabilities), not inherent to economic statecraft
 *   itself.
 *
 * KEY AGENTS:
 *   - Sanctioning Coalition (US, EU allies): Primary enforcer (institutional/arbitrage) — seeks to impose costs on target states; experiences extraction through credibility degradation
 *   - Sanctioned State Oil Exporters (Iran, Venezuela): Primary beneficiary and victim (powerful/mobile, or institutional/constrained depending on perspective) — benefits from export revenue access; bears legal, financial, and operational risks
 *   - Shadow Fleet Operators and Middleman Trading Companies: Primary beneficiary (institutional/arbitrage) — profits from coordination of circumvention; experiences low extraction
 *   - Compliant Oil Exporters and Legitimate Maritime Industry: Secondary victim (organized/constrained) — benefits from legitimate trade standards; harmed by evasion-driven price distortion
 *   - International Maritime Regulatory Bodies (IMO, flag states, port state control): Institutional custodian (institutional/arbitrage) — maintains regulatory framework that persists through inertia despite evasion pathways
 *   - Satellite Verification and Enforcement Coalition: Emerging organized response (organized/constrained) — builds technological enforcement capacity with implicit sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shadow_fleet_sanctions_evasion, 0.58).
domain_priors:suppression_score(shadow_fleet_sanctions_evasion, 0.68).
domain_priors:theater_ratio(shadow_fleet_sanctions_evasion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, extractiveness, 0.58).
narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(shadow_fleet_sanctions_evasion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shadow_fleet_sanctions_evasion, tangled_rope).
narrative_ontology:human_readable(shadow_fleet_sanctions_evasion, "Sanctions Evasion via Shadow Fleet").
narrative_ontology:topic_domain(shadow_fleet_sanctions_evasion, "geopolitical/economic_coercion").

domain_priors:requires_active_enforcement(shadow_fleet_sanctions_evasion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shadow_fleet_sanctions_evasion, sanctioned_state_oil_exporters).
narrative_ontology:constraint_beneficiary(shadow_fleet_sanctions_evasion, shadow_fleet_operators).
narrative_ontology:constraint_beneficiary(shadow_fleet_sanctions_evasion, middleman_trading_companies).
narrative_ontology:constraint_victim(shadow_fleet_sanctions_evasion, sanctioning_coalition_enforcement_credibility).
narrative_ontology:constraint_victim(shadow_fleet_sanctions_evasion, global_oil_price_stability).
narrative_ontology:constraint_victim(shadow_fleet_sanctions_evasion, compliant_exporters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANCTIONING COALITION ENFORCEMENT CREDIBILITY (SNARE) — The enforcement regime cannot exit the shadow fleet problem without abandoning sanctions themselves. As evasion methods improve and vessel identification becomes harder, the cost of maintaining credible enforcement rises exponentially. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Full extraction: the constraint degrades the sanctioning power's bargaining position.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SHADOW FLEET OPERATORS AND TRADING COMPANIES (ROPE) — Pure coordination mechanism: the constraint solves the collective action problem of moving sanctioned oil to market. Operators benefit from coordination (shared vessel registries, flag-hopping protocols, AIS spoofing techniques). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SANCTIONED STATE OIL EXPORTERS (TANGLED ROPE) — Both benefits (access to oil export markets despite sanctions) and costs (payment collection risks, vessel seizure exposure, diplomatic escalation). Can theoretically exit by accepting sanctions compliance, but export revenue dependency makes this mobile exit costly. d≈0.58, f(d)≈0.68, σ=1.2 → χ≈0.47. Mixed coordination-extraction: the constraint enables revenue while imposing operational costs and asymmetric legal risk.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPLIANT OIL EXPORTERS AND MARITIME INDUSTRY (TANGLED ROPE) — Organized agents benefit from legitimate trade and shipping standards, but shadow fleet distorts pricing (cheaper circumvention reduces market prices for legal exports) and creates reputational/operational friction. Vessel identification becomes harder; industry standards get subordinated to evasion logic. d≈0.72, f(d)≈1.10, σ=1.2 → χ≈0.77. Significant extraction: coordinated shipping protocols are undermined; compliant exporters subsidize sanction-evasion pricing.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL MARITIME REGULATORY BODIES (PITON) — IMO regulations, flag state procedures, and vessel registration standards persist through institutional inertia despite obvious evasion pathways. The regulatory framework (flag-hopping, reflagging, AIS spoofing detection) has become substantially performative: enforcement requires tracking thousands of vessels, coordinated intelligence, and political will across non-aligned states. theater_ratio=0.55 indicates moderate performativity. The system maintains ritual compliance (port state control inspections, documentation reviews) without preventing the constraint's core function.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING SATELLITE VERIFICATION COALITION (SCAFFOLD) — Organized tracking initiatives (satellite AIS monitoring, ship-tracking analytics, multinational enforcement task forces) represent temporary, coordinated responses with implicit sunset logic. As technology improves (synthetic aperture radar, real-time vessel tracking, AI identification), shadow fleet opacity becomes harder to maintain. The scaffold is the emerging enforcement capacity; it has a sunset because either (a) sanctions are eventually lifted, (b) evasion becomes sufficiently expensive to deter, or (c) tracking technology becomes mature enough to make evasion too costly. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.32. Moderate extraction; coalition has agency and sees a path forward (technological enforcement).
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL ECONOMIC LIMIT VIEW (MOUNTAIN) — From a civilizational perspective, sanctions evasion represents a structural property of enforcing economic coercion on trading partners: any embargo creates incentives to circumvent, and the gap between prohibition and enforcement is an inherent feature of economic statecraft, not a specific policy failure. This view risks naturalizing what are actually contingent institutional arrangements (flag state accountability, maritime insurance requirements, port state control authority). However, the structural data (ε=0.58, suppression=0.68, theater=0.55) contradicts the mountain classification — the engine will compute this as a false summit.
constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shadow_fleet_sanctions_evasion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shadow_fleet_sanctions_evasion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shadow_fleet_sanctions_evasion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shadow_fleet_sanctions_evasion, TR),
    TR >= 0.70.

:- end_tests(shadow_fleet_sanctions_evasion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Shadow fleet evasion imposes extraction on the sanctioning coalition (credibility loss, enforcement costs) and on compliant exporters (pricing pressure). Sanctioned exporters capture benefits (oil export revenue) but bear significant operational costs. The extractiveness has increased from 0.32 to 0.58 over the 10-year interval because: (a) fleet size scaled with sanctions scope, (b) detection techniques improved, forcing more sophisticated evasion (higher operational cost burden), (c) secondary effects on compliant traders intensified. Suppression (0.68): Moderately high. Barriers to enforcement include: vessel opacity (reflagging, AIS spoofing, beneficial ownership concealment), geographic dispersal (operations across multiple legal jurisdictions), flag state non-cooperation, and coordination among evasion networks. But suppression is not absolute — some vessels are seized, some transactions detected, some networks disrupted. Tracking and detection are possible but costly. Theater ratio (0.55): Moderate. Maritime regulatory processes (flag state inspections, port state control detention, INTERPOL notices) maintain a significant performative component: vessels are reflagged before detention, documentation is forged or concealed, insurance is provided through shell companies. But the theater is not dominant — actual enforcement actions occur, vessels are seized, payment channels are disrupted. The theater ratio has increased from 0.38 to 0.55 because evasion sophistication has outpaced public enforcement visibility.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range across the DR classification landscape. The sanctioning coalition sees a snare (they are trapped in enforcement escalation). Sanctioned exporters see tangled rope (benefits and costs intermixed). Shadow fleet operators see rope (pure coordination). Compliant exporters see tangled rope (mixed benefits and extraction). Maritime regulators see piton (their regulatory framework is performative). Emerging verification coalitions see scaffold (technological solutions with sunset logic). The analytical observer risks seeing a mountain (economic evasion as an inherent limit to coercion), but the structural data contradicts this. The perspectival gaps arise because different institutional positions experience different combinations of coordination and extraction benefits. The sanctioning coalition's snare perception stems from their trapped exit (they cannot enforce without credibility costs). The compliant exporters' tangled rope perception comes from being caught between price pressure (extraction) and trade benefits (coordination). The shadow fleet operators' rope perception reflects genuine coordination function with minimal coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Sanctioning coalition: Powerful + trapped (constrained enforcement capacity) → d≈0.92, f(d)≈1.38. Snare classification follows from high extraction relative to their position. Sanctioned exporters: Institutional + mobile (can exit through sanctions compliance) but costly → d≈0.58, f(d)≈0.68. Tangled rope classification follows from mixed benefits (revenue) and costs (legal/financial risk). Shadow fleet operators: Institutional + arbitrage (can exit if sanctions lifted or evasion becomes unprofitable) → d≈0.08, f(d)≈-0.10. Rope classification follows from beneficiary status with low extraction. Compliant exporters: Organized + constrained (cannot exit legitimate trade without accepting evasion) → d≈0.72, f(d)≈1.10. Tangled rope classification follows from both extraction (price pressure) and coordination benefits (trade access). Maritime regulators: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification follows from theater gate (0.55 > 0.00), not from high chi. Verification coalition: Organized + constrained → d≈0.45, f(d)≈0.48. Scaffold classification follows from organized agency + sunset logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the tangled rope classification (claimed_type) is correct because it contains BOTH coordination and asymmetric extraction. Coordination function: Shadow fleets enable oil flow to buyers and exporters to maintain revenue — this is genuine coordination that solves a collective action problem (buyers want cheap oil; exporters want market access). Asymmetric extraction: The coordination is asymmetrically enforced — sanctioned exporters and compliant traders bear disproportionate legal, financial, and reputational costs while shadow fleet operators and payment intermediaries capture margin without exposure. Active enforcement: The constraint requires active institutional enforcement (flag state authority, port state control, financial sanctions, INTERPOL coordination) to maintain the evasion suppression (0.68). Without enforcement, the constraint would collapse into pure rope (buyers and sellers coordinating freely). The snare perspective (sanctioning coalition) is perspectival, not the canonical classification. The mountain perspective (analytical observer naturalizing evasion as immutable) is a false summit caught by the engine — the structural data (ε=0.58, suppression=0.68) does not support natural law classification. The mandatrophy is resolved: this is a tangled rope from the canonical analytical perspective, with legitimate snare and rope perspectives from specific institutional positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_technology_race,
    'Can satellite/AI identification technology mature fast enough to outpace shadow fleet evasion techniques, or will evasion adapt indefinitely?',
    'Empirical tracking: detection rates over time, cost-per-detection, vessel seizure rates relative to fleet size, technological refresh cycles vs innovation cycles in evasion',
    'If technology wins: scaffold perspective confirmed, enforcement capacity increases, χ for sanctioners declines. If evasion adapts: mountain perspective gains credibility (enforcement limit is structural), entrenched snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_technology_race, empirical, 'Whether detection technology can outpace evasion innovation').

omega_variable(
    sanctions_regime_legitimacy_threshold,
    'What level of circumvention erodes the sanctioning coalition''s credibility sufficiently to trigger compliance defection or sanctions relaxation?',
    'Historical analysis of sanctions regime persistence vs evasion scale; coalition member cost-benefit; third-country defection timing',
    'If threshold is low: snare classification strengthens (sanctioners trapped by own credibility). If high: tangled rope persists longer. If threshold triggers coaltion collapse: scaffold sunset accelerates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctions_regime_legitimacy_threshold, empirical, 'Credibility threshold for sanctioning coalition persistence').

omega_variable(
    flag_state_accountability_fiction,
    'Is the flag state system a genuine coordination mechanism or purely performative cover for evasion?',
    'Analysis of flag state enforcement action rates, insurance company compliance, port state control detention statistics for shadow vessels, transparency of beneficial ownership',
    'If performative: piton classification gains strength (maritime system is theatrical). If functional: rope classification more plausible (genuine coordination with evasion as byproduct). Affects whether the constraint is primarily extraction or primarily coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flag_state_accountability_fiction, empirical, 'Whether flag state accountability is functional or performative').

omega_variable(
    alternative_payment_systems_stability,
    'How long can non-SWIFT payment channels (barter, bilateral settlement, cryptocurrency) maintain sufficient stability and volume to support sanctioned oil trade without creating their own extractive bottleneck?',
    'Tracking of non-SWIFT transaction volumes, payment default rates, currency volatility, operational costs for alternative settlement systems',
    'If stable: sanctioned exporters have genuinely mobile exit (upgrade from constrained to mobile), tangled rope strengthens, χ declines. If unstable: extraction intensifies as payment friction increases, snare elements dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_payment_systems_stability, empirical, 'Viability of alternative payment channels for circumventing financial sanctions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shadow_fleet_sanctions_evasion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shf_tr_t0, shadow_fleet_sanctions_evasion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(shf_tr_t5, shadow_fleet_sanctions_evasion, theater_ratio, 5, 0.47).
narrative_ontology:measurement(shf_tr_t10, shadow_fleet_sanctions_evasion, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(shf_be_t0, shadow_fleet_sanctions_evasion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(shf_be_t5, shadow_fleet_sanctions_evasion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(shf_be_t10, shadow_fleet_sanctions_evasion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shadow_fleet_sanctions_evasion, resource_allocation).
narrative_ontology:affects_constraint(shadow_fleet_sanctions_evasion, international_sanctions_enforcement).
narrative_ontology:affects_constraint(shadow_fleet_sanctions_evasion, petrostate_revenue_vulnerability).
narrative_ontology:affects_constraint(shadow_fleet_sanctions_evasion, maritime_flag_state_accountability).

% DUAL FORMULATION NOTE:
% Shadow fleet sanctions evasion decomposes into distinct constraints: (1) the financial payment circumvention problem (non-SWIFT channels, ε≈0.45), (2) the maritime vessel opacity problem (AIS spoofing, reflagging, ε≈0.52), (3) the enforcement coordination problem among sanctioning coalition members (ε≈0.38). This story models the integrated constraint (ε=0.58) that encompasses all three. Upstream constraints on sanctions regime design (ε≈0.35) feed into this. Downstream constraints on global oil pricing and OPEC coordination are affected by shadow fleet volume.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shadow_fleet_sanctions_evasion, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
