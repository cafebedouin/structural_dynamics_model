% ============================================================================
% CONSTRAINT STORY: us_china_hegemonic_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_hegemonic_competition, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_china_hegemonic_competition
 *   human_readable: US-China Hegemonic Competition
 *   domain: geopolitical/economic/strategic
 *
 * SUMMARY:
 *   US-China hegemonic competition represents a structural tension between
 *   two major powers competing for technological, economic, and military
 *   dominance in a multipolar world order. The constraint exhibits
 *   tangled_rope characteristics at the system level: both nations benefit
 *   from the coordination mechanisms of global trade, finance, and technology
 *   standards (genuine coordination function), yet each extracts
 *   asymmetrically through tariffs, technology controls, supply chain
 *   weaponization, and strategic decoupling initiatives. The competition
 *   generates collateral extraction across three victim categories:
 *   developing nations caught between competing power blocs with limited exit
 *   options; civilian populations in proxy conflict zones (Taiwan, South
 *   China Sea, Myanmar); and the global innovation ecosystem, which
 *   experiences fragmentation, brain drain, and access restrictions. The
 *   constraint's theater ratio (0.62) reflects that much competitive behavior
 *   is performative: strategic summit declarations, threat inflation,
 *   posturing that enacts hegemonic status more than operational military or
 *   economic capacity. The measurement trajectory (extractiveness rising from
 *   0.35 to 0.58 over 30 years, theater rising from 0.45 to 0.62) shows
 *   progressive hardening and theatricalization — as the competition
 *   intensifies, both nations increase visible signal cost and rhetorical
 *   escalation, increasing theater ratio faster than actual extraction.
 *
 * KEY AGENTS:
 *   - United States: Primary beneficiary (institutional/arbitrage) — controls global currency (dollar hegemony), technology standards (chip design, internet protocols), military alliances (NATO, bilateral security partnerships); arbitrage into defense spending and tech sector advantage
 *   - China: Primary beneficiary (institutional/arbitrage) — state-directed strategic industries, protected domestic market, Belt and Road infrastructure, rare earth monopoly; arbitrage into state-sponsored innovation and manufacturing scale
 *   - Developing Nations: Primary victims (powerless/trapped) — forced to choose between US-aligned and China-aligned frameworks (security, infrastructure, finance, technology standards) with severe consequences for either choice; no genuine exit option
 *   - Taiwan and South China Sea States: Secondary victims (powerless to organized/constrained) — geographic position makes them proxy conflict zones; face military pressure, economic coercion, and forced strategic alignment choices
 *   - Global Supply Chains (Semiconductors, Rare Earths, Advanced Manufacturing): Structured victim (organized/constrained) — genuine coordination functions but experiencing extraction through decoupling pressure, technology controls, restricted access
 *   - Research and Innovation Ecosystems: Mixed (moderate/mobile) — experience coordination benefits (collaborative standards, shared methods) alongside extraction (visa restrictions, IP controls, talent redistribution)
 *   - Multilateral Institutions: Organized observers (organized/constrained) — attempt norm-setting and conflict resolution but lack enforcement power against hegemon veto
 *   - Cold War Institutional Framework: Institutional artifact (institutional/arbitrage) — NATO, bilateral alliances, deterrence frameworks persist through inertia; maintain US advantage through alliance structure and sunk legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_hegemonic_competition, 0.58).
domain_priors:suppression_score(us_china_hegemonic_competition, 0.68).
domain_priors:theater_ratio(us_china_hegemonic_competition, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_hegemonic_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_hegemonic_competition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_china_hegemonic_competition, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_hegemonic_competition, tangled_rope).
narrative_ontology:human_readable(us_china_hegemonic_competition, "US-China Hegemonic Competition").
narrative_ontology:topic_domain(us_china_hegemonic_competition, "geopolitical/economic/strategic").

domain_priors:requires_active_enforcement(us_china_hegemonic_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_hegemonic_competition, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_china_hegemonic_competition, chinese_strategic_industries).
narrative_ontology:constraint_beneficiary(us_china_hegemonic_competition, defense_contractors_both_nations).
narrative_ontology:constraint_victim(us_china_hegemonic_competition, developing_nations_caught_between).
narrative_ontology:constraint_victim(us_china_hegemonic_competition, global_supply_chain_stability).
narrative_ontology:constraint_victim(us_china_hegemonic_competition, civilian_populations_in_proxy_zones).
narrative_ontology:constraint_victim(us_china_hegemonic_competition, technology_innovation_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING NATIONS (SNARE) — Trapped between competing hegemonic pressures with no genuine exit option. Forced to choose alignment, infrastructure partnerships, and technology standards with severe economic and political consequences. Zero alternatives; complete suppression of independent policy space. Bear full extraction cost while benefits flow to hegemons.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROXY CONFLICT CIVILIAN POPULATIONS (SNARE) — Constrained by geography and circumstance to bear military, economic, and humanitarian costs of hegemonic competition. Myanmar, Taiwan, South China Sea territorial waters, semiconductor supply zones. Cannot exit without uprooting; high suppression via military presence and economic dependency.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUPPLY CHAIN ACTORS (TANGLED ROPE) — Coordinated interdependency (genuine coordination around production, logistics, design) overlaid with asymmetric extraction. Taiwan semiconductor fabs serve both markets; rare earth supply concentrated in China; advanced chip design concentrated in US. Actors benefit from the coordination but bear constant extraction pressure via threatened decoupling, sanctions, and technology controls.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Net beneficiary with significant arbitrage options. Hegemonic competition drives defense spending (>$800B annually), technology contracts, and strategic market access. Experiences the constraint as coordination: defining 'China threat' narratives, prioritizing industrial capacity, organizing allied relationships. Benefits outweigh costs; can arbitrage into civilian tech spinoffs.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CHINESE STRATEGIC INDUSTRIES (ROPE) — Net beneficiary with arbitrage options. Hegemonic competition justifies state support, technology coordination, and protected domestic markets. Experiences constraint as coordination framework: state-directed capitalism, industrial policy, strategic sector integration. Benefits from technology transfer restrictions (protect IP), from US decoupling (reduce competition), and from Belt and Road as strategic infrastructure.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL RESEARCH ECOSYSTEMS (TANGLED ROPE) — Mobile but bearing significant extraction. Universities, labs, tech companies experience both coordination benefits (collaborative standards, shared methods) and extraction (visa restrictions, technology export controls, talent flight, research access fragmentation). Can physically relocate but identity and institutional relationships are friction-loaded. Moderate extraction with real exit options but high switching costs.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: MULTILATERAL INSTITUTIONS (SCAFFOLD) — Organized but constrained by hegemonic veto power. WTO, IPCC, IAEA, arms control bodies attempt to coordinate global behavior. See hegemonic competition as temporary (rational actors will defect to cooperation when extraction costs exceed benefits). Suppression is high but not total — institutions have narrative and coordination functions that persist despite hegemonic pressure. Theater ratio high but not maximal because some institutional mechanisms (transparency, dispute resolution) remain functional.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: COLD WAR INSTITUTIONAL FRAMEWORK (PITON) — Theater-driven persistence. NATO, bilateral security alliances, nuclear deterrence frameworks, arms control regimes. These structures maintained through inertia and ritual performance rather than current functional necessity. Theater ratio 0.70+: regular summits, threat declarations, posturing, strategic reviews that enact the constraint more than operationalize it. Arbitrage options exist (decoupling from NATO, disarmament) but institutions persist through sunk cost and identity fusion.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / STRUCTURAL REALISM (MOUNTAIN) — From a civilizational timescale and universal scope, hegemonic competition may appear as a natural law of international relations: the structural anarchy of the state system makes hegemonic competition inevitable (Waltz, Mearsheimer). This perspective sees the constraint as immutable — no exit because the underlying anarchic structure cannot be reformed. However, the base properties data contradicts the mountain classification: extractiveness (0.58), suppression (0.68), theater ratio (0.62), and active enforcement requirements all indicate a contingent institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(us_china_hegemonic_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_hegemonic_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_hegemonic_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_hegemonic_competition, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_hegemonic_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_hegemonic_competition, TR),
    TR >= 0.70.

:- end_tests(us_china_hegemonic_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The US-China competition generates significant asymmetric benefits: Chinese strategic industries receive state protection and market coordination worth $200+ billion annually; US military-industrial complex receives defense contracting ($800B+ annually) and technology sector advantage through standards control. This extraction flows from: (1) tariff asymmetries, (2) technology export controls and IP restrictions, (3) supply chain weaponization, (4) currency/payment system monopoly (US dollar for US, digital currency trials for China), and (5) alliance fee extraction (US allies pay NATO/security costs; China extracts through BRI debt diplomacy). The value of 0.58 reflects that the extraction is real and growing but not at maximum intensity — significant mutual trade continues ($600B+), technology collaboration persists despite tensions, and neither nation has successfully decoupled despite rhetoric. Suppression (0.68): High. Barriers to exit or defection are structural: (a) developing nations cannot exit without economic and security costs (trade retaliation, military pressure, lost investment), (b) supply chain actors are locked into interdependency despite decoupling rhetoric, (c) technology standards create switching costs, (d) alliance commitments create political costs to defection, (e) military presence in contested zones (South China Sea bases, US carrier deployment) creates physical suppression. The value reflects that suppression is real and sustained but not total — some actors do negotiate exit (India's strategic autonomy, Vietnam's balancing act), some supply chains do relocate, and some nations do attempt non-aligned positioning. Theater ratio (0.62): Moderate-high. The measurement reflects that hegemonic competition includes significant performative elements: (a) strategic threat inflation (annual China military threat reports, Taiwan contingency planning rhetoric), (b) summit and high-level meeting cycles that enact competition more than operationalize it, (c) sanctions and restrictions that are announced with greater political cost than economic impact, (d) military posturing (carrier deployments, bomber flights, exercises) that signal commitment more than change capability, (e) Cold War institutional maintenance (NATO cohesion theater, deterrence rhetoric) that maintains structures whose original function has atrophied. However, theater ratio is not maximal (0.70+) because underlying extraction and coordination functions are real — actual defense spending is high, technology controls do reduce access, and supply chain fragmentation does occur. The theater enacts and amplifies the real structure; it does not replace it entirely.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is driven by position asymmetry. US and Chinese beneficiary institutions see a constraint that enables them (rope classification, d ≈ 0.10); they experience the coordination framework as functional and beneficial. Developing nations trapped between competing hegemons see pure extraction (snare classification, d ≈ 0.95); they have no genuine choice and bear all costs. Supply chain actors see the constraint as mixed (tangled_rope, d ≈ 0.70); they benefit from the global coordination structure but are extracted from through technology controls and decoupling threats. Multilateral institutions see the constraint as a temporary coordination failure (scaffold, d ≈ 0.60); they believe rational actors will eventually prefer cooperation. Cold War institutional frameworks persist through theater despite loss of original function (piton, d ≈ 0.10 for beneficiaries, but theater ratio 0.70+). The analytical observer viewing through structural realism risks seeing an immutable law (mountain) but the actual structural data reveals this as a false summit: the competition is real but contingent on institutional choices (technology control policies, alliance maintenance, supply chain weaponization) that could be altered through negotiation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures each agent's structural relationship to the extraction flow. Beneficiary institutions (d ≈ 0.05-0.15) experience low or negative effective extraction because the constraint benefits them; they have arbitrage options and can exit to favorable alternatives. Powerless victims in developing nations (d ≈ 0.95) experience maximum extraction because they have no exit, face full suppression, and cannot organize. Organized moderate agents like supply chain actors (d ≈ 0.65-0.75) experience moderate-to-high extraction because they are embedded in the interdependency and cannot exit without disruption, yet they retain some coordination function and some negotiation capacity. The sigma function applies scope modifiers: large-scope constraints (global hegemonic competition, σ = 1.2) amplify extractiveness because verification and exit costs scale with verification difficulty and jurisdictional complexity. The beneficiaries' d values are low because arbitrage options are substantial — defense contractors can shift to civilian markets, Chinese state industries can reorient to domestic consumption, US tech firms can lobby for deregulation — yet they continue to benefit from the constraint, which implies the extraction mechanism is deeply embedded and actively maintained.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival legitimacy rather than type error. All six classifications are structurally accurate for their respective observers. The snare classification for powerless victims is not a misreading — they genuinely face extraction with no exit. The rope classification for beneficiary institutions is not coordination inflation — they genuinely benefit from the structure and perceive it as enabling. The tangled_rope classification for supply chain actors is not a fence-straddling artifact — they genuinely experience both coordination and extraction. The scaffold classification for multilateral institutions is not false hope — they genuinely attempt sunset mechanisms (arms control treaties, trade renegotiation). The piton classification for Cold War frameworks is not institutional pessimism — they genuinely persist through theater despite function loss. The mountain classification for structural realism is the false summit: it naturalizes what is contingent. The mandatrophy is resolved by recognizing that hegemonic competition is real and structurally embedded but not immutable. The constraint persists through: (a) genuine coordination functions (global trade, technology standards) that benefit both hegemons; (b) institutional path dependency (alliance structures, military presence, regulatory arrangements); (c) theater maintenance (rhetorical escalation, performative threat inflation); and (d) suppression mechanisms (trade coercion, military presence, alliance enforcement). Restructuring the constraint would require negotiations that alter the institutional arrangement — not because the arrangement is unchangeable, but because both hegemons benefit from the current structure and lack incentive to change it without external pressure (third-party coalition power) or mutual recognition that the cost-benefit ratio has shifted (proxy conflict escalation, decoupling costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility,
    'Is technological and economic decoupling between US and China actually feasible or does interdependency make it structurally impossible?',
    'Historical precedent analysis (Cold War Soviet-US decoupling, current trade data), supply chain simulation models, critical technology dependency mapping',
    'If feasible: decoupling could transform the constraint from tangled_rope (both embedded) to tangled_rope (each extracts separately) or separate snares. If impossible: interdependency lock-in persists and extraction continues indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility, empirical, 'Whether US-China decoupling is technologically and economically feasible').

omega_variable(
    hegemonic_stability_theory_validity,
    'Does the structural realism prediction (hegemonic competition is inevitable) hold or is cooperation sustainable through institutional design?',
    'Comparative historical analysis of hegemonic transitions (UK→US, US→potential China), institutional effectiveness data, game-theoretic modeling of cooperation thresholds',
    'If cooperation is structurally sustainable: mountain classification is false summit; constraint is contingent. If hegemonic competition is inevitable: mountain classification may be correct; constraint is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hegemonic_stability_theory_validity, conceptual, 'Whether hegemonic competition is structurally inevitable or institutionally contingent').

omega_variable(
    developing_nation_coalition_power,
    'Can powerless developing nations organize into a coalition powerful enough to reduce extraction and increase exit options?',
    'Analysis of historical non-aligned movements, current BRICS/SCO coordination capacity, voting power in international institutions, alternative alliance formation',
    'If coalition possible: powerless agents reclassify as organized; snare classification becomes tangled_rope. If coalition impossible: powerless classification persists; extraction remains maximal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_nation_coalition_power, empirical, 'Coalition formation capacity of developing nations to increase bargaining power').

omega_variable(
    technological_bifurcation_permanence,
    'Is the current bifurcation of technology standards (US-allied vs Chinese) temporary or permanent institutional feature?',
    'Tracking of competing standards (5G/6G, payment systems, cloud platforms, semiconductor architectures), compatibility mapping, institutional coordination attempts',
    'If temporary: constraint reverts to tangled_rope coordination with mutual embedding. If permanent: bifurcation locks in parallel extraction mechanisms and reduces supply chain extraction pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_bifurcation_permanence, empirical, 'Permanence of technological standard bifurcation between US and Chinese ecosystems').

omega_variable(
    proxy_conflict_escalation_risk,
    'What probability threshold marks transition from constrained regional proxy conflicts to uncontrolled escalation?',
    'Strategic stability assessment, historical escalation ladder analysis, nuclear signaling game models, Taiwan scenario modeling',
    'If threshold breached: constraint transforms from tangled_rope/snare to catastrophic failure (regional wars, potential nuclear exchange). This is the mutation risk that defines the constraint''s worst-case trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_conflict_escalation_risk, empirical, 'Escalation probability threshold for proxy conflicts becoming direct conflict').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_hegemonic_competition, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ushc_theater_1975, us_china_hegemonic_competition, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ushc_theater_2000, us_china_hegemonic_competition, theater_ratio, 15, 0.55).
narrative_ontology:measurement(ushc_theater_2026, us_china_hegemonic_competition, theater_ratio, 30, 0.62).
narrative_ontology:measurement(ushc_theater_2031, us_china_hegemonic_competition, theater_ratio, 35, 0.68).

% Extraction over time
narrative_ontology:measurement(ushc_extract_1975, us_china_hegemonic_competition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ushc_extract_2000, us_china_hegemonic_competition, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ushc_extract_2026, us_china_hegemonic_competition, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ushc_extract_2031, us_china_hegemonic_competition, base_extractiveness, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_hegemonic_competition, global_infrastructure).
narrative_ontology:boltzmann_floor_override(us_china_hegemonic_competition, 0.2).
narrative_ontology:affects_constraint(us_china_hegemonic_competition, taiwan_strait_security_dilemma).
narrative_ontology:affects_constraint(us_china_hegemonic_competition, semiconductor_supply_chain_weaponization).
narrative_ontology:affects_constraint(us_china_hegemonic_competition, rare_earth_monopoly_extraction).
narrative_ontology:affects_constraint(us_china_hegemonic_competition, south_china_sea_territorial_competition).
narrative_ontology:affects_constraint(us_china_hegemonic_competition, technology_standard_bifurcation).
narrative_ontology:affects_constraint(us_china_hegemonic_competition, brics_alternative_institution_formation).

% DUAL FORMULATION NOTE:
% US-China hegemonic competition is upstream to six regional/sectoral constraints. Each downstream constraint inherits the suppression and extraction mechanisms from the hegemonic competition while operating at different spatial scopes and institutional levels. The coordination type is global_infrastructure because the constraint involves planet-scale coordination of trade, finance, technology, and security frameworks, albeit through competing institutional arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_hegemonic_competition, institutional, 0.08).
constraint_indexing:directionality_override(us_china_hegemonic_competition, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
