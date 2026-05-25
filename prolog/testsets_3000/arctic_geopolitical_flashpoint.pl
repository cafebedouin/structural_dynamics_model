% ============================================================================
% CONSTRAINT STORY: arctic_geopolitical_flashpoint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-07-17
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_geopolitical_flashpoint, []).

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
 *   constraint_id: arctic_geopolitical_flashpoint
 *   human_readable: The Melting Ice and the Scramble for Greenland
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Rapid Arctic ice melt has transformed Greenland and the Northern Sea
 *   Route from peripheral geopolitical zones into strategic flashpoints.
 *   Newly exposed mineral reserves (rare earths, uranium, lithium) and the
 *   economically viable Northern Sea Route have triggered a multilayered
 *   scramble involving Arctic military powers (US, Russia, China),
 *   colonial-legacy states (Denmark, Norway), extractive industries,
 *   indigenous communities, and climate-vulnerable nations. The constraint
 *   exhibits the full diagnostic pattern of tangled rope: genuine
 *   coordination benefits for some actors (supply chain security, military
 *   deterrence, technological development) coexist with severe asymmetric
 *   extraction (colonial resource appropriation, environmental expropriation,
 *   indigenous dispossession, geopolitical exclusion of climate-vulnerable
 *   nations). The theatrical dimension (Arctic Council statements, UNCLOS
 *   negotiations, environmental assessments) has risen over the interval as
 *   the gap between stated coordination objectives and actual power-driven
 *   outcomes has widened. Indigenous Greenlandic communities face colonialism
 *   redux: external capital and military power appropriate resources and
 *   sovereignty without meaningful consent. Climate-vulnerable nations bear
 *   catastrophic costs (sea level rise, ocean disruption) from Arctic changes
 *   they did not cause and have no voice in governing. The constraint cannot
 *   be resolved by technical means alone—the coordination benefits are real,
 *   but the extraction mechanisms are structurally enforced through military,
 *   capital, and state power. The scaffold perspective (climate action +
 *   indigenous rights coalition) identifies a sunset pathway: carbon
 *   transition would make Arctic resource extraction uneconomical within
 *   15-25 years if the energy transition accelerates. But geopolitical
 *   entrenchment may lock in extraction mechanisms even if economic
 *   justification disappears, converting tangled rope into permanent piton
 *   (institutional shells with no real function).
 *
 * KEY AGENTS:
 *   - Indigenous Greenlandic Communities (Kalaallit/Inuit): Primary victims (powerless/trapped) — face resource extraction, sovereignty violation, and climate impacts without consent mechanisms.
 *   - Climate-Vulnerable Nations (Small island states, low-lying regions): Primary victims (powerless/trapped) — bear catastrophic sea level and ecosystem costs with zero participation in Arctic governance.
 *   - Arctic Military Powers (US, Russia, China): Primary beneficiaries (powerful/mobile) — gain supply chain security, strategic positioning, and deterrence advantages; enforce constraint through military presence.
 *   - Greenland Mineral Extractors and Energy Companies: Primary beneficiaries (organized/arbitrage) — access rare earth and uranium reserves; arbitrage optionality allows cost shifting to communities and environment.
 *   - Danish State and Greenlandic Administration: Institutional beneficiaries (institutional/arbitrage) — coordinate access and revenue capture; asymmetric bargaining power favors Denmark.
 *   - Arctic Council and International Maritime Law Institutions: Piton actor (institutional/arbitrage) — governance frameworks persist as theater while enforcement mechanisms degrade.
 *   - Climate Action and Indigenous Rights Coalition: Organized constraint agent (organized/constrained) — advocates for sunset mechanisms (carbon transition, extraction moratoriums, indigenous veto) with low direct power but rising norm-shifting capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_geopolitical_flashpoint, 0.58).
domain_priors:suppression_score(arctic_geopolitical_flashpoint, 0.68).
domain_priors:theater_ratio(arctic_geopolitical_flashpoint, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_geopolitical_flashpoint, extractiveness, 0.58).
narrative_ontology:constraint_metric(arctic_geopolitical_flashpoint, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(arctic_geopolitical_flashpoint, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_geopolitical_flashpoint, tangled_rope).
narrative_ontology:human_readable(arctic_geopolitical_flashpoint, "The Melting Ice and the Scramble for Greenland").
narrative_ontology:topic_domain(arctic_geopolitical_flashpoint, "geopolitical/economic").

domain_priors:requires_active_enforcement(arctic_geopolitical_flashpoint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_geopolitical_flashpoint, greenland_mineral_extractors).
narrative_ontology:constraint_beneficiary(arctic_geopolitical_flashpoint, arctic_military_powers).
narrative_ontology:constraint_beneficiary(arctic_geopolitical_flashpoint, northern_sea_route_operators).
narrative_ontology:constraint_victim(arctic_geopolitical_flashpoint, indigenous_greenlandic_communities).
narrative_ontology:constraint_victim(arctic_geopolitical_flashpoint, arctic_indigenous_populations).
narrative_ontology:constraint_victim(arctic_geopolitical_flashpoint, climate_vulnerable_nations).
narrative_ontology:constraint_victim(arctic_geopolitical_flashpoint, global_maritime_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS GREENLANDIC COMMUNITIES (SNARE) — Kalaallit and Inuit populations face resource extraction and geopolitical appropriation of ancestral territories without consent mechanisms. Trapped by colonial legacy structures and lack of exit options from extractive development. High suppression: mineral wealth requires external capital/technology, creating dependency on foreign investors and state actors. Maximum effective extraction — no coordination benefit experienced, full costs borne.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CLIMATE VULNERABLE NATIONS (SNARE) — Small island states and low-lying regions bear catastrophic costs of Arctic melt (sea level rise, disrupted ocean circulation, ecosystem collapse) while having no meaningful participation in Arctic resource allocation decisions. Trapped by geography and development asymmetry. Suppression enforced through institutional exclusion from Arctic Council and UN convention mechanics. Pure extraction: no coordination benefit, existential bearing of costs.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ARCTIC MILITARY POWERS (TANGLED ROPE) — Powerful state actors (US, Russia, China) experience genuine coordination function: NSR access, mineral supply security, and strategic positioning reduce supply chain vulnerability and enhance deterrence capabilities. BUT also extract asymmetrically: military contestation raises cost of passage for commercial operators, forced militarization of civilian infrastructure, and subordination of environmental governance to strategic competition. Active enforcement required: naval positioning, territorial claims, sanctions regimes. Suppression moderate (nuclear umbrella prevents open conflict) but high enough to constrain non-military actors' options. Mobile exit (could withdraw, but strategic cost makes exit effectively constrained). Perspectival gap: beneficiaries see coordination; victims see extraction.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: GREENLAND MINERAL EXTRACTORS (TANGLED ROPE) — Mining corporations and energy companies benefit from access to rare earth elements, uranium, and oil reserves now economically viable due to ice melt. Arbitrage exit available: can shift operations across jurisdictions or pause extraction if regulatory costs rise. Active enforcement required: licensing regimes, environmental permits, labor standards create overhead but also predictability. Suppression moderate: competitive pressure among extractors reduces monopoly rents, but geopolitical risk premium can be passed to global consumers. Mixed coordination-extraction hybrid: coordination function is access-to-resources-at-scale; extraction mechanism is capture of Greenlandic sovereignty and environmental externalities. Beneficiary position: companies experience net resource gain and strategic optionality.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DANISH STATE / GREENLANDIC ADMINISTRATION (ROPE) — Coordinate access to mineral wealth, military basing, and strategic positioning through partnership frameworks and licensing. Denmark and Greenland both benefit from resource revenues and geopolitical relevance, though with asymmetric bargaining power (Denmark retains defense responsibility, Greenland retains nominal sovereignty). Arbitrage available: can shift partnerships, renegotiate licensing, or leverage bidders. Suppression relatively low: institutional relationships are formalized rather than coercive. Theater moderate: diplomatic ritual (Arctic Council statements, mining permits) has performative elements but also genuine policy content. Experienced as coordination mechanism for mutual strategic benefit — low effective extraction relative to beneficiary status.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ARCTIC COUNCIL AND INTERNATIONAL MARITIME LAW (PITON) — Arctic governance institutions (Arctic Council, UNCLOS, LOSC) persist as coordination frameworks despite functional degradation. Original purpose: manage shared resources and environmental protection through consensus-based negotiation. Current reality: frameworks operate as theatrical venues while real power lies in military positioning and bilateral deals. Theater ratio high (0.62): Arctic Council statements and environmental assessments continue but lack enforcement mechanisms; UNCLOS provisions on extended continental shelves are contested; maritime law is subordinated to geopolitical competition. Institutional inertia: these frameworks maintain legitimacy but diminishing actual governance function. Classification as piton reflects degradation of coordination function, not high experienced extraction. Suppression increases over time as military contestation overrides institutional mechanisms.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE ACTION / INDIGENOUS RIGHTS COALITION (SCAFFOLD) — Environmental organizations, indigenous rights advocates, and climate-focused states see the Arctic scramble as a temporary institutional failure with a sunset. The coalition advocates for constraint mechanisms (carbon pricing, extraction moratoriums, indigenous veto rights, climate reorientation) that would make Arctic resource development uneconomical. Constrained exit: these actors have limited direct enforcement power but can raise costs through activism, litigation, and norm-shifting. Theater moderate: climate activism contains both genuine analysis and performative elements. Coordination function: coalition provides alternative institutional pathway (climate governance, indigenous rights frameworks) with explicit sunset: if carbon economy transitions, Arctic resource extraction loses viability. Suppression rising: state and corporate resistance to constraint mechanisms. But scaffold classification holds because sunset clause is structural (carbon transition is inevitable if political will consolidates), even if timeline is uncertain.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE, CIVILIZATIONAL) — From a structural perspective, the Arctic scramble combines genuine coordination function (solving supply chain risk, enabling technological development) with extractive asymmetry (colonialism redux, climate expropriation, militarization). The constraint cannot be classified as pure coordination (Rope) because the distribution of costs and benefits is radically asymmetric and enforced via power differentials. It cannot be classified as pure extraction (Snare) because there are real coordination gains for some actors. The tangled rope classification at the civilizational scale reveals that the system is self-undermining: extraction mechanisms (militarization, institutional capture, environmental externalities) destroy the coordination benefits they claim to enable. Arctic governance institutions are degrading (piton) as military competition overrides institutional consensus. Theater ratio reflects this degradation: diplomatic processes persist but lack substantive authority.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_geopolitical_flashpoint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_geopolitical_flashpoint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arctic_geopolitical_flashpoint, TR),
    TR >= 0.70.

:- end_tests(arctic_geopolitical_flashpoint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantially from indigenous communities (sovereignty, resources, self-determination) and climate-vulnerable nations (catastrophic costs with no voice). But extraction is not as severe as pure snare (0.75+) because institutional beneficiaries (Denmark, Greenland, mining companies) do experience genuine coordination gains (supply chain access, revenue, strategic security). The asymmetry is structural but not total. Suppression (0.68): High. Multiple suppression mechanisms: military deterrence prevents open contestation; capital concentration limits indigenous participation in extractive industries; institutional exclusion (Arctic Council membership rules) bars climate-vulnerable nations; power asymmetry makes exit costly. However, suppression is not extreme (0.85+) because some institutional channels remain (international law, activist litigation, Nordic social-democratic constraints). Theater ratio (0.62): Moderate-high and rising. Arctic Council deliberations, UNCLOS extended continental shelf negotiations, and environmental impact assessments continue but increasingly lack enforcement authority. Real decisions are made through military positioning and bilateral deals. The theater ratio has risen over the interval (0.48 to 0.62) as military competition has displaced institutional consensus.
 *
 * PERSPECTIVAL GAP:
 *   Six distinct perspectives produce four different classifications, revealing the structure of the constraint. Beneficiaries (military powers, extractive companies, Denmark) experience Rope or low-χ Tangled Rope: genuine coordination with some overhead. Victims (indigenous communities, climate-vulnerable nations) experience Snare: pure extraction with no coordination benefit. Greenland experiences Tangled Rope from a more moderate position: both benefits and extraction, constrained optionality. Institutions experience degradation: Arctic Council sees itself as Rope; all other perspectives see it as Piton (theater without function). The climate/indigenous coalition sees a Scaffold: temporary constraint with a sunset mechanism if carbon transition succeeds. The civilizational analytical observer sees Tangled Rope at the system level, with a warning: the system is self-undermining because extraction mechanisms destroy coordination benefits, and the power asymmetries that enable extraction also prevent the institutional redesign needed to preserve coordination without extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. Indigenous communities: victims (high d ≈ 0.90) with trapped exit → maximum f(d) ≈ 1.38 → maximum experienced extraction. Climate-vulnerable nations: victims (high d ≈ 0.85) with trapped exit → high f(d) ≈ 1.15 → severe experienced extraction despite low nominal power. Arctic military powers: beneficiaries with mobile exit (d ≈ 0.35) → f(d) ≈ 0.35 → low experienced extraction because they can exit; also experience positive coordination gains. Extractive companies: beneficiaries with arbitrage exit (d ≈ 0.15) → f(d) ≈ -0.01 → negative effective extraction (pure benefit). Greenland mineral sector: dual position (local beneficiary but victim of external control); constrained exit (d ≈ 0.55) → f(d) ≈ 0.75 → moderate extraction despite nominal beneficiary status, because external control limits genuine optionality. Danish state: institutional beneficiary with arbitrage (d ≈ 0.10) → f(d) ≈ -0.10 → negative experienced extraction (pure coordination). Arctic Council: institutional observer (d ≈ 0.65) → f(d) ≈ 1.00 → moderate experienced extraction relative to governance authority, because gap between stated coordination and actual enforcement means the institution itself bears some extraction cost (reputational damage, relevance erosion).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint embodies the core mandatrophy tension — it claims to be coordination (Arctic Council, international law, resource development enabling technological progress) but is structurally organized as extraction (military control, capital gatekeeping, indigenous dispossession). The resolution follows from recognizing that coordination and extraction are not dichotomous categories but can coexist as a tangled rope: some actors genuinely benefit from the coordination function, while others bear the extraction costs. The mandatrophy dissolves when we ask: 'From which index perspective?' For military powers and extractive companies, the constraint is coordination with acceptable overhead. For indigenous communities and climate-vulnerable nations, it is pure extraction. For institutions, it is degrading coordination (piton). For the carbon-transition coalition, it is temporary coordination with a sunset. The system-level mandatrophy is that the extraction mechanisms (militarization, institutional capture, environmental externalization) are undermining the coordination benefits they claim to enable. If Arctic military competition escalates into an unstoppable security dilemma, the constraint becomes snare-like even for beneficiaries (deterrence logic becomes self-referential). If carbon transition accelerates, the constraint becomes piton-like for all actors (institutions persist without function, extraction logic collapses). The constraint is stable as tangled rope only if: (1) military deterrence remains bounded, (2) carbon transition is slow enough that Arctic resources remain economically valuable, and (3) institutional frameworks are credible enough to distribute some benefits downward. All three conditions are currently fragile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    greenland_sovereignty_threshold,
    'At what point does Greenland''s resource autonomy shift from constrained sovereignty to de facto independence or external control?',
    'Tracking of Greenlandic legislative action, referendum results on independence, external military basing agreements, and revenue capture from mining operations. Threshold markers: >50% of government revenue from local resources, unilateral resource licensing without Danish veto, military bases without Danish command authority.',
    'If threshold reached: constraint shifts from tangled_rope (Greenland/Denmark benefit) to snare (indigenous communities + small nations bear costs). If threshold not reached: Danish-Greenlandic partnership remains the primary institutional actor, constraining Greenlandic agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(greenland_sovereignty_threshold, empirical, 'Sovereignty threshold for Greenlandic resource autonomy').

omega_variable(
    arctic_military_escalation_cascade,
    'Does Arctic military competition trigger a security dilemma where each power''s deterrence measures force others'' escalation, creating an unstoppable arms race independent of resource scarcity?',
    'Analysis of military deployment patterns, doctrinal statements, and escalation trigger points. Markers: nuclear-armed submarines, integrated air defense systems, joint military exercises, militarization of civilian ports and shipping. Cascade detection: when military presence becomes self-justifying (exist to deter each other, not to control resources).',
    'If cascade confirmed: constraint becomes increasingly decoupled from resource competition and driven by security logic alone. Suppression increases toward 0.85+. Snare classification (victimhood) strengthens for indigenous and climate-vulnerable actors. If cascade averted: resource competition remains primary driver, tangled_rope persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arctic_military_escalation_cascade, empirical, 'Whether Arctic military competition enters unstoppable security dilemma').

omega_variable(
    carbon_transition_irreversibility,
    'Will global carbon transition (energy, transportation, materials) proceed rapidly enough to make Arctic resource extraction economically obsolete before geopolitical competition becomes locked-in?',
    'Tracking of renewable energy deployment curves, battery technology maturity, carbon pricing momentum, and corporate decarbonization commitments. Timeline: if carbon transition achieves 80%+ coverage by 2040-2045, Arctic mining loses economic justification. If delayed past 2050, geopolitical entrenchment may prevent reversal even if transition accelerates.',
    'If transition is rapid: scaffold sunset clause activates. Arctic minerals become stranded assets. Constraint collapses to piton (institutional shells with no function). If transition is slow: Arctic becomes permanent geopolitical flashpoint. Extraction mechanisms lock in, suppression rises to 0.75+. Snare classification solidifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_transition_irreversibility, empirical, 'Carbon transition timeline relative to Arctic geopolitical entrenchment').

omega_variable(
    indigenous_coalition_power,
    'Can indigenous Arctic communities + climate coalition + small states create credible veto power over Arctic development, or are power asymmetries insurmountable?',
    'Monitoring of indigenous legal victories (land rights, consultation veto, extraction moratoriums), coalition coordination mechanisms (joint treaties, investment pressure, diplomacy), and state response. Threshold: if indigenous groups achieve >30% control over licensing decisions or >20 countries impose Arctic sanctions, coalition has structural power.',
    'If coalition power emerges: constraint classification shifts toward more balanced perspectives. Scaffolding accelerates (sunset timelines shorten). Snare classification weakens for indigenous actors (increase in exit options). If power asymmetry persists: coalition remains organized but constrained, scaffold perspective remains aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_coalition_power, empirical, 'Coalition power for indigenous + climate + small-state Arctic vetoes').

omega_variable(
    false_mountain_detection,
    'Is the Arctic ''scramble'' a natural geopolitical law (competition inevitably follows resource access), or a contingent institutional arrangement that naturalizes power asymmetries as inevitable?',
    'Comparative historical analysis: do all cases of newly accessible resources produce geopolitical competition, or only when power asymmetries exist? Counterfactual: what institutions would change the outcome? Conceptual test: if all actors had equal power, would competition still be inevitable, or is it an artifact of asymmetry?',
    'If false summit confirmed: the ''natural competition'' framing is a rationalization for extraction mechanisms. The constraint is contingent on power asymmetry, not on physics. Reframing enables institutional redesign (equal-voice governance, indigenous veto, commons frameworks). If mountain is genuine: competition is inevitable regardless of institutions, and adaptation is the only option.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_mountain_detection, conceptual, 'Whether Arctic geopolitical competition is natural law or contingent power asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_geopolitical_flashpoint, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arctic_tr_t0, arctic_geopolitical_flashpoint, theater_ratio, 0, 0.48).
narrative_ontology:measurement(arctic_tr_t4, arctic_geopolitical_flashpoint, theater_ratio, 4, 0.58).
narrative_ontology:measurement(arctic_tr_t11, arctic_geopolitical_flashpoint, theater_ratio, 11, 0.62).

% Extraction over time
narrative_ontology:measurement(arctic_be_t0, arctic_geopolitical_flashpoint, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(arctic_be_t4, arctic_geopolitical_flashpoint, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(arctic_be_t11, arctic_geopolitical_flashpoint, base_extractiveness, 11, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_geopolitical_flashpoint, global_infrastructure).
narrative_ontology:boltzmann_floor_override(arctic_geopolitical_flashpoint, 0.55).
narrative_ontology:affects_constraint(arctic_geopolitical_flashpoint, rare_earth_supply_chain).
narrative_ontology:affects_constraint(arctic_geopolitical_flashpoint, northern_sea_route_governance).
narrative_ontology:affects_constraint(arctic_geopolitical_flashpoint, indigenous_resource_rights).
narrative_ontology:affects_constraint(arctic_geopolitical_flashpoint, carbon_transition_lock_in).
narrative_ontology:affects_constraint(arctic_geopolitical_flashpoint, arctic_military_deterrence).

% DUAL FORMULATION NOTE:
% The Arctic geopolitical flashpoint decomposes into multiple structurally distinct constraints: (1) Resource Competition (who controls mineral extraction and at what cost to indigenous communities), (2) Strategic Chokepoint (who controls Northern Sea Route access and military positioning), (3) Indigenous Rights (whether indigenous communities have veto power or are subject to external decisions), (4) Climate Transition (whether Arctic development is made obsolete by carbon transition), and (5) Military Deterrence (whether security logic becomes decoupled from resource competition and self-sustaining). Each has distinct ε values and institutional dynamics. The current story treats them as a unified constraint because they are structurally coupled: resource competition creates military incentives, military competition shapes extraction rules, indigenous resistance shapes military costs, and carbon transition affects all. But if any decoupling occurs (e.g., military entrenchment without resource justification, or carbon transition without geopolitical consequence), separate constraint stories would be needed. Links in affects_constraints are to the specific decomposed constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arctic_geopolitical_flashpoint, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
