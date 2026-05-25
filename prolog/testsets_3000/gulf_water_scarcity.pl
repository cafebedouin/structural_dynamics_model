% ============================================================================
% CONSTRAINT STORY: gulf_water_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gulf_water_scarcity, []).

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
 *   constraint_id: gulf_water_scarcity
 *   human_readable: Gulf Water Scarcity and Regional Extraction Asymmetry
 *   domain: environmental/geopolitical/economic
 *
 * SUMMARY:
 *   Water scarcity in the Persian Gulf and surrounding arid regions
 *   represents a prototypical tangled rope constraint where regional
 *   coordination on water-sharing frameworks masks asymmetric extraction of
 *   shared aquifers. High-income Gulf states benefit from capital-intensive
 *   desalination while simultaneously extracting non-renewable groundwater
 *   for agricultural export, creating a temporal extraction window where
 *   current benefits are captured while costs are deferred to future
 *   generations and marginalized communities. The constraint exhibits high
 *   suppression (0.72) because alternative water sources require massive
 *   capital investment, exit is geographically constrained, and international
 *   agreements subordinate downstream access to upstream development claims.
 *   Extractiveness (0.58) reflects that the extraction is real but partly
 *   legitimate — desalination enables continued prosperity in an arid zone,
 *   and agricultural exports provide regional employment. The theater ratio
 *   (0.45) is moderate because sustainability narratives (aquifer recovery
 *   targets, conservation mandates) are partially performative while genuine
 *   technological transitions (solar desalination) are underway. The
 *   classification as tangled rope holds because high-income states
 *   coordinate water infrastructure while asymmetrically extracting aquifers;
 *   if the extraction were purely exploitative with zero coordination, it
 *   would be snare.
 *
 * KEY AGENTS:
 *   - High-Income Gulf States (Saudi Arabia, UAE, Qatar): Primary beneficiary (powerful/mobile) — capture agricultural export rents and energy subsidies; have capital for desalination exit
 *   - Subsistence Communities: Primary victim (powerless/trapped) — geographically and economically locked to declining aquifers; no exit options
 *   - Downstream Riparian States (Jordan, Iraq, Palestine): Secondary victim (moderate/constrained) — constrained by upstream extraction and treaty subordination; limited capital for alternatives
 *   - Desalination Industry: Beneficiary (institutional/arbitrage) — profits from scarcity-driven technology deployment; globally arbitraged
 *   - Agricultural Export Sector: Beneficiary (powerful/mobile) — extracts value through export subsidies; gradually exiting as aquifers deplete
 *   - International Water Governance (GCC, Jordan River Commission): Organized mediator (organized/constrained) — coordinates transition pathways; limited enforcement capacity
 *   - Aquifer Commons (Groundwater Ecosystems): Victim/Commons (powerless/trapped) — no agency; purely absorbs extraction costs
 *   - Future Generations: Structural victim (powerless/trapped in biographical horizon) — bear cost of depleted aquifers and degraded commons
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gulf_water_scarcity, 0.58).
domain_priors:suppression_score(gulf_water_scarcity, 0.72).
domain_priors:theater_ratio(gulf_water_scarcity, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gulf_water_scarcity, extractiveness, 0.58).
narrative_ontology:constraint_metric(gulf_water_scarcity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gulf_water_scarcity, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gulf_water_scarcity, tangled_rope).
narrative_ontology:human_readable(gulf_water_scarcity, "Gulf Water Scarcity and Regional Extraction Asymmetry").
narrative_ontology:topic_domain(gulf_water_scarcity, "environmental/geopolitical/economic").

domain_priors:requires_active_enforcement(gulf_water_scarcity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gulf_water_scarcity, high_income_gulf_states).
narrative_ontology:constraint_beneficiary(gulf_water_scarcity, desalination_industry).
narrative_ontology:constraint_beneficiary(gulf_water_scarcity, agricultural_exporters).
narrative_ontology:constraint_victim(gulf_water_scarcity, groundwater_commons).
narrative_ontology:constraint_victim(gulf_water_scarcity, subsistence_communities).
narrative_ontology:constraint_victim(gulf_water_scarcity, downstream_riparian_states).
narrative_ontology:constraint_victim(gulf_water_scarcity, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE COMMUNITIES (SNARE) — Trapped by geographical and economic dependence on declining aquifers. No alternative water sources, no capital for desalination, no exit option. Bear full cost of resource depletion with zero control over extraction rates. Maximum experienced extraction.
constraint_indexing:constraint_classification(gulf_water_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DOWNSTREAM RIPARIAN STATES (TANGLED ROPE) — Constrained by upstream extraction and international agreements that subordinate downstream access to upstream development claims. Experience both coordination (shared watershed management frameworks) and extraction (upstream groundwater depletion reducing transboundary flow). Exit requires breaking treaties or massive capital investment in alternatives.
constraint_indexing:constraint_classification(gulf_water_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DESALINATION INDUSTRY (ROPE) — Benefits from scarcity through technology deployment, long-term contracts, and international investment. Experiences the constraint as coordination problem (securing reliable energy, brine disposal, capital availability). Net beneficiary with high exit optionality through global supply chains and technology markets.
constraint_indexing:constraint_classification(gulf_water_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-INCOME GULF STATES (TANGLED ROPE) — State actors that benefit from aquifer extraction for agriculture and export while coordinating regional water sharing agreements. Experience genuine coordination function (irrigation networks, pricing regimes) alongside asymmetric extraction of shared groundwater commons. Mobile through desalination exit, but aquifer depletion creates long-term path dependence.
constraint_indexing:constraint_classification(gulf_water_scarcity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL WATER GOVERNANCE (SCAFFOLD) — Organizations like the Jordan River Commission and GCC water councils see aquifer depletion as a solvable coordination problem with sunset logic: desalination scale-up, agricultural intensification limits, and aquifer recovery timelines create a transition pathway from groundwater dependence to renewable alternatives. Constrained by state sovereignty but organized enough to impose coordination constraints.
constraint_indexing:constraint_classification(gulf_water_scarcity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: AGRICULTURAL EXPORT REGIMES (PITON) — Water-intensive crop exports (wheat, dates, livestock feed) are performatively justified as regional food security while operating as aquifer depletion mechanisms. The institutional framework (subsidies, export licensing, water rights allocation) persists through policy inertia despite declining aquifer yields and mounting unsustainability. Theater ratio reflects gap between stated conservation goals and continuing extraction.
constraint_indexing:constraint_classification(gulf_water_scarcity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / HYDROLOGICAL LIMIT (MOUNTAIN) — From a geological/hydrological perspective, the constraint appears as an immutable physical law: aquifer recharge rates in arid zones are ~1-5 mm/year while extraction rates are 100-500 mm/year. The 50-100x overdraft ratio is a direct consequence of geology, not policy. However, structural data reveals this mountain as partially false: the overshoot is contingent on policy choices (subsidy levels, export targets, irrigation technology) that could reduce extraction to match recharge rates.
constraint_indexing:constraint_classification(gulf_water_scarcity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gulf_water_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gulf_water_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gulf_water_scarcity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gulf_water_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gulf_water_scarcity, TR),
    TR >= 0.70.

:- end_tests(gulf_water_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint involves genuine rent extraction (agricultural subsidies capturing aquifer value, desalination premiums), but also legitimate coordination (irrigation networks, energy integration, regional stability). The value reflects that much of what appears as extraction is actually deferred cost — prosperity today via aquifer depletion shifts costs to the future. Suppression (0.72): High. Arid-zone geography constrains water availability; capital requirements for desalination exclude poor communities; international treaties entrench upstream advantage; climate trends reduce recharge rates. Exit barriers are structural, not purely institutional. Theater ratio (0.45): Moderate. Sustainability commitments are partly sincere (desalination expansion, conservation mandates) and partly performative (aquifer 'recovery' targets that assume unrealistic recharge rates, food security narratives that mask export optimization). The theater has not yet peaked because genuine technological transitions are underway; it remains below the piton threshold.
 *
 * PERSPECTIVAL GAP:
 *   Gap derives from differential exit optionality and temporal horizons. High-income powerful states with immediate horizons and mobile exit options see rope (coordination). Subsistence communities with biographical horizons and trapped exit see snare (pure extraction). Downstream states with generational horizons and constrained exits see tangled rope (mixed). International governance with generational horizons and organized (but limited) agency see scaffold (temporary with sunset). The analytical observer risks naturalizing the constraint as hydrological law when it is actually a contingent policy equilibrium. This gap is the diagnostic signal: if all perspectives agree on classification, the constraint is likely truly immutable; if perspectives diverge sharply by exit option and power, the constraint is institutional and potentially changeable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values capture each agent's structural position relative to extraction flow. Subsistence communities experience d ≈ 0.95 (pure targets) — all costs, no benefits, no exit. Downstream riparian states experience d ≈ 0.75 (victims with some constrained agency) — bear costs but have limited coordination options. High-income states as beneficiaries experience d ≈ 0.20 (net beneficiaries with high exit optionality through capital markets) — capture rents but can exit to desalination. Desalination industries experience d ≈ 0.05 (arbitrage beneficiaries) — benefit from scarcity with global supply chain mobility. International institutions experience d ≈ 0.50 (symmetric positions) — coordinate but cannot enforce against powerful state actors. The piton perspective (agricultural exporters) experiences d ≈ 0.15 despite being beneficiaries because the extraction mechanism is degraded — it persists through inertia, not current efficiency.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: Partially Resolved. The constraint avoids false mountaineering because the base extraction (0.58) is sufficiently high and beneficiary/victim declarations are clear — this is not a coordination problem being mislabeled as extraction. However, the classification resolves mandatrophy incompletely at the state actor level: high-income states can legitimately claim the constraint is a coordination problem (they genuinely do benefit from regional water infrastructure coordination) while subsistence communities legitimately claim it is extraction (they bear costs with zero benefit). The resolution is perspectival, not objective — which type is 'correct' depends entirely on whether you ask the beneficiary or the victim. The analytical observer's mountain classification is a false summit and represents the mandatrophy risk in environmental constraints: naturalizing contingent policy inequities as hydrological laws. The scaffold classification (from international governance perspective) is the mandatrophy resolution path — it acknowledges both coordination and extraction while declaring the extraction window has a sunset (20-30 years until desalination and renewable energy mature). This sunset logic dissolves the ethical tension: temporary extraction is acceptable if structural mechanisms exist to end it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aquifer_recharge_measurement_ambiguity,
    'Are measured aquifer recharge rates accurate, or are they biased by climate variability and measurement methodology?',
    'Multi-decade isotope analysis (oxygen-18, carbon-14) of groundwater age cohorts; comparison of recharge estimates from hydrological balance vs. tracer methods',
    'If recharge is underestimated: extraction rates may be closer to sustainable than current data suggests, reducing mountain classification credibility. If recharge is overestimated: depletion timeline is shorter than policy models assume, increasing urgency of scaffold exit pathways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aquifer_recharge_measurement_ambiguity, empirical, 'Accuracy of aquifer recharge rate measurement and climate signal isolation').

omega_variable(
    desalination_energy_sustainability,
    'Can renewable energy reliably power desalination scale-up, or is the scaffold exit pathway dependent on fossil fuel subsidies?',
    'Techno-economic analysis of solar/wind desalination vs. grid-powered systems across GCC region; tracking of renewable capacity addition vs. desalination build schedule',
    'If renewable desalination is achievable: scaffold perspective gains credibility and extraction window closes. If dependent on subsidies: scaffold becomes aspirational rather than structural, and snare classification persists longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desalination_energy_sustainability, empirical, 'Feasibility of renewable-powered desalination as sustainable exit mechanism').

omega_variable(
    export_subsidy_political_removability,
    'Can agricultural export subsidies be politically removed, or are they locked into state legitimacy narratives?',
    'Historical analysis of subsidy removal episodes; tracking of policy proposals and their political success/failure across GCC states; study of state-level identity narratives tied to food self-sufficiency claims',
    'If removable: piton classification shifts toward rope as institutional inertia can be broken through political action. If locked: piton persists and agricultural extraction persists despite unsustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(export_subsidy_political_removability, conceptual, 'Political removability of agricultural export subsidies tied to aquifer depletion').

omega_variable(
    transboundary_aquifer_treaty_enforceability,
    'Can international aquifer-sharing treaties enforce extraction limits, or do upstream states retain de facto override capacity?',
    'Analysis of treaty compliance history for Nubian Sandstone, Western Desert, and Arabian Aquifer Formations; study of dispute resolution mechanisms and their invocation',
    'If enforceable: downstream riparian classification improves from trapped toward constrained. If unenforceable: downstream agents remain snare-classified and extraction asymmetry persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transboundary_aquifer_treaty_enforceability, empirical, 'Enforceability of transboundary aquifer treaties against upstream state extraction').

omega_variable(
    identity_locked_food_security_narrative,
    'Is regional commitment to food self-sufficiency through aquifer extraction structurally locked by state identity, or can it be reframed?',
    'Discourse analysis of state messaging around water and agriculture; tracking of policy shifts when reframing occurs (e.g., Saudi Arabia''s gradual shift from wheat export to food imports); study of legitimacy maintenance post-reframing',
    'If identity-locked: state actors remain invested in piton/tangled_rope classifications despite mounting unsustainability. If reframeable: exit pathways become available and scaffold logic can accelerate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_food_security_narrative, conceptual, 'Identity-lock of state actors to aquifer-dependent agricultural export model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gulf_water_scarcity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gulf_water_tr_t0, gulf_water_scarcity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gulf_water_tr_t10, gulf_water_scarcity, theater_ratio, 10, 0.4).
narrative_ontology:measurement(gulf_water_tr_t20, gulf_water_scarcity, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(gulf_water_be_t0, gulf_water_scarcity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gulf_water_be_t10, gulf_water_scarcity, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gulf_water_be_t20, gulf_water_scarcity, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gulf_water_scarcity, resource_allocation).
narrative_ontology:affects_constraint(gulf_water_scarcity, agricultural_water_subsidies).
narrative_ontology:affects_constraint(gulf_water_scarcity, desalination_energy_dependence).
narrative_ontology:affects_constraint(gulf_water_scarcity, transboundary_aquifer_treaties).

% DUAL FORMULATION NOTE:
% Gulf water scarcity decomposes into three related constraints: (1) aquifer depletion as a biogeophysical limit (higher ε, mountain-risk), (2) water rights allocation regimes (medium ε, tangled rope), and (3) desalination technology transition pathways (lower ε, scaffold). This story focuses on the institutional constraint family (allocation and politics); the biogeophysical limit is upstream and affects this constraint's extractiveness baseline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gulf_water_scarcity, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
