% ============================================================================
% CONSTRAINT STORY: thai_cambodian_military_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_cambodian_military_asymmetry, []).

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
 *   constraint_id: thai_cambodian_military_asymmetry
 *   human_readable: Thai-Cambodian Military Asymmetry and Regional Extraction
 *   domain: geopolitical/military/regional_stability
 *
 * SUMMARY:
 *   The Thai-Cambodian military asymmetry is a structural feature of
 *   Southeast Asian geopolitics that emerges from differential Cold War
 *   legacies, post-colonial state formation trajectories, and geographic
 *   proximity. Thailand's superior military capacity creates a constraint
 *   structure that simultaneously coordinates regional power balance (through
 *   deterrence and alliance formation) and extracts resources, sovereignty
 *   limitations, and perennial insecurity from Cambodia. The constraint is
 *   not purely extractive — it genuinely solves the coordination problem of
 *   managing power differentials in a region with contested borders and
 *   overlapping great-power interests. However, the extraction component has
 *   increased over the measurement interval (extractiveness rising from 0.42
 *   to 0.58) as military modernization, border incidents, and cross-border
 *   resource politics have intensified the asymmetry's practical effects. The
 *   theater_ratio has also risen (0.48 to 0.65), indicating that an
 *   increasing proportion of military activity is performative threat
 *   projection (military exercises, nationalist rhetoric, institutional
 *   ceremony) rather than genuine operational capability deployment. The
 *   constraint exhibits six distinct classification types depending on
 *   observer position, revealing the depth of perspectival conflict: what the
 *   Thai military sees as benign coordination, the Cambodian population
 *   experiences as structural trapping; what regional elites understand as
 *   alliance management, realist international theory risks naturalizing as
 *   immutable geopolitical law.
 *
 * KEY AGENTS:
 *   - Thai Military Establishment: Primary beneficiary (institutional/arbitrage) — maintains regional superiority; uses asymmetry for domestic and international influence
 *   - Thai Regional Political Elite: Secondary beneficiary (powerful/mobile) — consolidates domestic legitimacy through nationalist narrative; manages regional relationships
 *   - Cambodian Population: Primary victim (powerless/trapped) — bears costs of border tension, defense spending, and resource diversion; cannot exit constraint
 *   - Cambodian State Authority: Secondary victim (organized/constrained) — manages structural inferiority through alliance deepening with China and Vietnam; constrained agency
 *   - ASEAN Regional Institutions: Coordinator (institutional/mobile) — attempts to manage asymmetry through multilateral forums; partial success
 *   - Regional Stability Commons: Distributed victim/coordinator (powerful/mobile) — experiences constraint as generating both cooperation costs and extraction (risk premiums)
 *   - Great Powers (China, Vietnam, US): External actors (institutional/arbitrage) — influence regional asymmetry through alliance formation and military support
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_cambodian_military_asymmetry, 0.58).
domain_priors:suppression_score(thai_cambodian_military_asymmetry, 0.72).
domain_priors:theater_ratio(thai_cambodian_military_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_cambodian_military_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(thai_cambodian_military_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(thai_cambodian_military_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_cambodian_military_asymmetry, tangled_rope).
narrative_ontology:human_readable(thai_cambodian_military_asymmetry, "Thai-Cambodian Military Asymmetry and Regional Extraction").
narrative_ontology:topic_domain(thai_cambodian_military_asymmetry, "geopolitical/military/regional_stability").

domain_priors:requires_active_enforcement(thai_cambodian_military_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thai_cambodian_military_asymmetry, thai_military_establishment).
narrative_ontology:constraint_beneficiary(thai_cambodian_military_asymmetry, regional_thai_political_elite).
narrative_ontology:constraint_victim(thai_cambodian_military_asymmetry, cambodian_sovereignty).
narrative_ontology:constraint_victim(thai_cambodian_military_asymmetry, cambodian_population).
narrative_ontology:constraint_victim(thai_cambodian_military_asymmetry, regional_stability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAMBODIAN POPULATION (SNARE) — Trapped within geographic and geopolitical boundaries. Cannot exit the regional constraint structure. Bears disproportionate cost of military posturing, border incidents, and resource diversion to defense. Maximum suppression — limited capacity for collective action against the asymmetric military reality. Zero degrees of freedom for exit.
constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CAMBODIAN STATE AUTHORITY (TANGLED ROPE) — Constrained by structural military inferiority but also benefits from the constraint through institutional coordination with allied powers (China, Vietnam). The state experiences genuine extraction (resource burden, sovereignty limitation) alongside a coordination function (alliance deepening, security guarantees). Cannot exit without geopolitical realignment, but also maintains agency through alliance manipulation.
constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THAI MILITARY ESTABLISHMENT (ROPE) — Benefits from coordinating regional power balance. The military asymmetry serves as a coordination mechanism: Thailand's superior capacity enables regional influence over resource allocation, trade patterns, and alliance formation. Experiences the constraint as beneficial coordination. Net beneficiary with high exit arbitrage — can credibly threaten or withdraw threat as strategic interest requires.
constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THAI REGIONAL POLITICAL ELITE (TANGLED ROPE) — Powerful agents who experience both coordination benefits (regional influence, domestic political consolidation through nationalist narrative) and extraction costs (ongoing military spending, border tension management, alliance management complexity). Mobile enough to shift strategy but constrained by domestic politics. Experience is mixed — genuine benefit with significant maintenance costs.
constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY COLD WAR REGIONAL ARCHITECTURE (PITON) — The military asymmetry persists as institutional inertia long after its original strategic purpose (containing communism) has atrophied. Regional security architecture maintains the threat perception through ritual military exercises, border patrols, and intelligence sharing that is largely performative. Theater_ratio high (0.65): much of the asymmetry signaling is maintained through institutional choreography rather than genuine military threat escalation. The system persists because alternatives haven't replaced it, not because it functions optimally.
constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: REGIONAL STABILITY COMMONS (TANGLED ROPE) — The constraint coordinates arms-race deterrence and alliance formation (genuine coordination function) while simultaneously extracting resources, constraining economic integration, and generating perennial insecurity (extraction function). Global actors with mobile exit options — ASEAN institutions, international investors, development organizations — experience the asymmetry as a coordination problem that generates both cooperation (multilateral forums) and extraction (risk premiums, conflict insurance costs). Moderate extraction with real coordination benefits.
constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL REALISM VIEW (MOUNTAIN) — From a universal realist perspective, the military asymmetry is an immutable law of geopolitics: power differentials create structural constraints that are inherent to the international system. No agent can transcend the constraint — it emerges naturally from the distribution of capabilities and geographic proximity. However, this mountain classification may be a false summit naturalizing what is actually contingent historical arrangement shaped by Cold War alliances and post-colonial state formation.
constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_cambodian_military_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_cambodian_military_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thai_cambodian_military_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(thai_cambodian_military_asymmetry, TR),
    TR >= 0.70.

:- end_tests(thai_cambodian_military_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The asymmetry extracts from Cambodia through multiple channels: direct military threat (resource diversion to defense), sovereignty limitation (border disputes, cross-border incident management), and geopolitical subordination (constraint on alliance flexibility). However, extraction is not maximal (snare level ≥0.66) because Cambodia maintains agency through alliance diversification and because the constraint has genuine coordination functions (deterrence, borders stability). The rising trajectory (0.42→0.58 over 30 years) reflects intensification of cross-border resource politics and military modernization rather than fundamental change in the asymmetry's nature. Suppression (0.72): High. Cambodia's structural position creates substantial barriers to exit: geographic proximity prevents strategic distance, military capability gap prevents credible deterrence, and great-power alignments constrain independent foreign policy. However, suppression is not total (snare level ≥0.80) because Cambodia has successfully leveraged China alliance as partial counterbalance. Theater_ratio (0.65): Moderate-high. Significant portion of military activity is ritualized threat projection — border exercises, military parades, intelligence sharing ceremonies — that maintains the constraint structure through institutional choreography rather than genuine operational escalation. Rising theater reflects that actual military capability gaps have partially stabilized while institutional performance has intensified. Claimed_type (tangled_rope): The constraint simultaneously coordinates (manages regional power balance, deters opportunistic aggression) and extracts (resource burden, sovereignty limitation). Requires active enforcement through military presence, alliance management, and diplomatic signaling. Both beneficiaries and victims are clearly identified.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the Thai institutional perspective (rope — benign coordination) and the Cambodian victim perspectives (snare — structural trapping / tangled_rope — mixed extraction with constrained agency). Thai military sees the asymmetry as providing stability and deterrence; Cambodian population experiences it as perennial threat and resource burden. Secondary gap exists between the realist analytical perspective (mountain — immutable geopolitical law) and the structural perspective (tangled_rope — contingent arrangement shaped by Cold War alliances and post-colonial legacies). The mountain classification risks naturalizing what is actually a historical contingency. ASEAN institutional perspective (tangled_rope with modest coordination benefit) differs from great-power perspectives (institutional/arbitrage — treat asymmetry as negotiable feature of great-power competition). The perspectival gaps reflect genuine structural differences in exit options and beneficiary status: Thai military has arbitrage (can credibly withdraw or escalate); Cambodia has trapped/constrained (limited exit capacity without geopolitical realignment); regional institutions have mobile (can shift rules and forum focus). These structural differences generate the classificatory diversity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural relationship to the extraction flow and exit capacity. Thai Military Establishment (institutional/arbitrage): d≈0.15 — primary beneficiary with credible exit/threat-withdrawal options (arbitrage), experiences low or negative effective extraction. Thai Regional Elite (powerful/mobile): d≈0.40 — secondary beneficiary but faces domestic political costs and alliance management complexity (mobile exit constrains by political competition, not structural barrier); moderate extraction. Cambodian State Authority (organized/constrained): d≈0.75 — victim subject with constrained but non-zero exit options (alliance realignment possible but costly); high extraction. Cambodian Population (powerless/trapped): d≈0.95 — maximum victimhood, no exit options; maximum experienced extraction. Regional Stability Commons (powerful/mobile): d≈0.58 — symmetric position (both benefits from deterrence, costs from perennial tension); moderate extraction. ASEAN Institutions (institutional/mobile): d≈0.50 — intermediate position (benefits from managing disputes, costs from limited enforcement power); moderate extraction. Analytical Observer (analytical/analytical): d≈0.72 — risks naturalization (false mountain) without cross-position analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolution of mandatrophy requires acknowledging that the asymmetry is neither pure coordination (rope) nor pure extraction (snare) but a hybrid that genuinely serves coordination functions while extracting asymmetrically from victims without alternative capacity. The tangled_rope classification prevents two errors: (1) reducing the constraint to 'natural stability mechanism' (false mountain), which naturalizes contingent arrangements; (2) treating it as pure power domination (snare), which ignores genuine deterrence and alliance-coordination benefits. The Cambodian state's perspective (organized/constrained) is diagnostically crucial: they clearly experience extraction but maintain agency through alliance leverage. If Cambodia were powerless/trapped, the classification would be snare. The fact that organized/constrained produces tangled_rope rather than snare indicates the constraint has genuine mixed character. The six-perspective ensemble demonstrates mandatrophy resolution: no single type is 'correct'; the full perspectival structure IS the constraint's reality. Thai military sees rope (their genuine experience); Cambodian population sees snare (their genuine experience); realist analyst risks mountain (which is structurally false — this is historical contingency, not immutable law). The prescriptive insight: reducing extraction requires either (a) capability transfer to Cambodia (reducing asymmetry), (b) alliance recalibration that increases Cambodia's exit options (increasing agency), or (c) ASEAN institutional strengthening to provide coordination without extraction. Pure military capability equalization is infeasible; institutional coordination improvement is the plausible sunset mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_perception_authenticity,
    'Is the perceived military threat genuine capability asymmetry or ritualized threat inflation maintained through institutional narrative?',
    'Comparative analysis of stated military doctrines vs actual deployment capacity; historical reconstruction of threat escalation cycles; measurement of defense spending allocation to actual vs performative capabilities',
    'If genuine: constraint is immutable structural feature (mountain). If ritualized: constraint is extractive performance (snare/piton). Different resolutions suggest different sunset timelines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_perception_authenticity, empirical, 'Whether military asymmetry represents genuine threat or institutional theater').

omega_variable(
    cambodian_alliance_exit_feasibility,
    'Can Cambodia exit the constraint through alliance recalibration (deepening Vietnam/China ties) without triggering Thai military response?',
    'Scenario analysis of alliance shift consequences; historical precedent from other regional realignments; assessment of Thai military doctrine toward Vietnam-aligned Cambodia',
    'If exit is feasible: exit_options upgrade from trapped/constrained to arbitrage or mobile, classification shifts toward rope/scaffold. If exit triggers response: constraint is snare (suppression confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cambodian_alliance_exit_feasibility, empirical, 'Whether Cambodia can credibly shift alliances to reduce asymmetry').

omega_variable(
    asean_coordination_effectiveness,
    'Do ASEAN institutions provide genuine coordination of the asymmetry or merely ritualistic conflict management theater?',
    'Analysis of ASEAN dispute resolution outcomes; comparison of stated commitment vs actual enforcement; measurement of extraction reduction following ASEAN mechanisms',
    'If effective: ASEAN perspective shifts toward rope (coordination dominates). If ineffective: asymmetry persists as piton (theatrical maintenance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asean_coordination_effectiveness, empirical, 'Whether ASEAN mechanisms reduce or legitimate the asymmetry').

omega_variable(
    bangkok_military_regime_stability,
    'How does Thailand''s internal political instability and coup cycles affect the extraction function of external military asymmetry?',
    'Correlation analysis between coup cycles and border tension escalation; assessment of how military regimes use external threat for domestic legitimation',
    'If regime stability correlates with threat escalation: Thai elite extracting from constraint for domestic political consolidation (snare from Thai perspective). If decoupled: extraction is purely regional power assertion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bangkok_military_regime_stability, empirical, 'Relationship between Thai domestic political instability and external threat projection').

omega_variable(
    regional_economic_integration_trajectory,
    'Can ASEAN economic integration overcome the military asymmetry constraint or does economic interdependence reinforce extractive power dynamics?',
    'Longitudinal analysis of trade patterns, investment flows, and supply chain coupling; assessment of whether economic integration reduces or intensifies military posturing',
    'If integration transcends asymmetry: constraint weakens (scaffold sunset logic applies). If integration intensifies asymmetry: economic coupling amplifies extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_economic_integration_trajectory, empirical, 'Whether regional economic integration resolves or entrenches military asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_cambodian_military_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcma_tr_t0, thai_cambodian_military_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(tcma_tr_t15, thai_cambodian_military_asymmetry, theater_ratio, 15, 0.58).
narrative_ontology:measurement(tcma_tr_t30, thai_cambodian_military_asymmetry, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(tcma_be_t0, thai_cambodian_military_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tcma_be_t15, thai_cambodian_military_asymmetry, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(tcma_be_t30, thai_cambodian_military_asymmetry, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_cambodian_military_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(thai_cambodian_military_asymmetry, southeast_asian_supply_chain_vulnerability).
narrative_ontology:affects_constraint(thai_cambodian_military_asymmetry, mekong_river_basin_resource_asymmetry).
narrative_ontology:affects_constraint(thai_cambodian_military_asymmetry, asean_institutional_coordination_capacity).

% DUAL FORMULATION NOTE:
% The Thai-Cambodian military asymmetry is upstream of multiple downstream constraints: supply-chain vulnerability (exports depend on stable borders), water resources (Mekong dam politics), and institutional coordination capacity (ASEAN effectiveness). The military asymmetry has lower ε than some downstream constraints because it has genuine coordination function; the downstream constraints often show purer extraction (higher ε) because they lack coordination justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(thai_cambodian_military_asymmetry, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
