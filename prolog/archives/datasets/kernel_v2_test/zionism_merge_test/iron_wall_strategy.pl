% ============================================================================
% CONSTRAINT STORY: iron_wall_strategy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iron_wall_strategy, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iron_wall_strategy
 *   human_readable: Iron Wall Strategy: Military Supremacy as Prerequisite for Political Settlement
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The Iron Wall doctrine, articulated by Ze'ev Jabotinsky in 1923 and
 *   adopted by Labor Zionism after the 1936-1939 Arab Revolt, established
 *   military supremacy as the prerequisite for any political settlement with
 *   Palestinian Arabs. The doctrine explicitly theorized that only after
 *   Arabs despaired of eliminating the Jewish presence would they accept it
 *   and negotiate. This constraint exhibits the full complexity of tangled
 *   rope classification: it contains both a genuine coordination function
 *   (military capacity was structurally necessary for state establishment in
 *   a hostile regional environment) and substantial extraction (the
 *   permanence of military supremacy beyond initial state-building, the
 *   explicit targeting of despair as mechanism, the foreclosure of diplomatic
 *   alternatives). The doctrine's persistence after achieving its stated goal
 *   (state establishment in 1948) and its expansion after 1967 reveal the
 *   extraction component. The theater_ratio remains relatively low
 *   (0.35-0.42) because the military capacity is functional, not performative
 *   — the doctrine delivers on its promise of military supremacy. The
 *   extraction lies not in theatrical enforcement but in the structural
 *   foreclosure of alternatives and the permanence of the asymmetry.
 *
 * KEY AGENTS:
 *   - Palestinian Arab Population: Primary victim (powerless/trapped) — bears maximum extraction through land dispossession, political agency denial, and systematic military pressure designed to induce despair
 *   - Arab Political Leadership: Secondary victim (moderate/constrained) — forced to negotiate from position of induced despair; retains some diplomatic agency but constrained by military asymmetry
 *   - Zionist Military Establishment: Primary beneficiary (institutional/arbitrage) — captures resource allocation, institutional priority, and political influence through doctrine's permanence
 *   - Labor Zionist Leadership: Mixed position (institutional/constrained) — adopted doctrine after 1936 Arab Revolt; benefits from strategic framework but constrained by foreclosure of diplomatic alternatives
 *   - International Diplomatic Community: Organized actors (organized/mobile) — sees doctrine as temporary phase requiring transitional framework; mobile exit through diplomatic disengagement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies both genuine coordination function (state establishment required military capacity) and substantial extraction (permanence beyond initial necessity, despair-targeting mechanism)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iron_wall_strategy, 0.78).
domain_priors:suppression_score(iron_wall_strategy, 0.82).
domain_priors:theater_ratio(iron_wall_strategy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iron_wall_strategy, extractiveness, 0.78).
narrative_ontology:constraint_metric(iron_wall_strategy, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(iron_wall_strategy, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iron_wall_strategy, tangled_rope).
narrative_ontology:human_readable(iron_wall_strategy, "Iron Wall Strategy: Military Supremacy as Prerequisite for Political Settlement").
narrative_ontology:topic_domain(iron_wall_strategy, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(iron_wall_strategy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(iron_wall_strategy, '6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9').
narrative_ontology:cs_kernel_codification('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', formalized).
narrative_ontology:cs_authority_grounding('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', lineage).
narrative_ontology:cs_interpretation_layer_present('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9').
narrative_ontology:cs_reading_relation('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', iron_wall_strategy__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', iron_wall_strategy__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', foundational, persecution_driven_defensive_necessity).
narrative_ontology:cs_axiom_status(persecution_driven_defensive_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', persecution_driven_defensive_necessity, empirically_contingent).
narrative_ontology:cs_axiom('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', foundational, military_supremacy_precedes_acceptance).
narrative_ontology:cs_axiom_status(military_supremacy_precedes_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', military_supremacy_precedes_acceptance, instrumental).
narrative_ontology:cs_axiom('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', secondary, despair_inducement_legitimate_mechanism).
narrative_ontology:cs_axiom_status(despair_inducement_legitimate_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', despair_inducement_legitimate_mechanism, instrumental).
narrative_ontology:cs_reference_frame('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', defensive_necessity_framework).
narrative_ontology:cs_drift_state('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', post_1967_territorial_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6cd177ea-5d1e-4ed6-a2e6-0e0441490fc9', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iron_wall_strategy, zionist_military_establishment).
narrative_ontology:constraint_beneficiary(iron_wall_strategy, jewish_settlement_enterprise).
narrative_ontology:constraint_beneficiary(iron_wall_strategy, labor_zionist_leadership).
narrative_ontology:constraint_victim(iron_wall_strategy, palestinian_arab_population).
narrative_ontology:constraint_victim(iron_wall_strategy, arab_political_agency).
narrative_ontology:constraint_victim(iron_wall_strategy, regional_diplomatic_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARAB POPULATION (SNARE) — Trapped by military asymmetry with no exit options. The doctrine explicitly targets their despair as the mechanism for acceptance. Maximum extraction: land dispossession, political agency denial, and systematic military pressure designed to break resistance. The coordination story (security necessity) is cover for displacement.
constraint_indexing:constraint_classification(iron_wall_strategy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ARAB POLITICAL LEADERSHIP (TANGLED ROPE) — Constrained by military inferiority but retains some diplomatic agency. Experiences both genuine security coordination problem (regional stability requires some framework) and asymmetric extraction (forced to negotiate from position of induced despair). The doctrine structures the negotiation space itself as an extraction mechanism.
constraint_indexing:constraint_classification(iron_wall_strategy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ZIONIST MILITARY ESTABLISHMENT (ROPE) — Primary beneficiary with arbitrage-level exit options. Experiences the doctrine as coordination: building military capacity solves the genuine security problem of establishing a state in hostile territory. Extraction flows toward this agent through resource allocation, institutional priority, and political influence.
constraint_indexing:constraint_classification(iron_wall_strategy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL DIPLOMATIC COMMUNITY (SCAFFOLD) — Organized actors (UN, regional powers, international mediators) see the military asymmetry as temporary condition requiring transitional framework. The doctrine is understood as phase in conflict resolution, not permanent structure. Mobile exit options through diplomatic disengagement. Sunset logic: military supremacy should eventually yield to negotiated settlement.
constraint_indexing:constraint_classification(iron_wall_strategy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: LABOR ZIONIST LEADERSHIP POST-1936 (TANGLED ROPE) — Institutional power but constrained by adoption of Jabotinsky's doctrine after rejecting it initially. Experiences both coordination (the doctrine provides strategic framework for state-building) and extraction (commits resources to permanent military posture, forecloses diplomatic alternatives). The 1936 Arab Revolt forced adoption of a doctrine that structures all subsequent political options.
constraint_indexing:constraint_classification(iron_wall_strategy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the doctrine exhibits both genuine coordination function (military capacity was structurally necessary for state establishment in 1948) and substantial extraction (the permanence of the doctrine beyond initial state-building phase, the explicit targeting of despair as mechanism, the foreclosure of diplomatic alternatives). The doctrine's persistence after achieving its stated goal (state establishment) reveals the extraction component.
constraint_indexing:constraint_classification(iron_wall_strategy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iron_wall_strategy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iron_wall_strategy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iron_wall_strategy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iron_wall_strategy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(iron_wall_strategy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The doctrine extracts from Palestinian Arabs through land dispossession, political agency denial, and systematic military pressure. The extraction increased over the interval: initial articulation (0.55) reflected theoretical framework; 1936 adoption (0.68) reflected operational implementation; 1948 state establishment (0.78) reflected achievement of initial goal; 1967 expansion (0.82) reflected doctrine's extension beyond defensive necessity; Oslo process (0.85) revealed permanence despite diplomatic framework; contemporary period (0.88) shows full institutionalization. The coordination function (military capacity for state establishment) was genuine but time-limited; the extraction function (permanent asymmetry, despair-targeting) persists. Suppression (0.82): Very high. The doctrine systematically suppresses alternatives through military dominance, foreclosure of diplomatic options, and explicit targeting of despair as acceptance mechanism. Suppression increased over interval as military capacity expanded and alternatives were foreclosed. Theater_ratio (0.35): Moderate-low. The military capacity is functional, not performative — the doctrine delivers on its promise of military supremacy. The theater component reflects the gap between stated defensive necessity and actual territorial expansion, and the persistence of the doctrine after achieving its initial goal.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by structural position. The Palestinian Arab population experiences pure extraction (Snare) — trapped by military asymmetry with the doctrine explicitly targeting their despair. Arab political leadership experiences mixed coordination-extraction (Tangled Rope) — constrained by military inferiority but retaining some diplomatic agency. The Zionist military establishment experiences coordination (Rope) — the doctrine solves their genuine security problem and channels resources toward them. International diplomatic community sees temporary framework (Scaffold) — military asymmetry should yield to negotiated settlement. Labor Zionist leadership experiences mixed coordination-extraction (Tangled Rope) — benefits from strategic framework but constrained by foreclosure of alternatives. The analytical observer identifies both genuine coordination function (military capacity was necessary for state establishment) and substantial extraction (permanence beyond necessity, despair-targeting mechanism). The gap between Snare (powerless/trapped) and Rope (institutional/arbitrage) is maximal — the same doctrine that appears as pure extraction from below appears as pure coordination from above.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab population: Full victim status + trapped exit → maximum directionality (d ≈ 0.95) → maximum effective extraction. The doctrine explicitly targets their despair. Arab political leadership: Victim status + constrained exit → high directionality (d ≈ 0.75) → high effective extraction, modulated by retained diplomatic agency. Zionist military establishment: Primary beneficiary + arbitrage exit → low directionality (d ≈ 0.15) → negative effective extraction (subsidy). Labor Zionist leadership: Mixed beneficiary-victim + constrained exit → moderate directionality (d ≈ 0.45) → moderate effective extraction. International diplomatic community: Neither clear beneficiary nor victim + mobile exit → neutral directionality (d ≈ 0.50) → baseline effective extraction. Analytical observer: Analytical context → derived from structural analysis rather than experienced position.
 *
 * MANDATROPHY ANALYSIS:
 *   The Iron Wall doctrine resolves mandatrophy by demonstrating that tangled rope classification captures constraints with both genuine coordination function AND substantial extraction operating simultaneously. The doctrine was not pure coordination (Rope) — it explicitly targeted despair and foreclosed alternatives. It was not pure extraction (Snare) — military capacity was genuinely necessary for state establishment in hostile environment. The coordination function was real but time-limited (1920s-1948); the extraction function persists (1948-present). The doctrine's classification depends on temporal scope: at biographical horizon during state-building phase, coordination function dominates; at generational horizon after state establishment, extraction function dominates. The mandatrophy is resolved by recognizing that the same structural mechanism can serve coordination at one phase and extraction at another, and that the perspectival gap (Snare from below, Rope from above) is itself diagnostic of tangled rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    despair_threshold_ambiguity,
    'What level of induced despair constitutes the threshold for ''acceptance'' versus radicalization and perpetual resistance?',
    'Historical analysis of resistance movements under sustained military pressure; comparison of negotiated settlements following military defeat versus those following mutual exhaustion or third-party mediation',
    'If despair induces acceptance: doctrine is effective coordination mechanism (more Rope perspectives). If despair induces radicalization: doctrine is self-perpetuating extraction mechanism (more Snare perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(despair_threshold_ambiguity, empirical, 'Whether induced despair produces acceptance or radicalization').

omega_variable(
    security_necessity_vs_expansion_cover,
    'Is the doctrine''s military supremacy requirement a genuine security necessity or a cover story for territorial expansion?',
    'Analysis of territorial acquisition patterns relative to security perimeter requirements; comparison of military expenditure to defensive versus offensive capabilities; examination of settlement patterns in relation to strategic depth claims',
    'If genuine security necessity: coordination function is primary (Tangled Rope from more perspectives). If expansion cover: extraction is primary (Snare from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_necessity_vs_expansion_cover, conceptual, 'Whether military supremacy serves security or expansion').

omega_variable(
    alternative_pathway_foreclosure,
    'Did the doctrine foreclose viable diplomatic alternatives that existed in 1920s-1930s, or was military supremacy the only structurally available path to state establishment?',
    'Counterfactual analysis of binational state proposals, British partition plans, and Arab-Jewish cooperation initiatives; assessment of whether these were structurally viable or politically impossible given demographic and territorial realities',
    'If alternatives were viable: doctrine is extractive foreclosure (Snare from more perspectives). If alternatives were structurally impossible: doctrine is coordination under constraint (Rope/Tangled Rope from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_pathway_foreclosure, conceptual, 'Whether diplomatic alternatives were structurally viable').

omega_variable(
    reading_under_determination,
    'Does the ''historical right'' kernel ground the Iron Wall as national liberation necessity (persecution-driven defensive posture) or as settler-colonial displacement logic (military supremacy enables land acquisition)?',
    'The national_liberation_reading frames military supremacy as defensive necessity against existential threat; the settler_colonial_reading frames it as offensive mechanism for indigenous displacement. The kernel''s ambiguity allows both readings to claim the same historical warrant.',
    'National liberation reading: coordination function is primary, extraction is incidental to survival. Settler colonial reading: extraction is primary, coordination story is cover. The kernel''s contested status means the doctrine''s classification depends on which reading the observer holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_under_determination, conceptual, 'Which kernel reading determines the doctrine''s structural classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iron_wall_strategy, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iron_wall_theater_1920, iron_wall_strategy, theater_ratio, 0, 0.25).
narrative_ontology:measurement(iron_wall_theater_1936, iron_wall_strategy, theater_ratio, 16, 0.28).
narrative_ontology:measurement(iron_wall_theater_1948, iron_wall_strategy, theater_ratio, 28, 0.3).
narrative_ontology:measurement(iron_wall_theater_1967, iron_wall_strategy, theater_ratio, 47, 0.35).
narrative_ontology:measurement(iron_wall_theater_1993, iron_wall_strategy, theater_ratio, 73, 0.38).
narrative_ontology:measurement(iron_wall_theater_2020, iron_wall_strategy, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(iron_wall_extract_1920, iron_wall_strategy, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(iron_wall_extract_1936, iron_wall_strategy, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(iron_wall_extract_1948, iron_wall_strategy, base_extractiveness, 28, 0.78).
narrative_ontology:measurement(iron_wall_extract_1967, iron_wall_strategy, base_extractiveness, 47, 0.82).
narrative_ontology:measurement(iron_wall_extract_1993, iron_wall_strategy, base_extractiveness, 73, 0.85).
narrative_ontology:measurement(iron_wall_extract_2020, iron_wall_strategy, base_extractiveness, 100, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(iron_wall_suppress_1920, iron_wall_strategy, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(iron_wall_suppress_1936, iron_wall_strategy, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(iron_wall_suppress_1948, iron_wall_strategy, suppression_requirement, 28, 0.75).
narrative_ontology:measurement(iron_wall_suppress_1967, iron_wall_strategy, suppression_requirement, 47, 0.82).
narrative_ontology:measurement(iron_wall_suppress_1993, iron_wall_strategy, suppression_requirement, 73, 0.85).
narrative_ontology:measurement(iron_wall_suppress_2020, iron_wall_strategy, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iron_wall_strategy, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The Iron Wall doctrine is downstream of british_mandate_scaffolding (the British Mandate provided the institutional framework within which Zionist military capacity developed) but represents a distinct structural constraint with its own extractiveness profile. The upstream constraint's sunset (British withdrawal 1948) did not sunset the Iron Wall doctrine — instead, the doctrine persisted and intensified after achieving its initial goal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
