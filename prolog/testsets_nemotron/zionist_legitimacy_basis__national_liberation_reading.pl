% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionist Legitimacy Basis: National Liberation Reading
 *   domain: political/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story represents the national_liberation_reading of the
 *   contested kernel zionist_legitimacy_basis. The reading frames Zionism as
 *   the national liberation movement of a persecuted indigenous people (Jews)
 *   returning to their ancestral homeland after two millennia of exile and
 *   oppression. The core claim: historical persecution (pogroms, Holocaust,
 *   systemic discrimination in Europe and MENA) combined with continuous
 *   historical-religious connection to the land creates a moral and legal
 *   right to sovereignty that justifies the establishment of a Jewish state.
 *   Arab opposition is framed as rejection of Jewish national rights — often
 *   coded as antisemitism or denial of indigenous return — rather than as a
 *   competing nationalist claim. The constraint operates as a Tangled Rope:
 *   it coordinates Jewish collective action, immigration, state-building, and
 *   international recognition (genuine coordination function for a stateless
 *   people), while simultaneously extracting territory, sovereignty, and
 *   rights from the Palestinian Arab population (asymmetric extraction
 *   requiring active enforcement through military, legal, and demographic
 *   mechanisms). The reading's ε (0.68) reflects substantial extraction — the
 *   displacement of ~750,000 Palestinians in 1948, ongoing occupation since
 *   1967, and structural inequality for Palestinian citizens of Israel —
 *   assessed by this reading's own lights as the cost of liberation. The
 *   sibling readings (settler_colonial_reading,
 *   religious_restoration_reading) are distinct constraints with different ε,
 *   different beneficiary/victim structures, and different types, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - zionist_movement: Primary agenda_setter and beneficiary (institutional/biographical/arbitrage) — builds institutions, organizes immigration, secures international recognition
 *   - israeli_state_institutions: Agenda_setter and beneficiary (institutional/generational/arbitrage) — administers the constraint, controls territory, distributes rights
 *   - jewish_diaspora_identifying_with_national_liberation: Beneficiary (organized/biographical/mobile) — gains homeland option, collective security, identity anchor; exit options vary by country of residence
 *   - palestinian_arab_population: Primary victim (organized/biographical/constrained) — bears displacement, statelessness, military rule; exit blocked by geography and politics
 *   - palestinian_refugees: Victim (powerless/generational/trapped) — intergenerational statelessness, right of return denied, dependency on UNRWA
 *   - palestinian_citizens_of_israel: Victim (moderate/biographical/constrained) — formal citizenship with structural inequality (land, planning, budget); exit possible but costly
 *   - international_diplomatic_community: Observer (institutional/generational/analytical) — legitimizes via UN resolutions, recognition, aid; divided between rights-based and realpolitik frames
 *   - arab_states: Excluded/Observer (institutional/generational/constrained) — initially excluded from partition process; later became negotiation parties but Palestinian agency remained marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.68).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.72).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionist Legitimacy Basis: National Liberation Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36').
narrative_ontology:cs_kernel_codification('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', distributed).
narrative_ontology:cs_authority_grounding('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', lineage).
narrative_ontology:cs_interpretation_layer_present('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36').
narrative_ontology:cs_reading_relation('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', foundational, jewish_indigeneity_to_land_of_israel).
narrative_ontology:cs_axiom_status(jewish_indigeneity_to_land_of_israel, holdable).
narrative_ontology:cs_axiom_grounding('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', jewish_indigeneity_to_land_of_israel, empirically_contingent).
narrative_ontology:cs_axiom('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', foundational, persecution_creates_right_to_sovereign_homeland).
narrative_ontology:cs_axiom_status(persecution_creates_right_to_sovereign_homeland, holdable).
narrative_ontology:cs_axiom_grounding('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', persecution_creates_right_to_sovereign_homeland, deontological).
narrative_ontology:cs_axiom('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', secondary, arab_opposition_is_denial_of_jewish_rights).
narrative_ontology:cs_axiom_status(arab_opposition_is_denial_of_jewish_rights, holdable).
narrative_ontology:cs_axiom_grounding('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', arab_opposition_is_denial_of_jewish_rights, conventional).
narrative_ontology:cs_reference_frame('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', jewish_national_return_post_exile).
narrative_ontology:cs_drift_state('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', post_1967_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60bfdcc8-6189-4aa8-b0fb-7a0d8bf43d36', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, zionist_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_identifying_with_national_liberation).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, jewish_national_self_determination).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, historical_connection_justifies_sovereignty).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, persecution_creates_right_to_homeland).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The organized political movement (WZO, Jewish Agency, pre-state institutions) that built the infrastructure of immigration, settlement, defense, and diplomacy. It sets the agenda (partition acceptance, declaration of independence, absorption policies) and collects the primary gains: sovereignty, institutional control, narrative authority. Its exit options are maximal — it could have accepted partition, binationalism, or other frameworks — but chose the sovereign state path. It administers the constraint's enforcement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_movement, agenda_setter,
    institutional, generational, arbitrage, global).

% The sovereign state apparatus (government, military, courts, bureaucracy) that succeeded the pre-state movement. It administers the constraint daily: controls territory, allocates land, enforces citizenship law, manages occupation, defines who belongs. It collects the extraction (land, water, demographic control, security resources) and distributes it to the beneficiary populations. It has arbitrage-grade exit — it could change policies, accept two-state solutions, equalize citizenship — but the constraint's logic (security, demographic majority, ideological commitment) makes structural change costly.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Jewish communities worldwide who identify with Israel as their national liberation homeland. They gain: right of return (Law of Return), collective security guarantee, cultural center, identity anchor, political advocacy vehicle. They bear diffuse costs: association with Israeli policies, antisemitism spikes during conflicts, communal divisions. Exit is mobile — they can disengage politically, emigrate to Israel, or remain diaspora critics; the constraint does not force their participation but structures their communal options.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_identifying_with_national_liberation, beneficiary,
    organized, biographical, mobile, global).

% The Arab population of Mandate Palestine and their descendants in West Bank, Gaza, and diaspora. They bear the primary extraction: 1948 displacement (~750,000), 1967 occupation, ongoing land expropriation, movement restrictions, denial of return, statelessness. They have organized national representation (PLO/PA) but constrained exit — geography, politics, and the constraint's enforcement machinery block meaningful mobility. Their resistance (intifadas, diplomacy, BDS, sumud) meets high suppression.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_population, payer,
    organized, biographical, constrained, national).

% Descendants of 1948 and 1967 displaced persons, holding UNRWA refugee status, denied right of return, stateless in host countries (Lebanon, Syria, Jordan, Gaza, West Bank). Intergenerational trap: no citizenship, no return, no integration, dependent on international aid. The constraint's enforcement (denial of return, demographic engineering) targets them directly. Exit is structurally blocked — they are the constraint's primary extraction residue.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Palestinians who remained in 1948 and gained Israeli citizenship. Formal rights (vote, Knesset representation) but structural inequality: land allocation (JNF, absentee property law), planning restrictions, budget discrimination, nation-state law (2018) declaring Jewish self-determination exclusive. They bear extraction (second-class citizenship) with constrained exit — can leave but lose homeland; can resist politically but face suppression. Their situation reveals the constraint's internal extraction logic.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, local).

% UN, major powers, EU, Arab League, international NGOs. They legitimized the constraint (Partition Resolution 181, recognition of Israel, Oslo process) but also contest its expansion (settlements, occupation, apartheid findings). They experience the constraint as an external stability problem — managing conflict, refugees, regional security. Their analytical exit is full; they are neither beneficiaries nor payers but their recognition/confrontation shapes the constraint's enforcement capacity.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_diplomatic_community, observer,
    institutional, generational, analytical, global).

% Neighboring states (Egypt, Jordan, Syria, Lebanon, Gulf) initially excluded from the partition decision, became war parties, then negotiation parties (Camp David, Oslo, Abraham Accords). They bear costs (refugees, wars, instability) and gain occasional benefits (US aid, normalization). Palestinian agency was marginalized in their diplomacy — they were excluded from the constraint's core framing (Jewish rights vs. Arab rejection). Their exit is constrained by regional politics and US hegemony.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states, excluded,
    institutional, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the Jewish people's statelessness and vulnerability by establishing a sovereign state with immigration control, defense capacity, and international recognition — a single trusted homeland instead of diaspora dependence on host majorities.
% TRANSFER_FUNCTION: Moves land, water, sovereignty, demographic control, and narrative authority from the Palestinian Arab population to Jewish national institutions (pre-state and post-state), as the price of Jewish national liberation.
% ABSENT_VOICES: Palestinian peasants and urban notables (1917-1948) who were not consulted on Balfour, partition, or displacement — their opposition was coded as 'riots' or 'rejectionism' rather than legitimate nationalist claim. Mizrahi Jews (from MENA countries) whose indigenous Middle Eastern identity was subsumed into the European Zionist framework — they were beneficiaries of the state but their distinct colonial/indigenous experience was erased. Internal Jewish critics (Bundists, cultural Zionists, anti-Zionist ultra-Orthodox) who argued for diaspora nationalism, binationalism, or religious quietism — excluded from the liberation framework as 'traitors' or 'assimilationists.'
% DISAPPEARANCE_RATIONALE: If the national liberation constraint vanished overnight: Israeli state institutions would lose their founding legitimacy logic (though not necessarily their power); Palestinian refugees would gain legal path to return; settlements would lose ideological justification; the Jewish diaspora-homeland relationship would shift from 'centrality of Israel' to voluntary affiliation; regional security architecture would reorganize around new legitimacy claims. The world would rearrange because the constraint structures the primary political cleavage of the region.
% FOUNDING_PROBLEM: The persecution of Jews in Europe (pogroms, legal discrimination, Holocaust) and MENA (dhimmi status, expulsions) combined with the failure of emancipation/assimilation to provide security — the Jewish people as a stateless nation required a sovereign homeland for collective self-defense and cultural survival.
% FOUNDING_PROBLEM_CORROBORATION: The persecution founding problem is historically corroborated by non-Zionist sources: European antisemitism documentation, Holocaust historiography, MENA Jewish expulsion records. However, whether the founding problem *remains live* (requiring the current constraint configuration) is contested: Israeli historians (Benny Morris, Tom Segev) and international legal scholars attest the core vulnerability was resolved by 1948 statehood; the Zionist establishment attests ongoing existential threat justifies continued constraint; Palestinian and critical Israeli voices attest the founding problem was solved but the constraint expanded into extraction. Corroboration for 'contested' status comes from the existence of these divergent attested positions outside the beneficiary set.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers land, sovereignty, and demographic control from Palestinians to Jewish national institutions — assessed by this reading as the necessary cost of liberation, not as rent. Suppression (0.72) is high because the constraint's persistence requires active enforcement: military occupation, legal restrictions on return, demographic engineering, and ideological suppression of competing narratives. Theater ratio (0.28) is moderate-low: the liberation coordination function (ingathering, state-building, defense) is genuine and substantial, but a growing share of enforcement activity (settlements, nation-state law, judicial reform) serves extraction/consolidation rather than the original coordination. Accessibility collapse (0.65) reflects that alternatives (binational state, partition with full refugee return, confederation) become structurally invisible once the 'return of indigenous people' frame is accepted — the frame itself closes the option space. Resistance (0.62) is high: Palestinian national movement, international solidarity, legal challenges, and internal Israeli dissent all contest the constraint. The measurement series (1897-2024) shows rising extraction (0.35→0.68) as the movement gains state power and the coordination function shifts from 'building a home' to 'controlling the whole land'; rising suppression (0.3→0.72) as enforcement machinery expands; rising theater (0.15→0.28) as liberation rhetoric increasingly covers consolidation. All metrics share the same time grid.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat classifications from the structural data. From the agenda_setter seats (zionist_movement, israeli_state_institutions), the constraint computes as Rope or low-extraction Tangled Rope — genuine coordination for a persecuted people, extraction framed as necessary cost. From the victim seats (palestinian_arab_population, palestinian_refugees), it computes as Snare or high-extraction Tangled Rope — displacement and denial of return are the point, not a side effect. From the beneficiary-with-exit seat (jewish_diaspora), it computes near symmetric — real benefit, diffuse cost. From observer seats, it computes as contested Tangled Rope — both coordination and extraction are visible. This seat divergence IS the measurement; the authored claimed_type (tangled_rope) is the authoring-seat assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   The zionist_movement and israeli_state_institutions are structural beneficiaries (d ≈ 0.15-0.25): they collect the gains (territory, sovereignty, resources, narrative control) and control the rules. Jewish_diaspora_identifying_with_national_liberation are beneficiaries with more exit (d ≈ 0.3-0.4): they gain identity/homeland option but don't directly administer extraction. Palestinian_arab_population and palestinian_refugees are full targets (d ≈ 0.85-0.95): they bear the costs (displacement, statelessness, occupation) with minimal exit. Palestinian_citizens_of_israel are targets with partial exit (d ≈ 0.6-0.7): they bear structural inequality but have citizenship rights and some mobility. International_diplomatic_community and arab_states are observers/excluded with analytical or constrained exit — they experience the constraint's externalities but don't directly pay or collect. The directionality derivation follows from beneficiary/victim declarations + exit options + power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (persecution of Jews requiring national self-determination) was live in 1897-1948. By 1948, statehood was achieved — the core coordination problem (statelessness, vulnerability) was substantially solved. The constraint persisted and expanded (1967, settlements, nation-state law) because the liberation framework had no sunset clause and no internal mechanism to declare 'mission accomplished.' The mandate atrophied: the liberation movement became a sovereign state that no longer needed the same coordination structure, but the constraint's enforcement machinery (military, legal, demographic) was repurposed for territorial consolidation. This is classic mandatrophy — a Rope that became a Tangled Rope when the founding problem was solved but the arrangement persisted and expanded extraction. The reading's own tradition contains voices (early cultural Zionists, post-Zionists, some religious Zionists) that identified this drift, but they were marginalized by the dominant institutional trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Is the national liberation claim a structural feature of international law/morality, or a constructed justification that benefits identifiable agents?',
    'Comparative analysis of decolonization precedents where indigeneity claims were recognized vs. rejected; examination of whether the ''return after exile'' framework has consistent application or is exceptionalized for this case.',
    'If constructed, the constraint operates as a false summit — Mountain claim masking Tangled Rope extraction. If structural, the coordination function is genuine and the extraction is the price of liberation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, conceptual, 'Whether the national liberation framing reflects natural law or serves as cover for extraction').

omega_variable(
    kernel_reading_identity,
    'This constraint is the national_liberation_reading of kernel zionist_legitimacy_basis. What would the sibling readings (settler_colonial_reading, religious_restoration_reading) change structurally?',
    'Map the structural deltas: settler_colonial_reading shifts victims to primary structural position and beneficiaries to colonial agents; religious_restoration_reading shifts authority_grounding to theological and alters the temporal horizon to messianic.',
    'Sibling readings produce different ε, different beneficiary/victim sets, different claimed_type — confirming these are distinct constraints linked by network.affects_constraints, not one constraint with measurement variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this reading''s structural distinctness from sibling readings of the same kernel').

omega_variable(
    displacement_justification_mechanism,
    'Does the national liberation framework structurally require displacement of the existing population, or is displacement a contingent historical outcome?',
    'Analyze whether the ''land without a people'' / ''return to empty homeland'' premise is logically necessary to the liberation claim or a separable empirical assertion that could be falsified without dissolving the claim.',
    'If displacement is structurally necessary, the constraint''s extraction from Palestinians is intrinsic to its coordination function — Tangled Rope with high asymmetric extraction. If contingent, a non-displacement liberation path was structurally possible and the extraction is avoidable overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_justification_mechanism, conceptual, 'Whether displacement is intrinsic to the national liberation claim or a contingent historical choice').

omega_variable(
    arab_opposition_delegitimation,
    'Is the delegitimation of Arab opposition as ''denial of Jewish rights'' a structural feature of this reading''s coordination function, or an escalation that could be separated from the core claim?',
    'Trace the rhetorical and institutional history: when and how did opposition to Zionist claims become coded as antisemitism/denial of rights vs. legitimate nationalist competition? Compare with other national liberation movements'' treatment of local opposition.',
    'If structurally necessary, the constraint''s suppression (0.72) includes ideological suppression of rival nationalist claims — the coordination function requires not just territory but narrative monopoly. If separable, the suppression is escalation, not essence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_opposition_delegitimation, empirical, 'Whether delegitimation of Arab opposition is intrinsic to the national liberation reading or an escalation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_tr_t1897, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1897, 0.15).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_tr_t1917, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1917, 0.18).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_tr_t1947, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1947, 0.22).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.27).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.28).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_tr_t2024, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_be_t1897, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1897, 0.35).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_be_t1917, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1917, 0.42).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_be_t1947, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1947, 0.55).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.62).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.66).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_be_t2024, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_su_t1897, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1897, 0.3).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_su_t1917, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1917, 0.45).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_su_t1947, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1947, 0.6).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.71).
narrative_ontology:measurement(zionist_legitimacy_basis__national_liberation_reading_su_t2024, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__national_liberation_reading, 0.08).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, palestinian_national_movement_constraint).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, international_law_of_decolonization).

% DUAL FORMULATION NOTE:
% This constraint family (zionist_legitimacy_basis kernel) decomposes the colloquial label 'Zionism's legitimacy' into three structurally distinct constraints with different ε values: national_liberation_reading (ε≈0.68, Tangled Rope), settler_colonial_reading (ε≈0.82, Snare), religious_restoration_reading (ε≈0.45, Scaffold with theological sunset). The national_liberation_reading is upstream — its claim (persecution + historical connection = right to sovereignty) is often cited as evidence by the religious_restoration_reading, and its outcome (state establishment) is the referent the settler_colonial_reading analyzes as colonial displacement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__national_liberation_reading, institutional, 0.2).
constraint_indexing:directionality_override(zionist_legitimacy_basis__national_liberation_reading, organized, 0.35).
constraint_indexing:directionality_override(zionist_legitimacy_basis__national_liberation_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
