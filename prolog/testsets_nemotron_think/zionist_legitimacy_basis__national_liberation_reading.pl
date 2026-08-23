% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Zionism as National Liberation Movement for Persecuted Indigenous People Returning to Ancestral Homeland
 *   domain: political/historical/nationalism
 *
 * SUMMARY:
 *   This constraint story instantiates the national_liberation_reading of the
 *   zionist_legitimacy_basis kernel. The reading frames Zionism as the
 *   national liberation movement of the Jewish people — an indigenous nation
 *   returning to its ancestral homeland after two millennia of persecution,
 *   culminating in the Holocaust. The constraint is the Zionist project
 *   itself: the establishment and maintenance of a Jewish state in
 *   Palestine/Israel. This reading claims the constraint is a Mountain
 *   (historical justice, natural law of return). The authored metrics
 *   describe a substantially extractive, actively enforced structure that
 *   displaces Palestinian Arabs. The claim/metric divergence is deliberate:
 *   the reading claims Mountain; the metrics describe Tangled Rope or Snare
 *   operation. The engine computes per-seat classifications from the
 *   structural data; this story provides the national liberation reading's
 *   declared structural data.
 *
 * KEY AGENTS:
 *   - jewish_people: Primary beneficiary (persecuted/indigenous claim) — receives sovereignty, security, return
 *   - palestinian_arabs: Primary payer (displaced) — bears land loss, statelessness, occupation
 *   - zionist_movement: Agenda setter (pre-1948) / israeli_state: Agenda setter (post-1948) — administers the constraint
 *   - british_mandate_authorities: Historical agenda setter — created legal framework (Balfour, Mandate)
 *   - arab_states: Payer — lost wars, absorbed refugees, confront Israeli state
 *   - international_community: Observer — UN, ICC, ICJ, states recognize/condemn variably
 *   - palestinian_refugees: Payer — specific victim class denied return, stateless
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.78).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.82).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, mountain).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionism as National Liberation Movement for Persecuted Indigenous People Returning to Ancestral Homeland").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political/historical/nationalism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).
domain_priors:emerges_naturally(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '89e5afc5-4877-439d-93b1-b8687fa4e2ca').
narrative_ontology:cs_kernel_codification('89e5afc5-4877-439d-93b1-b8687fa4e2ca', formalized).
narrative_ontology:cs_authority_grounding('89e5afc5-4877-439d-93b1-b8687fa4e2ca', lineage).
narrative_ontology:cs_interpretation_layer_present('89e5afc5-4877-439d-93b1-b8687fa4e2ca').
narrative_ontology:cs_reading_relation('89e5afc5-4877-439d-93b1-b8687fa4e2ca', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('89e5afc5-4877-439d-93b1-b8687fa4e2ca', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('89e5afc5-4877-439d-93b1-b8687fa4e2ca', foundational, jewish_indigeneity_and_return_right).
narrative_ontology:cs_axiom_status(jewish_indigeneity_and_return_right, holdable).
narrative_ontology:cs_axiom_grounding('89e5afc5-4877-439d-93b1-b8687fa4e2ca', jewish_indigeneity_and_return_right, deontological).
narrative_ontology:cs_axiom('89e5afc5-4877-439d-93b1-b8687fa4e2ca', foundational, persecution_justifies_sovereign_statehood).
narrative_ontology:cs_axiom_status(persecution_justifies_sovereign_statehood, holdable).
narrative_ontology:cs_axiom_grounding('89e5afc5-4877-439d-93b1-b8687fa4e2ca', persecution_justifies_sovereign_statehood, instrumental).
narrative_ontology:cs_axiom('89e5afc5-4877-439d-93b1-b8687fa4e2ca', secondary, arab_opposition_as_denial_of_jewish_rights).
narrative_ontology:cs_axiom_status(arab_opposition_as_denial_of_jewish_rights, holdable).
narrative_ontology:cs_axiom_grounding('89e5afc5-4877-439d-93b1-b8687fa4e2ca', arab_opposition_as_denial_of_jewish_rights, conventional).
narrative_ontology:cs_reference_frame('89e5afc5-4877-439d-93b1-b8687fa4e2ca', jewish_national_rights_framework).
narrative_ontology:cs_drift_state('89e5afc5-4877-439d-93b1-b8687fa4e2ca', post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89e5afc5-4877-439d-93b1-b8687fa4e2ca', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_people).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, zionist_movement).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_states).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, jewish_indigeneity_to_land_of_israel).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, right_of_return_as_universal_principle).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, national_self_determination_as_remedy_for_persecution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Jewish people, defined as a nation with historical connection to the land, are the primary beneficiaries of the constraint. They receive sovereignty, security, immigration rights (Law of Return), and cultural revival. Their exit is identity-locked: Jewish identity is constituted through the relationship to Zion and the state; leaving the constraint means abandoning the national project that defines contemporary Jewish collective existence. Pre-1948, the Zionist movement (organized) set the agenda; post-1948, the Israeli state administers it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_people, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, jewish_people, agenda_setter).

% The Palestinian Arab people (pre-1948 majority, post-1948 minority in Israel, occupied population in West Bank/Gaza) bear the constraint's extraction: land expropriation (1948, 1967), denial of sovereignty, military occupation, fragmentation, and legal inequality. Their exit options are constrained: they cannot leave the land without abandoning their national claim; they cannot achieve sovereignty while the constraint operates; resistance is met with overwhelming force. The national liberation reading delegitimizes their opposition as 'denial of Jewish rights,' structurally raising the cost of resistance.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs, payer,
    organized, generational, constrained, national).

% The 1948 and 1967 refugees and their descendants (5-7 million) are a distinct victim class: denied return (Absentee Property Law, prevention of physical return), stateless or precariously hosted in Arab states, dependent on UNRWA. Their exit is trapped: no right of return, no citizenship in host states (mostly), no sovereignty. The constraint's enforcement machinery (border control, citizenship law, settlement policy) exists partly to maintain this trap.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% The pre-state Zionist movement (WZO, Jewish Agency, Haganah) set the agenda: land acquisition, immigration, institution-building, diplomatic lobbying (Balfour, UN partition). It had arbitrage-grade exit: could redirect migration (Uganda plan, other territories), shift strategies. Post-1948, its role merges into the Israeli state. The movement's beneficiaries were the Jewish people; its payers were Palestinian Arabs (land sellers under pressure, displaced villagers).
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_movement, agenda_setter,
    organized, generational, arbitrage, global).

% Post-1948, the Israeli state administers the constraint: enforces Law of Return, Absentee Property Law, military occupation, settlement enterprise. It has arbitrage-grade exit: nuclear deterrent, US strategic alliance, OECD economy, global diplomatic integration. It could change the constraint (end occupation, allow return, become binational) but the cost to its Jewish nationalist identity and coalition politics is prohibitive. It collects the extraction (land, water, security control) and distributes it to Jewish citizens.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% The British Mandate (1920-1948) created the legal-administrative framework: Balfour Declaration (1917), Mandate text (1922), White Papers (1939), UN referral (1947). As imperial power, it had arbitrage exit (withdrew 1948). It set the agenda by facilitating Jewish immigration and land purchase while suppressing Palestinian revolt (1936-39). Its withdrawal left the constraint in the hands of the Zionist movement and the nascent Israeli state.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, british_mandate_authorities, agenda_setter,
    institutional, immediate, arbitrage, regional).

% Neighboring Arab states (Egypt, Jordan, Syria, Lebanon, Iraq) bear costs: wars (1948, 1967, 1973), refugee absorption, economic boycott costs, political instability. Their exit is constrained: regional order, US pressure, internal politics prevent normalization without Palestinian resolution (mostly). Some (Egypt, Jordan) exited partially via peace treaties but remain payers via cold peace and refugee burden. The national liberation reading frames their opposition as illegitimate rejectionism.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states, payer,
    institutional, biographical, constrained, regional).

% UN (partition resolution 181, refugee resolution 194, occupation resolutions 242/338), ICJ (2004 Wall advisory opinion, 2024 genocide case), ICC (Palestine investigation), states (recognition, arms trade, diplomacy). Analytical seat: observes full structure, issues legal opinions, applies pressure variably. Exit is analytical: no material stake, but credibility stake in international law. The national liberation reading leverages Holocaust guilt and Western alliance to shape international perception.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, israeli_state).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a sovereign safe haven for the Jewish people, solving the problem of statelessness and persecution through national self-determination in the ancestral homeland. Coordinates immigration, defense, Hebrew revival, and institutional state-building for a globally dispersed nation.
% TRANSFER_FUNCTION: Moves land (from 7% Jewish-owned in 1947 to 93% state-controlled post-1948), sovereignty (from British to Jewish state), water resources, and demographic control from Palestinian Arabs to the Jewish state. The transfer is enforced by military power and legal frameworks (Absentee Property Law, Law of Return, planning laws).
% ABSENT_VOICES: Palestinian Arabs in 1947-48 (rejected partition, had no vote in UN); 1948 refugees (denied return, no representation in armistice agreements); Palestinian citizens of Israel (1948-1966 under military rule, no political voice); Palestinians in occupied territories (1967-present, no vote in Israeli elections that control their fate). These voices are structurally excluded by the constraint's enforcement machinery.
% DISAPPEARANCE_RATIONALE: If the Zionist national liberation constraint vanished overnight: Israeli state would lose its legitimating framework; settlements would lose legal basis; Law of Return would be contested; Palestinian refugees would demand return; regional order would collapse and reorganize. The Middle East political map, US foreign policy, Jewish diaspora identity, and international law of self-determination would all fundamentally rearrange.
% FOUNDING_PROBLEM: The persecution of Jews in Europe (pogroms, legal discrimination, culminating in the Holocaust) and in MENA countries, combined with the statelessness of the Jewish people, created an existential need for a sovereign safe haven and national self-determination in the ancestral homeland.
% FOUNDING_PROBLEM_CORROBORATION: Historical record of European antisemitism and Holocaust corroborated by universal historians and international tribunals. Palestinian and Arab historians corroborate the persecution but contest that the remedy justified displacing an indigenous Arab population. Israeli 'New Historians' (Morris, Pappé, Shlaim) corroborate the displacement occurred but debate its necessity. International law (ICJ, UNHRC) recognizes Jewish right to self-determination but also Palestinian right to self-determination and return — the tension remains unresolved.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, ExtMetricName, E),
    domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zionist_legitimacy_basis__national_liberation_reading),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: The constraint transfers land (78% of Mandate Palestine in 1948, 100% post-1967), sovereignty, water, and resources from Palestinian Arabs to Jewish state. Suppression 0.82: Military enforcement of displacement (1948, 1967), denial of return (Absentee Property Law, Law of Return asymmetry), occupation regime, fragmentation of Palestinian territory. Theater 0.42: Genuine coordination functions exist (state institutions, security, Hebrew revival, ingathering) but growing share of enforcement serves settlement expansion and demographic engineering rather than liberation. Accessibility_collapse 0.78: Binational, federal, or partition-with-full-rights alternatives collapsed by 1948 war and subsequent structural lock-in. Resistance 0.73: Continuous Palestinian resistance (armed, political, legal, cultural) plus international legal challenges. Emerges_naturally=true declared per reading's claim (historical right), triggering FSM evaluation since beneficiaries declared.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish national liberation seat, the constraint is genuine coordination solving persecution — a Mountain of historical justice. From the Palestinian seat, the same structure is enforced displacement — a Snare of settler colonialism. The engine computes this divergence from the declared beneficiaries/victims and exit options. The national liberation reading's claim of Mountain is the false summit hypothesis: it declares natural emergence and beneficiaries, but the metrics show active enforcement and asymmetric extraction. FSM signature should detect this.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish people / zionist_movement are structural beneficiaries (d ~ 0.15): constraint subsidizes them with sovereignty, land, immigration rights, security. Palestinian Arabs / refugees are structural targets (d ~ 0.9): constraint extracts land, sovereignty, mobility, resources; exit options are trapped (refugees) to identity_locked (citizens of Israel) to constrained (West Bank/Gaza). Israeli state is agenda_setter with arbitrage exit (nuclear deterrent, US alliance, economic integration). Arab states are payers with constrained exit (regional order, US pressure). International community is analytical observer. The reading's delegitimization of Arab opposition as 'denial of Jewish rights' structurally raises suppression by framing resistance as illegitimate rather than political.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish persecution and statelessness) is contested: antisemitism persists but Jewish sovereignty exists; the arrangement now extracts from Palestinians beyond the remedial scope. The mandate has partially outlived its function (state exists) but the liberation framework persists to justify ongoing extraction (settlements, occupation). Mandatrophy_resolved=false: the constraint continues to expand extraction (settlements) under the liberation banner.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_claim_vs_continuous_presence,
    'Does Jewish historical connection and sporadic presence constitute indigeneity that overrides continuous Palestinian Arab presence since 7th century?',
    'Comparative analysis of indigeneity criteria across UN frameworks (UNDRIP ILO 169) applied to both Jewish and Palestinian claims; archaeological and demographic history.',
    'If Jewish claim fails indigeneity criteria, the national liberation reading loses its foundational premise; if it passes, the reading gains Mountain-like structural weight despite displacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigeneity_claim_vs_continuous_presence, conceptual, 'Whether the Jewish people meet international legal criteria for indigenous status in Palestine/Israel').

omega_variable(
    persecution_justifies_displacement,
    'Does historical persecution of Jews in Europe and MENA morally and legally justify the displacement of Palestinian Arabs?',
    'Legal analysis of self-determination vs. territorial integrity in international law; moral philosophy of remedial justice; comparative cases (population transfers post-WWII, partition of India).',
    'If persecution does not justify displacement, the reading''s coordination function is structurally extractive (Tangled Rope/Snare); if it does, the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_justifies_displacement, preference, 'Whether the remedial justification for Jewish sovereignty structurally requires Palestinian dispossession').

omega_variable(
    arab_opposition_delegitimization_mechanism,
    'Is the delegitimization of Arab/Palestinian opposition as ''denial of Jewish rights'' a structural feature of the national liberation reading, or a contingent political strategy?',
    'Discourse analysis of Zionist leadership texts 1897-1948; comparison with other national liberation movements'' treatment of local opposition; legal analysis of UN partition debates.',
    'If structural, the reading inherently suppresses Palestinian political agency (high suppression); if contingent, the reading could theoretically accommodate Palestinian self-determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_opposition_delegitimization_mechanism, empirical, 'Whether the national liberation reading''s framing of Arab opposition is internally necessary or externally imposed').

omega_variable(
    kernel_reading_national_liberation,
    'This constraint is the national_liberation_reading of kernel zionist_legitimacy_basis. What would the settler_colonial_reading and religious_restoration_reading change structurally?',
    'Structural comparison of the three readings'' beneficiary/victim sets, coordination functions, and extraction profiles. The settler_colonial_reading centers Palestinian displacement as the core extraction; the religious_restoration_reading centers theological mandate as coordination function.',
    'If readings are structurally distinct constraints (different ε, different victims), they are separate stories linked by network.affects_constraints. If they are perspectival variants of one constraint, the kernel frame is analytical scaffolding only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_national_liberation, conceptual, 'Commitment-system framing: this constraint is one reading of a contested kernel; sibling readings are separate constraints with their own ε and structural data').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zlb_nlr_tr_t1897, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(zlb_nlr_tr_t1917, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(zlb_nlr_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(zlb_nlr_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.45).
narrative_ontology:measurement(zlb_nlr_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.38).
narrative_ontology:measurement(zlb_nlr_tr_t2024, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(zlb_nlr_be_t1897, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement(zlb_nlr_be_t1917, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1917, 0.25).
narrative_ontology:measurement(zlb_nlr_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement(zlb_nlr_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(zlb_nlr_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.75).
narrative_ontology:measurement(zlb_nlr_be_t2024, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(zlb_nlr_su_t1897, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1897, 0.1).
narrative_ontology:measurement(zlb_nlr_su_t1917, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement(zlb_nlr_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(zlb_nlr_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement(zlb_nlr_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.75).
narrative_ontology:measurement(zlb_nlr_su_t2024, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__national_liberation_reading, 0.08).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, palestinian_national_movement).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, israeli_occupation_regime).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, right_of_return_palestinian).

% DUAL FORMULATION NOTE:
% Kernel zionist_legitimacy_basis decomposes into three constraint stories: national_liberation_reading (this file, ε=0.78 claimed Mountain), settler_colonial_reading (ε=0.85 claimed Snare), religious_restoration_reading (ε=0.65 claimed Scaffold/Tangled Rope). They share the same empirical referent (Zionist project) but different structural framings produce different ε and different beneficiary/victim sets. This reading's network edges point to the constraints it structurally enables: Palestinian national movement (as reaction), Israeli occupation regime (as enforcement), right_of_return_palestinian (as counter-claim).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__national_liberation_reading, institutional, 0.15).
constraint_indexing:directionality_override(zionist_legitimacy_basis__national_liberation_reading, organized, 0.85).
constraint_indexing:directionality_override(zionist_legitimacy_basis__national_liberation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
