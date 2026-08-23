% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis (Settler-Colonial Reading)
 *   domain: political/historical/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of the
 *   contested kernel 'Zionist legitimacy basis.' The reading asserts that
 *   Zionism is structurally a European settler-colonial movement that
 *   established an ethno-state through the displacement of the indigenous
 *   Palestinian people — and that this displacement is constitutive of the
 *   project, not incidental. The constraint is the legitimacy claim itself:
 *   that Zionism has a legitimate basis to establish Jewish sovereignty in
 *   Palestine. From this reading, the constraint operates as a snare: pure
 *   extraction where the coordination story (Jewish refuge, return to
 *   homeland) functions as cover for the ongoing displacement, enclosure, and
 *   elimination of the native population. The constraint requires active
 *   enforcement (military occupation, legal apartheid, narrative control),
 *   suppresses alternatives (one-state, binationalism, right of return), and
 *   has identifiable victims (the Palestinian people in all their segments).
 *   The claimed_type is 'snare' from this reading's analytical seat; the
 *   engine will compute per-seat types from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.85).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political/historical/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'fc80a95c-a444-4f94-8352-b36f676ead20').
narrative_ontology:cs_kernel_codification('fc80a95c-a444-4f94-8352-b36f676ead20', distributed).
narrative_ontology:cs_authority_grounding('fc80a95c-a444-4f94-8352-b36f676ead20', extraction).
narrative_ontology:cs_interpretation_layer_present('fc80a95c-a444-4f94-8352-b36f676ead20').
narrative_ontology:cs_reading_relation('fc80a95c-a444-4f94-8352-b36f676ead20', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('fc80a95c-a444-4f94-8352-b36f676ead20', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('fc80a95c-a444-4f94-8352-b36f676ead20', foundational, zionism_is_european_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_is_european_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('fc80a95c-a444-4f94-8352-b36f676ead20', zionism_is_european_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('fc80a95c-a444-4f94-8352-b36f676ead20', foundational, palestinian_displacement_is_constitutive).
narrative_ontology:cs_axiom_status(palestinian_displacement_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('fc80a95c-a444-4f94-8352-b36f676ead20', palestinian_displacement_is_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('fc80a95c-a444-4f94-8352-b36f676ead20', settler_colonial_analysis_framework).
narrative_ontology:cs_drift_state('fc80a95c-a444-4f94-8352-b36f676ead20', contemporary_settler_colonial_scholarship, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fc80a95c-a444-4f94-8352-b36f676ead20', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, zionist_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_people).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinians_under_occupation).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, settler_colonial_theory).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, indigenous_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces the legal, military, and diplomatic framework that sustains Jewish demographic majority and territorial control. Administers land allocation, citizenship law, military occupation, and international representation. Collects the primary gains of the constraint (sovereignty, territory, resources, international legitimacy) while bearing security and diplomatic costs that are externalized to the constrained population.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive subsidized land, housing, infrastructure, and legal protections in settlements across the West Bank and East Jerusalem. Their presence is incentivized by state policy; they participate in and benefit from the displacement regime but do not administer it. Exit would mean abandoning subsidized livelihoods and ideological commitment; many hold dual citizenship providing arbitrage-grade exit individually but collective exit is constrained.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_settlers, beneficiary,
    organized, biographical, constrained, regional).

% Include the Jewish Agency, World Zionist Organization, Jewish National Fund, and major diaspora organizations (AIPAC, ADL, etc.). They channel funding, immigration, and political advocacy to sustain the settlement enterprise. They collect organizational survival, relevance, and resource flows from the constraint's operation. Exit is mobile — they could pivot to other Jewish communal priorities — but institutional identity is fused to the Zionist project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_institutions, beneficiary,
    organized, generational, mobile, global).

% The indigenous population subjected to the settler-colonial constraint across historic Palestine. Bear the extraction of land (93% within Green Line, expanding in West Bank), water, movement rights, political sovereignty, and physical security. Resistance is met with disproportionate force. Exit options are structurally blocked: Gaza is besieged, West Bank is fragmented, refugees are denied return, citizens of Israel face legal discrimination. Identity is fused to the land — exit means erasure.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Descendants of the 750,000+ displaced in 1948 and 300,000+ in 1967, now numbering over 7 million. Denied right of return under UNGA 194 while Jewish return is enshrined in law. Confined to camps in Lebanon, Jordan, Syria, Gaza, West Bank — stateless, aid-dependent, politically excluded. Their situation is the constitutive outside of the constraint: the constraint's legitimacy requires their permanent exclusion.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, global).

% 2.5+ million in West Bank, 2+ million in Gaza under direct military rule. Subject to permit regime, administrative detention, home demolitions, settlement expansion, resource theft. No citizenship, no vote, no recourse to Israeli courts that consistently uphold the occupation. Exit is individually impossible (permits denied) and collectively blocked (borders controlled by occupier).
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinians_under_occupation, payer,
    powerless, biographical, trapped, local).

% UN bodies, ICC, ICJ, EU, major states. Formally uphold international law (Fourth Geneva Convention, UNSC 242/338, ICJ 2024 advisory opinion) but politically sustain the constraint through diplomatic cover, arms transfers, trade agreements, and refusal to enforce accountability. Their analytical seat sees the full structure; their institutional role maintains it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_community, observer,
    institutional, generational, analytical, global).

% Neighboring states (Egypt, Jordan, Lebanon, Syria, Gulf) that host refugees, border the occupation, and have fought wars with Israel. Would object to the constraint's continuation but are excluded from meaningful decision-making by US-Israeli hegemony, normalization pressures, and internal authoritarian dependencies. Their exit from the US security umbrella is constrained; their opposition is managed, not heard.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, arab_states, excluded,
    powerful, generational, constrained, regional).

% Jewish organizations and individuals (JVP, IfNotNow, Neturei Karta, academic critics) who reject the constraint's legitimacy from within the Jewish tradition. Excluded from mainstream Jewish institutional representation, labeled antisemitic by the constraint's beneficiaries, but structurally positioned to expose the constraint's claim to speak for 'the Jewish people' as false. Exit is mobile individually but institutionally blocked.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, anti_zionist_jewish_voices, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the settlement of a European-origin population onto inhabited land, establishing a demographically engineered ethno-state through law, planning, and force. Solves the 'coordination problem' of how to replace an indigenous population with a settler population while maintaining international legitimacy — achieved through legal fictions (terra nullius via 'absentee property,' 'state land' declarations), demographic engineering, and great-power patronage.
% TRANSFER_FUNCTION: Moves land, water, mineral resources, labor, tax revenue, and political sovereignty from the Palestinian people to the Israeli state and Jewish settlers. Transfers the cost of enforcement (military, diplomatic, economic) to the international community and the Palestinian Authority. Transfers the risk of resistance onto the occupied population through collective punishment. Transfers the moral burden of the European 'Jewish Question' onto the indigenous people of Palestine.
% ABSENT_VOICES: Palestinian voices were absent at Balfour (1917), San Remo (1920), UN Partition (1947), Lausanne (1949), Oslo (1993), Camp David (2000), and every subsequent 'peace process.' The 1948 refugees were never consulted on their dispossession. Palestinians in Gaza and West Bank have no vote in the government that controls their lives. Anti-Zionist Jewish voices are excluded from 'the Jewish consensus' by institutional gatekeeping. The constraint's legitimacy is produced by the systematic exclusion of those it displaces.
% DISAPPEARANCE_RATIONALE: If the settler-colonial legitimacy constraint vanished overnight, the legal basis for Jewish supremacy in land law (JNF, Absentee Property Law, Basic Law: Israel Lands) would collapse. The military occupation would lose its founding justification. The Right of Return (UNGA 194) would become implementable. The demographic engineering would reverse. The international diplomatic architecture (US veto, EU association agreements, normalization treaties) would lose its object. The entire political geography of Palestine/Israel would rearrange — not necessarily peacefully, but the structural logic of displacement would be broken.
% FOUNDING_PROBLEM: The 'Jewish Question' in late 19th/early 20th century Europe: antisemitic persecution, pogroms, exclusion from national life, and the failure of emancipation/assimilation to secure Jewish safety. Zionism proposed a territorial solution: a Jewish state as refuge and normalization.
% FOUNDING_PROBLEM_CORROBORATION: European antisemitism as a historical fact is corroborated by all non-Zionist historians of European Jewry. That this problem *required* a settler-colonial solution in Palestine is contested: the Bund, Jewish socialists, liberal assimilationists, and Orthodox anti-Zionists (pre-1948) all proposed alternatives (autonomy, revolution, emigration to Americas, religious waiting). Palestinian historians (Khalidi, Pappé, Masalha) and Israeli 'New Historians' (Morris, Shlaim, Pappé) corroborate that the Zionist leadership understood the indigenous population as an obstacle to be removed, not a partner. The founding problem (European antisemitism) is substantially resolved in its original form (Jews have full rights in Europe/US); the arrangement persists for different functions (regional hegemony, resource control, identity politics).
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.85) is very high because the constraint transfers the overwhelming majority of land, water, and sovereignty from Palestinians to Jewish-Israelis, with the transfer accelerating over time (1948: 78% of mandate land; 1967: remainder; ongoing: Areas C, East Jerusalem, Jordan Valley). Suppression (0.90) is near-maximum because the constraint's persistence depends on military enforcement (occupation, siege, permit regime), legal suppression (denial of return, citizenship law, nation-state law), and narrative control (criminalization of BDS, IHRA definition, anti-normalization laws). Theater_ratio (0.40) reflects that genuine security coordination exists but a growing share of enforcement is performative — maintaining the 'only democracy in the Middle East' façade while entrenching Jewish supremacy. Accessibility_collapse (0.82) is high because alternatives (binational state, full return, equal citizenship) are structurally collapsed by the constraint's logic; they exist only as intellectual exercises. Resistance (0.75) is high and sustained: armed struggle, popular uprisings (1987, 2000, 2021), legal challenges (ICC, ICJ), BDS, sumud — but meets overwhelming force.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute sharp seat divergence: from the israeli_state seat, the constraint computes as rope/scaffold (coordination of Jewish collective security); from the palestinian_people seat, it computes as snare (pure extraction). The settler_colonial_reading declares the latter as structurally true. The national_liberation_reading would declare the former. This perspectival gap IS the kernel contest — the engine measures it, does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   The israeli_state (agenda_setter, institutional, arbitrage exit) sits at d≈0.05 — full beneficiary, the constraint subsidizes its sovereignty and resource control. jewish_israeli_settlers (beneficiary, organized, constrained exit) sit at d≈0.15 — they benefit materially but bear some risk. zionist_institutions (beneficiary, organized, mobile exit) at d≈0.10 — organizational survival depends on the constraint. palestinian_people (payer, powerless, trapped exit) at d≈0.95 — full target, identity-locked to the land. palestinian_refugees (payer, powerless, trapped exit) at d≈0.98 — the constraint's legitimacy requires their permanent exclusion. palestinians_under_occupation (payer, powerless, trapped exit) at d≈0.97 — direct daily extraction. international_community (observer, institutional, analytical exit) at d=0.5 — symmetric analytical seat. arab_states (excluded, powerful, constrained exit) at d≈0.6 — they bear regional instability costs but are excluded from agency. anti_zionist_jewish_voices (excluded, moderate, mobile exit) at d≈0.3 — they bear reputational costs but have exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (European antisemitism) is substantially resolved in its original form — Jews have full civil rights, safety, and integration in Europe and the Americas. The arrangement persists not because the founding problem remains live, but because it has acquired new functions: regional military hegemony for US empire, resource control (water, gas, land), identity politics for Israeli and diaspora Jews, and a global antisemitism shield. The mandatrophy is resolved in the reading's terms: the constraint has outlived its founding function and persists through extraction and enforcement. The base_properties.mandatrophy_resolved is not set because the status is contested (Zionist institutions claim antisemitism is rising and the refuge function remains essential).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Zionist legitimacy basis a single kernel with multiple readings, or are these fundamentally different kernels (different referents) that only share a label?',
    'Trace the genealogical provenance of each reading''s core premises: national_liberation_reading derives from Herzl/Borochov (European nationalist framework); religious_restoration_reading derives from Kook/Cook (messianic theology); settler_colonial_reading derives from Palestinian testimony (Nimr, Kanaana) and settler-colonial theory (Veracini, Wolfe, Patrick Wolfe). If the premises share no common referent, they are different kernels.',
    'If different kernels, the ε-invariance principle requires separate constraint stories with no structural linkage. If single kernel, the readings are competing interpretations of one commitment and the cs_structure fields (reading_relations, axioms, drift_state) correctly model their contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the three declared readings share a common kernel or are distinct kernels sharing a label.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.90) primarily structural (military, legal, economic barriers) or partially internalized (Palestinian acceptance of constraint''s legitimacy, Oslo-era collaboration, ''peace industry'' normalization)?',
    'Post-Oslo trajectory analysis: if suppression persists/intensifies after structural barriers are partially lowered (e.g., PA security coordination, Area A ''autonomy''), the internalized component is significant. Compare First Intifada (low internalization, high structural suppression) vs. post-2007 (high structural + high internalized via PA).',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression internally. This affects mandatrophy assessment: internalized suppression persists after structural enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanisms in the Palestinian condition.').

omega_variable(
    extraction_measurement_boundary,
    'Does the extractiveness metric (0.85) capture the full extraction including: (a) future value of denied development (Gaza offshore gas, West Bank aquifers, Jerusalem tourism), (b) extraction from diaspora Jews (donations, aliya capital, political capital), (c) extraction from US taxpayers (aid, diplomatic cover, military subsidy)?',
    'Expand the extraction boundary beyond direct land/resource transfer to include opportunity costs, externalized enforcement costs, and transnational resource flows. Compare with settler-colonial extraction metrics in Algeria, South Africa, Northern Ireland.',
    'If extraction boundary is wider, ε approaches 0.95+, reinforcing snare classification. If narrower (only direct Palestinian losses), ε may be lower but still snare-threshold. Affects cross-constraint comparison with other settler-colonial cases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_measurement_boundary, conceptual, 'Boundary problem in measuring extraction for transnational settler-colonial constraints.').

omega_variable(
    coordination_function_genuineness,
    'Does the constraint perform ANY genuine coordination function for the Palestinian people (e.g., municipal services in Area A, electricity, water infrastructure built by Israel), or is the coordination entirely for the settler population?',
    'Audit the material flows: what percentage of infrastructure, services, and planning serves Palestinian communities vs. settlements? The Oslo Accords created a Palestinian Authority that administers civilian life in Areas A/B under Israeli security control — is this coordination or subcontracting of suppression?',
    'If genuine coordination for Palestinians exists, the constraint may be tangled_rope rather than pure snare from some seats. If coordination is entirely for settlers, snare classification holds across all non-beneficiary seats. This is the core methodological question for settler-colonial constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_genuineness, conceptual, 'Whether the settler-colonial constraint has any genuine coordination function for the displaced population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 0, 127).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zlb_scr_tr_t0, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(zlb_scr_tr_t17, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 17, 0.2).
narrative_ontology:measurement(zlb_scr_tr_t31, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 31, 0.28).
narrative_ontology:measurement(zlb_scr_tr_t50, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(zlb_scr_tr_t73, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 73, 0.38).
narrative_ontology:measurement(zlb_scr_tr_t100, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(zlb_scr_tr_t127, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 127, 0.4).

% Extraction over time
narrative_ontology:measurement(zlb_scr_be_t0, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(zlb_scr_be_t17, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 17, 0.45).
narrative_ontology:measurement(zlb_scr_be_t31, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 31, 0.65).
narrative_ontology:measurement(zlb_scr_be_t50, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(zlb_scr_be_t73, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 73, 0.82).
narrative_ontology:measurement(zlb_scr_be_t100, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 100, 0.85).
narrative_ontology:measurement(zlb_scr_be_t127, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 127, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(zlb_scr_su_t0, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(zlb_scr_su_t17, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 17, 0.65).
narrative_ontology:measurement(zlb_scr_su_t31, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 31, 0.8).
narrative_ontology:measurement(zlb_scr_su_t50, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(zlb_scr_su_t73, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 73, 0.88).
narrative_ontology:measurement(zlb_scr_su_t100, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 100, 0.9).
narrative_ontology:measurement(zlb_scr_su_t127, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 127, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__settler_colonial_reading, 0.18).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israeli_occupation_regime).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, jerusalem_status).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, gaza_blockade).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, settlement_expansion_regime).

% DUAL FORMULATION NOTE:
% This constraint (settler_colonial_reading) and national_liberation_reading are dual formulations of the same kernel contest. The national_liberation_reading claims the founding problem (European antisemitism) remains live and the arrangement is a rope/scaffold. This reading claims the founding problem is resolved/contested and the arrangement is a snare. They share the kernel_id but have incompatible axioms and reading_relations. The religious_restoration_reading coexists with both in Israeli society but forecloses the secular national_liberation_reading's purely political framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__settler_colonial_reading, institutional, 0.05).
constraint_indexing:directionality_override(zionist_legitimacy_basis__settler_colonial_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
