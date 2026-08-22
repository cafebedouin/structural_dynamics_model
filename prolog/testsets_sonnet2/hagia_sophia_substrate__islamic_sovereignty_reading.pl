% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia as Sovereign Islamic Waqf Under Turkish State Authority
 *   domain: cultural heritage / sovereignty / religious authority
 *
 * SUMMARY:
 *   This story authors the Islamic sovereignty reading of the Hagia Sophia
 *   kernel: the claim that the site's legitimate status derives from the 1453
 *   Ottoman conquest and Sultan Mehmed II's perpetual waqf endowment, such
 *   that the 1934 Kemalist secularization was itself the illegitimate
 *   deviation, corrected by the 2020 Council of State ruling and presidential
 *   decree restoring the site to active mosque use under Turkish sovereign
 *   and Diyanet administration. This is one reading among three of a
 *   contested kernel; the orthodox restitution reading (Byzantine cathedral
 *   origin, Orthodox ecclesiastical claim) and the universal heritage reading
 *   (shared human patrimony transcending any single religious/national claim)
 *   are separate constraint stories with their own ε values and stakeholder
 *   sets, linked via network.affects_constraints. This story's ε is assessed
 *   from within the Islamic-sovereignty reading's own lights, applied to the
 *   standing arrangement (the post-2020 mosque status) it defends, not to any
 *   rival arrangement.
 *
 * KEY AGENTS:
 *   - akp_political_coalition: agenda_setter/beneficiary (institutional/arbitrage) - engineered and administers the reconversion
 *   - turkish_islamic_constituency: beneficiary (organized/mobile) - gains restored worship site
 *   - non_muslim_visitors: payer (powerless/constrained) - bears restricted access
 *   - unesco_heritage_regime: payer (institutional/constrained) - jurisdiction denied
 *   - secularist_turks: payer (moderate/constrained) - bears ideological defeat
 *   - orthodox_christian_communities: excluded (powerless/trapped) - historic claim treated as superseded
 *   - turkish_state_courts: agenda_setter/observer (institutional/analytical) - supplied the legal doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.58).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia as Sovereign Islamic Waqf Under Turkish State Authority").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural heritage / sovereignty / religious authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '4aaa5898-b658-42be-a0b0-9d0228f0c6ea').
narrative_ontology:cs_kernel_codification('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', formalized).
narrative_ontology:cs_authority_grounding('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', extraction).
narrative_ontology:cs_interpretation_layer_present('4aaa5898-b658-42be-a0b0-9d0228f0c6ea').
narrative_ontology:cs_reading_relation('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', foundational, ottoman_conquest_establishes_perpetual_islamic_title).
narrative_ontology:cs_axiom_status(ottoman_conquest_establishes_perpetual_islamic_title, holdable).
narrative_ontology:cs_axiom_grounding('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', ottoman_conquest_establishes_perpetual_islamic_title, conventional).
narrative_ontology:cs_axiom('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', foundational, waqf_endowment_inalienable_by_subsequent_secular_authority).
narrative_ontology:cs_axiom_status(waqf_endowment_inalienable_by_subsequent_secular_authority, holdable).
narrative_ontology:cs_axiom_grounding('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', waqf_endowment_inalienable_by_subsequent_secular_authority, conventional).
narrative_ontology:cs_axiom('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', secondary, id_1934_secularization_was_ultra_vires_state_overreach).
narrative_ontology:cs_axiom_status(id_1934_secularization_was_ultra_vires_state_overreach, holdable).
narrative_ontology:cs_axiom_grounding('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', id_1934_secularization_was_ultra_vires_state_overreach, conventional).
narrative_ontology:cs_reference_frame('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', ottoman_waqf_perpetual_endowment).
narrative_ontology:cs_drift_state('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', post_1934_secularization_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('4aaa5898-b658-42be-a0b0-9d0228f0c6ea', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engineered the 2020 Council of State ruling reversing the 1934 secularization decree and the subsequent presidential decree reconverting the site to a mosque. Administers the site's status through the Diyanet (Directorate of Religious Affairs) and controls prayer schedules, visitor access rules, and the covering/uncovering of Christian iconography during prayer times. Uses the reconversion as a signature domestic political achievement consolidating religious-nationalist support.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, beneficiary).

% Gains a restored functioning mosque of immense symbolic weight, framed as correcting a Kemalist-era secularization felt as a historical wrong. Worships at the site free of charge on the terms the waqf reading establishes; experiences the reconversion as vindication of religious identity long subordinated to state secularism.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% A diffuse symbolic beneficiary rather than an organized actor: the reconversion is cited across Sunni Muslim political and religious discourse internationally as restoration of a historic mosque to its 'true' function, generating prestige for Turkey as custodian without any concrete claim-holder collecting a material benefit.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).

% Tourists and pilgrims of other faiths, including Orthodox Christians for whom the site is the historic mother church of their tradition, now visit under restricted hours, mandatory shoe removal and covering, curtained-off Christian mosaics during prayer times, and no permitted non-Muslim devotional activity. Their access is contingent on the mosque's prayer schedule rather than on heritage-visit terms; they can decline to visit but cannot alter the terms of access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, constrained, global).

% Formally requested consultation before the 2020 reconversion under World Heritage Site obligations and was rebuffed by Turkish assertion of sovereign domestic jurisdiction. Its authority to condition the site's management on shared-heritage preservation standards was structurally denied; it retains only the leverage of listing review and reputational pressure, no binding enforcement power over Turkish territory.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime, payer,
    institutional, generational, constrained, global).

% View the 1934 Atatürk-era secularization as a founding achievement of the Turkish Republic converting the site into a museum open to all; experience the 2020 reversal as a direct ideological defeat and erosion of state secularism. Can protest, litigate (as they did, unsuccessfully, up to the Council of State), or accept the outcome, but hold no institutional lever to reverse an executive decree validated by a captured judiciary.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    moderate, generational, constrained, national).

% The Ecumenical Patriarchate and global Orthodox communities regard Hagia Sophia as the historic seat and mother church of Orthodox Christianity, converted by conquest in 1453. Under this reading their claim is treated as historically superseded rather than live; they are not party to the decree process and have no standing in Turkish domestic law to contest the site's status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_christian_communities, excluded,
    powerless, civilizational, trapped, global).

% The Council of State issued the 2020 ruling holding the 1934 Cabinet decree that secularized the site invalid, on the theory that the waqf deed of Sultan Mehmed II endowing the building as a mosque in perpetuity could not be lawfully dissolved by a later Cabinet decision. Provided the formal legal instrument that converted a political decision into a doctrine of perpetual, inalienable Islamic endowment.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_courts, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legally stabilized answer to who administers a globally significant, functionally ambiguous religious site: the Turkish state, through the Diyanet, coordinates worship scheduling, physical maintenance, and visitor access under one authority rather than leaving the site in permanent status limbo between competing claimants.
% TRANSFER_FUNCTION: Moves symbolic and political capital from secularist Turkish identity and non-Muslim heritage claimants to the AKP coalition and Turkish Sunni religious identity; moves practical access conditions (free museum access on equal terms for all visitors) into conditional, prayer-schedule-gated access for non-Muslims; moves interpretive authority over the site's meaning from an international heritage framework to unilateral domestic religious-legal doctrine.
% ABSENT_VOICES: The Ecumenical Patriarchate and global Orthodox Christian communities, who regard the site as their historic mother church, have no standing in the Turkish domestic legal process that produced the 2020 ruling and were not consulted. UNESCO's advisory consultation request was acknowledged but not treated as binding or determinative.
% DISAPPEARANCE_RATIONALE: If the waqf-sovereignty doctrine were withdrawn and the site reverted to secular museum status, prayer scheduling and Diyanet administration would end, non-Muslim visitor access would return to unrestricted museum terms, UNESCO consultation leverage would be practically restored, and a central pillar of the AKP's religious-nationalist political narrative would be lost — the arrangement is actively load-bearing for multiple parties' current practices and legitimacy claims, not a description of an inert fact.
% FOUNDING_PROBLEM: Framed by proponents as correcting a historical wrong: the 1934 secularization is presented as an act of Kemalist state imposition against the perpetual waqf status established by Sultan Mehmed II's foundational deed, which under Islamic endowment law cannot be lawfully dissolved by any subsequent secular authority.
% FOUNDING_PROBLEM_CORROBORATION: The claim that the 1934 secularization was itself the illegitimate act is asserted by the AKP government, the Diyanet, and the reversing court — all direct beneficiaries or instruments of the 2020 reconversion. Independent legal historians and comparative waqf-law scholars outside Turkey dispute that a waqf deed from an absolute imperial sovereign in a since-abolished caliphate structure binds a modern secular successor state in perpetuity; UNESCO and international heritage law scholars corroborate that the 1934 secularization was internationally recognized and stable for 86 years, treating the 'always already a mosque' framing as a retrospective legal construction rather than an uncontested historical continuity.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 (moderate-high, per the expected structural delta) reflecting political consolidation value captured by the AKP coalition, symbolic religious-identity extraction from the broader Sunni constituency's investment in the narrative, and real diplomatic friction cost imposed on Turkey's international relationships (Greece, Russia, UNESCO members) — all borne asymmetrically relative to a genuine but real coordination function (a single administering authority resolving what would otherwise be permanent status ambiguity). Suppression (0.58) reflects the active exclusion of non-Muslim devotional use, curtaining of iconography, and the denial of UNESCO consultative authority — none of this is passive; each requires ongoing administrative enforcement. Theater ratio (0.4) is moderate: real worship function exists, but a substantial share of the reconversion's political value is performative — the symbolic act of correction matters more to its beneficiaries than any change in the site's actual prior use pattern (it was accessible and well-maintained as a museum). Accessibility collapse (0.5) and resistance (0.55) are mid-range because meaningful institutional resistance persists (secularist legal challenges, international protest, UNESCO's ongoing engagement) even though the domestic legal alternative was foreclosed by the 2020 ruling.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP coalition and Turkish courts sit at the beneficiary/agenda-setter pole: they authored the doctrine, control enforcement, and capture the political and religious-identity gains. Non-Muslim visitors, UNESCO, and secularist Turks sit at the target pole via the victims declaration: each bears a cost (restricted access, denied jurisdiction, ideological defeat respectively) through the same structure that produces the beneficiaries' gains — this is the asymmetric-extraction leg of the tangled rope reading. The Sunni ummah is marked non_agent/symbolic because no concrete claim-holder collects a material benefit; it is beneficiary in narrative-legitimation terms only, which is why it carries no directionality weight in the enforcement analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing (correcting the 1934 secularization as an illegitimate act) is precisely the genealogy claim that must not be taken at face value: it is asserted almost exclusively by the constraint's own beneficiaries (the government, Diyanet, and the court whose ruling constitutes the reconversion). The founding_problem_status is marked contested rather than resolved because independent legal-historical corroboration outside the beneficiary set disputes the perpetual-waqf-overrides-secular-state theory, and because the 1934 secularization itself stood unchallenged and internationally stable for 86 years — undermining a 'restoration of the true state' narrative in favor of a 'politically motivated re-founding' reading. This is exactly the mismatch (status=contested + verdict=world_rearranges) the R5 apparatus exists to flag: a genealogy claim advanced only by the arrangement's own beneficiaries, absent outside corroboration of its urgency, layered onto a structure that demonstrably reallocates real access and authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_perpetuity_vs_state_sovereignty,
    'Does a waqf deed executed by an absolute imperial sovereign of a since-abolished caliphate bind a modern secular successor state in perpetuity, or does state sovereignty (exercised via the 1934 secularization) lawfully supersede a private religious endowment instrument?',
    'Comparative analysis of waqf law doctrine across post-Ottoman successor states, and examination of whether other Ottoman-era waqf-endowed properties secularized in the same period have been subject to equivalent ''perpetual endowment'' reversal claims.',
    'If waqf perpetuity is held to override state secularization authority as a general legal principle, the 2020 ruling is doctrinally consistent and other secularized former-waqf properties in Turkey become vulnerable to identical reconversion claims, expanding the constraint''s reach; if state sovereignty is held to override waqf perpetuity, the 2020 ruling is better characterized as a political act using legal form rather than a doctrinally compelled correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_perpetuity_vs_state_sovereignty, conceptual, 'Whether waqf endowment law or state secular sovereignty has legal priority — the core doctrinal question underlying the reading''s legitimacy claim.').

omega_variable(
    kernel_reading_selection_pressure,
    'This story is one of three readings of the hagia_sophia_substrate kernel (islamic_sovereignty, orthodox_restitution, universal_heritage). What structural or political conditions determine which reading a given authority (Turkish domestic courts vs. UNESCO vs. the Ecumenical Patriarchate) treats as operative, and could more than one reading hold simultaneously in different institutional arenas?',
    'Track which reading each relevant authority (Turkish courts, UNESCO World Heritage Committee, European Court of Human Rights if petitioned, Ecumenical Patriarchate statements) formally invokes over time, and whether any forum has jurisdiction to adjudicate between readings rather than merely assert one.',
    'If no forum has authority to adjudicate between readings, all three persist indefinitely as parallel, non-convergent legitimacy claims — the kernel remains permanently contested rather than resolving to one reading, and this story''s ε (extraction attributable to the sovereignty reading specifically) should be read as bounded to the domestic Turkish legal-political arena rather than globally settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'How the three sibling readings of the kernel relate structurally — whether they compete for a single resolution or persist as parallel, non-converging claims held by different authorities.').

omega_variable(
    diplomatic_friction_magnitude,
    'How large and how durable is the diplomatic and reputational cost to Turkey from the reconversion, relative to the domestic political benefit captured by the AKP coalition?',
    'Track UNESCO listing status changes, formal protest statements from Greece/Russia/other Orthodox-majority states, and any measurable shift in Turkish tourism revenue or international heritage-diplomacy standing over a 10-year window post-2020.',
    'If diplomatic cost proves large and durable, the extraction is partially offset by a real cost borne by the beneficiary coalition itself, moderating the net ε; if cost proves negligible and transient, the extractiveness figure understates the asymmetry, since the coalition captures nearly pure benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diplomatic_friction_magnitude, empirical, 'Whether the reconversion imposes a meaningful cost on its own beneficiaries via diplomatic friction, or whether that friction is symbolic and non-binding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 1934, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1934, 0.05).
narrative_ontology:measurement(hagi_tr_t1980, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(hagi_tr_t2005, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(hagi_tr_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(hagi_tr_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1934, 0.1).
narrative_ontology:measurement(hagi_be_t1980, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(hagi_be_t2005, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2005, 0.18).
narrative_ontology:measurement(hagi_be_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2016, 0.3).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(hagi_be_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1934, 0.15).
narrative_ontology:measurement(hagi_su_t1980, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1980, 0.15).
narrative_ontology:measurement(hagi_su_t2005, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2005, 0.2).
narrative_ontology:measurement(hagi_su_t2016, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(hagi_su_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2022, 0.56).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the hagia_sophia_substrate kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: islamic_sovereignty_reading (this story; moderate-high ε from political consolidation and access restriction), orthodox_restitution_reading (ε authored separately around the Ecumenical Patriarchate's restitution claim and its own victim/beneficiary structure), and universal_heritage_reading (ε authored separately around UNESCO/global-public-good framing). The three do not average into one ε; each is assessed from within its own reading's lights against the same physical site under contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
