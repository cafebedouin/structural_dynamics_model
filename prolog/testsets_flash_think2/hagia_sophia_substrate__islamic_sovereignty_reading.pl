% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty and Waqf Reading
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Islamic sovereignty' reading of
 *   Hagia Sophia's status. It asserts that the site's legitimacy derives from
 *   the 1453 Ottoman conquest and continuous Islamic endowment (waqf), making
 *   it sovereign Islamic worship space under Turkish state authority. The
 *   metrics reflect the operation of this claim, particularly its reassertion
 *   in 2020. The interval from 1934 to 2020 traces the re-emergence of this
 *   reading's dominance, from a period where its claims were suppressed
 *   (museum status) to its full reassertion as a mosque.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.7).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.75).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty and Waqf Reading").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '9d1737b4-8539-481d-8b09-aac05d66fcf2').
narrative_ontology:cs_kernel_codification('9d1737b4-8539-481d-8b09-aac05d66fcf2', formalized).
narrative_ontology:cs_authority_grounding('9d1737b4-8539-481d-8b09-aac05d66fcf2', lineage).
narrative_ontology:cs_interpretation_layer_present('9d1737b4-8539-481d-8b09-aac05d66fcf2').
narrative_ontology:cs_reading_relation('9d1737b4-8539-481d-8b09-aac05d66fcf2', hagia_sophia_substrate__universal_heritage_reading, forecloses).
narrative_ontology:cs_reading_relation('9d1737b4-8539-481d-8b09-aac05d66fcf2', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('9d1737b4-8539-481d-8b09-aac05d66fcf2', foundational, ottoman_conquest_establishes_sovereignty).
narrative_ontology:cs_axiom_status(ottoman_conquest_establishes_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9d1737b4-8539-481d-8b09-aac05d66fcf2', ottoman_conquest_establishes_sovereignty, conventional).
narrative_ontology:cs_axiom('9d1737b4-8539-481d-8b09-aac05d66fcf2', foundational, waqf_status_is_immutable).
narrative_ontology:cs_axiom_status(waqf_status_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('9d1737b4-8539-481d-8b09-aac05d66fcf2', waqf_status_is_immutable, conventional).
narrative_ontology:cs_reference_frame('9d1737b4-8539-481d-8b09-aac05d66fcf2', islamic_waqf_sovereignty_1453).
narrative_ontology:cs_drift_state('9d1737b4-8539-481d-8b09-aac05d66fcf2', contemporary_2020_reconversion, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('9d1737b4-8539-481d-8b09-aac05d66fcf2', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ruling political party in Turkey, which initiated and executed the 2020 reconversion of Hagia Sophia into a mosque. They benefit from consolidating political power, appealing to a religious conservative base, and asserting national sovereignty.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% A significant portion of the Turkish population that views the reconversion as a restoration of historical justice and a symbol of Islamic identity and national pride. They gain symbolic and religious affirmation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, constrained, national).

% The broader global Sunni Muslim community, which perceives the reconversion as a symbolic victory for Islam and a reassertion of historical Islamic presence. Their benefit is primarily ideological and symbolic.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically, beneficiary,
    powerless, civilizational, identity_locked, global).

% Individuals of non-Muslim faiths or secular backgrounds who previously visited Hagia Sophia as a museum. They now face restrictions on access, dress codes, and the inability to visit during prayer times, experiencing a loss of universal access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, constrained, global).

% The United Nations Educational, Scientific and Cultural Organization, which designated Hagia Sophia a World Heritage site. They view the unilateral change in status as a violation of international heritage norms and a denial of their jurisdiction, incurring diplomatic and reputational costs.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime, payer,
    institutional, generational, constrained, global).

% Turkish citizens who adhere to the secular principles of the Republic's founder, Atatürk. They view the reconversion as an ideological defeat, a step backward for secularism, and a politicization of cultural heritage. Their resistance is largely political and intellectual.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    organized, biographical, constrained, national).

% The Ecumenical Patriarchate of Constantinople and the global Orthodox Christian community, who view Hagia Sophia as a foundational site of their faith. From this reading's perspective, their claims for restitution or shared worship are entirely excluded and foreclosed by the assertion of Islamic sovereignty.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_church, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a significant segment of the Turkish populace and the broader Sunni Muslim world around a shared religious and national identity, asserting Turkish sovereignty over a historically contested site.
% TRANSFER_FUNCTION: Transfers symbolic and practical control of Hagia Sophia from a universal heritage/secular museum status to an exclusive Islamic worship space under Turkish state authority, restricting access and denying international jurisdiction.
% ABSENT_VOICES: The Ecumenical Patriarchate and global Orthodox Christian community, international heritage bodies (beyond UNESCO's formal objections), and a broader coalition of secularist and human rights advocates are structurally excluded from the decision-making process, though their objections are voiced externally.
% DISAPPEARANCE_RATIONALE: If the current status and its underlying claims vanished, it would trigger a profound political and religious crisis in Turkey, reshape its international relations, and reopen intense debates about national identity, secularism, and historical memory. The site's status is a central pillar of current Turkish political and religious discourse.
% FOUNDING_PROBLEM: To establish and maintain Islamic sovereignty and religious identity over Hagia Sophia following the Ottoman conquest of Constantinople in 1453, transforming it from a Christian cathedral into a mosque and endowing it as a waqf.
% FOUNDING_PROBLEM_CORROBORATION: Turkish state institutions, religious authorities, and a significant portion of the Turkish populace corroborate this. International bodies and secularist groups contest it.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it simultaneously coordinates a strong sense of national and religious identity for its beneficiaries (Turkish Islamic constituency, AKP coalition) while extracting significant costs from victims (non-Muslim visitors, UNESCO, secularist Turks) through active state enforcement. Extractiveness (0.7) is high due to the denial of universal access and international jurisdiction. Suppression (0.75) is substantial, reflecting the state's active role in enforcing the new status and suppressing dissenting views. Theater ratio (0.4) indicates that while there is genuine religious function, there's also a significant performative aspect tied to political signaling. The temporal measurements show a clear trend of increasing extractiveness and suppression, and decreasing 'theater' (from the perspective of the Islamic sovereignty reading, the museum period was 'theater' for its true function), culminating in the 2020 reconversion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the AKP coalition and Turkish Islamic constituency, the constraint is a legitimate restoration of historical and religious rights, a form of identity coordination. From the perspective of non-Muslim visitors, UNESCO, and secularist Turks, it is an act of political and religious extraction, denying universal access and secular principles. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP political coalition and the Turkish Islamic constituency are clear beneficiaries, gaining political capital and religious affirmation. The broader Sunni Ummah benefits symbolically. Non-Muslim visitors, UNESCO, and secularist Turks are targets, bearing costs through restricted access, denied jurisdiction, and ideological defeat, respectively. The Orthodox Church is structurally excluded, with its claims entirely foreclosed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_legitimacy_contestation,
    'Is the 1453 Ottoman conquest and subsequent waqf status universally accepted as the sole legitimate basis for Hagia Sophia''s sovereignty, or is its Byzantine Christian origin an equally valid, competing claim?',
    'International legal arbitration or a shift in geopolitical power dynamics that re-evaluates historical claims to cultural sites.',
    'If competing historical claims are recognized as equally legitimate, the ''islamic_sovereignty_reading'' would lose its foundational authority, potentially reclassifying it as a Snare or Piton due to its reliance on active suppression rather than universally accepted historical fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_legitimacy_contestation, conceptual, 'Contestation over the primary historical grounding of Hagia Sophia''s legitimacy.').

omega_variable(
    political_vs_religious_motivation,
    'To what extent was the 2020 reconversion primarily a political maneuver by the AKP coalition to consolidate power, versus a genuine expression of religious and national sentiment?',
    'Analysis of voting patterns, public discourse, and policy decisions following the reconversion, as well as internal party documents (if accessible).',
    'If primarily political, the ''extractiveness'' metric might be higher, reflecting rent-seeking from the political base, and the ''theater_ratio'' might increase, as the religious function serves a political end. If primarily religious, the coordination function for the Islamic constituency would be stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_religious_motivation, empirical, 'Ambiguity regarding the primary motivation behind the reconversion.').

omega_variable(
    suppression_mechanism_secularist_turks,
    'Is the suppression experienced by secularist Turks structural (state policy, legal barriers) or internalized (fear of reprisal, self-censorship)?',
    'Post-policy-reversal trajectory: if suppression of secularist expression persists after the reconversion is reversed (hypothetically), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression on secularist Turks is higher than the structural measure suggests — they carry the suppression with them after any policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_secularist_turks, empirical, 'Structural vs. internalized suppression mechanism for secularist Turks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 1934, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1934, 0.6).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1950, 0.55).
narrative_ontology:measurement(hagi_tr_t1970, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1970, 0.5).
narrative_ontology:measurement(hagi_tr_t1990, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1934, 0.2).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(hagi_be_t1970, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(hagi_be_t1990, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1934, 0.3).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(hagi_su_t1970, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(hagi_su_t1990, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Hagia Sophia Substrate' kernel. Each reading presents a distinct structural claim about the site's legitimacy and function, with differing beneficiaries, victims, and metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
