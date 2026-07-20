% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Hagia Sophia Islamic Sovereignty Reading
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint instantiates the Islamic sovereignty reading of the Hagia
 *   Sophia kernel: the site's legitimacy derives from the 1453 Ottoman
 *   conquest and continuous waqf endowment, making it sovereign Islamic
 *   worship space under Turkish state authority. The Turkish state enforces
 *   this through executive decree (2020), court reversal of the 1934 museum
 *   decision, and Diyanet administration of prayer and visitor restrictions.
 *   The constraint operates as a tangled rope: it coordinates genuine Islamic
 *   worship and endowment management while asymmetrically extracting
 *   sovereignty prestige, political consolidation for the AKP, and identity
 *   signaling for the Sunni ummah, at the cost of non-Muslim visitor access,
 *   UNESCO jurisdiction, and secularist Turkish heritage governance. The
 *   claim/metric independence is maintained: the reading claims legitimate
 *   continuity and coordination, while the metrics capture the substantial
 *   extraction, active enforcement, and suppression of alternatives required
 *   to maintain this exclusive framework.
 *
 * KEY AGENTS:
 *   - AKP political coalition (agenda_setter/institutional/arbitrage): Sets and enforces the site's status; captures political consolidation.
 *   - Turkish Islamic constituency (beneficiary/organized/constrained): Receives worship access and symbolic ownership.
 *   - Broader Sunni ummah (beneficiary/organized/identity_locked): Receives transnational religious prestige.
 *   - Non-Muslim visitors (payer/moderate/constrained): Bear access restrictions and diminished heritage experience.
 *   - UNESCO regime (payer/institutional/constrained): Loses heritage jurisdiction and enforcement capacity.
 *   - Secularist Turks (payer/organized/constrained): Suffer ideological defeat of neutral/secular governance.
 *   - International legal scholars (observer/analytical/analytical): Document the competing claims without institutional stake.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.72).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty Reading").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '2e98dc4e-3cf8-4952-8df8-93be46e11183').
narrative_ontology:cs_kernel_codification('2e98dc4e-3cf8-4952-8df8-93be46e11183', fixed_text).
narrative_ontology:cs_authority_grounding('2e98dc4e-3cf8-4952-8df8-93be46e11183', lineage).
narrative_ontology:cs_interpretation_layer_present('2e98dc4e-3cf8-4952-8df8-93be46e11183').
narrative_ontology:cs_reading_relation('2e98dc4e-3cf8-4952-8df8-93be46e11183', hagia_sophia_substrate__universal_heritage_reading, forecloses).
narrative_ontology:cs_reading_relation('2e98dc4e-3cf8-4952-8df8-93be46e11183', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('2e98dc4e-3cf8-4952-8df8-93be46e11183', foundational, waqf_inalienability).
narrative_ontology:cs_axiom_status(waqf_inalienability, holdable).
narrative_ontology:cs_axiom_grounding('2e98dc4e-3cf8-4952-8df8-93be46e11183', waqf_inalienability, theological).
narrative_ontology:cs_axiom('2e98dc4e-3cf8-4952-8df8-93be46e11183', foundational, conquest_establishes_legitimate_title).
narrative_ontology:cs_axiom_status(conquest_establishes_legitimate_title, holdable).
narrative_ontology:cs_axiom_grounding('2e98dc4e-3cf8-4952-8df8-93be46e11183', conquest_establishes_legitimate_title, conventional).
narrative_ontology:cs_reference_frame('2e98dc4e-3cf8-4952-8df8-93be46e11183', ottoman_waqf_continuity).
narrative_ontology:cs_drift_state('2e98dc4e-3cf8-4952-8df8-93be46e11183', contemporary_post_2020, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2e98dc4e-3cf8-4952-8df8-93be46e11183', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the site's status through executive decree and judicial apparatus, converting it from museum to mosque in 2020. Derives political consolidation and sovereignty signaling from the arrangement. Could reverse the decree but would face electoral backlash from its Islamic base.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains unrestricted worship access and symbolic ownership of a major Islamic site. Bears no direct costs of the constraint. Exit would mean leaving the country or abandoning religious community identity.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, biographical, constrained, national).

% Receives symbolic religious prestige from the site's active mosque status under Sunni Islamic authority. The site functions as a global ummah identity marker. Exit from this symbolic framework is effectively impossible without abandoning religious identity.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah, beneficiary,
    organized, generational, identity_locked, global).

% Face restricted visiting hours, gender-specific areas, covered dress requirements, and partial inaccessibility during prayer times. Pay the cost of diminished heritage access and altered aesthetic and religious experience. Can avoid the site but not easily reverse the constraint.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    moderate, biographical, constrained, global).

% Has its World Heritage jurisdiction and conservation authority actively denied by Turkish state sovereignty claims. Cannot enforce heritage standards or neutral-status requirements against the Turkish state's exclusive control. Diplomatic and legal mechanisms are exhausted or blocked.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime, payer,
    institutional, generational, constrained, global).

% Experience ideological defeat as the 1934 secular museum settlement is overturned. Their vision of neutral heritage governance loses state backing. Political opposition exists but lacks institutional power to reverse the decree. Exit from the national ideological conflict is costly, requiring emigration or silence.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    organized, biographical, constrained, national).

% Claims ecclesiastical and historical legitimacy over the site as the founding Christian cathedral. Is structurally excluded from Turkish legal and political deliberation over the site's status. Would demand restitution or neutral shared governance if admitted to the conversation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_patriarchate, excluded,
    institutional, generational, trapped, global).

% Analyze the competing sovereignty and heritage claims without institutional stake in the outcome. Document the legal and historical arguments for waqf continuity versus secular heritage regimes.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Islamic worship and congregational prayer at a historically significant site under unitary state management, centralizing religious access and endowment administration for the Muslim community.
% TRANSFER_FUNCTION: Moves control over heritage access, symbolic religious legitimacy, and sovereignty prestige from secular and international heritage frameworks to Turkish state authority and its Islamic constituency.
% ABSENT_VOICES: Orthodox Patriarchate seeking ecclesiastical restitution, minority Christian communities in Turkey, and international heritage bodies advocating for neutral or shared status are structurally excluded from the Turkish legal framework governing the site.
% DISAPPEARANCE_RATIONALE: Worship schedules would shift, tourism and visitor demographics would reconfigure, UNESCO heritage protocols would reassert influence, and Turkey's sovereignty claim would lose its primary religious anchor â the regional and domestic political symbolism would require recalibration.
% FOUNDING_PROBLEM: Governance of a major religious site following the 1453 Ottoman conquest; provision of Islamic worship space and administration of a waqf endowment within a conquered Byzantine capital.
% FOUNDING_PROBLEM_CORROBORATION: International historians and legal scholars corroborate the contested nature of the 1453 conversion and its legitimacy. Turkish secularist historians and some Islamic jurists acknowledge the 1934 secularization decree as a valid intervening act, challenging the narrative of uninterrupted waqf continuity. The AKP and its religious allies assert the founding problem remains live; external academic and diplomatic voices contest this framing.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is moderate-high because the constraint extracts sovereignty prestige, political capital, and exclusive religious identity value that exceeds the coordination cost of running a mosque. Suppression (0.68) is high because the constraint requires active state enforcement to exclude UNESCO jurisdiction, restrict non-Muslim access patterns, and override the 1934 secular settlement. Theater_ratio (0.45) reflects that genuine worship coordination occurs, but a substantial portion of state activity is political performance of sovereignty and Islamic identity. Accessibility_collapse (0.75) is high because alternatives (shared heritage, neutral museum, Christian worship) are legally and diplomatically foreclosed. Resistance (0.60) is substantial: international bodies, domestic secularists, and Orthodox authorities mount active opposition but lack power to reverse the decree. The temporal series show the constraint's reactivation after dormancy: extraction and suppression were low under the secular museum regime (2000-2018), then spiked with the 2020 decree as the state deployed enforcement machinery to realize the Islamic sovereignty reading.
 *
 * PERSPECTIVAL GAP:
 *   From the AKP agenda-setter seat, the constraint is legitimate restoration of historical justice and worship coordination; from the non-Muslim visitor, UNESCO, and secularist payer seats, it is state-enforced exclusion and identity-based extraction. The Turkish Islamic constituency experiences low-extraction coordination (gaining worship access), while the Sunni ummah experiences symbolic subsidy at minimal cost. The engine will compute divergent per-seat classifications from this structural asymmetry: beneficiaries near the subsidy end, trapped payers near the full-target end.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (akp_coalition, turkish_islamic_constituency, sunni_ummah) derive low directionality: the constraint subsidizes their political, religious, and identity interests. Victims/payers (non_muslim_visitors, unesco_regime, secularist_turks) derive high directionality: the constraint extracts access, jurisdiction, and ideological standing from them. The AKP has arbitrage-grade exit (could reverse the decree if politically expedient), while the Sunni ummah is identity_locked (exit means abandoning religious community attachment), and the UNESCO regime is constrained by international law limitations.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling the constraint as pure coordination (Rope) by requiring the declared victim set and active enforcement â the non-Muslim visitor restrictions and UNESCO jurisdiction denial are not incidental byproducts but structural features. It also prevents mislabeling as pure extraction (Snare) by acknowledging the genuine coordination function: active mosque operations, congregational prayer, and waqf administration serve real religious needs. The Scaffold classification is ruled out by the absence of a sunset clause and the indefinite, not transitional, justification. Piton is ruled out because the AKP coalition captures concentrated benefits and actively maintains the constraint with high political investment â the function has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_continuity_1934_gap,
    'Was the waqf endowment legally broken by the 1934 secularization decree, rendering the 2020 restoration a new creation rather than a continuity?',
    'Archival discovery of waqf legal status in Ottoman and Republican records; Islamic jurisprudence on waqf dissolution by secular state act.',
    'If broken, the Islamic sovereignty reading loses its historical continuity claim and becomes a contemporary political construction, potentially shifting authority grounding from lineage toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_continuity_1934_gap, conceptual, 'Whether the waqf continuity claim survives the 1934 secularization intervention.').

omega_variable(
    extraction_vs_coordination_ratio,
    'What proportion of the constraint''s persistence is driven by genuine worship coordination versus political identity extraction?',
    'Demographic analysis of worship attendance versus political rally symbolism; comparative analysis of other state-managed mosque networks.',
    'If primarily political, the extraction is higher than structurally necessary and the coordination function is cover; if primarily religious, the constraint remains tangled rope rather than snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_ratio, empirical, 'Religious necessity versus political instrumentalization of the sovereignty claim.').

omega_variable(
    suppression_beyond_standard_mosque,
    'Is the suppression of non-Muslim access and international jurisdiction a necessary feature of active mosque status, or an intensified extraction mechanism beyond standard mosque norms?',
    'Comparative study of tourist access restrictions at major mosque-museums versus Hagia Sophia post-2020.',
    'If restrictions exceed comparable active worship sites, the extraction is targeted rather than incidental to coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_beyond_standard_mosque, empirical, 'Whether access suppression exceeds baseline mosque coordination requirements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hagi_tr_t6, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(hagi_tr_t12, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(hagi_tr_t18, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(hagi_tr_t20, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(hagi_tr_t24, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hagi_be_t6, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement(hagi_be_t12, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(hagi_be_t18, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(hagi_be_t20, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(hagi_be_t24, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hagi_su_t6, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 6, 0.15).
narrative_ontology:measurement(hagi_su_t12, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 12, 0.25).
narrative_ontology:measurement(hagi_su_t18, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 18, 0.4).
narrative_ontology:measurement(hagi_su_t20, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(hagi_su_t24, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hagia_sophia_substrate kernel, which decomposes into three structurally distinct claims: Islamic sovereignty (this file), universal heritage, and Orthodox restitution. Each has a different epsilon, beneficiary/victim structure, and authority grounding. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
