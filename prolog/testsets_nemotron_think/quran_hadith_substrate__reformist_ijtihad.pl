% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad Mandate: Quranic Ethical Trajectory Over Literalist Hadith
 *   domain: religious/legal/theoretical
 *
 * SUMMARY:
 *   The reformist ijtihad reading of the Quran-hadith substrate mandates
 *   contextual reinterpretation when classical fiqh rulings conflict with
 *   contemporary ethics, human rights, or maslaha, prioritizing the Quran's
 *   ethical trajectory over literalist hadith application. This reading
 *   instantiates a constraint on Islamic legal interpretation that operates
 *   as a tangled rope: it coordinates a coherent hermeneutical method for
 *   modern Muslim societies (benefiting progressive Muslims, women, LGBTQ+
 *   individuals, religious minorities) while extracting interpretive
 *   authority and material resources from traditional ulema, madhhab
 *   institutions, and scholarly councils whose legitimacy depends on the
 *   classical consensus (ijma) and taqlid. The constraint requires active
 *   enforcement where institutionalized — through state family law codes,
 *   constitutional courts, ministry of religious affairs directives, and
 *   reformist seminary curricula — but faces traditionalist
 *   counter-mobilization that contest its founding problem and seek to
 *   reassert classical authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.35).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad Mandate: Quranic Ethical Trajectory Over Literalist Hadith").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal/theoretical").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, 'e8cd1da4-b3ff-418a-8de9-03cb3d902d58').
narrative_ontology:cs_kernel_codification('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', fixed_text).
narrative_ontology:cs_authority_grounding('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', lineage).
narrative_ontology:cs_interpretation_layer_present('e8cd1da4-b3ff-418a-8de9-03cb3d902d58').
narrative_ontology:cs_reading_relation('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', foundational, quran_ethical_trajectory_supersedes_literalist_hadith).
narrative_ontology:cs_axiom_status(quran_ethical_trajectory_supersedes_literalist_hadith, holdable).
narrative_ontology:cs_axiom_grounding('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', quran_ethical_trajectory_supersedes_literalist_hadith, theological).
narrative_ontology:cs_axiom('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', secondary, contextual_ijtihad_mandated_for_contemporary_ethics).
narrative_ontology:cs_axiom_status(contextual_ijtihad_mandated_for_contemporary_ethics, holdable).
narrative_ontology:cs_axiom_grounding('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', contextual_ijtihad_mandated_for_contemporary_ethics, instrumental).
narrative_ontology:cs_reference_frame('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', quranic_ethical_trajectory).
narrative_ontology:cs_drift_state('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e8cd1da4-b3ff-418a-8de9-03cb3d902d58', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, women).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_individuals).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulema).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, madhhab_institutions).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditionalist_scholarly_councils).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, quranic_ethical_trajectory_hermeneutic).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, contextual_ijtihad_as_obligation).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__reformist_ijtihad, maslaha_as_supreme_legal_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and advocate the hermeneutical method prioritizing Quran's ethical trajectory over literalist hadith; publish tafsir, issue fatwas, teach in reformist seminaries and universities; face professional marginalization from traditionalist institutions but gain recognition in academic and some state contexts.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_scholars, agenda_setter,
    organized, generational, constrained, global).

% Seek Islamic legitimacy for gender-egalitarian, rights-affirming practice; use reformist ijtihad to navigate religious identity alongside contemporary ethics; constrained by community pressure and lack of institutional infrastructure in many regions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, biographical, constrained, global).

% Directly affected by classical fiqh rulings on marriage, divorce, inheritance, testimony, and public participation; reformist readings enable legal reforms in family law and public rights; exit from traditionalist frameworks often means social ostracism or loss of community.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, women, beneficiary,
    powerless, biographical, constrained, global).

% Classical rulings mandate severe penalties; reformist ijtihad offers only hermeneutical path to inclusion within Islamic discourse; in most jurisdictions exit from traditionalist framework means either apostasy accusations or complete disengagement from religious identity.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_individuals, beneficiary,
    powerless, biographical, trapped, global).

% Non-Muslim minorities in Muslim-majority societies benefit when reformist readings limit application of classical dhimmi rules and blasphemy codes; their situation improves when state adopts reformist framework but remains vulnerable to traditionalist counter-mobilization.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities, beneficiary,
    moderate, generational, constrained, regional).

% Hold interpretive monopoly through madhhab affiliation, fatwa authority, control of religious education and endowments (awqaf); reformist ijtihad directly threatens their epistemic authority and material base; identity fused with role as guardians of tradition makes exit conceptually impossible.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulema, payer,
    institutional, generational, identity_locked, global).

% Four Sunni schools (Hanafi, Maliki, Shafi'i, Hanbali) plus Ja'fari Shia school; maintain curricula, certify scholars, administer courts in some states; reformist mandate to bypass madhhab rulings via direct Quranic engagement undermines their institutional rationale.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, madhhab_institutions, payer,
    institutional, generational, identity_locked, global).

% State-appointed bodies (e.g., Al-Azhar, Council of Islamic Ideology Pakistan, Indonesian MUI) that issue official fatwas; their legitimacy rests on representing classical consensus; reformist readings bypass them, creating parallel authority structures.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditionalist_scholarly_councils, payer,
    institutional, generational, identity_locked, national).

% In some jurisdictions (Tunisia, Morocco, Indonesia, Turkey historically) adopt reformist readings selectively for family law, women's rights, minority protections; in others (Saudi Arabia, Iran, Pakistan) enforce traditionalist or state_hybrid readings; arbitrage between readings to manage legitimacy and international pressure.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, state_authorities, observer,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, state_authorities, agenda_setter).

% Muslims who view taqlid as religious obligation and reformist ijtihad as bid'ah (innovation); would object to characterization of classical rulings as unethical; structurally excluded from reformist scholarly production but constitute demographic majority in many regions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditionalist_lay_adherents, excluded,
    moderate, biographical, constrained, global).

% UN treaty bodies, INGOs, local rights groups use reformist readings as advocacy leverage; document how traditionalist readings violate CEDAW, CRC, ICCPR; provide external corroboration for reformist founding problem but do not participate in Islamic hermeneutics.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Scholars of Islamic law, comparative religion, legal anthropology, political theory who analyze the constraint from outside the commitment; no material stake in any reading's victory.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutical method for interpreting Islamic law in contemporary contexts by prioritizing the Quran's ethical trajectory over literalist hadith application, enabling context-sensitive rulings on ethics, human rights, and maslaha (public interest) without abandoning Islamic normative framework.
% TRANSFER_FUNCTION: Moves interpretive authority from traditional madhhab institutions and their certified scholars to contextually-engaged scholars and affected communities, redistributing the power to define Islamic normativity in family law, criminal law, minority rights, and gender relations.
% ABSENT_VOICES: Traditionalist lay adherents who view taqlid as binding obligation and reformist ijtihad as bid'ah; conservative communities in Muslim-majority societies where reformist readings have minimal institutional purchase; queer Muslims and women in traditionalist-dominated spaces who are excluded from both traditionalist discourse and state-mediated reformist frameworks that still marginalize them.
% DISAPPEARANCE_RATIONALE: If the reformist ijtihad mandate vanished overnight, traditionalist taqlid would reclaim interpretive monopoly in institutional spaces across multiple jurisdictions, reversing family law reforms (Tunisia, Morocco, Indonesia), rolling back women's rights gains, re-criminalizing LGBTQ+ existence under hudud, and restoring classical dhimmi frameworks for minorities. The constraint currently enables legal reforms that would be legislatively or judicially undone.
% FOUNDING_PROBLEM: Classical fiqh rulings derived in pre-modern historical contexts produce outcomes incompatible with contemporary ethics, universal human rights norms, and the Quran's own ethical trajectory when applied literally today — specifically on gender equality, religious freedom, bodily autonomy, and minority protections.
% FOUNDING_PROBLEM_CORROBORATION: Quranic scholars Fazlur Rahman (hermeneutics of ethical trajectory), Amina Wadud (gender jihad), Abdullahi An-Na'im (Islamic law and human rights) attest the founding problem from within the Islamic scholarly tradition. UN CEDAW Committee, OHCHR, and ICJ corroborate from outside the beneficiary set, documenting how classical rulings violate treaty obligations. Traditionalist scholars (Al-Azhar Senior Scholars Council, Pakistani Council of Islamic Ideology, Indonesian MUI) contest the founding problem, asserting the conflict is with Western secular values, not Quranic ethics.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the reformist reading redistributes interpretive capital from traditional institutions to new actors without fully displacing the traditionalist infrastructure; suppression is lower (0.35) than the traditionalist reading because the reformist method explicitly lowers barriers to alternative readings, though institutional enforcement in some states creates suppression of traditionalist dissent; theater ratio is low-moderate (0.25) as performative adherence to reformist language by state actors (e.g., Morocco's Moudawana reforms) sometimes masks unchanged patriarchal practice; accessibility collapse is moderate (0.45) because traditionalist readings remain widely accessible and socially dominant in most Muslim-majority societies; resistance is moderate-high (0.55) reflecting sustained traditionalist counter-mobilization through scholarly networks, social media, and state capture.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist scholar seat, the constraint is a genuine coordination mechanism (rope-like) solving the problem of Islamic law's contemporary relevance. From the traditional ulema seat, it is an extractive displacement of their authority (snare-like). From the women/LGBTQ+/minority seats, it is a necessary but incomplete liberation tool — the hermeneutic opens space but institutional enforcement lags. The engine computes this divergence from the structural data; the claimed tangled_rope captures the hybrid coordination/extraction structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars are agenda_setters (d ~ 0.2) — they structure the hermeneutic but bear professional costs. Progressive Muslims, women, LGBTQ+ individuals, and religious minorities are beneficiaries (d ~ 0.15-0.25) — they gain interpretive resources but remain constrained by social enforcement of traditional norms. Traditional ulema, madhhab institutions, and scholarly councils are payers (d ~ 0.85) — they lose epistemic monopoly and material control, with identity_locked exit making the extraction severe. State authorities are observers with secondary agenda_setter role (d ~ 0.4) — they arbitrage between readings for political legitimacy. Traditionalist lay adherents are excluded (d ~ 0.6) — they would reject the reformist framing but lack scholarly voice. Human rights advocates and analytical observers sit at analytical exit (d ~ 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classical fiqh's incompatibility with contemporary ethics) remains contested — traditionalists deny the incompatibility, attributing it to Western imposition. The reformist reading prevents mislabeling its coordination function as pure extraction by demonstrating tangible legal reforms benefiting vulnerable groups, but cannot resolve the mandatrophy because the traditionalist reading's founding problem (preservation of revealed law against historical contingency) is also live for its adherents. The constraint persists as tangled_rope because neither reading can fully displace the other across the Muslim world.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'How does the classification of this constraint change when evaluated as one reading of the quran_hadith_substrate kernel versus as a standalone constraint?',
    'Compare engine output for this reading alone against joint analysis with sibling readings traditionalist_taqlid and state_hybrid; trace how beneficiary/victim sets, ε values, and suppression metrics shift across readings of the same kernel.',
    'If kernel-level analysis shows the substrate itself has Mountain-like features (fixed text corpus) while readings are extractive overlays, the reformist reading''s moderate ε may be reinterpreted as the cost of coordinating interpretation over a fixed substrate rather than intrinsic extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the kernel''s fixed-text nature alters the extraction assessment for individual readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist readings under reformist institutional backing structural (state enforcement, court rulings) or internalized (scholarly consensus pressure, professional marginalization)?',
    'Track suppression metrics in jurisdictions where reformist readings gain then lose state backing (e.g., Turkey post-1980, Iran post-1979, Egypt post-2013); if suppression persists after state enforcement removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measure suggests — traditionalist scholars carry the suppression with them into exile or underground networks, affecting cross-generational transmission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist actors under reformist institutional dominance.').

omega_variable(
    institutional_vulnerability_of_moderate_epsilon,
    'The moderate ε (0.35-0.50) depends on institutional backing; what is the extraction trajectory when state_hybrid or traditionalist counter-mobilization captures institutions?',
    'Measure ε in jurisdictions undergoing reading-transition (e.g., Afghanistan 2001-2021, Tunisia 2011-2023, Malaysia 1980s-present); model ε as function of state_reading_alignment index.',
    'If ε spikes toward snare levels during traditionalist capture, the reformist reading''s tangled_rope classification is contingent on institutional conditions, not structural invariance — suggesting the reading itself may be a scaffold rather than stable tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vulnerability_of_moderate_epsilon, empirical, 'Institutional contingency of the reformist reading''s moderate extractiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qhs_reformist_tr_t1900, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(qhs_reformist_tr_t1930, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(qhs_reformist_tr_t1960, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(qhs_reformist_tr_t1980, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(qhs_reformist_tr_t2000, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(qhs_reformist_tr_t2024, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(qhs_reformist_be_t1900, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(qhs_reformist_be_t1930, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1930, 0.22).
narrative_ontology:measurement(qhs_reformist_be_t1960, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(qhs_reformist_be_t1980, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(qhs_reformist_be_t2000, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(qhs_reformist_be_t2024, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qhs_reformist_su_t1900, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(qhs_reformist_su_t1930, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(qhs_reformist_su_t1960, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(qhs_reformist_su_t1980, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(qhs_reformist_su_t2000, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(qhs_reformist_su_t2024, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__reformist_ijtihad, 0.08).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, family_law_reform).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, criminal_law_reform).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, minority_rights_protections).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, blasphemy_law_reform).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, womens_testimony_rights).

% DUAL FORMULATION NOTE:
% This constraint (reformist_ijtihad) and traditionalist_taqlid are distinct readings of the quran_hadith_substrate kernel with different ε, beneficiaries, victims, and suppression profiles. The kernel's fixed text (Quran + hadith corpus) is Mountain-like; the readings are extractive overlays. State_hybrid is a third reading that selectively instantiates elements of both depending on legal domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, institutional, 0.15).
constraint_indexing:directionality_override(quran_hadith_substrate__reformist_ijtihad, powerless, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
