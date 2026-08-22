% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Usul al-Fiqh: Medinan Practice and Maslaha Mursala as Independent Sources
 *   domain: legal/religious
 *
 * SUMMARY:
 *   The Maliki school of Islamic law (predominant in North and West Africa)
 *   treats the continuous practice of Medina ('amal ahl al-Madina) as an
 *   independent source of law alongside the Quran and hadith, validates
 *   maslaha mursala (public interest unrestricted by specific text) as a
 *   legislative principle, and integrates local custom ('urf) where it does
 *   not contradict textual sources. This constraint story captures the Maliki
 *   reading of the usul al-fiqh kernel — the methodological commitments that
 *   distinguish it from Hanafi, Shafi'i, and Hanbali readings. The constraint
 *   operates as a tangled rope: it genuinely coordinates legal continuity and
 *   local legitimacy across vast regions (coordination function), but
 *   simultaneously extracts interpretive authority from universalist
 *   textualists and subordinates minority communities to majority customary
 *   norms (asymmetric extraction), and requires active enforcement through
 *   judicial appointments, educational curricula, and state recognition of
 *   the madhhab.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.32).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.25).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Usul al-Fiqh: Medinan Practice and Maslaha Mursala as Independent Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "legal/religious").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde').
narrative_ontology:cs_kernel_codification('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', formalized).
narrative_ontology:cs_authority_grounding('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', lineage).
narrative_ontology:cs_interpretation_layer_present('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde').
narrative_ontology:cs_reading_relation('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', usul_al_fiqh_method__shafii_reading, forecloses).
narrative_ontology:cs_reading_relation('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', foundational, medinan_practice_independent_hujja).
narrative_ontology:cs_axiom_status(medinan_practice_independent_hujja, holdable).
narrative_ontology:cs_axiom_grounding('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', medinan_practice_independent_hujja, deontological).
narrative_ontology:cs_axiom('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', foundational, maslaha_mursala_valid_legislative_source).
narrative_ontology:cs_axiom_status(maslaha_mursala_valid_legislative_source, holdable).
narrative_ontology:cs_axiom_grounding('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', maslaha_mursala_valid_legislative_source, instrumental).
narrative_ontology:cs_axiom('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', secondary, urf_legislative_where_not_contradicting_text).
narrative_ontology:cs_axiom_status(urf_legislative_where_not_contradicting_text, holdable).
narrative_ontology:cs_axiom_grounding('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', urf_legislative_where_not_contradicting_text, conventional).
narrative_ontology:cs_reference_frame('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', prophetic_medinan_practice_continuity).
narrative_ontology:cs_drift_state('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', post_colonial_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0cb9be7-3ac6-45c9-9a4b-f02ea7b64fde', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_scholarly_tradition).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_custom_holders).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, jurists_using_maslaha).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualist_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, minority_communities_under_customary_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_scholarly_establishment).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, medinan_practice_independent_evidentiary_weight).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, maslaha_mursala_valid_source).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, urf_integration_permitted).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the Madhhab's institutional continuity across North and West Africa, al-Andalus, and diaspora communities. The Maliki school's identity is constituted through the 'amal ahl al-Madina doctrine — abandoning it would dissolve the school's distinctive character. Collects institutional authority and interpretive monopoly from elevating Medinan practice to a source parallel to hadith.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_scholarly_establishment, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__maliki_reading, maliki_scholarly_establishment, beneficiary).

% The living carriers of Medinan practice in Medina and its intellectual diaspora. Their interpretive authority derives directly from proximity to the Prophet's city and the continuous transmission of 'amal. They benefit epistemically and institutionally when their practice is treated as an independent proof (hujja) rather than merely corroborating hadith.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_scholarly_tradition, beneficiary,
    organized, generational, constrained, regional).

% Communities whose 'urf (customary practices) — marriage customs, commercial norms, land tenure — gain legal recognition through the Maliki integration of 'urf where not contradicting text. They benefit when local knowledge is treated as legislatively relevant rather than dismissed as mere habit. Exit is constrained because customary law is embedded in social fabric; opting out means leaving the community.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_custom_holders, beneficiary,
    moderate, biographical, constrained, local).

% Scholars and judges who invoke maslaha mursala (unrestricted public interest) to legislate in areas without textual guidance — public finance, administrative law, medical ethics, technology regulation. They gain interpretive flexibility and policy relevance. Their exit is mobile: they can adopt other schools' more restrictive usul, but lose the Maliki toolkit's distinctive reach.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, jurists_using_maslaha, beneficiary,
    powerful, biographical, mobile, global).

% Scholars of other madhahib (especially Shafi'i and Hanbali) and modern Salafi/reformist currents who argue that only Quran, authenticated Sunnah, and consensus (ijma) are valid sources. They bear the cost of having their methodological purity contested — their demand for textual anchoring is treated as one position among many rather than the default. Exit is constrained because the Maliki methodology dominates entire legal systems (Morocco, Mauritania, parts of West Africa); engaging it is professionally unavoidable.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualist_scholars, payer,
    organized, generational, constrained, global).

% Non-Muslim or minority Muslim communities (Ibadi, Shia, Jewish, Christian) living under Maliki-administered customary law where 'urf integration incorporates majority customs that disadvantage them. They bear extraction when local custom overrides their own communal norms or when maslaha mursala is invoked to justify majoritarian policy. Exit is trapped: geographic, legal, and social barriers prevent opting out of the jurisdiction.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, minority_communities_under_customary_law, payer,
    powerless, biographical, trapped, local).

% Scholars of Islamic law, legal pluralism, and comparative jurisprudence who analyze the Maliki methodology as a case study in customary law recognition, public interest reasoning, and school identity formation. They neither collect nor pay; they map the structure.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_legal_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legal continuity across diverse regions by anchoring doctrine in the living practice of the Prophet's city (Medina) rather than in potentially fragmented hadith reports; provides a public-interest escape valve (maslaha mursala) for novel problems without textual precedent; integrates stable local customs ('urf) as legislative input, reducing friction between state law and social practice.
% TRANSFER_FUNCTION: Moves interpretive authority and legislative validity from universal textualist criteria (is there an authenticated hadith?) to situated epistemic criteria (what did the people of Medina continuously practice? what serves public interest? what does local custom establish?). The transfer is from textualist gatekeepers to practice-bearers and policy-oriented jurists.
% ABSENT_VOICES: Women's customary practices in domestic and commercial life — often distinct from the male scholarly 'amal recorded in the mukhtasar literature — are filtered through male jurists' selection of what counts as 'urf. Pre-colonial African customary systems that the Maliki school encountered and partially absorbed are represented only through the school's retrospective categorization, not their own self-description.
% DISAPPEARANCE_RATIONALE: If the Maliki distinctive sources vanished overnight, the legal systems of Morocco, Mauritania, Mali, Niger, Chad, and parts of Algeria, Tunisia, Libya, and Sudan would lose their methodological foundation. Judges would lack the tools (maslaha, 'urf, 'amal) they currently use for family law, finance, and administrative regulation. The Shafi'i or Hanbali methodologies would not seamlessly substitute — they reject maslaha mursala and treat 'urf as subordinate. The legal landscape would reorganize around either statutory codification (imported civil codes) or a different madhhab's usul.
% FOUNDING_PROBLEM: Early Islamic legal practice in Medina faced a crisis of hadith fabrication and geographic fragmentation: by the mid-2nd/8th century, thousands of hadiths of dubious authenticity circulated, and scholars in Iraq, Syria, and Egypt diverged wildly. The Medinan response was to trust the continuous, public, multi-generational practice of Medina's scholars and residents — 'amal ahl al-Madina — as a living transmission of the Prophetic sunna more reliable than isolated reports. Later, maslaha mursala and 'urf integration addressed the problem of governing newly Islamized societies where textual sources were silent.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars (Ibn al-Qasim, Sahnun, al-Qarafi, al-Shatibi) attest the founding problem is live: hadith unreliability persists, novel problems multiply, and custom remains legislatively relevant. Shafi'i theorists (al-Shafi'i himself, al-Juwayni, al-Ghazali) and Hanbali scholars (Ibn Taymiyya, Ibn Qayyim) attest the founding problem is substantially solved by hadith criticism methodology (mustalah al-hadith) and that maslaha mursala/'urf integration introduce subjective legislation. Modern historians of Islamic law (Schacht, Hallaq, Lucas, Lowry) corroborate from outside the beneficiary set: the hadith fabrication crisis was real, but the Maliki solution created its own path-dependency that now sustains the school's distinctiveness more than it solves the original epistemic problem.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).
:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) is moderate: the constraint transfers legislative validity from a universal textualist standard to situated epistemic communities, but the transfer is not purely zero-sum — the coordination function (legal stability in diverse societies) is real and valued by many participants. Suppression (0.25) is low-moderate: alternative methodologies (Shafi'i, Hanbali) are not banned; they coexist in the same intellectual ecosystem and even in the same jurisdictions. Theater ratio (0.18) is low: the 'amal and maslaha doctrines perform genuine epistemic and legislative work, though some invocations of maslaha in modern codification serve to legitimize state policy. Accessibility collapse (0.42) is moderate: a jurist trained in Maliki usul finds it difficult to 'unsee' the Medinan practice as a source, but can and does engage other methodologies. Resistance (0.38) is moderate: textualist critics (classical and modern) consistently challenge the epistemic status of 'amal and maslaha, but the school's institutional entrenchment absorbs the critique.
 *
 * PERSPECTIVAL GAP:
 *   From the Maliki establishment seat, the constraint is a rope: it solves the genuine coordination problem of legal unity across diversity with minimal coercion. From the universalist textualist seat, it is a snare: the coordination story is cover for elevating a particular regional practice to universal status. From the minority community seat, it is a snare with trapped exit: their subordination to majority 'urf is enforced by the same structure. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid reality that no single seat experiences purely.
 *
 * DIRECTIONALITY LOGIC:
 *   The Maliki scholarly establishment is the structural agenda-setter and beneficiary (d ~ 0.15): it administers the constraint and collects institutional authority from it. Medinan scholarly tradition and local custom holders are beneficiaries (d ~ 0.25-0.35): they gain epistemic recognition and legal validity. Jurists using maslaha are beneficiaries with mobile exit (d ~ 0.30): they gain tools but can leave. Universalist textualist scholars are payers (d ~ 0.70): their methodological claim is marginalized by the constraint's operation. Minority communities under customary law are trapped payers (d ~ 0.90): they bear the cost of majority custom's legal force with no exit. The analytical observer sits at d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (hadith fabrication crisis, governing new societies) was real and the Maliki tools addressed it. But the tools have become the school's identity: maslaha mursala and 'urf integration now serve as much to distinguish Maliki from Shafi'i/Hanbali as to solve legislative gaps. The mandatrophy is contested — the problem is not fully dead (novel problems persist) but the arrangement persists partly because it constitutes the school's boundary. The six-questions corroboration field captures this: Maliki scholars say live; outside historians say the original epistemic crisis is largely solved by hadith science, and the arrangement's persistence is now driven by identity-maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maliki_identity_vs_epistemic_necessity,
    'To what extent does the contemporary Maliki school maintain ''amal, maslaha, and ''urf because they remain epistemically necessary, versus because they constitute the school''s distinctive identity against rivals?',
    'Counterfactual analysis: if hadith criticism methodology (now mature) and modern statutory codification fully solved the original epistemic problems, would Maliki jurists still defend these sources as vigorously? Track scholarly discourse on whether maslaha/''urf are ''essential to the school'' vs ''useful tools''.',
    'If identity-maintenance dominates, the constraint''s extractiveness is higher than its coordination function warrants — the school sustains sources that extract from textualists and minorities partly to remain ''Maliki''. If epistemic necessity dominates, the tangled_rope classification is more balanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maliki_identity_vs_epistemic_necessity, conceptual, 'Whether the constraint''s persistence is driven by ongoing coordination need or school-identity maintenance.').

omega_variable(
    urf_gender_bias_in_custom_recognition,
    'Does the Maliki integration of ''urf systematically recognize male-dominated public customs (commercial, tribal) while filtering out or reshaping women''s domestic and informal customs through male juristic selection?',
    'Comparative analysis of ''urf citations in classical fiqh vs. anthropological records of women''s customary practices in Maliki regions; review of modern family law codes (Moudawana, etc.) for which customs were codified and which excluded.',
    'If systematic, the ''beneficiary: local_custom_holders'' declaration masks a gendered extraction — women in those communities are payers, not beneficiaries. This would add a victim class and increase measured extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(urf_gender_bias_in_custom_recognition, empirical, 'Gender bias in which customs gain legal recognition through ''urf integration.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the Maliki reading''s core premise (Medinan practice as independent source) logically foreclose the Shafi''i reading''s core premise (hadith authentication as prerequisite), or do they coexist as live options for different parties?',
    'Analyze whether a single legal framework could simultaneously treat Medinan practice as an independent hujja AND require hadith authentication for all derivations. The Shafi''i usul explicitly rejects ''amal ahl al-Madina as a proof; the Maliki usul treats it as one. A judge cannot apply both simultaneously in the same case.',
    'If forecloses, the kernel has a genuine logical fracture — readings are mutually exclusive within a framework. If coexists_with, the kernel hosts a stable pluralism where different jurisdictions adopt different readings. This determines the reading_relations declaration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between Maliki and Shafi''i readings of the usul kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 150, 1450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t150, usul_al_fiqh_method__maliki_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement(usul_tr_t300, usul_al_fiqh_method__maliki_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(usul_tr_t500, usul_al_fiqh_method__maliki_reading, theater_ratio, 500, 0.13).
narrative_ontology:measurement(usul_tr_t700, usul_al_fiqh_method__maliki_reading, theater_ratio, 700, 0.15).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__maliki_reading, theater_ratio, 900, 0.17).
narrative_ontology:measurement(usul_tr_t1100, usul_al_fiqh_method__maliki_reading, theater_ratio, 1100, 0.18).
narrative_ontology:measurement(usul_tr_t1450, usul_al_fiqh_method__maliki_reading, theater_ratio, 1450, 0.18).

% Extraction over time
narrative_ontology:measurement(usul_be_t150, usul_al_fiqh_method__maliki_reading, base_extractiveness, 150, 0.12).
narrative_ontology:measurement(usul_be_t300, usul_al_fiqh_method__maliki_reading, base_extractiveness, 300, 0.18).
narrative_ontology:measurement(usul_be_t500, usul_al_fiqh_method__maliki_reading, base_extractiveness, 500, 0.25).
narrative_ontology:measurement(usul_be_t700, usul_al_fiqh_method__maliki_reading, base_extractiveness, 700, 0.28).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__maliki_reading, base_extractiveness, 900, 0.3).
narrative_ontology:measurement(usul_be_t1100, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1100, 0.31).
narrative_ontology:measurement(usul_be_t1450, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1450, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t150, usul_al_fiqh_method__maliki_reading, suppression_requirement, 150, 0.1).
narrative_ontology:measurement(usul_su_t300, usul_al_fiqh_method__maliki_reading, suppression_requirement, 300, 0.15).
narrative_ontology:measurement(usul_su_t500, usul_al_fiqh_method__maliki_reading, suppression_requirement, 500, 0.2).
narrative_ontology:measurement(usul_su_t700, usul_al_fiqh_method__maliki_reading, suppression_requirement, 700, 0.22).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__maliki_reading, suppression_requirement, 900, 0.24).
narrative_ontology:measurement(usul_su_t1100, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1100, 0.25).
narrative_ontology:measurement(usul_su_t1450, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1450, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__maliki_reading, 0.08).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, maliki_family_law_codification).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, maliki_islamic_finance_framework).

% DUAL FORMULATION NOTE:
% This constraint (maliki_reading) and its three siblings (hanafi_reading, shafii_reading, hanbali_reading) form the usul_al_fiqh_method constraint family. They share the kernel_id 'usul_al_fiqh_method' but instantiate different constraints with different ε values, beneficiary/victim structures, and claimed types. The Maliki reading's ε (0.32) reflects its hybrid coordination/extraction structure; the Shafi'i reading's ε is lower (more rope-like, stricter textualism coordinates with less extraction); the Hanbali reading's ε is lower still (mountain-like textual fidelity); the Hanafi reading's ε is comparable (tangled rope with different beneficiary/payer structure). The decomposition follows the ε-invariance principle: 'usul al-fiqh' is not one constraint measured differently — it is four constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__maliki_reading, organized, 0.7).
constraint_indexing:directionality_override(usul_al_fiqh_method__maliki_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
