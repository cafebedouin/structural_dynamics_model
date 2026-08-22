% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__revisable_translation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV as Revisable Translation: Better Manuscripts and Linguistic Knowledge Justify Revision
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   This reading treats the KJV as a landmark 1611 translation that has been
 *   superseded by advances in manuscript discovery (especially Sinaiticus,
 *   Vaticanus, and the Oxyrhynchus papyri) and linguistic knowledge (Koine
 *   Greek, Biblical Hebrew, textual criticism methodology). The constraint is
 *   the scholarly and publishing consensus that translations should be
 *   revised when better evidence becomes available. This creates a
 *   coordination function: scholars set standards, publishers produce
 *   translations meeting those standards, and consumers choose among them.
 *   Extraction is low but nonzero — modern publishers capture revenue through
 *   copyrighted translations, and academic gatekeeping creates barriers for
 *   non-standard versions. Suppression is low: KJV-only advocates are
 *   excluded from the mainstream apparatus but maintain parallel
 *   institutions; no one is forced to use a modern translation. The theater
 *   ratio reflects increasing marketing-driven differentiation among
 *   functionally similar translations (e.g., multiple 'essentially literal'
 *   versions competing for the same market segment).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.28).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.15).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV as Revisable Translation: Better Manuscripts and Linguistic Knowledge Justify Revision").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious_studies/textual_criticism/theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, 'b84c9da9-5a56-4575-8c08-7e5a61866233').
narrative_ontology:cs_kernel_codification('b84c9da9-5a56-4575-8c08-7e5a61866233', fixed_text).
narrative_ontology:cs_authority_grounding('b84c9da9-5a56-4575-8c08-7e5a61866233', expertise).
narrative_ontology:cs_interpretation_layer_present('b84c9da9-5a56-4575-8c08-7e5a61866233').
narrative_ontology:cs_reading_relation('b84c9da9-5a56-4575-8c08-7e5a61866233', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('b84c9da9-5a56-4575-8c08-7e5a61866233', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('b84c9da9-5a56-4575-8c08-7e5a61866233', foundational, critical_text_superiority_over_textus_receptus).
narrative_ontology:cs_axiom_status(critical_text_superiority_over_textus_receptus, holdable).
narrative_ontology:cs_axiom_grounding('b84c9da9-5a56-4575-8c08-7e5a61866233', critical_text_superiority_over_textus_receptus, empirically_contingent).
narrative_ontology:cs_axiom('b84c9da9-5a56-4575-8c08-7e5a61866233', foundational, translation_should_reflect_best_available_evidence).
narrative_ontology:cs_axiom_status(translation_should_reflect_best_available_evidence, holdable).
narrative_ontology:cs_axiom_grounding('b84c9da9-5a56-4575-8c08-7e5a61866233', translation_should_reflect_best_available_evidence, instrumental).
narrative_ontology:cs_reference_frame('b84c9da9-5a56-4575-8c08-7e5a61866233', westcott_hort_critical_text_paradigm).
narrative_ontology:cs_drift_state('b84c9da9-5a56-4575-8c08-7e5a61866233', contemporary_eclectic_text_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b84c9da9-5a56-4575-8c08-7e5a61866233', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, modern_bible_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, consumers_of_modern_translations).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, textual_criticism_methodology).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, linguistic_advancement_justifies_revision).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, translation_quality_improves_with_better_sources).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce critical editions, evaluate manuscript evidence, and establish translation standards. Their expertise determines which readings enter modern translations. They compete for academic prestige and grant funding; their authority derives from methodological rigor recognized by peer institutions worldwide.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_biblical_scholars, agenda_setter,
    institutional, generational, arbitrage, global).

% License and market modern translations (NIV, ESV, CSB, NLT, etc.). They invest in translation teams, secure copyright on new versions, and capture revenue from consumer choice among translations. Their business model depends on the revisable-translation premise creating recurring demand for updated versions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, modern_bible_publishers, beneficiary,
    organized, biographical, mobile, global).

% Choose among competing modern translations based on readability, theological tradition, study features, and price. They benefit from linguistic clarity and access to better manuscript evidence. Switching translations is low-cost; they can own multiple versions simultaneously.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, consumers_of_modern_translations, beneficiary,
    organized, biographical, mobile, global).

% Hold that the KJV is exclusively inspired and inerrant. They view modern translations as corrupted and oppose their use in churches and institutions. Their position is excluded from mainstream academic and publishing discourse; they maintain parallel institutions (schools, publishers, churches). Exit from their position would require abandoning core identity commitments.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_only_advocates, excluded,
    organized, generational, identity_locked, global).

% Produce and advocate for thought-for-thought translations (NLT, CEV, Message). They share the revisable premise but differ on translation philosophy. They participate in the same scholarly ecosystem but occupy a distinct methodological niche; they do not contest the revisable premise itself.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, functional_equivalence_practitioners, observer,
    moderate, biographical, analytical, global).

% The methodological framework that evaluates manuscript evidence, applies linguistic knowledge, and produces critical texts. It is not an agent but a vindicated proposition: the constraint's operation validates this methodology as the legitimate arbiter of translation quality.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, textual_criticism_methodology, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kjv_text_1611__revisable_translation_reading, textual_criticism_methodology).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production and dissemination of Bible translations by establishing scholarly standards for manuscript evaluation, linguistic analysis, and translation quality. Enables consumers, churches, and institutions to choose among competing versions with confidence that each meets baseline scholarly criteria.
% TRANSFER_FUNCTION: Moves revenue from translation consumers (churches, individuals, institutions) to modern Bible publishers through copyrighted translation licenses. Moves epistemic authority from traditional textual traditions (Textus Receptus, KJV-only) to academic textual criticism. Moves interpretive control from single-authority traditions to a pluralistic marketplace of translations.
% ABSENT_VOICES: KJV-only advocates and Textus Receptus loyalists are structurally excluded from the scholarly and publishing apparatus that governs modern translation production. They would object to the premise that the KJV is improvable and that modern critical texts are superior. They exist in parallel institutional ecosystems (Independent Baptist, some Presbyterian, some Dutch Reformed circles) with their own publishers, seminaries, and distribution networks.
% DISAPPEARANCE_RATIONALE: If the revisable-translation premise vanished, the modern Bible publishing industry would lose its primary justification for new editions and copyrighted translations. Churches and consumers would revert to a single-authority model (likely KJV or Textus Receptus-based). Academic biblical scholarship would lose its central practical application. The entire ecosystem of modern translations, study Bibles, and translation-specific resources would collapse or radically restructure.
% FOUNDING_PROBLEM: The KJV (1611) was translated from late Byzantine manuscripts (Textus Receptus) and 16th-century Hebrew texts, using Early Modern English. By the late 19th century, discovery of earlier manuscripts (Codex Sinaiticus, Vaticanus, papyri) and advances in Greek/Hebrew linguistics revealed thousands of textual variants and archaic renderings that obscured meaning for modern readers.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by the entire field of textual criticism outside the KJV-only tradition: Metzger & Ehrman (Text of the NT), Aland & Aland (Text of the NT), the Nestle-Aland/UBS critical apparatuses, and every major seminary outside the KJV-only ecosystem. No significant scholar in mainstream biblical studies disputes that earlier manuscripts and better linguistics exist; the dispute is only over their theological significance.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).
:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects publisher copyright rents on modern translations and the academic prestige economy, but the constraint is primarily coordinative: it solves the problem of how to produce reliable translations from evolving evidence. Suppression (0.15) is low because participation is voluntary — churches and individuals freely choose translations, and KJV-only communities operate legally protected parallel institutions. Theater (0.22) captures the proliferation of translation brands (ESV, CSB, LSB, NASB, NKJV, MEV) that differentiate more on marketing and theological positioning than on manuscript base or linguistic methodology. Accessibility collapse (0.35) is moderate: the KJV remains fully available and widely used; alternatives have not collapsed. Resistance (0.45) is significant from KJV-only communities, but they are excluded from the coordinating mechanism rather than suppressed by it.
 *
 * PERSPECTIVAL GAP:
 *   From the scholarly/publisher seat, this is a genuine coordination rope: standards evolve, consumers benefit, the system self-corrects. From the KJV-only seat, the same structure appears as a snare: their textual tradition is marginalized, their identity is threatened, and the 'scholarly consensus' operates as an exclusionary gate. The engine will compute this divergence from the declared roles, power, and exit options. The claimed_type 'rope' reflects the authoring seat's structural judgment; the engine's per-seat computation may differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic scholars are agenda_setters with arbitrage-grade exit (global institutional mobility, methodological consensus as currency) — they sit at the beneficiary end (d ≈ 0.1). Modern publishers are beneficiaries with mobile exit (can shift catalog, acquire competitors) — also low d. Consumers are beneficiaries with mobile exit (costless switching) — d ≈ 0.2. KJV-only advocates are excluded and identity-locked — they experience the constraint as suppressive (high d) but their exclusion is structural (they reject the premise) not enforced by it. Functional equivalence practitioners are observers — they share the revisable premise and participate in the same market.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (archaic language, inferior manuscript base) remains live — new manuscripts are still discovered (e.g., P.Oxy. LXXXIII 5345, 2019) and linguistic understanding advances. The constraint has not atrophied into a piton; its coordination function is active. However, the rising theater_ratio suggests some extraction is accumulating around marketing-driven differentiation rather than scholarly improvement. The extractiveness trajectory (0.12→0.28) tracks the growth of the copyrighted Bible publishing industry. This is not mandatrophy but a genuine coordination function with an extractive fringe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    publisher_copyright_vs_scholarly_necessity,
    'How much of modern translation proliferation is driven by genuine scholarly improvement versus publisher incentives to create differentiated, copyrightable products?',
    'Comparative analysis of translation changes between editions: measure the proportion of revisions that reflect new manuscript evidence or linguistic advances versus stylistic/theological repositioning. Market concentration data (top publishers'' share of translation revenue).',
    'If publisher-driven differentiation dominates, the coordination function is being exploited for extraction (tangled_rope). If scholarly improvements dominate, the rope classification holds. Affects whether the rising extractiveness trajectory reflects rent-seeking or the cost of maintaining scholarly infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_copyright_vs_scholarly_necessity, empirical, 'Whether the constraint''s extraction is scholarly necessity or publisher rent-seeking.').

omega_variable(
    kjv_only_exclusion_mechanism,
    'Is the exclusion of KJV-only advocates from mainstream scholarly/publishing institutions structural (they reject the premise) or enforced (they would participate if allowed)?',
    'Survey KJV-only institutions: would they seek accreditation/participation in SBL, ETS, or major publisher review processes if doctrinal statements were not required? Track historical cases of scholars moving between ecosystems.',
    'If exclusion is self-selected (premise rejection), suppression is low and the rope classification is robust. If exclusion is enforced (gatekeeping), suppression is under-measured and the constraint trends toward tangled_rope. Also affects the identity_locked classification of KJV-only advocates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kjv_only_exclusion_mechanism, conceptual, 'Whether KJV-only exclusion is premise-rejection or institutional gatekeeping.').

omega_variable(
    reading_relations_framing,
    'Does the revisable_translation_reading logically foreclose the exclusive_inspiration_reading, or do they coexist as competing commitments held by different communities?',
    'Analyze whether any single institutional framework (denomination, seminary, church) formally holds both readings simultaneously. Track historical transitions: do institutions switch readings, or do they split?',
    'If forecloses: the readings are mutually exclusive within any coherent framework; the kernel is a genuine fault line. If coexists_with: the kernel hosts stable pluralism; the constraint family models a persistent dispute. Determines the reading_relations declaration in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_framing, conceptual, 'Structural relationship between revisable and exclusive readings of the KJV kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 1881, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1881, kjv_text_1611__revisable_translation_reading, theater_ratio, 1881, 0.05).
narrative_ontology:measurement(kjv__tr_t1901, kjv_text_1611__revisable_translation_reading, theater_ratio, 1901, 0.08).
narrative_ontology:measurement(kjv__tr_t1946, kjv_text_1611__revisable_translation_reading, theater_ratio, 1946, 0.12).
narrative_ontology:measurement(kjv__tr_t1971, kjv_text_1611__revisable_translation_reading, theater_ratio, 1971, 0.18).
narrative_ontology:measurement(kjv__tr_t1978, kjv_text_1611__revisable_translation_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(kjv__tr_t2001, kjv_text_1611__revisable_translation_reading, theater_ratio, 2001, 0.21).
narrative_ontology:measurement(kjv__tr_t2011, kjv_text_1611__revisable_translation_reading, theater_ratio, 2011, 0.22).
narrative_ontology:measurement(kjv__tr_t2024, kjv_text_1611__revisable_translation_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1881, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1881, 0.12).
narrative_ontology:measurement(kjv__be_t1901, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1901, 0.15).
narrative_ontology:measurement(kjv__be_t1946, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1946, 0.18).
narrative_ontology:measurement(kjv__be_t1971, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1971, 0.22).
narrative_ontology:measurement(kjv__be_t1978, kjv_text_1611__revisable_translation_reading, base_extractiveness, 1978, 0.25).
narrative_ontology:measurement(kjv__be_t2001, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2001, 0.26).
narrative_ontology:measurement(kjv__be_t2011, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2011, 0.27).
narrative_ontology:measurement(kjv__be_t2024, kjv_text_1611__revisable_translation_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t1881, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1881, 0.25).
narrative_ontology:measurement(kjv__su_t1901, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1901, 0.22).
narrative_ontology:measurement(kjv__su_t1946, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1946, 0.18).
narrative_ontology:measurement(kjv__su_t1971, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1971, 0.15).
narrative_ontology:measurement(kjv__su_t1978, kjv_text_1611__revisable_translation_reading, suppression_requirement, 1978, 0.14).
narrative_ontology:measurement(kjv__su_t2001, kjv_text_1611__revisable_translation_reading, suppression_requirement, 2001, 0.13).
narrative_ontology:measurement(kjv__su_t2011, kjv_text_1611__revisable_translation_reading, suppression_requirement, 2011, 0.14).
narrative_ontology:measurement(kjv__su_t2024, kjv_text_1611__revisable_translation_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__revisable_translation_reading, 0.02).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, modern_bible_publishing_industry).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, biblical_textual_criticism_standards).

% DUAL FORMULATION NOTE:
% This constraint decomposes the 'KJV debate' kernel into three structurally distinct readings. The revisable_translation_reading has low extraction (0.28) and low suppression (0.15), functioning as a rope. The exclusive_inspiration_reading has high suppression and identity-locked victims, functioning as a snare for its adherents. The functional_equivalence_reading shares the revisable premise but adds a complementary-purpose coordination layer. Their ε values differ because they describe different standing arrangements: the revisable reading describes the scholarly/publishing consensus; the exclusive reading describes the KJV-only institutional ecosystem; the functional reading describes the consumer-choice marketplace.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__revisable_translation_reading, organized, 0.15).
constraint_indexing:directionality_override(kjv_text_1611__revisable_translation_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
