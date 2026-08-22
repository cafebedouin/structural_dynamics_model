% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: Functional Equivalence Translation Pluralism (KJV 1611 Context)
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   This constraint describes the functional-equivalence reading of the KJV
 *   1611 kernel: multiple English translations serve complementary purposes,
 *   with the KJV valued for literary/historical reasons and modern versions
 *   for clarity. The constraint emerged from the 1881 Revised Version
 *   breakthrough and matured through the 20th-century translation boom (RSV,
 *   NASB, NIV, NKJV, ESV, NLT, CSB). It operates as a rope: genuine
 *   coordination (shared scriptural access across diverse needs) with minimal
 *   coercion, low extraction, and no active enforcement of a single text. The
 *   commercial publishing ecosystem and denominational pluralism sustain it
 *   without central mandate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.18).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.08).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "Functional Equivalence Translation Pluralism (KJV 1611 Context)").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious_studies/textual_criticism/theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, '948fd4ab-0e6f-4093-b0db-cbd39ab041b0').
narrative_ontology:cs_kernel_codification('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', fixed_text).
narrative_ontology:cs_authority_grounding('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', distributed).
narrative_ontology:cs_reading_relation('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', kjv_text_1611__exclusive_inspiration_reading, coexists_with).
narrative_ontology:cs_reading_relation('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', kjv_text_1611__revisable_translation_reading, influences).
narrative_ontology:cs_axiom('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', foundational, translation_pluralism_epistemic_humility).
narrative_ontology:cs_axiom_status(translation_pluralism_epistemic_humility, holdable).
narrative_ontology:cs_axiom_grounding('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', translation_pluralism_epistemic_humility, deontological).
narrative_ontology:cs_axiom('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', foundational, functional_equivalence_methodology).
narrative_ontology:cs_axiom_status(functional_equivalence_methodology, holdable).
narrative_ontology:cs_axiom_grounding('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', functional_equivalence_methodology, instrumental).
narrative_ontology:cs_reference_frame('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', post_revised_version_pluralism).
narrative_ontology:cs_drift_state('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', contemporary_translation_ecosystem, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('948fd4ab-0e6f-4093-b0db-cbd39ab041b0', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, bible_readers_general).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, scholarly_translators).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, pastoral_ministry_practitioners).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, academic_biblical_studies).
narrative_ontology:constraint_vindicates(kjv_text_1611__functional_equivalence_reading, translation_pluralism_epistemic_humility).
narrative_ontology:constraint_vindicates(kjv_text_1611__functional_equivalence_reading, functional_equivalence_methodology).
narrative_ontology:constraint_vindicates(kjv_text_1611__functional_equivalence_reading, textual_criticism_ongoing_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access multiple English translations suited to different needs: KJV for literary beauty and historical continuity, modern versions (NIV, ESV, NLT, CSB) for contemporary clarity. Can freely choose among translations without institutional penalty. No single translation is mandated for salvation or orthodoxy.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, bible_readers_general, beneficiary,
    moderate, biographical, mobile, global).

% Produce and revise translations using evolving manuscript discoveries and linguistic scholarship. Their professional legitimacy rests on the acceptance that translation is revisable and that multiple approaches (formal equivalence, functional equivalence, optimal equivalence) have complementary value. Coordinate through scholarly societies and peer review rather than ecclesiastical mandate.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, scholarly_translators, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, scholarly_translators, agenda_setter).

% Select translations for congregational use based on pastoral judgment: KJV for traditions valuing literary heritage, modern versions for accessibility. Their coordination challenge is managing congregational preferences and inter-generational tension, not enforcing a single text. Some face pressure from exclusive-inspiration factions within their denominations.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, pastoral_ministry_practitioners, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, pastoral_ministry_practitioners, agenda_setter).

% Treats the KJV as a historical artifact of early 17th-century English Protestantism and translation history, while using critical editions (NA28/UBS5, BHS) and modern translations for research and teaching. The field's professional structure assumes textual criticism improves understanding; no single translation holds authority.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, academic_biblical_studies, beneficiary,
    institutional, generational, arbitrage, global).

% Hold that the KJV (or the Textus Receptus underlying it) is uniquely inspired and inerrant. View functional-equivalence pluralism as theological compromise. Are structurally excluded from the coordination benefits of translation pluralism because their identity commitment requires rejecting the premise that multiple translations can be complementary. Their exit from this constraint would require identity rupture.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, exclusive_inspiration_adherents, excluded,
    organized, generational, identity_locked, national).

% Produces and markets multiple translations (KJV, NKJV, NIV, ESV, NLT, CSB, etc.) as distinct products for different market segments. Benefits commercially from pluralism: no single translation dominates the market completely, creating sustained demand for new editions, study Bibles, and niche translations. Coordinates de facto through market dynamics rather than ecclesiastical authority.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, bible_publishing_industry, agenda_setter,
    powerful, biographical, arbitrage, global).

% Study the reception history of the KJV and the shift from single-authoritative-text models to pluralistic translation ecosystems. Analyze how the constraint's coordination function (shared scriptural access across linguistic diversity) interacts with its extraction dynamics (commercial, ideological, identity-based).
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, historical_theology_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables diverse English-speaking Christian communities to access Scripture in forms suited to their linguistic, liturgical, and theological needs without requiring a single mandated translation. Solves the coordination problem of 'which English Bible shall we use?' by legitimizing multiple answers for different contexts.
% TRANSFER_FUNCTION: Moves translational labor and scholarly investment from a single centralized text-production model (the KJV's 1611 monopoly) to a distributed ecosystem where publishers, scholars, and denominations invest in multiple translation projects. Readers bear the cognitive cost of navigating translation differences but gain accessibility matched to their context.
% ABSENT_VOICES: Exclusive-inspiration adherents (KJV-Onlyists) are structurally excluded from the pluralistic coordination because their theological commitment requires rejecting the constraint's core premise. They exist primarily in Independent Baptist, some Reformed, and certain fundamentalist circles — their absence from the pluralistic table is not geographic but identity-constitutive.
% DISAPPEARANCE_RATIONALE: If functional-equivalence pluralism vanished overnight, either a single translation would be re-imposed (likely the KJV or a new institutional standard) — recreating the pre-1881 monopoly — or fragmentation would accelerate with no shared coordination framework. The publishing industry would lose its multi-product model; denominations would face renewed translation wars; readers would lose calibrated accessibility options.
% FOUNDING_PROBLEM: The post-KJV monopoly problem: by the late 19th century, the KJV's language had become archaic, its manuscript base (Textus Receptus) was superseded by older and better manuscripts, and English-speaking Christianity had diversified beyond a single ecclesiastical authority that could mandate one translation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the Revised Version (1881-1885) preface explicitly citing archaic language and superior manuscripts; by the Lockman Foundation (NASB), International Bible Society (NIV), and Crossway (ESV) founding documents stating accessibility and manuscript advances as motivation; by mainline Protestant and Catholic ecclesiastical bodies that authorized multiple translations for liturgical use. No corroboration from exclusive-inspiration circles, who deny the problem exists.
narrative_ontology:disappearance_verdict(kjv_text_1611__functional_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__functional_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__functional_equivalence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kjv_text_1611__functional_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__functional_equivalence_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).
:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because no party extracts monopoly rents from translation control — readers choose freely, publishers compete, scholars revise openly. Suppression is minimal (0.08) because alternatives are not suppressed; the exclusive-inspiration reading exists but cannot block others. Theater ratio is low (0.12) because the coordination function (accessible Scripture) is genuinely performed, not merely performed. Accessibility collapse is low (0.15) because the KJV remains available and used; alternatives have not collapsed. Resistance (0.35) comes primarily from exclusive-inspiration adherents who contest the constraint's legitimacy, not from those it governs.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, this is a rope: genuine coordination with negligible extraction. From the excluded seat (exclusive-inspiration adherents), the same structure appears as a snare: their identity-commitment is delegitimized by the pluralistic framework, and they experience the constraint as suppressing their textual authority. The engine computes this divergence from the structural data — the claimed_type (rope) reflects the dominant coordination experience, not the excluded seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (readers, translators, pastors, academics) have mobile or arbitrage exit — they can use any translation or none. The publishing industry (agenda_setter) has arbitrage exit across its product portfolio. Exclusive-inspiration adherents are excluded and identity-locked: their exit would require theological rupture. The engine should compute low d for beneficiaries, near-symmetric d for publishers, high d for excluded identity-locked agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (archaic language + better manuscripts + fragmented authority) remains live: English continues to evolve, manuscript discoveries continue, and no single ecclesiastical authority has re-emerged to mandate one translation. The constraint has not atrophied into a piton — its coordination function is actively performed by the publishing ecosystem and denominational choices. Mandatrophy is not resolved because the problem persists and the arrangement continues solving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_capture_of_pluralism,
    'Does the bible publishing industry''s commercial incentive structure covertly shape which translations are produced and promoted, such that pluralism serves market segmentation more than reader need?',
    'Compare translation portfolios across major publishers (Zondervan/HarperCollins, Crossway, Broadman & Holman, Tyndale) for redundancy vs. complementarity; analyze marketing spend allocation; track whether niche-audience translations receive sustained investment or are loss-leaders.',
    'If commercial capture is substantial, the constraint''s extraction is higher than measured — publishers extract rent from segmentation while presenting it as coordination. Would shift classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_capture_of_pluralism, empirical, 'Whether translation pluralism is genuinely reader-serving or publisher-captured.').

omega_variable(
    excluded_identity_lock_severity,
    'For exclusive-inspiration adherents, is the identity-lock to the KJV-Only position primarily theological (soteriological conviction) or sociological (community boundary maintenance)?',
    'Ethnographic study of KJV-Only communities: track deconversion narratives; analyze whether theological argument or social embeddedness is the primary retention mechanism; measure correlation between KJV-Only commitment and other identity-fused positions.',
    'If primarily sociological, the constraint''s suppression of this group is more structural than internalized — the pluralistic framework actively marginalizes a community that could otherwise participate. If primarily theological, the exclusion is self-imposed by the group''s own epistemic commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_identity_lock_severity, conceptual, 'Mechanism of identity-lock for the excluded seat.').

omega_variable(
    coordination_cost_vs_extraction_tradeoff,
    'Do the increased coordination costs of pluralism (pastoral decision fatigue, congregational division, catechetical complexity) constitute a form of diffuse extraction borne by communities rather than a centralized extractor?',
    'Survey pastors and denominational leaders on time/resources spent managing translation issues; compare congregational cohesion metrics across single-translation vs. multi-translation churches; analyze catechetical material costs.',
    'If coordination costs are high and systematically borne by less-resourced communities, the constraint extracts diffusely — a rope with hidden extraction. Would support a tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_vs_extraction_tradeoff, empirical, 'Whether pluralism''s coordination costs function as diffuse extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 1881, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t1881, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1881, 0.05).
narrative_ontology:measurement(kjv__tr_t1901, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1901, 0.06).
narrative_ontology:measurement(kjv__tr_t1946, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1946, 0.08).
narrative_ontology:measurement(kjv__tr_t1971, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(kjv__tr_t1978, kjv_text_1611__functional_equivalence_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(kjv__tr_t2001, kjv_text_1611__functional_equivalence_reading, theater_ratio, 2001, 0.11).
narrative_ontology:measurement(kjv__tr_t2011, kjv_text_1611__functional_equivalence_reading, theater_ratio, 2011, 0.11).
narrative_ontology:measurement(kjv__tr_t2025, kjv_text_1611__functional_equivalence_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(kjv__be_t1881, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1881, 0.05).
narrative_ontology:measurement(kjv__be_t1901, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1901, 0.07).
narrative_ontology:measurement(kjv__be_t1946, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1946, 0.1).
narrative_ontology:measurement(kjv__be_t1971, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1971, 0.12).
narrative_ontology:measurement(kjv__be_t1978, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 1978, 0.15).
narrative_ontology:measurement(kjv__be_t2001, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 2001, 0.16).
narrative_ontology:measurement(kjv__be_t2011, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 2011, 0.17).
narrative_ontology:measurement(kjv__be_t2025, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t1881, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1881, 0.15).
narrative_ontology:measurement(kjv__su_t1901, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1901, 0.12).
narrative_ontology:measurement(kjv__su_t1946, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1946, 0.1).
narrative_ontology:measurement(kjv__su_t1971, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1971, 0.08).
narrative_ontology:measurement(kjv__su_t1978, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 1978, 0.07).
narrative_ontology:measurement(kjv__su_t2001, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 2001, 0.07).
narrative_ontology:measurement(kjv__su_t2011, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 2011, 0.08).
narrative_ontology:measurement(kjv__su_t2025, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__functional_equivalence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__functional_equivalence_reading, 0.02).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% KJV 1611 kernel family: three readings decomposing the natural-language claim 'the KJV's status.' exclusive_inspiration_reading = snare (high extraction, identity-locked victims). functional_equivalence_reading = rope (low extraction, pluralistic coordination). revisable_translation_reading = scaffold (transitional, sunset toward critical-text consensus). This reading (functional_equivalence) is the stable pluralistic equilibrium; exclusive_inspiration is the extractive rejection; revisable_translation is the scholarly trajectory that functional_equivalence institutionalized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__functional_equivalence_reading, organized, 0.15).
constraint_indexing:directionality_override(kjv_text_1611__functional_equivalence_reading, powerful, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
