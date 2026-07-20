% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew Living Language via Haskalah Literary Production
 *   domain: historical linguistics / language revitalization / commitment systems
 *
 * SUMMARY:
 *   The Haskalah (Jewish Enlightenment) produced a voluminous modern Hebrew
 *   literature â journalism, poetry, fiction, and scientific writing â
 *   without an accompanying community of native Hebrew speakers. This
 *   constraint story models the claim that such written generative competence
 *   constitutes linguistic life sufficient to render Hebrew a 'living
 *   language.' It is one reading of a three-way contested kernel: the
 *   liturgical continuity reading locates life in unbroken religious study
 *   and prayer; the native generation reading locates life only in
 *   mother-tongue speech. This reading coordinates a dispersed European
 *   Jewish intelligentsia around a secular print culture, extracting
 *   minimally and operating with low suppression.
 *
 * KEY AGENTS:
 *   - maskilim_intellectuals: Primary agenda-setter (moderate power, mobile exit) â defines the standard of linguistic life through literary production.
 *   - hebrew_reading_public: Primary beneficiary (moderate power, constrained exit) â consumes and orients around Hebrew print.
 *   - traditional_rabbinic_establishment: Excluded observer (institutional power, mobile exit) â maintains a competing liturgical definition of Hebrew continuity.
 *   - zionist_linguistic_reformers: Analytical observer (organized power, analytical exit) â later contests the sufficiency of written competence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew Living Language via Haskalah Literary Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical linguistics / language revitalization / commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '3b2ae82c-1788-474e-84db-c170173dec8d').
narrative_ontology:cs_kernel_codification('3b2ae82c-1788-474e-84db-c170173dec8d', fixed_text).
narrative_ontology:cs_authority_grounding('3b2ae82c-1788-474e-84db-c170173dec8d', lineage).
narrative_ontology:cs_interpretation_layer_present('3b2ae82c-1788-474e-84db-c170173dec8d').
narrative_ontology:cs_reading_relation('3b2ae82c-1788-474e-84db-c170173dec8d', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b2ae82c-1788-474e-84db-c170173dec8d', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('3b2ae82c-1788-474e-84db-c170173dec8d', foundational, written_generative_competence_suffices_for_linguistic_life).
narrative_ontology:cs_axiom_status(written_generative_competence_suffices_for_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('3b2ae82c-1788-474e-84db-c170173dec8d', written_generative_competence_suffices_for_linguistic_life, conventional).
narrative_ontology:cs_axiom('3b2ae82c-1788-474e-84db-c170173dec8d', foundational, literary_practice_unbroken_continuity).
narrative_ontology:cs_axiom_status(literary_practice_unbroken_continuity, holdable).
narrative_ontology:cs_axiom_grounding('3b2ae82c-1788-474e-84db-c170173dec8d', literary_practice_unbroken_continuity, conventional).
narrative_ontology:cs_reference_frame('3b2ae82c-1788-474e-84db-c170173dec8d', unbroken_written_generative_tradition).
narrative_ontology:cs_drift_state('3b2ae82c-1788-474e-84db-c170173dec8d', native_speaker_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b2ae82c-1788-474e-84db-c170173dec8d', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, maskilim_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce modern Hebrew literature, journalism, and scientific works across nineteenth-century Europe. Assert that the ability to compose new texts in Hebrew â generative written competence â keeps the language alive regardless of whether anyone speaks it at home. Their authority rests on mastery of the classical textual tradition and innovation within modern genres. Exit means abandoning Hebrew for German or Yiddish intellectual circles, which some did.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, maskilim_intellectuals, agenda_setter,
    moderate, generational, mobile, continental).

% Read Hebrew newspapers, novels, and essays as part of a modernizing Jewish identity. They benefit from a shared high-culture language that is neither the local vernacular nor the language of traditional religious study. They are not compelled to participate; alternatives in Yiddish and European languages are readily available, but Hebrew print offers a distinct secular Jewish cultural sphere.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_reading_public, beneficiary,
    moderate, biographical, constrained, continental).

% Maintain Hebrew through liturgical prayer, halakhic study, and legal writing. They are not party to the Haskalah's secular literary project and view maskilic Hebrew with suspicion, yet they do not suffer direct extraction from it. Their own framework treats Hebrew as continuously alive through religious obligation, independent of secular literary fashion.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditional_rabbinic_establishment, excluded,
    institutional, generational, mobile, continental).

% Emerge in the late nineteenth and early twentieth centuries to argue that only native mother-tongue speech can fully revive Hebrew. They observe the Haskalah's literary achievement but contend it produced a cultivated elite language, not a living tongue in the full linguistic sense. They stand outside the Haskalah beneficiary set and contest its sufficiency claim from the analytical seat of modern linguistics and nation-building.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, zionist_linguistic_reformers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared modern Hebrew literary culture across geographically dispersed Jewish communities without requiring religious observance or a community of native speakers.
% TRANSFER_FUNCTION: Moves cultural prestige and reader attention from traditional liturgical centers and from vernacular alternatives (Yiddish, German) to a secular Hebrew print sphere.
% ABSENT_VOICES: Traditional rabbinic authorities who hold that Hebrew life is liturgical; later Zionist linguists who hold that only native speech constitutes linguistic life. Both are outside the Haskalah's secular modernizing frame.
% DISAPPEARANCE_RATIONALE: Without the claim that written generative competence suffices, the Haskalah loses its linguistic foundation. Hebrew likely contracts to liturgical and study functions; modern Jewish secular expression defaults to Yiddish or co-territorial languages, and the expanded lexicon and syntax that enabled later native revival never develop.
% FOUNDING_PROBLEM: Jewish modernization in eighteenth- and nineteenth-century Europe needed a pan-Jewish high-culture language that was neither the 'low' vernacular (Yiddish) nor the assimilatory route (German/French), but Hebrew lacked native speakers and modern secular registers.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians and Yiddishist scholars outside the Haskalah beneficiary set corroborate the crisis of Jewish linguistic modernity; traditionalist sources attest the threat of assimilation from the opposite flank. The Haskalah's specific solution is contested by parties arguing for Yiddish modernism or native Hebrew revival.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.15) because the constraint coordinates voluntary literary participation without coercive extraction; suppression is minimal (0.08) because no active enforcement excludes alternatives (Yiddish, German, and liturgical Hebrew remain available). Theater ratio is low (0.12) â the literary production is functionally generative, not performative maintenance of an empty form. Accessibility collapse is moderate (0.35): once committed to the Hebrew literary sphere, alternatives are cognitively available but socially costly for the maskilic identity. Resistance is negligible (0.05) because the arrangement is opt-in. The measurement series shows slight drift upward in both extractiveness and theater as the native revival emerges and the literary-only claim becomes contested, but values remain in the rope band.
 *
 * PERSPECTIVAL GAP:
 *   The maskilim experience the constraint as genuine coordination they author; the traditionalist seat experiences it as a rival claimant to Hebrew authenticity; the later Zionist observer seat experiences it as an incomplete solution. The engine computes these divergences from the structural data â there is no victim seat because no party bears asymmetric cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim intellectuals sit near the beneficiary end (low d): they define the constraint and gain cultural prestige from its operation. Hebrew reading public sits near the symmetric-to-beneficiary range (low-moderate d): they gain access to a shared cultural sphere, paying only the opportunity cost of learning modern Hebrew. Traditional rabbinic establishment and Zionist reformers are outside the beneficiary structure, experiencing the constraint as an analytical or competing claim rather than an extractive one. No agent is structurally targeted.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as a snare or tangled rope because there is no active enforcement, no identified victim set, and no asymmetric extraction. The low theater ratio prevents piton classification. If the founding problem were read as dead and the constraint as persisting only by inertia, a piton signature might fire; the contested founding_problem_status and the ongoing functional coordination of Hebrew literary culture block that path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'How does the literary_revival_reading of hebrew_living_language structurally differ from its sibling readings?',
    'Comparative analysis of the three constraint stories in the kernel family, comparing beneficiary sets, victim sets, and coordination functions.',
    'Establishes that this reading has no victim set and very low extractiveness, whereas the native_generation_reading would introduce a victim set (non-native speakers) and higher extractiveness by delegitimizing literary competence alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer omega locating this reading within the hebrew_living_language kernel.').

omega_variable(
    written_sufficiency_for_linguistic_life,
    'Does sustained written generative competence without native daily speech satisfy the criteria for a language to be considered living?',
    'Cross-linguistic comparison with other literary languages (e.g., Latin, Sanskrit) and assessment of whether generative capacity in writing alone sustains the full functional range of language.',
    'If resolved affirmatively, this reading remains a robust rope; if resolved negatively, the reading may be reclassified as a scaffold (transitional to native speech) or a piton (inertial claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_sufficiency_for_linguistic_life, conceptual, 'Whether written competence alone constitutes linguistic life.').

omega_variable(
    continuity_or_revival,
    'Is the Haskalah literary chain a continuous extension of earlier Hebrew textual practice, or a discontinuous revival that invents modern registers?',
    'Historical philology tracing syntactic and lexical innovations in Haskalah texts against medieval and early modern Hebrew precursors.',
    'If discontinuous, the ''unbroken chain'' framing is theatrical and theater_ratio should rise; if continuous, the rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_or_revival, empirical, 'Whether the Haskalah literary tradition represents continuity or revival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__literary_revival_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t15, hebrew_living_language__literary_revival_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement(hebr_tr_t30, hebrew_living_language__literary_revival_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(hebr_tr_t45, hebrew_living_language__literary_revival_reading, theater_ratio, 45, 0.11).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__literary_revival_reading, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__literary_revival_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hebr_be_t15, hebrew_living_language__literary_revival_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(hebr_be_t30, hebrew_living_language__literary_revival_reading, base_extractiveness, 30, 0.13).
narrative_ontology:measurement(hebr_be_t45, hebrew_living_language__literary_revival_reading, base_extractiveness, 45, 0.14).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__literary_revival_reading, base_extractiveness, 60, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, native_generation_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct claims about linguistic vitality. This file models the Haskalah literary-production reading; siblings model liturgical and native-speech readings. Each has distinct beneficiary sets, coordination functions, and empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
