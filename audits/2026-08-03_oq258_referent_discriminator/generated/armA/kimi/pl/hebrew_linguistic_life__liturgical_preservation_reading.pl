% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Vitality as Unbroken Liturgical Transmission
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the liturgical_preservation_reading of
 *   the hebrew_linguistic_life kernel. The constraint asserts that Hebrew
 *   linguistic vitality is constituted solely by the unbroken chain of sacred
 *   textual recitation and study, independent of vernacular mother-tongue
 *   acquisition. From this reading, Hebrew never died; the Ben-Yehuda revival
 *   is not resurrection but desecration of a language that was already alive
 *   in yeshivot and synagogues. The constraint is enforced by rabbinic
 *   lineages and diaspora recitation networks, and it structurally
 *   delegitimizes modern Hebrew, secular Zionist education, and the
 *   historical narrative of national revival.
 *
 * KEY AGENTS:
 *   - liturgical_authorities: Primary agenda_setter (institutional/constrained/global) — controls certification and transmission
 *   - traditional_recitation_communities: Primary beneficiary (organized/constrained/global) — receives continuity and identity
 *   - modern_hebrew_advocates: Primary payer (powerful/mobile/national) — bears delegitimization of their linguistic project
 *   - secular_educational_systems: Secondary payer (institutional/constrained/national) — state pedagogy contested as desecration
 *   - ben_yehuda_legacy_institutions: Tertiary payer (moderate/constrained/national) — historical narrative explicitly rejected
 *   - sacred_tradition: Non-agent payer (powerless/trapped/global) — the textual tradition itself instrumentalized as boundary marker
 *   - linguistic_anthropologists: Analytical observer (analytical/analytical/global) — tracks the contest without normative commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.6).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.6).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Vitality as Unbroken Liturgical Transmission").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, 'b8f1838a-136d-49d6-bc4a-23bf49f99082').
narrative_ontology:cs_kernel_codification('b8f1838a-136d-49d6-bc4a-23bf49f99082', fixed_text).
narrative_ontology:cs_authority_grounding('b8f1838a-136d-49d6-bc4a-23bf49f99082', lineage).
narrative_ontology:cs_interpretation_layer_present('b8f1838a-136d-49d6-bc4a-23bf49f99082').
narrative_ontology:cs_reading_relation('b8f1838a-136d-49d6-bc4a-23bf49f99082', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('b8f1838a-136d-49d6-bc4a-23bf49f99082', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('b8f1838a-136d-49d6-bc4a-23bf49f99082', foundational, liturgical_continuity_constitutes_vitality).
narrative_ontology:cs_axiom_status(liturgical_continuity_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('b8f1838a-136d-49d6-bc4a-23bf49f99082', liturgical_continuity_constitutes_vitality, theological).
narrative_ontology:cs_axiom('b8f1838a-136d-49d6-bc4a-23bf49f99082', foundational, modern_revival_is_desecration).
narrative_ontology:cs_axiom_status(modern_revival_is_desecration, holdable).
narrative_ontology:cs_axiom_grounding('b8f1838a-136d-49d6-bc4a-23bf49f99082', modern_revival_is_desecration, theological).
narrative_ontology:cs_reference_frame('b8f1838a-136d-49d6-bc4a-23bf49f99082', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('b8f1838a-136d-49d6-bc4a-23bf49f99082', zionist_revival_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b8f1838a-136d-49d6-bc4a-23bf49f99082', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, traditional_recitation_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, modern_hebrew_advocates).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, secular_educational_systems).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_legacy_institutions).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_transmission_narrative).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, diasporic_religious_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control certification of correct Hebrew pronunciation, scribal standards, and curriculum for advanced textual study. They enforce the standard that Hebrew linguistic vitality is constituted solely by sacred recitation and transmission, delegitimizing modern Hebrew innovation as desecration. Their institutional authority depends on maintaining exclusivity over legitimate language use.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Maintain prayer, Talmud study, and liturgical reading in Hebrew across diaspora communities. The unbroken-chain claim underwrites their communal identity and distinguishes their practice from secular Zionist language projects. They receive continuity, meaning, and boundary maintenance from the constraint.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditional_recitation_communities, beneficiary,
    organized, generational, constrained, global).

% Promote modern Hebrew as a native tongue and national language, primarily in Israel. Their historical narrative of linguistic resurrection is explicitly rejected by the liturgical reading, which classifies their speech as desecration rather than revival. They bear the cost of delegitimization in religious discourse and diaspora legitimacy contests.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, modern_hebrew_advocates, payer,
    powerful, biographical, mobile, national).

% Teach modern Hebrew as a first language through state schooling. The liturgical-preservation standard undermines their pedagogical legitimacy by denying that native acquisition or secular curricular content constitutes genuine Hebrew linguistic life, framing it instead as a profanation of the sacred tongue.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_educational_systems, payer,
    institutional, generational, constrained, national).

% Preserve and promote the historical narrative of Eliezer Ben-Yehuda as the reviver of Hebrew. The liturgical reading explicitly targets this narrative, labeling the modern revival project desecration rather than resurrection, thereby extracting historical legitimacy from these institutions.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_legacy_institutions, payer,
    moderate, generational, constrained, national).

% The textual and performative tradition itself, invoked as the justification for the constraint but bearing the cost of instrumentalization. Its semantic and historical fluidity is denied in favor of an idealized unbroken stasis; its spiritual content is subordinated to its function as a boundary marker against modernity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).

% Study the sociolinguistic contest between liturgical and modern Hebrew without taking a normative stance on which constitutes genuine linguistic life. They document the ideological work performed by the vitality definition itself.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, linguistic_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves transhistorical Jewish religious continuity across dispersed diaspora communities by maintaining a shared sacred language for prayer, textual study, and halakhic discourse independent of local vernaculars.
% TRANSFER_FUNCTION: Moves authority over Hebrew legitimacy from secular nationalist and academic institutions to rabbinic lineage holders and liturgical recitation networks; transfers semantic innovation potential away from vernacular speakers to preserved textual meanings.
% ABSENT_VOICES: Secular Hebrew poets, Palestinian Arabic speakers for whom Hebrew functions as a state language of control, and non-Orthodox Jewish movements that use Hebrew liturgically but reject the exclusivity claim are excluded or marginalized in the legitimacy conversation.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation standard vanished, the authority to define Hebrew vitality would shift to demotic and state institutions; modern Hebrew would lose its contested status as desecration, and diaspora liturgical communities would lose their primary claim to maintaining the sole authentic life of the language.
% FOUNDING_PROBLEM: The dissolution of Jewish political sovereignty and dispersion into diaspora communities speaking diverse vernaculars created a need to maintain religious and textual cohesion without territorial or state infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Liturgical authorities and historians of Jewish law attest the problem is still live, citing ongoing assimilation pressures. Secular Zionist historians and sociolinguists attest the problem was solved by modern statehood and revived vernacular Hebrew, rendering the liturgical-framing either obsolete or a minority preference; this corroboration comes from outside the benefiting parties.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint coordinates genuine religious continuity across dispersed communities (identity_coordination), but asymmetrically extracts authority from secular Zionist institutions and modern Hebrew speakers by denying their practice constitutes legitimate linguistic life. Active enforcement is required: rabbinic certification, curriculum control, and communal boundary maintenance. Extractiveness (0.60) reflects the substantial delegitimization of modern Hebrew; suppression (0.60) reflects the active exclusion of vernacular innovation from sacred spaces; theater_ratio (0.45) reflects the performative maintenance of 'unbroken chain' claims that smooth over historical textual variation. Resistance is high (0.70) because the modern Hebrew establishment and secular state constitute a powerful counter-hegemony, especially in Israel. The temporal measurements show extraction and suppression intensifying during the Zionist period (T=40-80) and remaining elevated, indicating that the constraint's adversarial function grew precisely when a viable alternative emerged.
 *
 * PERSPECTIVAL GAP:
 *   From the liturgical-authority seat, the constraint is protective coordination preserving a two-thousand-year heritage against nationalist desecration; from the modern-Hebrew-advocate seat, the same constraint is an obstructionist snare denying the empirical reality of millions of native speakers. The engine computes this divergence from the same structural data. The claimed type (tangled_rope) reflects the author's judgment that genuine coordination function and asymmetric extraction are both structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Liturgical authorities and recitation communities sit near the beneficiary end: the constraint subsidizes their institutional role and communal identity. Modern Hebrew advocates, secular educational systems, and Ben-Yehuda legacy institutions sit near the target end: the constraint extracts legitimacy from their practice and transfers it to liturgical gatekeepers. Sacred tradition, declared as a non-agent victim, occupies a paradoxical position: it is the nominal beneficiary of preservation but structurally pays the cost of instrumentalization into a political boundary marker; its authored position reflects the reading's own structural delta that identifies sacred tradition as the victim of modernist encroachment.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination function of transhistorical religious continuity; it prevents mislabeling it as pure coordination (rope) by requiring the declared victim set and active enforcement. The temporal measurements show extraction intensifying during the Zionist period and remaining elevated, a hallmark of tangled_rope rather than mountain or rope. If the founding problem (diasporic religious cohesion) were genuinely unsolved, a rope or scaffold classification might be warranted; the contested status of the problem and the persistent adversarial enforcement indicate hybrid coordination-extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_locus,
    'Is the locus of Hebrew linguistic life located in unbroken liturgical transmission or in native mother-tongue acquisition and practical coordination?',
    'Historical sociolinguistic analysis of Hebrew communicative function across the 18th-20th centuries, combined with ethnographic study of contemporary Ultra-Orthodox communities where liturgical Hebrew is insulated from modern Israeli Hebrew.',
    'If liturgical continuity is sufficient for linguistic life, this reading is structurally sound; if native acquisition is necessary, the liturgical reading describes a specialized religious register rather than a living language in the full sociolinguistic sense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_locus, conceptual, 'Contested kernel locus between liturgical and native-acquisition definitions of vitality').

omega_variable(
    sacred_tradition_instrumentalization,
    'Does the enforcement of liturgical-preservation-as-vitality protect sacred tradition or instrumentalize it as a boundary marker against modernity?',
    'Comparative analysis of liturgical Hebrew semantic drift within insular communities versus the semantic range of Modern Hebrew; assessment of whether the unbroken-chain claim obscures historical textual variation.',
    'If instrumentalized, the constraint extracts from the very tradition it claims to protect, shifting classification toward snare; if protective, the extraction is primarily external to the tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_tradition_instrumentalization, conceptual, 'Ambiguity over whether the tradition is beneficiary or instrument of extraction').

omega_variable(
    historical_death_or_dormancy,
    'Was Hebrew a linguistically dormant register between the late Biblical and modern periods, or did continuous liturgical and literary use constitute unbroken linguistic life?',
    'Linguistic historiography tracing Hebrew morphological, syntactic, and lexical development across the medieval and early modern periods; analysis of whether liturgical use maintained full linguistic competence or a restricted ceremonial competence.',
    'If dormant, the liturgical reading misdescribes the historical record and the constraint rests on a false genealogy; if continuously alive, the native-generational reading is historically inaccurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_death_or_dormancy, empirical, 'Empirical status of Hebrew linguistic continuity prior to modern revival').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(hebr_tr_t80, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(hebr_tr_t120, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 120, 0.42).
narrative_ontology:measurement(hebr_tr_t160, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 160, 0.45).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(hebr_be_t80, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(hebr_be_t120, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 120, 0.58).
narrative_ontology:measurement(hebr_be_t160, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 160, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(hebr_su_t80, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(hebr_su_t120, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 120, 0.58).
narrative_ontology:measurement(hebr_su_t160, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 160, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the hebrew_linguistic_life kernel. Each reading instantiates a different definition of linguistic vitality with different beneficiary/victim structures and different epsilon values. They are linked as a constraint family because they compete to classify the same historical object (the Hebrew language) but are not reducible to one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
