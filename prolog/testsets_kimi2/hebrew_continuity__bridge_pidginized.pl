% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact Bridge (Pidginized Reading)
 *   domain: sociolinguistic/commitment_systems
 *
 * SUMMARY:
 *   Hebrew occupies a contested space in contemporary Jewish life: for
 *   liturgical authorities it lives through sacred recitation, for Israeli
 *   native speakers through daily generative intuition, and for the diaspora
 *   institutional complex it lives as a functional contact bridge. This
 *   constraint story instantiates the bridge_pidginized reading of the
 *   hebrew_continuity kernel: Hebrew persists as a high-register written code
 *   and marketplace pidgin for diaspora interaction, neither purely
 *   liturgical nor fully native. Sparse native speakers outside Israel, heavy
 *   reliance on institutional instruction, and the delegitimization of
 *   spontaneous contact varieties characterize this arrangement. Both
 *   liturgical and native readings dismiss this as 'not really Hebrew,' yet
 *   the continuity bureaucracy, educational publishers, and prestige
 *   producers extract significant institutional and economic rents from
 *   maintaining the bridge. The claim is tangled_rope: genuine coordination
 *   function (inter-diaspora communication) fused with asymmetric extraction
 *   (delegitimization of contact speakers for institutional gain).
 *
 * KEY AGENTS:
 *   - continuity_bureaucracy: Primary agenda-setter and beneficiary (institutional/identity_locked) â administers the diaspora Hebrew apparatus and captures state and philanthropic funding.
 *   - diaspora_contact_speakers: Primary target (moderate/identity_locked) â bear the costs of acquiring a non-native high register while their own contact variety is dismissed.
 *   - hebrew_prestige_producers: Secondary beneficiary (organized/mobile) â confer legitimacy through scarce high-register fluency.
 *   - educational_publishers: Secondary beneficiary (organized/mobile) â monetize the legitimacy gap.
 *   - liturgical_authorities: Excluded institutional voice (institutional/identity_locked) â dismiss contact Hebrew as profane.
 *   - native_speaker_purists: Excluded organized voice (organized/mobile) â dismiss contact Hebrew as deficient interlanguage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.62).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.58).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact Bridge (Pidginized Reading)").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistic/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '283e5b53-93e1-4c8d-a287-c20118d2eda4').
narrative_ontology:cs_kernel_codification('283e5b53-93e1-4c8d-a287-c20118d2eda4', formalized).
narrative_ontology:cs_authority_grounding('283e5b53-93e1-4c8d-a287-c20118d2eda4', extraction).
narrative_ontology:cs_interpretation_layer_present('283e5b53-93e1-4c8d-a287-c20118d2eda4').
narrative_ontology:cs_reading_relation('283e5b53-93e1-4c8d-a287-c20118d2eda4', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('283e5b53-93e1-4c8d-a287-c20118d2eda4', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('283e5b53-93e1-4c8d-a287-c20118d2eda4', foundational, instrumental_sufficiency).
narrative_ontology:cs_axiom_status(instrumental_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('283e5b53-93e1-4c8d-a287-c20118d2eda4', instrumental_sufficiency, instrumental).
narrative_ontology:cs_axiom('283e5b53-93e1-4c8d-a287-c20118d2eda4', foundational, contact_legitimacy).
narrative_ontology:cs_axiom_status(contact_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('283e5b53-93e1-4c8d-a287-c20118d2eda4', contact_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('283e5b53-93e1-4c8d-a287-c20118d2eda4', instrumental_diaspora_coordination).
narrative_ontology:cs_drift_state('283e5b53-93e1-4c8d-a287-c20118d2eda4', post_statehood_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('283e5b53-93e1-4c8d-a287-c20118d2eda4', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, continuity_bureaucracy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, educational_publishers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, hebrew_prestige_producers).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, diaspora_contact_speakers).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, contact_language_vitality).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, instrumental_continuity_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers diaspora Hebrew education, certification, and programming through state and philanthropic funding; sets curricular standards that impose high-register written Hebrew and marketplace pidgin on learners; collects institutional budget and mission legitimacy from the claim that Hebrew remains a living bridge language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, continuity_bureaucracy, agenda_setter,
    institutional, generational, identity_locked, global).

% Use Hebrew for travel, business, study, and ritual within diaspora contexts; their spontaneous spoken variety is a contact pidgin influenced by local vernaculars; invest heavily in formal instruction to acquire institutional legitimacy; routinely dismissed by native speakers and liturgical authorities as not producing real Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_contact_speakers, payer,
    moderate, biographical, identity_locked, global).

% Authors, journalists, and academics who produce the high-register written Hebrew that serves as the target model for diaspora learners; their scarce fluency confers cultural prestige and authority over what counts as correct Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_prestige_producers, beneficiary,
    organized, biographical, mobile, national).

% Produce textbooks, digital courses, and standardized examinations for the diaspora Hebrew market; profit from the persistent gap between learners' contact needs and the legitimate high-register model they are required to master.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, educational_publishers, beneficiary,
    organized, generational, mobile, global).

% Rabbinic and religious courts that control sacred Hebrew liturgy; dismiss contact pidgin and marketplace Hebrew as profane or incorrect; hold latent definitional veto through theological legitimacy but are not consulted in secular diaspora language programming.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_authorities, excluded,
    institutional, civilizational, identity_locked, global).

% Israeli native speakers and linguists who insist Hebrew lives only through intuitive generative use; treat diaspora contact speech as error-ridden interlanguage; their dismissal is structurally echoed in testing and certification standards.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_speaker_purists, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared instrumental code for Jewish diaspora communities across different native languages, enabling trade, pilgrimage, study, and inter-community communication without requiring full native acquisition.
% TRANSFER_FUNCTION: Moves linguistic capital and institutional legitimacy from diaspora learners to the continuity bureaucracy and prestige producers; moves tuition and publishing revenue from learners to educational publishers; moves definitional authority away from liturgical exclusivity toward instrumental utility.
% ABSENT_VOICES: Liturgical authorities who hold sacred exclusivity over Hebrew, and native speaker purists who treat only intuitive generative use as legitimate, are structurally excluded from the bridge arrangement's design; they would object that contact pidgin profanes or impoverishes the language. English-as-lingua-franca alternatives are ideologically suppressed.
% DISAPPEARANCE_RATIONALE: If the bridge arrangement vanished, diaspora Jewish communities would default to English or local vernaculars for inter-community contact; the institutional complex of ulpanim, testing, and diaspora Hebrew media would lose its primary function; the prestige economy of high-register Hebrew would contract to Israel-only circulation.
% FOUNDING_PROBLEM: Jewish diaspora communities lacked a shared medium for inter-community communication after the decline of Yiddish and the dispersal of languages; early Zionist revival sought to make Hebrew a national language, while diaspora continuity required a manageable contact register for international Jewish interaction.
% FOUNDING_PROBLEM_CORROBORATION: The continuity bureaucracy and educational publishers attest the problem is still live. Independent sociolinguists outside the beneficiary set note that English now serves most diaspora contact functions, suggesting the founding problem is either solved by other means or has shifted to identity-marking rather than instrumental need; no independent corroboration exists that instrumental contact deficiency remains acute.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the arrangement requires diaspora learners to invest heavily in a high-register model that exceeds their contact needs, while the contact variety they naturally produce is denied legitimacy. Suppression (0.58) is active: the continuity bureaucracy enforces curricular standards, testing regimes, and cultural narratives that delegitimize contact pidgin and suppress English alternatives. Theater ratio (0.45) reflects the growing gap between performative declarations of Hebrew vitality and the sparse actual generative use in diaspora. Accessibility collapse (0.48) is moderate: alternatives (English, accepting contact Hebrew) exist but are ideologically foreclosed for identity-committed learners. Resistance (0.48) comes from diaspora learners who code-switch or abandon formal study, and from liturgical and native purists who reject the bridge model.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat perceives the constraint as necessary coordination preserving Jewish peoplehood across dispersion; the payer seat experiences it as an endless, expensive pursuit of a legitimacy that recedes upon contact with native speakers. The engine computes this divergence from the structural asymmetry in power (institutional vs. moderate), exit (identity_locked vs. mobile), and role (agenda_setter/beneficiary vs. payer).
 *
 * DIRECTIONALITY LOGIC:
 *   The continuity bureaucracy sits near the beneficiary end: it sets rules, collects budgets, and defines legitimacy. Diaspora contact speakers sit near the target end: they pay tuition, absorb stigma, and their spontaneous speech is the object of suppression. Prestige producers and educational publishers are intermediate beneficiaries who capture secondary rents. Liturgical authorities and native speaker purists are structurally excluded from the bridge arrangement but their rejection exerts ambient pressure that increases enforcement costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdiaspora lack of shared mediumâhas arguably been solved by English or shifted to symbolic identity rather than instrumental need. If the problem is dead but the arrangement persists, the constraint risks piton status. However, the active coordination function (some inter-community communication still flows through Hebrew) and ongoing institutional investment prevent pure mandatrophy. The tangled_rope classification captures that genuine coordination and extraction are co-present, not that the coordination is merely a cover story (which would be snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contact_native_threshold,
    'At what point does a contact pidgin with sparse native speakers become a distinct language rather than a variety of Hebrew?',
    'Comparative linguistic analysis of mutual intelligibility thresholds and community identification surveys.',
    'If the contact variety is linguistically distinct, the constraint is misidentified as Hebrew continuity and is actually a language shift; if it remains mutually intelligible, the bridge reading is structurally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contact_native_threshold, empirical, 'Linguistic threshold between contact variety and distinct language').

omega_variable(
    institutional_extraction_vs_coordination,
    'Does the continuity bureaucracy''s maintenance of the bridge model serve primarily to sustain diaspora coordination or to secure institutional funding and mission relevance?',
    'Budget and enrollment trajectory analysis relative to actual diaspora communication needs; counterfactual of English substitution.',
    'High institutional dependency with low communicative yield would reclassify the coordination function as theatrical extraction; high genuine coordination yield would validate the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_vs_coordination, empirical, 'Whether institutional maintenance tracks coordination need or rent-seeking').

omega_variable(
    kernel_sibling_structural_delta,
    'This constraint is the bridge_pidginized reading of kernel hebrew_continuity. How would classification change if the liturgical_preservation or native_generative reading were adopted instead?',
    'Generate the sibling constraint stories and compare epsilon values, beneficiary structures, and victim sets.',
    'The liturgical reading would likely show lower base extractiveness (no marketplace pidgin) but higher suppression of profane use; the native reading would reclassify diaspora contact speakers as non-speakers entirely, potentially reducing measured extraction but also eliminating their voice from the stakeholder surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Structural delta between sibling readings of Hebrew continuity kernel').

omega_variable(
    contact_variety_suppression_mechanism,
    'Is the suppression of contact Hebrew legitimacy enforced through institutional barriers (testing, certification, curriculum) or internalized through learner shame and self-delegitimization?',
    'Ethnographic study of diaspora learner communities post-exit from formal instruction; measurement of self-reported language competence versus actual communicative success.',
    'If internalized, effective extraction exceeds the structural measure because learners carry the suppression with them outside institutional contexts; if purely structural, extraction is bounded by institutional contact time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contact_variety_suppression_mechanism, empirical, 'Structural versus internalized suppression of contact Hebrew legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_bridge_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebrew_bridge_tr_t14, hebrew_continuity__bridge_pidginized, theater_ratio, 14, 0.22).
narrative_ontology:measurement(hebrew_bridge_tr_t28, hebrew_continuity__bridge_pidginized, theater_ratio, 28, 0.3).
narrative_ontology:measurement(hebrew_bridge_tr_t42, hebrew_continuity__bridge_pidginized, theater_ratio, 42, 0.36).
narrative_ontology:measurement(hebrew_bridge_tr_t56, hebrew_continuity__bridge_pidginized, theater_ratio, 56, 0.41).
narrative_ontology:measurement(hebrew_bridge_tr_t70, hebrew_continuity__bridge_pidginized, theater_ratio, 70, 0.45).

% Extraction over time
narrative_ontology:measurement(hebrew_bridge_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebrew_bridge_be_t14, hebrew_continuity__bridge_pidginized, base_extractiveness, 14, 0.38).
narrative_ontology:measurement(hebrew_bridge_be_t28, hebrew_continuity__bridge_pidginized, base_extractiveness, 28, 0.45).
narrative_ontology:measurement(hebrew_bridge_be_t42, hebrew_continuity__bridge_pidginized, base_extractiveness, 42, 0.52).
narrative_ontology:measurement(hebrew_bridge_be_t56, hebrew_continuity__bridge_pidginized, base_extractiveness, 56, 0.58).
narrative_ontology:measurement(hebrew_bridge_be_t70, hebrew_continuity__bridge_pidginized, base_extractiveness, 70, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_bridge_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hebrew_bridge_su_t14, hebrew_continuity__bridge_pidginized, suppression_requirement, 14, 0.4).
narrative_ontology:measurement(hebrew_bridge_su_t28, hebrew_continuity__bridge_pidginized, suppression_requirement, 28, 0.46).
narrative_ontology:measurement(hebrew_bridge_su_t42, hebrew_continuity__bridge_pidginized, suppression_requirement, 42, 0.51).
narrative_ontology:measurement(hebrew_bridge_su_t56, hebrew_continuity__bridge_pidginized, suppression_requirement, 56, 0.55).
narrative_ontology:measurement(hebrew_bridge_su_t70, hebrew_continuity__bridge_pidginized, suppression_requirement, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one member of the hebrew_continuity family, decomposed per the epsilon-invariance principle: the liturgical, native, and bridge readings have different epsilon values, beneficiary structures, and failure modes. The label 'Hebrew continuity' conflates three structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
