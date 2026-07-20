% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity via Native Generative Use
 *   domain: sociolinguistic/cultural/political
 *
 * SUMMARY:
 *   This constraint instantiates the native_generative reading of the
 *   hebrew_continuity kernel: the claim that Hebrew survives as a living
 *   language exclusively through native child speakers and daily intuitive
 *   generative use, with the historical kernel reconstructed via lexical
 *   expansion and phonological standardization. Under this reading,
 *   liturgical-only communities become victims because their Hebrew is
 *   classified as 'dead'. The constraint is a tangled rope: it coordinates a
 *   genuine collective-action problem (national language revival) while
 *   asymmetrically extracting legitimacy from traditional custodians.
 *
 * KEY AGENTS:
 *   - language_revival_institutions (agenda_setter / institutional / constrained)
 *   - native_hebrew_speakers (beneficiary / organized / mobile)
 *   - liturgical_only_communities (payer / organized / identity_locked)
 *   - diaspora_hebrew_learners (payer / moderate / constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.65).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.58).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity via Native Generative Use").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistic/cultural/political").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '48cd5ded-881f-41c2-9c17-fb04db5d0720').
narrative_ontology:cs_kernel_codification('48cd5ded-881f-41c2-9c17-fb04db5d0720', formalized).
narrative_ontology:cs_authority_grounding('48cd5ded-881f-41c2-9c17-fb04db5d0720', practice).
narrative_ontology:cs_interpretation_layer_present('48cd5ded-881f-41c2-9c17-fb04db5d0720').
narrative_ontology:cs_reading_relation('48cd5ded-881f-41c2-9c17-fb04db5d0720', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('48cd5ded-881f-41c2-9c17-fb04db5d0720', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('48cd5ded-881f-41c2-9c17-fb04db5d0720', foundational, native_speaker_mandatory_for_vitality).
narrative_ontology:cs_axiom_status(native_speaker_mandatory_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('48cd5ded-881f-41c2-9c17-fb04db5d0720', native_speaker_mandatory_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('48cd5ded-881f-41c2-9c17-fb04db5d0720', foundational, institutional_reconstruction_legitimate).
narrative_ontology:cs_axiom_status(institutional_reconstruction_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('48cd5ded-881f-41c2-9c17-fb04db5d0720', institutional_reconstruction_legitimate, conventional).
narrative_ontology:cs_reference_frame('48cd5ded-881f-41c2-9c17-fb04db5d0720', native_generative_community).
narrative_ontology:cs_drift_state('48cd5ded-881f-41c2-9c17-fb04db5d0720', contemporary_global_jewry, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48cd5ded-881f-41c2-9c17-fb04db5d0720', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, language_revival_institutions).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_only_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_hebrew_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers lexical expansion, phonological standardization, and educational curricula. Mandate depends on the native-speaker vitality thesis; could not abandon it without self-abolition.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, language_revival_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Intuitive daily users of Modern Hebrew. Their vernacular competence is treated as the sole benchmark of linguistic legitimacy, conferring cultural prestige and institutional priority.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Preserve Hebrew for prayer and textual study across generations. Under the native-generative standard, their Hebrew is classified as non-living, stripping their centuries-long custodianship of legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_only_communities, payer,
    organized, generational, identity_locked, global).

% Second-language learners outside Israel whose Hebrew is permanently marked as deficient relative to the native-speaker ideal, restricting access to cultural production and recognition.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_hebrew_learners, payer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, language_revival_institutions).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Revives Hebrew as a national vernacular by creating a standardized, generative speech community capable of producing native intuitive competence and daily use.
% TRANSFER_FUNCTION: Transfers linguistic legitimacy and institutional resources from liturgical and diaspora learners to the native Israeli speech community and its standardizing institutions; moves prestige from passive textual knowledge to active generative performance.
% ABSENT_VOICES: Liturgical communities who regard Hebrew as inherently sacred and independent of native-speaker demographics; diaspora Jews for whom Hebrew functions as a religious lingua franca rather than a domestic vernacular.
% DISAPPEARANCE_RATIONALE: If the native-generative requirement vanished, liturgical Hebrew would regain equal standing as a living form, diaspora learners would not be permanently marked deficient, and the authority of standardizing institutions would shift from intuition-based gatekeeping to descriptive documentation.
% FOUNDING_PROBLEM: Hebrew had ceased to function as a daily spoken language among Jews; Zionist nation-building required a shared, modernized vernacular for collective self-governance and cultural production.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiographers and early revivalists attest the problem. However, traditional rabbinic authorities and Jewish diaspora historians outside the Zionist framework attest that Hebrew continuity was never broken in liturgical and textual domains, contesting the 'death' premise that justified the native-generative reconstruction.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the systematic delegitimization of non-native Hebrew forms. Suppression (0.58) captures active standardization, educational enforcement, and prestige allocation that marginalize liturgical and diaspora competence. Theater ratio (0.48) rises as the native-speaker ideology becomes performativeâmaintained even as multilingualism and L2 majorities complicate the purity narrative. Accessibility collapse (0.60) measures how thoroughly alternatives (liturgical Hebrew, Yiddish, Judeo-Arabic) were displaced by the national vernacular. Resistance (0.55) comes from organized liturgical communities and diaspora scholars who contest the 'dead language' framing.
 *
 * PERSPECTIVAL GAP:
 *   From the native-speaker seat, the constraint is natural linguistic life restored; from the liturgical seat, it is an ideological erasure of their continuous custodianship. The engine will compute low directionality for beneficiaries and high directionality for identity-locked liturgical communities, producing divergent per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (native speakers, revival institutions) receive low d because the constraint subsidizes their linguistic prestige and institutional existence. Victims (liturgical communities, diaspora learners) receive high d because the constraint extracts legitimacy from their practice and marks it as deficient. Liturgical communities are identity_locked, amplifying their effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the revival as pure rope (ignoring the delegitimization of liturgical Hebrew) or pure snare (ignoring the genuine coordination success of creating a modern speech community). The founding problemâlack of a shared daily vernacularâwas real for Zionist nation-builders but contested by liturgical communities who never experienced Hebrew as dead. The 'contested' status plus 'world_rearranges' disappearance verdict signals that the arrangement persists beyond its contested founding, but not yet as pure piton because beneficiaries remain concentrated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_ideology_or_science,
    'Is the native-speaker intuition threshold a falsifiable linguistic criterion for language vitality, or an ideological construct of nationalist modernity?',
    'Comparative analysis of language vitality in communities without native speakers (e.g., Latin, Sanskrit, Classical Arabic) to determine whether liturgical transmission counts as empirical continuity.',
    'If ideological, the constraint''s extraction is higher and its coordination function (national unity) is separable from its linguistic claim; if scientific, the constraint''s legitimacy is empirical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_ideology_or_science, conceptual, 'Empirical vs ideological grounding of native-speaker supremacy.').

omega_variable(
    liturgical_legitimacy_erasure,
    'Does classifying liturgical Hebrew as ''dead'' constitute structural violence against traditional communities, or merely a neutral descriptive distinction?',
    'Ethnographic study of liturgical communities'' self-concept and linguistic practice; legal status of their Hebrew in Israeli courts and education.',
    'If erasure, the victim set expands and the constraint leans snare; if neutral, the extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_legitimacy_erasure, preference, 'Normative status of liturgical Hebrew delegitimization.').

omega_variable(
    kernel_reading_position,
    'This constraint is the native_generative reading of the hebrew_continuity kernel; sibling readings (liturgical_preservation, bridge_pidginized) would reclassify the beneficiary/victim structure and the extraction profile. What changes if the kernel is instead read as requiring only ritual transmission?',
    'Cross-reading comparisonâevaluate the same sociolinguistic facts under the liturgical_preservation axioms.',
    'Under liturgical_preservation, liturgical_only_communities become beneficiaries or agenda_setters and native_hebrew_speakers may become payers in a different extraction dynamic; the Îµ value inverts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-position omega: sibling reading would restructure the entire agent surface.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the measured suppression of liturgical Hebrew structural (state policy, funding, institutional prestige) or internalized (communities adopting the ''dead language'' frame)?',
    'Post-policy shift trajectory: if liturgical Hebrew gains prestige when state support shifts, suppression was structural; if stigma persists, it is internalized.',
    'If internalized, effective suppression exceeds structural measure; if structural, removal of state support would restore standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural vs internalized suppression mechanism for liturgical Hebrew.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__native_generative, theater_ratio, 20, 0.25).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__native_generative, theater_ratio, 40, 0.38).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.45).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__native_generative, theater_ratio, 80, 0.48).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__native_generative, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__native_generative, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__native_generative, base_extractiveness, 80, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__native_generative, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__native_generative, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__native_generative, suppression_requirement, 80, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% The natural-language concept 'Hebrew continuity' decomposes into three structurally distinct claims: native generative vitality (this file), liturgical textual transmission (liturgical_preservation), and contact-language function (bridge_pidginized). Each has a different Îµ, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
