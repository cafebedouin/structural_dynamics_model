% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native-Generation Language Vitality Standard
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The native-generation reading of 'living language' asserts that only
 *   intergenerational mother-tongue transmission in daily life constitutes
 *   vitality. Liturgical recitation, literary production, and heritage
 *   learning are explicitly excluded — framed as preservation of a corpse.
 *   This reading became the operational standard for state language policy,
 *   UNESCO vitality assessments, and nationalist legitimacy claims from the
 *   19th century onward. It coordinates resource allocation but extracts
 *   recognition from communities whose transmission chains run through sacred
 *   or literary rather than domestic daily use. The constraint is a tangled
 *   rope: it solves a real coordination problem (which languages get state
 *   support) while asymmetrically extracting legitimacy and resources from
 *   liturgical-only communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.45).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.55).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native-Generation Language Vitality Standard").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, 'a2741e07-72e0-45d2-8985-e8413365b0a0').
narrative_ontology:cs_kernel_codification('a2741e07-72e0-45d2-8985-e8413365b0a0', distributed).
narrative_ontology:cs_authority_grounding('a2741e07-72e0-45d2-8985-e8413365b0a0', extraction).
narrative_ontology:cs_interpretation_layer_present('a2741e07-72e0-45d2-8985-e8413365b0a0').
narrative_ontology:cs_reading_relation('a2741e07-72e0-45d2-8985-e8413365b0a0', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('a2741e07-72e0-45d2-8985-e8413365b0a0', living_language_status__literary_continuity_reading, influences).
narrative_ontology:cs_axiom('a2741e07-72e0-45d2-8985-e8413365b0a0', foundational, native_generation_necessary_for_vitality).
narrative_ontology:cs_axiom_status(native_generation_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a2741e07-72e0-45d2-8985-e8413365b0a0', native_generation_necessary_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('a2741e07-72e0-45d2-8985-e8413365b0a0', foundational, liturgical_transmission_insufficient_for_vitality).
narrative_ontology:cs_axiom_status(liturgical_transmission_insufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a2741e07-72e0-45d2-8985-e8413365b0a0', liturgical_transmission_insufficient_for_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('a2741e07-72e0-45d2-8985-e8413365b0a0', pre_nationalist_multilingual_ecology).
narrative_ontology:cs_drift_state('a2741e07-72e0-45d2-8985-e8413365b0a0', contemporary_institutionalized_standard, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a2741e07-72e0-45d2-8985-e8413365b0a0', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, state_education_apparatus).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, mainstream_media_industries).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, heritage_learners).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, religious_minority_elders).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, intergenerational_transmission_as_vitality_metric).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, linguistic_sovereignty_requires_native_speakers).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, daily_use_as_life_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the official criteria for language vitality that determine state funding, educational policy, and minority recognition. Gains legitimacy by claiming to represent the 'living' national language against 'dead' liturgical forms. Can shift between nationalist and civic framings depending on political utility.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movement, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, secular_nationalist_movement, beneficiary).

% Receives resources and authority to implement mother-tongue education programs. The native-generation standard justifies a vast apparatus of schools, teacher training, curriculum development, and assessment. Benefits from the constraint's enforcement through institutional expansion and budget allocation.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_education_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Gains guaranteed audience and advertising markets in the standardized national language. The vitality standard marginalizes competing linguistic markets (liturgical, literary, diasporic). Can pivot to other languages if markets shift; not existentially dependent on this constraint.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, mainstream_media_industries, beneficiary,
    powerful, biographical, mobile, national).

% Maintain language through sacred recitation, study, and ritual use across generations. Framed by the dominant standard as 'preserving a corpse' rather than keeping a language alive. Denied state recognition, funding for transmission, and educational infrastructure. Exit would require abandoning the religious identity that constitutes their communal self-understanding — the language IS the vessel of their tradition.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    moderate, generational, identity_locked, local).

% Acquire the language through family exposure, community classes, or religious education but do not use it as a daily mother tongue. Fall into a categorization gap: not 'native speakers' by the standard, not 'foreign learners' by experience. Bear the cost of learning without the recognition or resources granted to native-speaker communities. Can shift to dominant language but lose the specific cultural inheritance.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, heritage_learners, payer,
    powerless, biographical, constrained, local).

% Hold the liturgical transmission chains but are structurally excluded from vitality policy discussions. Their authoritative knowledge of the language's ritual use is disqualified as evidence of vitality. Cannot effectively advocate in state frameworks; their successors face the same exclusion. The constraint renders their life's work officially 'dead' while they still practice it.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, religious_minority_elders, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, religious_minority_elders, excluded).

% Document and analyze the full ecology of language transmission — native, liturgical, literary, diasporic. See the native-generation standard as one ideological position among many, not a scientific fact. Their research shows vitality as a multidimensional continuum, but their expertise is selectively cited only when it supports the dominant standard.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, linguistic_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, measurable criterion (intergenerational mother-tongue transmission) for allocating state resources, educational infrastructure, and minority language rights. Solves the problem of which languages get institutional support by defining vitality in operationalizable terms.
% TRANSFER_FUNCTION: Moves state recognition, educational funding, media spectrum, and legal protections from liturgical-only and literary-continuity communities to native-speaker communities and the institutions that serve them. Transfers legitimacy from religious/traditional authorities to secular nationalist authorities.
% ABSENT_VOICES: Diasporic communities maintaining languages without territorial concentration; mixed-transmission families (liturgical at home, national language at school); scholars of language revitalization who document non-native pathways to vitality (e.g., Hebrew, Māori, Hawaiian). These voices are excluded because they complicate the binary native/dead framing.
% DISAPPEARANCE_RATIONALE: If the native-generation standard vanished, state language policy would lose its primary operational criterion. Liturgical communities would claim vitality status and demand resources. Literary-continuity advocates would press for recognition. The institutional apparatus built around mother-tongue metrics (census categories, school funding formulas, UNESCO vitality assessments) would need reconstruction. The nationalist legitimacy claim would lose its linguistic anchor.
% FOUNDING_PROBLEM: Post-imperial and post-colonial states needed a non-religious, non-dynastic basis for national cohesion. The native-generation standard offered a seemingly objective, democratic criterion: the language the people actually speak as mothers to children, not the language of priests or kings.
% FOUNDING_PROBLEM_CORROBORATION: Nationalist historians (e.g., Anderson, Gellner, Hobsbawm) corroborate the founding problem from outside the beneficiary set: the standard emerged with 19th-century nationalism as a tool for constructing homogeneous nation-states. Sociolinguists (e.g., Fishman's GIDS, UNESCO's vitality framework) document its institutionalization. The secular nationalist movement itself claims the problem remains live (ongoing assimilation pressures); liturgical communities and revitalization scholars attest the problem has shifted — the standard now obstructs the very vitality it was meant to measure.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).
:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: the constraint requires massive institutional infrastructure (schools, media, bureaucracy) that benefits native-speaker communities and the state apparatus, but the extraction from liturgical communities is real — they are denied resources their transmission practices would merit under alternative standards. Suppression (0.55) reflects active exclusion: liturgical communities are not merely passively unrecognized; their claims are actively disqualified by the vitality definition itself. Theater ratio (0.30) captures the gap between the standard's scientific presentation (demographic objectivity) and its ideological function (nationalist legitimacy). The metrics are measured at 2025 endpoint; the temporal series shows extraction and suppression rising as the standard hardened into policy, then stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   From the secular nationalist seat, the constraint is a rope: a coordination mechanism for democratic language policy. From the liturgical community seat, it is a snare: a definition that extracts their vitality and renders their transmission invisible. From the heritage learner seat, it is a tangled rope: they benefit from educational infrastructure but are categorized as non-vital. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analyst's view that the coordination function is real but the extraction is asymmetric and enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular nationalist movement and state education apparatus are structural beneficiaries (d near 0.0): they set the agenda and collect the institutional rewards. Mainstream media industries are beneficiaries with mobile exit (d ~ 0.2). Liturgical-only communities are identity-locked targets (d near 1.0): their communal self-understanding is fused with the liturgical transmission the constraint defines as 'dead.' Heritage learners are constrained payers (d ~ 0.7): they bear costs without recognition but can assimilate. Religious minority elders are trapped (d ~ 1.0): no exit that preserves their role. Linguistic anthropologists are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (national cohesion without religious/dynastic legitimacy) was live in 1850-1950. By 2025, the problem has shifted: the standard now obstructs recognition of documented vitality pathways (Hebrew, Māori, Hawaiian revitalization; Yiddish literary continuity; liturgical languages with growing speaker bases). The constraint persists because the institutional apparatus it created (education ministries, funding formulas, census categories) has become self-justifying. Mandatrophy is unresolved: the coordination function has attenuated but the extraction continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_metric_naturalness,
    'Is the native-generation standard a discovery of linguistic fact or a construction of nationalist ideology?',
    'Compare vitality outcomes across communities using different transmission modes (native, liturgical, literary, immersion) controlling for institutional support. If liturgical/literary communities show comparable intergenerational transmission stability when resourced equally, the standard is ideological.',
    'If constructed, the constraint is a false summit (mountain claim masking tangled rope operation). If natural, the extraction from liturgical communities is a tragic necessity of linguistic reality, not an injustice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vitality_metric_naturalness, conceptual, 'Whether the native-generation criterion reflects linguistic ontology or political epistemology.').

omega_variable(
    revitalization_counterexamples,
    'Do documented revitalization cases (Hebrew, Māori, Hawaiian, Welsh) falsify the claim that only native generation constitutes vitality?',
    'Longitudinal study of revitalized languages: measure whether communities that rebuilt native transmission from liturgical/literary/dormant bases achieve vitality metrics comparable to continuous native-transmission communities.',
    'If revitalization succeeds, the standard''s exclusion of liturgical/literary transmission as ''dead'' is empirically falsified. The constraint''s claimed coordination function (objective measurement) collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revitalization_counterexamples, empirical, 'Whether historical counterexamples structurally undermine the constraint''s natural-law claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of liturgical communities structural (policy design) or internalized (communities accepting ''dead language'' framing)?',
    'Ethnographic study of liturgical communities'' self-assessment: do they frame their language as ''dead'' or ''living in a different mode''? Track policy advocacy: do they demand recognition on the standard''s terms or reject the standard?',
    'If internalized, effective suppression is higher than structural measure — communities carry the disqualification internally. If structural only, suppression lifts with policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in liturgical communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 1850, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lls_ngr_tr_t1850, living_language_status__native_generation_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(lls_ngr_tr_t1900, living_language_status__native_generation_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(lls_ngr_tr_t1950, living_language_status__native_generation_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(lls_ngr_tr_t1975, living_language_status__native_generation_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(lls_ngr_tr_t2000, living_language_status__native_generation_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(lls_ngr_tr_t2025, living_language_status__native_generation_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(lls_ngr_be_t1850, living_language_status__native_generation_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(lls_ngr_be_t1900, living_language_status__native_generation_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(lls_ngr_be_t1950, living_language_status__native_generation_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(lls_ngr_be_t1975, living_language_status__native_generation_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement(lls_ngr_be_t2000, living_language_status__native_generation_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(lls_ngr_be_t2025, living_language_status__native_generation_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(lls_ngr_su_t1850, living_language_status__native_generation_reading, suppression_requirement, 1850, 0.2).
narrative_ontology:measurement(lls_ngr_su_t1900, living_language_status__native_generation_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(lls_ngr_su_t1950, living_language_status__native_generation_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(lls_ngr_su_t1975, living_language_status__native_generation_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(lls_ngr_su_t2000, living_language_status__native_generation_reading, suppression_requirement, 2000, 0.53).
narrative_ontology:measurement(lls_ngr_su_t2025, living_language_status__native_generation_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.1).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, language_education_policy).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, minority_language_rights_regime).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, unesco_vitality_assessment_framework).

% DUAL FORMULATION NOTE:
% This constraint (native_generation_reading) and its siblings (liturgical_preservation_reading, literary_continuity_reading) form a constraint family decomposing the colloquial label 'living language.' Each reading has a different ε: native_generation (moderate, 0.45) because it requires institutional infrastructure; liturgical_preservation (low, ~0.15) because it runs on existing religious infrastructure; literary_continuity (moderate-high, ~0.55) because it requires publication/cultural infrastructure. The native_generation_reading structurally influences the others by controlling the state resources they need for recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__native_generation_reading, moderate, 0.85).
constraint_indexing:directionality_override(living_language_status__native_generation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
