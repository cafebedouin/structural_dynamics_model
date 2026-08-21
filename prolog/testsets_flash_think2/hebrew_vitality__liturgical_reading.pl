% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality: Liturgical Preservation Reading
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint represents the 'liturgical_reading' of the
 *   'hebrew_vitality' kernel, asserting that unbroken liturgical use
 *   constitutes the vitality of the Hebrew language. From this perspective,
 *   the language's continuous presence in sacred texts, prayers, and rituals
 *   is the primary measure of its life, ensuring its transmission across
 *   generations within religious communities. This reading emphasizes
 *   preservation and continuity over vernacular or daily spoken use.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.1).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality: Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, 'dbba2295-c143-430a-9102-7b26457dc870').
narrative_ontology:cs_kernel_codification('dbba2295-c143-430a-9102-7b26457dc870', formalized).
narrative_ontology:cs_authority_grounding('dbba2295-c143-430a-9102-7b26457dc870', lineage).
narrative_ontology:cs_interpretation_layer_present('dbba2295-c143-430a-9102-7b26457dc870').
narrative_ontology:cs_reading_relation('dbba2295-c143-430a-9102-7b26457dc870', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbba2295-c143-430a-9102-7b26457dc870', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('dbba2295-c143-430a-9102-7b26457dc870', foundational, liturgical_use_is_vitality).
narrative_ontology:cs_axiom_status(liturgical_use_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('dbba2295-c143-430a-9102-7b26457dc870', liturgical_use_is_vitality, conventional).
narrative_ontology:cs_axiom('dbba2295-c143-430a-9102-7b26457dc870', secondary, sacred_language_continuity).
narrative_ontology:cs_axiom_status(sacred_language_continuity, holdable).
narrative_ontology:cs_axiom_grounding('dbba2295-c143-430a-9102-7b26457dc870', sacred_language_continuity, theological).
narrative_ontology:cs_reference_frame('dbba2295-c143-430a-9102-7b26457dc870', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('dbba2295-c143-430a-9102-7b26457dc870', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dbba2295-c143-430a-9102-7b26457dc870', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, religious_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, hebrew_sacred_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and uphold the liturgical practices and textual interpretations that ensure the continuous ritual use of Hebrew. They benefit from the authority and continuity derived from this unbroken tradition, which is central to their institutional identity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Participate in liturgical practices, deriving spiritual, cultural, and communal continuity from the use of Hebrew. This practice reinforces their collective identity and connection to a shared heritage, making exit from this tradition deeply challenging.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, religious_communities, beneficiary,
    organized, generational, identity_locked, global).

% Study the phenomenon of Hebrew revitalization from a linguistic perspective, often employing different criteria for 'language vitality' (e.g., native speakers, daily vernacular use) than those emphasized by this reading. They analyze the constraint's effects without being subject to its internal normative claims.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, secular_linguists, observer,
    analytical, biographical, analytical, global).

narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish religious communities globally around a shared, sacred language for prayer, study, and ritual, ensuring its continuous ritual presence and cultural transmission across generations.
% TRANSFER_FUNCTION: Transfers cultural, religious, and historical continuity across generations, reinforcing the authority of rabbinic traditions and the collective identity of religious communities through shared linguistic practice.
% ABSENT_VOICES: Proponents of vernacular Hebrew revival or secular linguists might argue that liturgical use alone is insufficient for true language vitality, emphasizing native speakers and daily use. Their criteria for vitality are outside the scope of this reading's definition.
% DISAPPEARANCE_RATIONALE: If the unbroken liturgical use of Hebrew vanished overnight, the global Jewish religious community would lose a foundational element of its identity, spiritual practice, and historical continuity. This would necessitate a profound reorganization of religious life, cultural transmission, and communal self-understanding.
% FOUNDING_PROBLEM: The historical challenge of maintaining Hebrew as a sacred language and a central marker of Jewish identity across millennia of diaspora, preventing its complete linguistic death and ensuring the continuity of religious tradition.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic scholars, religious historians, and community leaders universally attest to the ongoing importance of liturgical Hebrew for religious and cultural continuity. While secular historians acknowledge its role in preservation, they may dispute its sufficiency for 'full vitality' by modern linguistic standards.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope due to its genuine coordination function (maintaining a shared sacred language and identity) with minimal extraction from this reading's internal perspective. Extraction is low (0.15) as the practice is seen as a benefit to participants, not a burden. Suppression is low (0.10) because adherence is primarily cultural and identity-driven, rather than coercively enforced. Theater ratio is low (0.05) as the liturgical use is authentic and functional within its domain. Accessibility collapse is high (0.80) because, from this reading's view, the absence of liturgical use would signify a profound loss of vitality. Resistance is low (0.10) within the communities that uphold this view.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authorities and religious communities, this constraint is a vital, self-sustaining coordination mechanism. Other readings, such as those emphasizing vernacular use, would likely compute higher extraction or suppression, viewing the lack of daily spoken Hebrew as a cost or a failure of the language to thrive beyond ritual. This divergence is precisely what the kernel framework is designed to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and religious communities are beneficiaries, as the constraint directly supports their institutional and communal identity and continuity. Their 'identity_locked' exit options reflect the deep integration of this practice into their self-conception. Secular linguists are observers, analyzing the phenomenon without being subject to its internal normative claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criteria_ambiguity,
    'Is ''language vitality'' adequately defined by liturgical preservation, or does it require native speakers and daily vernacular use?',
    'Adoption of a universally accepted, multi-dimensional linguistic framework for vitality that integrates both ritual and vernacular metrics, or a consensus shift within the relevant academic and religious communities.',
    'If vernacular use is deemed essential, this constraint''s ''vitality'' claim would be reclassified as partial or insufficient, potentially increasing its effective extraction from those who desire a fully living language. If liturgical use is sufficient, the current low extraction holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_criteria_ambiguity, conceptual, 'Ambiguity in the definition of ''language vitality'' across different readings.').

omega_variable(
    liturgical_vs_vernacular_sufficiency,
    'Does the ''liturgical_reading'' sufficiently address the full scope of Hebrew''s potential vitality, or does it implicitly suppress other forms of linguistic life?',
    'Empirical study of resource allocation within language revitalization efforts: if resources are disproportionately directed to liturgical preservation at the expense of vernacular initiatives, it suggests implicit suppression.',
    'If implicit suppression is found, the constraint''s effective suppression metric would be higher, and its classification might shift towards a Tangled Rope or Snare from the perspective of vernacular proponents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_vernacular_sufficiency, empirical, 'Whether liturgical focus implicitly suppresses vernacular revitalization efforts.').

omega_variable(
    reading_structural_delta_native_daily,
    'How would the structural properties (extraction, beneficiaries, victims) of the ''hebrew_vitality'' kernel change if the ''native_daily_reading'' were adopted as primary?',
    'Comparative analysis of the ''native_daily_reading'' constraint story, once authored, to identify its distinct metric profile and stakeholder dynamics.',
    'The ''native_daily_reading'' would likely show higher extraction (from those unable to achieve native fluency) and a different set of beneficiaries (e.g., secular educators, Israeli state institutions) and potential victims (e.g., diaspora communities struggling with fluency).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_delta_native_daily, conceptual, 'Structural differences if ''native_daily_reading'' were primary.').

omega_variable(
    reading_structural_delta_hybrid_continuity,
    'How would the structural properties (extraction, beneficiaries, victims) of the ''hebrew_vitality'' kernel change if the ''hybrid_continuity_reading'' were adopted as primary?',
    'Comparative analysis of the ''hybrid_continuity_reading'' constraint story, once authored, to identify its distinct metric profile and stakeholder dynamics.',
    'The ''hybrid_continuity_reading'' would likely integrate elements of both liturgical and vernacular approaches, potentially leading to a more balanced extraction profile but also new coordination challenges and beneficiaries (e.g., language academies, cultural institutions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_delta_hybrid_continuity, conceptual, 'Structural differences if ''hybrid_continuity_reading'' were primary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__liturgical_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__liturgical_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__liturgical_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__liturgical_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__liturgical_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__liturgical_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__liturgical_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__liturgical_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__liturgical_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__liturgical_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(hebr_su_t60, hebrew_vitality__liturgical_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(hebr_su_t80, hebrew_vitality__liturgical_reading, suppression_requirement, 80, 0.1).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__liturgical_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, jewish_identity_transmission).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, rabbinic_authority_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_vitality' kernel, each representing a distinct structural claim about what constitutes language vitality. This specific reading focuses on liturgical preservation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
