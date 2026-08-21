% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Hebrew as a Living Language: Literary Revival Reading
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'literary revival' reading of Hebrew's
 *   status as a living language during the Haskalah (Jewish Enlightenment).
 *   It posits that Hebrew's vitality was demonstrated and maintained through
 *   its use in new, secular literary production, rather than solely through
 *   liturgical continuity or native daily speech. This reading emphasizes
 *   written generative competence as the criterion for a 'living' language.
 *   The constraint itself is the implicit agreement among Haskalah
 *   intellectuals that their literary output constituted a 'living' Hebrew,
 *   and the subsequent academic tradition that validates this view.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.15).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew as a Living Language: Literary Revival Reading").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '97317c8c-e920-4f8e-8f09-b2281d0444e5').
narrative_ontology:cs_kernel_codification('97317c8c-e920-4f8e-8f09-b2281d0444e5', formalized).
narrative_ontology:cs_authority_grounding('97317c8c-e920-4f8e-8f09-b2281d0444e5', practice).
narrative_ontology:cs_interpretation_layer_present('97317c8c-e920-4f8e-8f09-b2281d0444e5').
narrative_ontology:cs_reading_relation('97317c8c-e920-4f8e-8f09-b2281d0444e5', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('97317c8c-e920-4f8e-8f09-b2281d0444e5', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('97317c8c-e920-4f8e-8f09-b2281d0444e5', foundational, generative_written_production_equals_vitality).
narrative_ontology:cs_axiom_status(generative_written_production_equals_vitality, holdable).
narrative_ontology:cs_axiom_grounding('97317c8c-e920-4f8e-8f09-b2281d0444e5', generative_written_production_equals_vitality, conventional).
narrative_ontology:cs_reference_frame('97317c8c-e920-4f8e-8f09-b2281d0444e5', haskalah_literary_renaissance).
narrative_ontology:cs_drift_state('97317c8c-e920-4f8e-8f09-b2281d0444e5', post_native_generation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('97317c8c-e920-4f8e-8f09-b2281d0444e5', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_literature_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively produced new Hebrew literature (poetry, essays, novels) during the Haskalah (Jewish Enlightenment), demonstrating the language's generative capacity beyond sacred texts. They saw this literary output as proof of Hebrew's vitality and a path to cultural renewal.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_intellectuals, agenda_setter,
    organized, biographical, mobile, regional).

% Benefit from the existence of a rich body of modern Hebrew literature for study and interpretation. Their careers and academic fields are sustained by the literary output of the Haskalah and subsequent periods, which this reading validates as 'living' Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_literature_scholars, beneficiary,
    moderate, generational, constrained, global).

% Primarily used Hebrew for liturgical and scholarly purposes, not daily speech or secular literature. They viewed the Haskalah's secular literary use of Hebrew with suspicion or opposition, seeing it as a departure from sacred tradition. Their voice is largely absent from the 'literary revival' narrative.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditional_jewish_communities, excluded,
    organized, generational, identity_locked, local).

% Analyze the historical trajectory of Hebrew's revitalization. From this analytical seat, they assess whether literary production alone constitutes a 'living' language, or if native daily speech is a necessary condition. Their work often bridges historical readings with contemporary language planning.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, linguistic_revival_activists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of intellectuals and writers to produce new, secular literature in Hebrew, fostering a shared cultural project and demonstrating the language's expressive range beyond its traditional sacred uses.
% TRANSFER_FUNCTION: Transfers cultural capital and intellectual legitimacy to the Haskalah movement and its proponents, by validating their use of Hebrew as a sign of its 'living' status and potential for modernity.
% ABSENT_VOICES: Traditional Jewish communities, who maintained Hebrew through liturgical and scholarly use but often rejected its secularization, are largely absent from the narrative that defines Hebrew's 'living' status solely by literary output. They would argue for a different criterion of vitality.
% DISAPPEARANCE_RATIONALE: If the concept of Hebrew's vitality being tied to its literary production vanished, the historical narrative of the Haskalah would be fundamentally altered, and the justification for much of modern Hebrew literature's significance would be undermined. The cultural and academic fields built around this idea would need to re-evaluate their foundations.
% FOUNDING_PROBLEM: The perception that Hebrew was a 'dead' language, confined to religious texts and lacking modern generative capacity, hindering Jewish cultural modernization.
% FOUNDING_PROBLEM_CORROBORATION: Haskalah intellectuals themselves attested to this problem, and subsequent linguistic historians corroborate that the perception of Hebrew as 'dead' was widespread. However, the 'dead' status is contested by those who point to continuous liturgical and scholarly use. Linguistic revival activists acknowledge the historical perception but argue the problem was resolved by the later shift to native daily speech, not just literary output.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.08, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very low (0.08) because this constraint primarily describes a coordination among intellectuals and a subsequent academic interpretation; it does not directly extract resources from a broad population. Suppression is low (0.15) as there was no active enforcement to compel literary production, though social and intellectual pressures within the Haskalah encouraged it. Theater ratio is negligible (0.05) as the literary output was genuine and functional for its proponents. Accessibility collapse is high (0.8) because, within this reading, the 'living' status of Hebrew is largely self-evident from the existence of new literature, making alternative interpretations of its vitality less accessible to those who accept this premise. Resistance is low (0.05) because the literary movement faced more opposition from traditionalists than active resistance to its core premise of Hebrew's literary vitality.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between those who accept literary production as sufficient proof of a 'living' language (Haskalah intellectuals, scholars) and those who require native daily speech (linguistic revival activists) or liturgical continuity (traditional communities). This constraint captures the former perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Haskalah intellectuals are the primary beneficiaries and agenda-setters, as they actively shaped and benefited from this reading of Hebrew's vitality. Hebrew literature scholars are also beneficiaries, as their field is sustained by this interpretation. There are no direct 'victims' of this constraint, as it describes an intellectual and cultural movement rather than an extractive mechanism. Traditional Jewish communities are 'excluded' as their alternative criteria for Hebrew's vitality are not central to this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to demonstrate Hebrew's vitality through literature) was largely fulfilled by the end of the Haskalah period. However, the 'founding problem' of Hebrew being 'dead' is now contested, as the later native generation of speakers in Israel provided a different, more widely accepted, resolution. This suggests a potential for mandatrophy if the literary reading is held as the *sole* criterion for 'living' status, rather than a historical phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criterion_of_living_language,
    'Is generative literary production a sufficient criterion for a language to be considered ''living'', or is native daily speech a necessary condition?',
    'Conceptual clarification and consensus among linguists and language planners regarding the definition of ''living language'' in the context of revitalization.',
    'If native daily speech is deemed necessary, this reading''s claim of Hebrew being ''living'' during the Haskalah would be reclassified as ''partially living'' or ''pre-revival'', shifting its historical significance. If literary production is sufficient, this reading''s classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criterion_of_living_language, conceptual, 'Ambiguity in the definition of a ''living language'' for Hebrew''s historical status.').

omega_variable(
    natural_vs_constructed_vitality,
    'Was the ''living'' status of Hebrew during the Haskalah a natural consequence of its literary use, or a constructed claim by intellectuals to advance a cultural agenda?',
    'Historical-sociological analysis of the Haskalah movement''s goals and the reception of Hebrew literature outside its immediate intellectual circles.',
    'If primarily constructed, the constraint''s ''rope'' classification might shift towards ''tangled_rope'' or ''snare'' for those outside the benefiting intellectual elite, as the ''coordination'' aspect would be seen as cover for an agenda. If natural, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_vitality, empirical, 'Whether Hebrew''s literary vitality was an intrinsic property or a strategic claim.').

omega_variable(
    reading_scope_and_reachability,
    'Does this reading''s definition of ''living'' Hebrew imply strict reachability (i.e., could any literate Jew of the era have produced new literature generatively), or was it limited to an elite?',
    'Empirical study of literacy rates and generative writing competence in Hebrew across different social strata during the Haskalah period.',
    'If limited to an elite, the ''living'' status might be seen as less robust or more exclusive, potentially influencing its classification towards a more extractive type for those excluded from the elite. If widely reachable, it reinforces the ''rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_scope_and_reachability, empirical, 'The actual scope of generative Hebrew competence during the Haskalah.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1780, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1780, hebrew_living_language__literary_revival_reading, theater_ratio, 1780, 0.02).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_living_language__literary_revival_reading, theater_ratio, 1800, 0.03).
narrative_ontology:measurement(hebr_tr_t1820, hebrew_living_language__literary_revival_reading, theater_ratio, 1820, 0.04).
narrative_ontology:measurement(hebr_tr_t1840, hebrew_living_language__literary_revival_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_living_language__literary_revival_reading, theater_ratio, 1860, 0.05).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__literary_revival_reading, theater_ratio, 1880, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1780, hebrew_living_language__literary_revival_reading, base_extractiveness, 1780, 0.05).
narrative_ontology:measurement(hebr_be_t1800, hebrew_living_language__literary_revival_reading, base_extractiveness, 1800, 0.06).
narrative_ontology:measurement(hebr_be_t1820, hebrew_living_language__literary_revival_reading, base_extractiveness, 1820, 0.07).
narrative_ontology:measurement(hebr_be_t1840, hebrew_living_language__literary_revival_reading, base_extractiveness, 1840, 0.08).
narrative_ontology:measurement(hebr_be_t1860, hebrew_living_language__literary_revival_reading, base_extractiveness, 1860, 0.08).
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__literary_revival_reading, base_extractiveness, 1880, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1780, hebrew_living_language__literary_revival_reading, suppression_requirement, 1780, 0.1).
narrative_ontology:measurement(hebr_su_t1800, hebrew_living_language__literary_revival_reading, suppression_requirement, 1800, 0.12).
narrative_ontology:measurement(hebr_su_t1820, hebrew_living_language__literary_revival_reading, suppression_requirement, 1820, 0.13).
narrative_ontology:measurement(hebr_su_t1840, hebrew_living_language__literary_revival_reading, suppression_requirement, 1840, 0.14).
narrative_ontology:measurement(hebr_su_t1860, hebrew_living_language__literary_revival_reading, suppression_requirement, 1860, 0.15).
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__literary_revival_reading, suppression_requirement, 1880, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_living_language' kernel. It focuses on literary production as the criterion for vitality, distinct from liturgical continuity or native daily speech. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
