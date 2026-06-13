% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Living Language Status: Literary Continuity Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a 'living language' by its capacity for new
 *   literary and intellectual production, exemplified by the Haskalah
 *   movement and modern Hebrew literature. It explicitly de-emphasizes native
 *   speaker status. This is one reading of the 'living_language_status'
 *   kernel, which is contested by liturgical and native-generation readings.
 *   The low extractiveness reflects that this is primarily a coordination
 *   mechanism for an intellectual elite, but it does implicitly exclude other
 *   forms of linguistic vitality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.2).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.15).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Living Language Status: Literary Continuity Reading").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '4894fdc5-9710-4e9e-9919-024b5eda64d9').
narrative_ontology:cs_kernel_codification('4894fdc5-9710-4e9e-9919-024b5eda64d9', distributed).
narrative_ontology:cs_authority_grounding('4894fdc5-9710-4e9e-9919-024b5eda64d9', practice).
narrative_ontology:cs_interpretation_layer_present('4894fdc5-9710-4e9e-9919-024b5eda64d9').
narrative_ontology:cs_reading_relation('4894fdc5-9710-4e9e-9919-024b5eda64d9', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('4894fdc5-9710-4e9e-9919-024b5eda64d9', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('4894fdc5-9710-4e9e-9919-024b5eda64d9', foundational, literary_production_is_primary_vitality_metric).
narrative_ontology:cs_axiom_status(literary_production_is_primary_vitality_metric, holdable).
narrative_ontology:cs_axiom_grounding('4894fdc5-9710-4e9e-9919-024b5eda64d9', literary_production_is_primary_vitality_metric, conventional).
narrative_ontology:cs_axiom('4894fdc5-9710-4e9e-9919-024b5eda64d9', foundational, native_speaker_status_is_secondary_to_literary_use).
narrative_ontology:cs_axiom_status(native_speaker_status_is_secondary_to_literary_use, holdable).
narrative_ontology:cs_axiom_grounding('4894fdc5-9710-4e9e-9919-024b5eda64d9', native_speaker_status_is_secondary_to_literary_use, conventional).
narrative_ontology:cs_reference_frame('4894fdc5-9710-4e9e-9919-024b5eda64d9', haskalah_intellectual_tradition).
narrative_ontology:cs_drift_state('4894fdc5-9710-4e9e-9919-024b5eda64d9', contemporary_sociolinguistics_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4894fdc5-9710-4e9e-9919-024b5eda64d9', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, modern_hebrew_literature_scholars).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from a definition of 'living language' that validates their cultural production and intellectual work in Hebrew, without requiring mass native speaker adoption. It grants them cultural authority and legitimacy for their literary endeavors.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals, beneficiary,
    organized, generational, mobile, regional).

% Academics and researchers whose careers are built around the study and promotion of modern Hebrew literature. This reading provides a strong theoretical foundation for the vitality and significance of their field.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, modern_hebrew_literature_scholars, beneficiary,
    institutional, generational, analytical, global).

% These individuals are implicitly excluded from the definition of a 'living language' if their engagement with the language is primarily oral, ritualistic, or non-literary. Their forms of linguistic vitality are devalued by this reading.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers, payer,
    powerless, biographical, trapped, local).

% These groups, who emphasize the language's vitality through sacred texts and ritual, find their perspective marginalized by a purely literary definition. They would argue for the importance of continuous liturgical use.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, liturgical_scholars_and_rabbis, excluded,
    organized, generational, constrained, global).

% Advocates for the generational transmission of Hebrew as a mother tongue in daily life. They would argue that literary production alone is insufficient for a language to be truly 'living' and that native speakers are crucial.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, linguistic_revival_activists, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intellectual and literary communities around a shared understanding of linguistic vitality, validating their efforts to produce new works in Hebrew and fostering a sense of cultural continuity.
% TRANSFER_FUNCTION: Transfers cultural authority and legitimacy to literary and intellectual elites, defining the criteria for a language's 'life' in terms of their output, rather than broader popular usage or ritualistic preservation.
% ABSENT_VOICES: Liturgical scholars and linguistic revival activists are largely absent from this definition's core proponents; they would argue for alternative criteria of vitality based on sacred use or native generational transmission, respectively.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the cultural authority of modern Hebrew literary figures would diminish, and the justification for their work as evidence of a 'living' language would be challenged. Alternative definitions emphasizing native speakers or liturgical use would gain prominence, shifting the discourse around Hebrew's status.
% FOUNDING_PROBLEM: The challenge of asserting Hebrew's vitality in an era when it was not widely spoken as a mother tongue, particularly against arguments that it was a 'dead' language.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Haskalah movement and scholars of modern Hebrew literature corroborate that this problem was central to the intellectual debates of the 19th and early 20th centuries, and its echoes persist in contemporary discussions about language and national identity.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint primarily serves to coordinate and legitimize the work of a specific intellectual class, rather than to extract resources broadly. Suppression is also low, as it operates more through definitional exclusion and cultural authority than active coercion. Theater ratio is minimal, as the literary production is genuine. Accessibility collapse is moderate-high because once this definition is accepted, alternative criteria for vitality become less accessible or legitimate. Resistance is low because the primary 'victims' (non-literary speakers) are diffuse and lack organized power to challenge this definition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of maskilim and secular intellectuals, this is a vital and empowering definition that validates their cultural contributions. From the perspective of those who prioritize liturgical use or native generational transmission, this definition is incomplete or even misleading, as it overlooks other crucial aspects of linguistic life.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim and secular intellectuals are clear beneficiaries, as this definition grants their work legitimacy and cultural weight. Scholars of modern Hebrew literature also benefit by having a robust theoretical framework for their field. Illiterate or non-literary speakers are implicitly payers, as their forms of engagement with the language are devalued. Liturgical scholars and linguistic revival activists are excluded, as their criteria for vitality are not central to this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has not experienced mandatrophy; its mandate to define and validate Hebrew's vitality through literary production remains active for its beneficiaries. The contestation comes from alternative framings of vitality, not from the obsolescence of this particular definition's function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_vs_oral_vitality,
    'Does a focus on literary production adequately capture the full spectrum of a language''s vitality, or does it implicitly devalue oral traditions and non-literary forms of usage?',
    'Sociolinguistic studies that measure the impact of literary-centric definitions on the perceived status and support for oral linguistic practices within a community.',
    'If oral vitality is significantly devalued, the constraint''s effective extractiveness from non-literary speakers is higher than measured, potentially shifting its classification towards a Tangled Rope for those groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_vs_oral_vitality, conceptual, 'Ambiguity in what constitutes ''vitality'' beyond literary output.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''reading'' of the ''living_language_status'' kernel, or is it a distinct, unrelated claim about linguistic vitality?',
    'Analysis of historical and contemporary debates: if proponents of this reading directly engage with and respond to arguments from the liturgical and native-generation readings, it confirms its status as a reading within the same kernel.',
    'If it''s not a reading, the network links to other ''living_language_status'' constraints are invalid, and the CS structure analysis for this constraint would be re-evaluated as an independent claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint''s status as a reading within the ''living_language_status'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(livi_be_t1800, living_language_status__literary_continuity_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(livi_be_t1850, living_language_status__literary_continuity_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(livi_be_t1900, living_language_status__literary_continuity_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(livi_be_t1950, living_language_status__literary_continuity_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(livi_be_t2020, living_language_status__literary_continuity_reading, base_extractiveness, 2020, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1800, living_language_status__literary_continuity_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(livi_su_t1850, living_language_status__literary_continuity_reading, suppression_requirement, 1850, 0.12).
narrative_ontology:measurement(livi_su_t1900, living_language_status__literary_continuity_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(livi_su_t1950, living_language_status__literary_continuity_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(livi_su_t2020, living_language_status__literary_continuity_reading, suppression_requirement, 2020, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
