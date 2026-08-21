% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Archival Preservation
 *   domain: religious_studies/textual_preservation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.45).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.3).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, piton).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Archival Preservation").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious_studies/textual_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '0015c298-e93a-461d-9252-cc002c88859a').
narrative_ontology:cs_kernel_codification('0015c298-e93a-461d-9252-cc002c88859a', fixed_text).
narrative_ontology:cs_authority_grounding('0015c298-e93a-461d-9252-cc002c88859a', lineage).
narrative_ontology:cs_interpretation_layer_present('0015c298-e93a-461d-9252-cc002c88859a').
narrative_ontology:cs_reading_relation('0015c298-e93a-461d-9252-cc002c88859a', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('0015c298-e93a-461d-9252-cc002c88859a', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('0015c298-e93a-461d-9252-cc002c88859a', foundational, temple_system_defunct).
narrative_ontology:cs_axiom_status(temple_system_defunct, holdable).
narrative_ontology:cs_axiom_grounding('0015c298-e93a-461d-9252-cc002c88859a', temple_system_defunct, conventional).
narrative_ontology:cs_axiom('0015c298-e93a-461d-9252-cc002c88859a', foundational, study_as_historical_preservation).
narrative_ontology:cs_axiom_status(study_as_historical_preservation, holdable).
narrative_ontology:cs_axiom_grounding('0015c298-e93a-461d-9252-cc002c88859a', study_as_historical_preservation, instrumental).
narrative_ontology:cs_reference_frame('0015c298-e93a-461d-9252-cc002c88859a', post_temple_destruction_archival_mode).
narrative_ontology:cs_drift_state('0015c298-e93a-461d-9252-cc002c88859a', contemporary_secular_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0015c298-e93a-461d-9252-cc002c88859a', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, communal_identity).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, religious_scholars).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, intellectual_resources).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, students_of_halakha).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the continuous study of Kodashim as a marker of historical continuity and a source of collective memory, reinforcing a sense of shared heritage even without practical application.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, communal_identity, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, communal_identity).

% Administer and perpetuate the study of Kodashim, deriving professional legitimacy and academic careers from its preservation and interpretation. They frame the study as essential for historical and cultural literacy.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, religious_scholars, agenda_setter,
    organized, biographical, constrained, global).

% Represents the time, effort, and academic focus diverted from the study of currently applicable Jewish law (Halakha) or other contemporary intellectual pursuits. These resources are 'paid' into maintaining a defunct system.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, intellectual_resources, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, intellectual_resources).

% Are expected to engage with Kodashim as part of a comprehensive religious education, even when its practical relevance is denied. This diverts their limited study time from areas with direct contemporary application, but opting out carries social and academic costs.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_of_halakha, payer,
    moderate, biographical, constrained, local).

% Advocate for the literal rebuilding of the Temple and the resumption of sacrificial rites. This reading of Kodashim (study as archive) implicitly forecloses their active preparation for future performance, marginalizing their interpretive framework.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, messianic_restorationists, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of ancient religious texts and traditions, ensuring historical continuity and a shared cultural memory for a dispersed religious community.
% TRANSFER_FUNCTION: Transfers intellectual and communal legitimacy from the historical past to the present, reinforcing group identity, while diverting intellectual resources from contemporary legal or ethical concerns.
% ABSENT_VOICES: Those who believe Kodashim study should be purely functional (either for immediate spiritual efficacy or future restoration) are marginalized by this archival framing. They would argue for a more 'live' engagement with the text.
% DISAPPEARANCE_RATIONALE: If the practice of studying Kodashim as an archive vanished, a significant pillar of Jewish historical and communal identity would erode. While not impacting daily legal practice, it would create a profound cultural and intellectual void, forcing a re-evaluation of how historical texts are valued and transmitted.
% FOUNDING_PROBLEM: The destruction of the Second Temple left a vast body of sacrificial law without a physical context for performance, creating a crisis of meaning and continuity for a central religious practice.
% FOUNDING_PROBLEM_CORROBORATION: Historical consensus among secular historians and many contemporary religious scholars confirms the Temple's destruction and the cessation of sacrifices. The problem of 'how to perform sacrifices' is dead, but the problem of 'how to relate to these texts' persists. Messianic groups contest this status, asserting the problem is merely dormant.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).
:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_archival_value,
    'Is the value derived from Kodashim study primarily functional (e.g., spiritual efficacy, preparation for future performance) or archival (historical preservation, identity maintenance)?',
    'Empirical study of practitioner motivations and communal outcomes: if study correlates with active preparation for Temple restoration or reported spiritual transformation, reclassify as more functional.',
    'If more functional, the extractiveness and theater ratio would decrease, potentially reclassifying it as a Rope or even a Mountain (if cosmic function is accepted). If purely archival, the Piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_archival_value, conceptual, 'Ambiguity regarding the primary purpose and value of Kodashim study.').

omega_variable(
    resource_diversion_cost,
    'What is the actual opportunity cost of intellectual resources diverted to Kodashim study versus other areas of Jewish law or contemporary thought?',
    'Quantitative analysis of curriculum allocation, scholar publication trends, and student engagement with different legal areas, comparing outcomes and perceived relevance.',
    'A high opportunity cost would increase the measured extractiveness, potentially pushing it towards a Snare if the benefits to communal identity are deemed insufficient to offset the diversion. A low cost would reduce extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_diversion_cost, empirical, 'Quantification of the intellectual resource diversion as a cost.').

omega_variable(
    identity_lock_strength,
    'How strongly is communal identity truly ''locked'' into Kodashim study, such that its cessation would genuinely threaten identity, versus merely altering its expression?',
    'Sociological and anthropological studies of identity formation in religious communities, examining the impact of shifts in textual engagement on self-perception and group cohesion.',
    'If identity is less locked, the ''beneficiary'' aspect of communal identity weakens, increasing the net extractiveness and reinforcing the Piton classification by removing a key justification for its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The degree to which communal identity is genuinely dependent on Kodashim study.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.55).
narrative_ontology:measurement(koda_tr_t10, kodashim_obligation__study_as_archive, theater_ratio, 10, 0.57).
narrative_ontology:measurement(koda_tr_t20, kodashim_obligation__study_as_archive, theater_ratio, 20, 0.59).
narrative_ontology:measurement(koda_tr_t30, kodashim_obligation__study_as_archive, theater_ratio, 30, 0.6).
narrative_ontology:measurement(koda_tr_t40, kodashim_obligation__study_as_archive, theater_ratio, 40, 0.6).
narrative_ontology:measurement(koda_tr_t50, kodashim_obligation__study_as_archive, theater_ratio, 50, 0.6).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(koda_be_t10, kodashim_obligation__study_as_archive, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(koda_be_t20, kodashim_obligation__study_as_archive, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(koda_be_t30, kodashim_obligation__study_as_archive, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(koda_be_t40, kodashim_obligation__study_as_archive, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(koda_be_t50, kodashim_obligation__study_as_archive, base_extractiveness, 50, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_archive, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
