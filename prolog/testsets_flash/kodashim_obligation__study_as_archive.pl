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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical Archive
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint describes the practice of studying Kodashim (the order of
 *   the Mishnah dealing with Temple sacrifices) within Jewish tradition,
 *   specifically through the lens of 'study as archive.' In this reading,
 *   Kodashim documents a defunct system, and its study serves primarily for
 *   historical preservation and identity-maintenance, rather than as a guide
 *   for current legal obligation or cosmic function. The constraint is framed
 *   as a Tangled Rope because it genuinely coordinates the preservation of a
 *   cultural heritage (benefiting communal identity and scholars) but
 *   extracts intellectual resources from students who could otherwise focus
 *   on applicable law, and requires active enforcement of a curriculum.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.45).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.3).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical Archive").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious_studies/jewish_law/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '285cf56a-cd9d-41b6-883e-c90b1eda2385').
narrative_ontology:cs_kernel_codification('285cf56a-cd9d-41b6-883e-c90b1eda2385', fixed_text).
narrative_ontology:cs_authority_grounding('285cf56a-cd9d-41b6-883e-c90b1eda2385', lineage).
narrative_ontology:cs_interpretation_layer_present('285cf56a-cd9d-41b6-883e-c90b1eda2385').
narrative_ontology:cs_reading_relation('285cf56a-cd9d-41b6-883e-c90b1eda2385', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('285cf56a-cd9d-41b6-883e-c90b1eda2385', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('285cf56a-cd9d-41b6-883e-c90b1eda2385', foundational, sacrificial_law_defunct_in_diaspora).
narrative_ontology:cs_axiom_status(sacrificial_law_defunct_in_diaspora, holdable).
narrative_ontology:cs_axiom_grounding('285cf56a-cd9d-41b6-883e-c90b1eda2385', sacrificial_law_defunct_in_diaspora, conventional).
narrative_ontology:cs_axiom('285cf56a-cd9d-41b6-883e-c90b1eda2385', foundational, historical_preservation_is_identity_maintenance).
narrative_ontology:cs_axiom_status(historical_preservation_is_identity_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('285cf56a-cd9d-41b6-883e-c90b1eda2385', historical_preservation_is_identity_maintenance, conventional).
narrative_ontology:cs_reference_frame('285cf56a-cd9d-41b6-883e-c90b1eda2385', post_temple_diaspora_scholarship).
narrative_ontology:cs_drift_state('285cf56a-cd9d-41b6-883e-c90b1eda2385', contemporary_secular_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('285cf56a-cd9d-41b6-883e-c90b1eda2385', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, jewish_scholarly_community).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, communal_identity).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, intellectual_resources).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, students_of_halakha).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the curriculum and interpretive tradition for Kodashim study, framing it as essential for historical continuity and identity. Benefits from the intellectual capital and prestige associated with preserving ancient texts.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, jewish_scholarly_community, agenda_setter,
    institutional, generational, identity_locked, global).

% Invest significant intellectual effort and time into studying Kodashim, which, from this reading, offers no direct legal or practical application in contemporary Jewish life. Their intellectual resources are diverted from areas of more immediate halakhic relevance.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_of_halakha, payer,
    moderate, biographical, constrained, local).

% Benefits from the sense of deep historical continuity and shared heritage that the preservation and study of Kodashim provides, even if the laws are not currently applicable. This study reinforces a collective identity rooted in ancient traditions.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, communal_identity, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, communal_identity).

% View Kodashim as a valuable historical document, providing insight into ancient religious practices and societal structures, without attributing any contemporary legal or spiritual efficacy to its study. They analyze its content and transmission from an external, academic perspective.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, secular_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of ancient Jewish legal texts pertaining to Temple sacrifices, ensuring their transmission across generations as a historical and cultural archive, maintaining a shared intellectual heritage.
% TRANSFER_FUNCTION: Transfers intellectual resources (time, scholarly effort) from students and scholars towards the study of defunct sacrificial laws, in exchange for the maintenance of historical continuity and communal identity for the Jewish people.
% ABSENT_VOICES: Reformist or secular Jewish educators who might argue for a curriculum focused solely on currently applicable Jewish law or modern ethical concerns, viewing the extensive study of Kodashim as an inefficient allocation of educational resources. Their voices are often marginalized in traditional yeshiva settings.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim as an archive vanished, the Jewish scholarly world would experience a significant shift. Curricula would reorient, intellectual resources would be reallocated, and a key pillar of historical identity maintenance would be lost, leading to a re-evaluation of what constitutes essential Jewish knowledge.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, the problem arose of how to preserve the vast body of sacrificial law (Kodashim) which was no longer practically performable, without losing a crucial part of Jewish heritage and identity.
% FOUNDING_PROBLEM_CORROBORATION: The problem of preserving historical texts and maintaining identity in the face of changing circumstances remains live for many cultural and religious groups. Anthropologists and historians corroborate the importance of such archival practices for group cohesion and historical understanding, independent of the religious community's internal claims.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).
:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because intellectual resources are diverted from immediately applicable halakha, but the study also provides genuine, albeit indirect, benefits to communal identity. Suppression (0.3) is present through the institutional pressure of traditional curricula, but not overtly coercive. Theater ratio (0.1) is low, as the archival function is largely genuine, with minimal performative pretense of immediate applicability. Accessibility collapse (0.2) is low, as alternative areas of study are readily available, but cultural pressure can make choosing them difficult. Resistance (0.15) is low, as the value of historical study is widely accepted, even if its practical utility is debated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the scholarly community, this is a vital act of cultural preservation. From the perspective of a student, it can be a significant intellectual burden with unclear practical returns. The engine's classification as a Tangled Rope reflects this dual nature: a genuine coordination function (preserving heritage) intertwined with an asymmetric extraction (diverting intellectual resources).
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish scholarly community and communal identity are beneficiaries, as they gain from the preservation of heritage and intellectual prestige. Students of Halakha are payers, as they invest significant time in a subject with limited direct contemporary application. Secular historians are observers, analyzing the phenomenon without being bound by its internal logic. The 'study as archive' reading emphasizes the historical and cultural value, making the extraction primarily a diversion of intellectual resources rather than a direct financial or coercive burden.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_archival_value,
    'To what extent does the study of Kodashim, even in an archival sense, indirectly contribute to contemporary halakhic reasoning or spiritual development, beyond mere historical preservation?',
    'Qualitative analysis of contemporary halakhic responsa and theological writings to identify instances where Kodashim study, framed archivally, informs practical or spiritual decisions.',
    'If significant indirect functional value is found, the extractiveness of this constraint would be lower, as the intellectual resources are not purely ''diverted'' but contribute to a broader, albeit indirect, benefit. This would push the classification closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_archival_value, empirical, 'Assessing the hidden functional contributions of archival study.').

omega_variable(
    natural_vs_constructed_identity_maintenance,
    'Is the link between Kodashim study and Jewish communal identity a natural, emergent property of historical continuity, or is it a constructed narrative actively maintained by the scholarly community to justify resource allocation?',
    'Sociological and anthropological studies of identity formation in other diasporic communities, comparing the role of defunct legal texts in identity maintenance, alongside historical analysis of how this link was actively forged and reinforced over time.',
    'If largely constructed, the ''beneficiary'' status of communal_identity would be re-evaluated as a self-serving justification by the agenda-setter, increasing the effective extractiveness and pushing the classification closer to a Snare. If natural, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_identity_maintenance, conceptual, 'Distinguishing emergent from constructed identity-maintenance functions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.3) primarily structural (e.g., lack of alternative curricula, institutional pressure) or internalized (e.g., students'' belief in the inherent value of all traditional study, identity fusion with the scholarly path)?',
    'Post-exit suppression trajectory: if students who leave traditional yeshiva settings continue to prioritize Kodashim study despite no external pressure, it suggests a higher internalized component. Surveys of former students on their motivations for study.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit. This would amplify the perceived extractiveness for the students.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in educational contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 100, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_archive, theater_ratio, 100, 0.08).
narrative_ontology:measurement(koda_tr_t120, kodashim_obligation__study_as_archive, theater_ratio, 120, 0.09).
narrative_ontology:measurement(koda_tr_t140, kodashim_obligation__study_as_archive, theater_ratio, 140, 0.09).
narrative_ontology:measurement(koda_tr_t160, kodashim_obligation__study_as_archive, theater_ratio, 160, 0.1).
narrative_ontology:measurement(koda_tr_t180, kodashim_obligation__study_as_archive, theater_ratio, 180, 0.1).
narrative_ontology:measurement(koda_tr_t200, kodashim_obligation__study_as_archive, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_archive, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(koda_be_t120, kodashim_obligation__study_as_archive, base_extractiveness, 120, 0.42).
narrative_ontology:measurement(koda_be_t140, kodashim_obligation__study_as_archive, base_extractiveness, 140, 0.43).
narrative_ontology:measurement(koda_be_t160, kodashim_obligation__study_as_archive, base_extractiveness, 160, 0.44).
narrative_ontology:measurement(koda_be_t180, kodashim_obligation__study_as_archive, base_extractiveness, 180, 0.45).
narrative_ontology:measurement(koda_be_t200, kodashim_obligation__study_as_archive, base_extractiveness, 200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t100, kodashim_obligation__study_as_archive, suppression_requirement, 100, 0.25).
narrative_ontology:measurement(koda_su_t120, kodashim_obligation__study_as_archive, suppression_requirement, 120, 0.27).
narrative_ontology:measurement(koda_su_t140, kodashim_obligation__study_as_archive, suppression_requirement, 140, 0.28).
narrative_ontology:measurement(koda_su_t160, kodashim_obligation__study_as_archive, suppression_requirement, 160, 0.29).
narrative_ontology:measurement(koda_su_t180, kodashim_obligation__study_as_archive, suppression_requirement, 180, 0.3).
narrative_ontology:measurement(koda_su_t200, kodashim_obligation__study_as_archive, suppression_requirement, 200, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'Kodashim obligation' kernel. This 'study as archive' reading emphasizes historical preservation and identity maintenance, contrasting with 'study as performance' (cosmic function) and 'study as preparation' (messianic restoration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
