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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Study of Kodashim as Historical Archive and Identity Maintenance
 *   domain: religious/legal/cultural
 *
 * SUMMARY:
 *   This constraint describes the practice of studying Kodashim (the order of
 *   the Mishnah dealing with sacrificial laws) not as a preparation for
 *   future practice or a spiritual enactment, but as a means of historical
 *   preservation and identity maintenance. The system it describes is
 *   defunct, and its study primarily serves to connect the community to its
 *   past. This reading acknowledges a coordination function (preserving
 *   heritage, maintaining identity) but also identifies an extractive
 *   component: the diversion of intellectual resources from currently
 *   applicable Jewish law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.55).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.4).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.55).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Study of Kodashim as Historical Archive and Identity Maintenance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/legal/cultural").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, 'b80cea01-5cad-4bb3-9beb-69f9e6f41eeb').
narrative_ontology:cs_kernel_codification('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', fixed_text).
narrative_ontology:cs_authority_grounding('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', lineage).
narrative_ontology:cs_interpretation_layer_present('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb').
narrative_ontology:cs_reading_relation('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', foundational, historical_continuity_is_paramount).
narrative_ontology:cs_axiom_status(historical_continuity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', historical_continuity_is_paramount, conventional).
narrative_ontology:cs_axiom('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', foundational, textual_preservation_is_sacred).
narrative_ontology:cs_axiom_status(textual_preservation_is_sacred, holdable).
narrative_ontology:cs_axiom_grounding('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', textual_preservation_is_sacred, theological).
narrative_ontology:cs_reference_frame('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', post_temple_exile_preservation).
narrative_ontology:cs_drift_state('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', contemporary_secular_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b80cea01-5cad-4bb3-9beb-69f9e6f41eeb', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, communal_identity).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, religious_scholars_of_kodashim).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, intellectual_resources_for_applicable_halakha).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, students_of_jewish_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, community_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars dedicate their careers to the intricate study of Kodashim, preserving its texts and interpretations. Their professional identity and academic standing are deeply intertwined with this field, and they actively shape the curriculum and norms of study.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, religious_scholars_of_kodashim, agenda_setter,
    institutional, generational, identity_locked, global).

% Students are expected to engage with Kodashim as part of a comprehensive Jewish legal education. This diverts their intellectual energy and time from areas of Jewish law that are directly applicable to contemporary life, often due to social and institutional pressure to conform to traditional curricula.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_of_jewish_law, payer,
    moderate, biographical, constrained, local).

% The collective identity of the Jewish people benefits from the continuous study of Kodashim, as it provides a tangible link to ancient traditions, historical continuity, and a sense of shared heritage, even if the laws themselves are not practiced.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, communal_identity, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, communal_identity).

% The collective pool of intellectual resources (scholarly attention, curriculum development, student focus) that could be directed towards contemporary Jewish law (halakha) is diminished by the emphasis on Kodashim. This represents an opportunity cost for the development and application of living Jewish law.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, intellectual_resources_for_applicable_halakha, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, intellectual_resources_for_applicable_halakha).

% Leaders within the Jewish community benefit from the study of Kodashim as it reinforces traditional values and provides a stable foundation for communal identity. They often promote this study as a means of strengthening religious observance and cultural cohesion.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, community_leaders, beneficiary,
    organized, generational, constrained, national).

% These individuals argue for a greater emphasis on areas of Jewish law that are directly relevant to modern life. They are often marginalized in traditional institutions that prioritize classical textual study, finding their concerns about resource allocation unaddressed.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, advocates_for_practical_halakha, excluded,
    moderate, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the historical and textual heritage of the sacrificial system (Kodashim) after the destruction of the Temple, ensuring continuity of Jewish tradition and identity across generations.
% TRANSFER_FUNCTION: Transfers significant intellectual and institutional resources (scholarly careers, curriculum time, student focus) from the study and application of contemporary Jewish law to the historical preservation of a defunct system, in exchange for maintaining communal identity and historical continuity.
% ABSENT_VOICES: Advocates for a greater focus on practical, applicable Jewish law are often excluded from the core decision-making bodies of traditional religious institutions, where the emphasis on classical textual study is deeply entrenched. They would argue for a re-prioritization of intellectual resources.
% DISAPPEARANCE_RATIONALE: If the study of Kodashim as an archive vanished overnight, a fundamental pillar of Jewish historical memory and communal identity would be lost. This would necessitate a profound re-evaluation of how tradition is transmitted, potentially leading to a fragmentation of identity and a shift in intellectual priorities within Jewish legal scholarship.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the sacrificial laws of Kodashim practically inoperable, creating a crisis of meaning and continuity for a central aspect of Jewish religious life. The problem was how to maintain the integrity of the tradition and communal identity in the absence of its physical performance.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish thought and cultural anthropologists corroborate the ongoing challenge of maintaining historical continuity and identity in post-Temple Judaism. Community educators and cultural institutions also attest to the vital role of such textual preservation in identity formation, from perspectives outside the immediate scholarly beneficiaries.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is moderate, reflecting the opportunity cost of intellectual resources diverted from other areas of Jewish law. Suppression (0.40) is present through institutional norms and curriculum design that prioritize this traditional study. The theater ratio (0.40) reflects that while the study is genuinely about preservation, a significant portion of its maintenance is performative, reinforcing identity rather than functional knowledge. The measurement series shows a relatively stable, slightly increasing trend in extractiveness and suppression, indicating the enduring nature of this practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'religious_scholars_of_kodashim' and 'communal_identity', this constraint is a vital 'rope' or 'scaffold' for cultural and historical continuity. However, from the perspective of 'students_of_jewish_law' and 'intellectual_resources_for_applicable_halakha', it functions as a 'tangled_rope' or even a 'snare', diverting resources and attention from more pressing contemporary legal and ethical issues.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'religious_scholars_of_kodashim' and 'community_leaders' are clear beneficiaries and agenda-setters, as their roles and the communal identity they foster are reinforced by this study. 'Communal_identity' itself is a beneficiary. 'Students_of_jewish_law' and 'intellectual_resources_for_applicable_halakha' are the primary payers/victims, bearing the cost of diverted intellectual effort. 'Advocates_for_practical_halakha' are excluded, as their concerns are not central to the institutional framing of this study.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_diversion_magnitude,
    'What is the actual magnitude of intellectual resources diverted from applicable halakha due to the emphasis on Kodashim study?',
    'Quantitative analysis of curriculum hours, scholarly publications, and institutional funding allocations across different areas of Jewish law, compared to a counterfactual baseline prioritizing contemporary halakha.',
    'If the diversion is found to be substantially higher, the extractiveness of this constraint would be re-evaluated upwards, potentially shifting its classification closer to a Snare. If negligible, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_magnitude, empirical, 'Quantifying the opportunity cost of intellectual resources.').

omega_variable(
    functional_vs_symbolic_value,
    'Is the primary value of Kodashim study truly historical preservation, or has it become primarily symbolic, serving identity functions without deep historical engagement?',
    'Qualitative sociological and pedagogical studies examining the actual content and methods of Kodashim study, and its impact on students'' historical understanding versus their sense of communal belonging.',
    'If primarily symbolic, the ''theater_ratio'' would be re-evaluated upwards, indicating a greater performative aspect. If genuinely historical, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_symbolic_value, conceptual, 'Distinguishing between genuine historical function and symbolic performance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (diversion of intellectual resources) structural (institutional funding, curriculum design) or internalized (scholarly norms, identity fusion)?',
    'Post-exit suppression trajectory: if scholars or students who leave traditional institutions continue to prioritize Kodashim study over applicable halakha, it suggests internalized suppression. If their focus shifts, it points to structural suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after exit. This would amplify effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in intellectual pursuits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.38).
narrative_ontology:measurement(koda_tr_t6, kodashim_obligation__study_as_archive, theater_ratio, 6, 0.39).
narrative_ontology:measurement(koda_tr_t12, kodashim_obligation__study_as_archive, theater_ratio, 12, 0.4).
narrative_ontology:measurement(koda_tr_t18, kodashim_obligation__study_as_archive, theater_ratio, 18, 0.4).
narrative_ontology:measurement(koda_tr_t24, kodashim_obligation__study_as_archive, theater_ratio, 24, 0.4).
narrative_ontology:measurement(koda_tr_t30, kodashim_obligation__study_as_archive, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(koda_be_t6, kodashim_obligation__study_as_archive, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(koda_be_t12, kodashim_obligation__study_as_archive, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(koda_be_t18, kodashim_obligation__study_as_archive, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(koda_be_t24, kodashim_obligation__study_as_archive, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(koda_be_t30, kodashim_obligation__study_as_archive, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(koda_su_t6, kodashim_obligation__study_as_archive, suppression_requirement, 6, 0.37).
narrative_ontology:measurement(koda_su_t12, kodashim_obligation__study_as_archive, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(koda_su_t18, kodashim_obligation__study_as_archive, suppression_requirement, 18, 0.4).
narrative_ontology:measurement(koda_su_t24, kodashim_obligation__study_as_archive, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(koda_su_t30, kodashim_obligation__study_as_archive, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, jewish_legal_curriculum_design).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, halakhic_prioritization_norms).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_obligation' kernel, focusing on study as historical preservation and identity maintenance. It is linked to sibling readings that emphasize performance or preparation for future practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
