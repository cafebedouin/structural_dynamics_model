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
 * SUMMARY:
 *   This constraint represents the reading of Kodashim (laws of sacrifices
 *   and Temple service) study as primarily an act of historical preservation
 *   and identity maintenance, rather than preparation for future practice or
 *   a substitute for actual performance. The system it documents is defunct,
 *   and its study serves to archive knowledge and reinforce communal
 *   identity. This is one reading of the 'kodashim_obligation' kernel,
 *   distinct from 'study_as_performance' and 'study_as_preparation'.
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
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '531c56da-afa4-4337-b897-f8be2194981a').
narrative_ontology:cs_kernel_codification('531c56da-afa4-4337-b897-f8be2194981a', fixed_text).
narrative_ontology:cs_authority_grounding('531c56da-afa4-4337-b897-f8be2194981a', lineage).
narrative_ontology:cs_interpretation_layer_present('531c56da-afa4-4337-b897-f8be2194981a').
narrative_ontology:cs_reading_relation('531c56da-afa4-4337-b897-f8be2194981a', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('531c56da-afa4-4337-b897-f8be2194981a', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('531c56da-afa4-4337-b897-f8be2194981a', foundational, temple_service_defunct_no_physical_temple).
narrative_ontology:cs_axiom_status(temple_service_defunct_no_physical_temple, holdable).
narrative_ontology:cs_axiom_grounding('531c56da-afa4-4337-b897-f8be2194981a', temple_service_defunct_no_physical_temple, empirically_contingent).
narrative_ontology:cs_axiom('531c56da-afa4-4337-b897-f8be2194981a', foundational, study_as_historical_preservation_identity_maintenance).
narrative_ontology:cs_axiom_status(study_as_historical_preservation_identity_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('531c56da-afa4-4337-b897-f8be2194981a', study_as_historical_preservation_identity_maintenance, conventional).
narrative_ontology:cs_reference_frame('531c56da-afa4-4337-b897-f8be2194981a', post_temple_destruction_archival_mode).
narrative_ontology:cs_drift_state('531c56da-afa4-4337-b897-f8be2194981a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('531c56da-afa4-4337-b897-f8be2194981a', '').
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

% Administer and perpetuate the study of Kodashim. Their careers, academic institutions, and intellectual prestige are often tied to the preservation and interpretation of these texts, even if their practical relevance is limited. They frame the study as vital for historical and identity reasons.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, religious_scholars, agenda_setter,
    organized, biographical, constrained, global).

% Represents the time, effort, and intellectual capital diverted from the study of currently applicable Jewish law (Halakha) or other pressing communal needs. This diversion is a cost borne by the intellectual ecosystem.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, intellectual_resources, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, intellectual_resources).

% Are expected to engage with Kodashim as part of a comprehensive religious education, even if they perceive its direct relevance to their lives as minimal. They bear the cost of time and effort that could be spent on more immediately applicable legal or ethical studies.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, students_of_halakha, payer,
    moderate, biographical, constrained, local).

% Advocate for the literal, future re-establishment of the Temple and sacrificial system. They would argue that Kodashim study should be preparation for future performance, not mere archive, but their view is marginalized in this reading.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, messianic_restorationists, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective effort of preserving a significant body of historical religious texts and maintaining a sense of continuous communal identity through shared intellectual heritage.
% TRANSFER_FUNCTION: Transfers intellectual legitimacy and communal identity reinforcement to the religious community and scholars, in exchange for the diversion of intellectual resources and study time from other areas.
% ABSENT_VOICES: Messianic restorationists would argue that the study should be for future practical application, not just historical preservation. Those advocating for a more utilitarian approach to religious study would question the allocation of intellectual resources.
% DISAPPEARANCE_RATIONALE: If the practice of studying Kodashim as an archive vanished, a significant pillar of Jewish historical and communal identity would erode. While not impacting daily legal practice, it would create a profound cultural and intellectual void, forcing a re-evaluation of textual heritage and scholarly priorities.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the sacrificial system defunct, creating a need to preserve the knowledge of its laws while adapting religious life to its absence.
% FOUNDING_PROBLEM_CORROBORATION: Historians and cultural anthropologists attest to the ongoing need for historical preservation and identity maintenance within the Jewish community, corroborating the problem's live status from an external, non-theological perspective.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because intellectual resources are diverted from immediately applicable studies, and students invest time in a system with no direct functional output. Suppression is low (0.3) as there's no active coercion to study, but social and academic norms create pressure. Theater ratio is high (0.6) because the 'performance' of study maintains the appearance of relevance for a system that is functionally inert in this reading. The metrics reflect a system maintained more for its symbolic and archival value than for practical utility.
 *
 * PERSPECTIVAL GAP:
 *   Scholars and the community at large (beneficiaries) experience this as a vital act of cultural preservation. Students and those concerned with practical application (payers) may experience it as a less efficient use of intellectual resources. The engine will compute these different classifications based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal identity and religious scholars are beneficiaries, gaining continuity and prestige. Intellectual resources and students of Halakha are payers, bearing the cost of diverted attention and effort. Messianic restorationists are excluded, as their interpretation of Kodashim's purpose (preparation for future performance) is not acknowledged by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a piton because its primary function (preparing for actual Temple service) has atrophied, but the study persists due to institutional inertia and its new function as identity maintenance and archival preservation. The high theater ratio reflects this shift from original mandate to performative maintenance of historical connection. It avoids mislabeling as a snare because there's no concentrated beneficiary actively extracting from the 'defunctness' itself, but rather diffuse benefits from the act of preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_symbolic_value,
    'Is the value derived from Kodashim study primarily functional (e.g., preserving technical knowledge for a potential future Temple) or symbolic (e.g., maintaining historical continuity and identity)?',
    'Analysis of scholarly output and communal discourse: if the majority of engagement focuses on historical context and identity narratives, it supports symbolic value; if on technical details for hypothetical application, it supports functional value.',
    'If primarily functional, the extractiveness might be lower (as the ''cost'' is an investment in future utility), and the theater ratio would decrease. If purely symbolic, the piton classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_vs_symbolic_value, conceptual, 'Ambiguity in the primary purpose of Kodashim study.').

omega_variable(
    resource_diversion_cost,
    'What is the actual opportunity cost of intellectual resources diverted to Kodashim study versus other areas of Jewish law or communal need?',
    'Quantitative analysis of curriculum hours, scholarly publications, and communal funding allocations across different areas of Jewish study, compared to expressed communal priorities.',
    'A higher opportunity cost would increase the effective extractiveness for students and the intellectual ecosystem, potentially pushing the classification closer to a Tangled Rope if the benefits to communal identity are seen as disproportionate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_cost, empirical, 'Quantification of the cost of diverted intellectual resources.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''study_as_archive'' reading of the ''kodashim_obligation'' kernel. What would change structurally if a sibling reading, such as ''study_as_preparation'' (study preserves technical knowledge for messianic restoration), were adopted?',
    'Observing shifts in curriculum design, scholarly funding, and communal rhetoric. If resources shift towards technical, performative aspects and away from historical/identity narratives, the ''preparation'' reading is gaining ground.',
    'Adopting ''study_as_preparation'' would likely decrease the theater ratio and potentially increase the perceived functional value, shifting the constraint away from a piton towards a scaffold (if the preparation is seen as transitional support for a future state) or even a rope (if the preparation is seen as a genuine coordination for a future collective action). The victim set would also change, as ''diversion of resources'' would be re-framed as ''investment in future capacity''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of adopting a sibling reading (''study_as_preparation'') on the constraint''s structure.').

omega_variable(
    kernel_reading_identity_performance,
    'This constraint is the ''study_as_archive'' reading of the ''kodashim_obligation'' kernel. What would change structurally if a sibling reading, such as ''study_as_performance'' (studying sacrificial law enacts the cosmic function of sacrifice itself), were adopted?',
    'Observing shifts in theological discourse and ritual practice. If study is increasingly framed as a direct spiritual act with cosmic efficacy, the ''performance'' reading is gaining ground.',
    'Adopting ''study_as_performance'' would fundamentally alter the constraint''s claimed function from archival to active spiritual efficacy. This would likely decrease the theater ratio (as the ''performance'' becomes the actual function) and could reclassify it towards a rope (if seen as a genuine spiritual coordination) or even a mountain (if the cosmic function is deemed immutable). The victim set of ''intellectual resources'' would be re-framed as ''spiritual investment''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity_performance, conceptual, 'Impact of adopting a sibling reading (''study_as_performance'') on the constraint''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.5).
narrative_ontology:measurement(koda_tr_t25, kodashim_obligation__study_as_archive, theater_ratio, 25, 0.55).
narrative_ontology:measurement(koda_tr_t50, kodashim_obligation__study_as_archive, theater_ratio, 50, 0.6).
narrative_ontology:measurement(koda_tr_t75, kodashim_obligation__study_as_archive, theater_ratio, 75, 0.62).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_archive, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(koda_be_t25, kodashim_obligation__study_as_archive, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(koda_be_t50, kodashim_obligation__study_as_archive, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(koda_be_t75, kodashim_obligation__study_as_archive, base_extractiveness, 75, 0.46).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_archive, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(koda_su_t25, kodashim_obligation__study_as_archive, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(koda_su_t50, kodashim_obligation__study_as_archive, suppression_requirement, 50, 0.3).
narrative_ontology:measurement(koda_su_t75, kodashim_obligation__study_as_archive, suppression_requirement, 75, 0.3).
narrative_ontology:measurement(koda_su_t100, kodashim_obligation__study_as_archive, suppression_requirement, 100, 0.3).


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
