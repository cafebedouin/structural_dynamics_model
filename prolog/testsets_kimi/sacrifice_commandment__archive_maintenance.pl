% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrificial Law Study as Archive Maintenance for Messianic Restoration
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint instantiates the archive_maintenance reading of the
 *   contested sacrifice_commandment kernel within halakhic Judaism. The
 *   reading holds that study of sacrificial law (korbanot) preserves
 *   technical knowledge across the indefinite diaspora interruption, serving
 *   messianic preparation rather than present worship. It competes with
 *   study_as_performance (study IS fulfillment) and performance_only
 *   (commandment suspended until Temple restoration). The archive reading is
 *   transitional: its justification is the move from suspended practice to
 *   restored practice, not the steady state of permanent study.
 *
 * KEY AGENTS:
 *   - rabbinic_academies: Agenda setter (institutional/constrained) â administers curriculum and enforces the study requirement through ordination and communal status
 *   - contemporary_torah_students: Primary payer (moderate/constrained) â bears opportunity cost of studying technically inapplicable material
 *   - future_jewish_generations: Structural beneficiary (powerless/trapped) â receives preserved knowledge but cannot influence present decisions
 *   - reform_jewish_movement: Excluded voice (organized/mobile) â rejects the obligation but is outside the halakhic discourse
 *   - secular_historians_of_religion: Analytical observer (institutional/analytical) â corroborates preservation risk from outside the theological framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.42).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.38).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrificial Law Study as Archive Maintenance for Messianic Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).
narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'e007f358-2a9b-4d61-a51e-c278a0e40e94').
narrative_ontology:cs_kernel_codification('e007f358-2a9b-4d61-a51e-c278a0e40e94', fixed_text).
narrative_ontology:cs_authority_grounding('e007f358-2a9b-4d61-a51e-c278a0e40e94', lineage).
narrative_ontology:cs_interpretation_layer_present('e007f358-2a9b-4d61-a51e-c278a0e40e94').
narrative_ontology:cs_reading_relation('e007f358-2a9b-4d61-a51e-c278a0e40e94', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('e007f358-2a9b-4d61-a51e-c278a0e40e94', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('e007f358-2a9b-4d61-a51e-c278a0e40e94', foundational, study_as_preservation_not_worship).
narrative_ontology:cs_axiom_status(study_as_preservation_not_worship, holdable).
narrative_ontology:cs_axiom_grounding('e007f358-2a9b-4d61-a51e-c278a0e40e94', study_as_preservation_not_worship, deontological).
narrative_ontology:cs_axiom('e007f358-2a9b-4d61-a51e-c278a0e40e94', foundational, technical_continuity_obligated_for_messianic_restoration).
narrative_ontology:cs_axiom_status(technical_continuity_obligated_for_messianic_restoration, holdable).
narrative_ontology:cs_axiom_grounding('e007f358-2a9b-4d61-a51e-c278a0e40e94', technical_continuity_obligated_for_messianic_restoration, deontological).
narrative_ontology:cs_reference_frame('e007f358-2a9b-4d61-a51e-c278a0e40e94', temple_practice_with_interruption_preservation).
narrative_ontology:cs_drift_state('e007f358-2a9b-4d61-a51e-c278a0e40e94', contemporary_diaspora_condition, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e007f358-2a9b-4d61-a51e-c278a0e40e94', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_jewish_generations).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, contemporary_torah_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the yeshiva curriculum that mandates sustained study of sacrificial law (korbanot) across advanced Talmudic programs. Their authority derives from a claimed chain of transmission; they certify rabbinic ordination based partly on mastery of this technically inapplicable material. They bear the institutional cost of defending the curriculum's relevance against modernizing pressure.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinic_academies, agenda_setter,
    institutional, generational, constrained, global).

% Devote years of cognitive labor to mastering complex sacrificial regulations with no present liturgical application. They bear direct opportunity cost relative to studying applied civil, family, or commercial law. Advancement within the communal hierarchy depends on demonstrated fluency in this material; exit from the study requirement means exiting the credentialing path.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, contemporary_torah_students, payer,
    moderate, biographical, constrained, local).

% Stand to inherit a continuous technical tradition of sacrificial practice should the Temple be restored. They have no voice in present curricular decisions and cannot opt out of whatever preservation choices the present makes; their future access to authentic restored worship depends entirely on contemporary archival maintenance.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_jewish_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Rejects the binding obligation to study inapplicable sacrificial law, holding that Torah evolves with historical conditions. Would advocate redirecting study toward present ethical and social obligations, but is structurally excluded from the halakhic conversation that treats sacrifice law as currently incumbent.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, reform_jewish_movement, excluded,
    organized, generational, mobile, national).

% Document from outside the theological framework that technical ritual knowledge atrophies without live transmission, corroborating the preservation rationale. Simultaneously dispute that an obligation binds present believers, arguing that archival and academic methods could achieve the same continuity without ongoing religious enforcement.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, secular_historians_of_religion, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve detailed technical halakhic knowledge across an indefinite generational interruption so that sacrificial practice can resume without procedural discontinuity upon restoration of the Temple.
% TRANSFER_FUNCTION: Moves cognitive labor, memorization, and pedagogical resources from contemporary students and academies to future worshippers; moves institutional continuity and rabbinic authority across the temporal gap between Temple eras.
% ABSENT_VOICES: Reform Jewish movements rejecting the binding nature of the commandment; secular historians proposing archival preservation as a substitute for live study; women historically excluded from advanced Talmudic study in traditional academies despite being part of the beneficiary community.
% DISAPPEARANCE_RATIONALE: If the requirement to study sacrificial law vanished, yeshiva curricula would rapidly reallocate to applied legal domains; technical knowledge of korbanot would atrophy within two generations; future restoration would face a discontinuity forcing reconstruction from fragmented textual sources rather than living practice.
% FOUNDING_PROBLEM: The destruction of the Second Temple suspended sacrificial practice, creating a risk that the intricate technical knowledge of offeringsâtimings, qualifications, proceduresâwould be permanently lost during an indefinite interruption.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of religion and comparative ritualists attest from outside the rabbinic beneficiary framework that technical sacrificial knowledge atrophies in communities without active transmission, citing Samaritan and other parallel cases; these sources corroborate the preservation problem while disputing whether the solution requires present religious obligation rather than neutral academic archiving.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the contemporary generation bears real cognitive opportunity costs for a benefit that accrues to an indefinite future. Suppression is moderate (0.38) because the requirement is enforced through communal credentialing and curricular gatekeeping rather than physical coercion. Theater_ratio rises slowly (0.25 to 0.35) because as the messianic horizon recedes, the study increasingly functions as identity performance rather than imminent preparation. Accessibility_collapse (0.55) reflects that alternatives (textual archiving, Reform rejection) exist but are suppressed within the Orthodox framework. Resistance is low (0.20) because dissent is privatized among students rather than organized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (rabbinic academies) experiences the constraint as stewardship of a divine obligation across history; the payer seat (students) experiences it as a credentialing burden with deferred payoff; the beneficiary seat (future generations) is structurally silent. The engine should compute divergent classifications: the academy seat may compute toward rope or scaffold, while the student seat may compute toward tangled_rope due to asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Future_jewish_generations is declared beneficiary (low d, subsidized by the constraint) despite temporal displacement; contemporary_torah_students is declared victim/payer (high d, extraction target). Rabbinic_academies sit near the agenda_setter position with constrained exitâthey could theoretically change the curriculum but doing so would undermine their lineage authority. The asymmetry is intertemporal rather than spatial: extraction flows from present biographical time to future civilizational time.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this as pure extraction (snare) because the study does solve a genuine coordination problemâpreserving discontinuous technical knowledge. It prevents mislabeling as pure coordination (rope) because the benefit is asymmetrically distributed across time and the present generation pays a clear cost. The sunset clause (Temple restoration) anchors the transitional logic: if the sunset were removed, the constraint would drift toward piton or tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_reading_kernel_location,
    'Is the archive_maintenance reading a genuine scaffold with a messianic sunset clause, or has indefinite deferral transformed it into a performative piton maintained by institutional inertia?',
    'Evaluate whether the reading retains a coherent eschatological horizon or whether functional justifications have replaced restoration logic in rabbinic discourse.',
    'If the sunset is functionally dead, the constraint drifts from scaffold toward piton; if the sunset remains operative, the moderate extractiveness is bounded by transitional logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_reading_kernel_location, conceptual, 'Whether the messianic sunset clause is still live or merely nominal').

omega_variable(
    sibling_study_as_performance_pressure,
    'Does the archive_maintenance reading foreclose, coexist with, or merely influence the study_as_performance reading that claims intellectual engagement itself fulfills divine obligation?',
    'Examine whether halakhic authorities holding the archive reading logically treat study as devoid of present worship value, thereby rejecting the performance reading''s core premise, or whether the two readings merge in practice.',
    'If archive_maintenance treats study as purely instrumental, it exerts repudiation pressure on study_as_performance; if the boundary collapses, the kernel reading family converges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_study_as_performance_pressure, conceptual, 'Structural relationship between archive and performance readings').

omega_variable(
    future_beneficiary_standing,
    'Can a non-present generation function as a structural beneficiary in the directionality computation, or does this reading smuggle present institutional benefits under future-oriented theological cover?',
    'Compare the directionality profile when future_jewish_generations is treated as agent versus when rabbinic_academies is modeled as the effective beneficiary capturing continuity rents.',
    'If present institutions are the real beneficiaries, effective extraction for students rises and the constraint tilts toward snare or tangled_rope; if the future beneficiary is structurally real, the intertemporal transfer reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_beneficiary_standing, conceptual, 'Whether future-oriented beneficiary claims mask present capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.27).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.3).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__archive_maintenance, theater_ratio, 60, 0.32).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__archive_maintenance, theater_ratio, 80, 0.34).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__archive_maintenance, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__archive_maintenance, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__archive_maintenance, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__archive_maintenance, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__archive_maintenance, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(sacr_su_t40, sacrifice_commandment__archive_maintenance, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(sacr_su_t60, sacrifice_commandment__archive_maintenance, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(sacr_su_t80, sacrifice_commandment__archive_maintenance, suppression_requirement, 80, 0.36).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__archive_maintenance, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
