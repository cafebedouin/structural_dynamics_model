% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation: Study as Archiving
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the reading of the Temple sacrifice obligation
 *   where its study is seen as a vital act of archiving knowledge for future
 *   restoration, but explicitly does not fulfill the actual divine command.
 *   This creates a state of perpetual non-compliance for the Jewish people,
 *   with the rabbinic authority maintaining the binding status of an
 *   unperformable law. The constraint is classified as a Snare because it
 *   extracts a spiritual deficit from the Jewish people (the unfulfilled
 *   divine command) while providing a coordination function (knowledge
 *   preservation) that primarily benefits the rabbinic authority and
 *   religious scholars, whose roles are sustained by the complexity of the
 *   unperformable law. The high suppression reflects the theological and
 *   historical impossibility of performing the sacrifices, which is not a
 *   choice but a structural reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.65).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.9).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, snare).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation: Study as Archiving").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious_studies/halakhic_authority/commitment_systems").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '180e7283-b5f8-405e-a2a2-62d98f3fc114').
narrative_ontology:cs_kernel_codification('180e7283-b5f8-405e-a2a2-62d98f3fc114', fixed_text).
narrative_ontology:cs_authority_grounding('180e7283-b5f8-405e-a2a2-62d98f3fc114', lineage).
narrative_ontology:cs_interpretation_layer_present('180e7283-b5f8-405e-a2a2-62d98f3fc114').
narrative_ontology:cs_reading_relation('180e7283-b5f8-405e-a2a2-62d98f3fc114', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('180e7283-b5f8-405e-a2a2-62d98f3fc114', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_axiom('180e7283-b5f8-405e-a2a2-62d98f3fc114', foundational, obligation_remains_binding_unfulfilled).
narrative_ontology:cs_axiom_status(obligation_remains_binding_unfulfilled, holdable).
narrative_ontology:cs_axiom_grounding('180e7283-b5f8-405e-a2a2-62d98f3fc114', obligation_remains_binding_unfulfilled, deontological).
narrative_ontology:cs_axiom('180e7283-b5f8-405e-a2a2-62d98f3fc114', foundational, study_is_archival_not_substitutive).
narrative_ontology:cs_axiom_status(study_is_archival_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('180e7283-b5f8-405e-a2a2-62d98f3fc114', study_is_archival_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('180e7283-b5f8-405e-a2a2-62d98f3fc114', post_temple_halakhic_continuity).
narrative_ontology:cs_drift_state('180e7283-b5f8-405e-a2a2-62d98f3fc114', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('180e7283-b5f8-405e-a2a2-62d98f3fc114', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, religious_scholars).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, jewish_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the binding status of the Temple sacrifice laws, even in their unperformable state. Emphasizes study as a means of preserving knowledge for future restoration, but explicitly denies it fulfills the actual obligation. Benefits from the continued relevance of complex halakhic study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Dedicate their lives to the intricate study of sacrificial laws. Their intellectual and professional identity is deeply tied to this pursuit. They benefit from the academic and spiritual prestige associated with mastering this complex body of knowledge, even if it doesn't fulfill the core obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, religious_scholars, beneficiary,
    organized, biographical, identity_locked, global).

% Bear the burden of an unfulfilled divine command. While study offers spiritual engagement, the core obligation remains unmet, creating a sense of collective spiritual deficit. Their identity is tied to the covenant, making exit from the obligation unthinkable.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, jewish_people, payer,
    moderate, generational, identity_locked, global).

% The divine imperative for Temple sacrifice remains unfulfilled due to the absence of the Temple. This 'victim' is the abstract representation of the unmet covenantal requirement, from which extraction flows as a spiritual deficit.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation of a complex body of religious law and ritual knowledge across generations, ensuring its continuity and readiness for a future restoration.
% TRANSFER_FUNCTION: Transfers the spiritual burden of an unfulfilled divine command from the realm of active ritual performance to a state of perpetual study and anticipation, while transferring intellectual prestige and authority to those who master the laws.
% ABSENT_VOICES: Ancient prophets who emphasized immediate obedience over deferred hope might object, as would those who seek more direct, performative avenues for fulfilling divine commands in the present. Their voices are absent due to the historical destruction of the Temple and the subsequent shift in religious practice.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the entire framework of post-Temple Judaism, which balances the binding nature of the command with its current unperformability, would collapse. The spiritual and intellectual life of the Jewish people would need to fundamentally reorganize around a new understanding of divine obligation and its fulfillment.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central act of Jewish worship (sacrifices) impossible, creating a crisis of how to maintain divine obligation and covenantal relationship in its absence.
% FOUNDING_PROBLEM_CORROBORATION: The continued longing for the rebuilding of the Temple and the daily prayers for its restoration, attested by liturgical texts and communal practice across millennia, corroborate that the problem of unfulfilled sacrifice remains live for the Jewish people. This is attested by the community's own continuous tradition, not just the rabbinic authority.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.65) because the entire post-Temple period is characterized by non-compliance with a central divine command, creating a spiritual deficit. Suppression is very high (0.90) due to the physical impossibility of performing sacrifices without a Temple, making exit from this state of non-compliance structurally impossible. Theater ratio is low (0.10) because the study is genuinely aimed at knowledge preservation, not merely performative maintenance of a defunct ritual. The claimed type is Snare because the coordination function (knowledge preservation) serves to maintain the authority and intellectual life of the rabbinic class, while the core obligation remains unfulfilled for the broader community, creating a persistent extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority and scholars, this constraint is a necessary and meaningful adaptation to historical circumstances, preserving tradition. From the perspective of the unfulfilled divine command and the Jewish people, it represents a persistent state of non-compliance and spiritual longing. The engine's classification as a Snare highlights this asymmetry, where the 'solution' (study) benefits certain stakeholders while the core 'problem' (unfulfilled obligation) persists for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and religious scholars are beneficiaries, as their roles and intellectual pursuits are sustained by the ongoing study of these complex laws. The Jewish people are payers, bearing the spiritual burden of the unfulfilled command. The 'unfulfilled divine command' is an abstract victim, representing the unmet covenantal requirement from which extraction flows as a spiritual deficit. The identity-locked exit option for the Jewish people reflects the deep covenantal ties that make abandoning the obligation unthinkable.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a Rope or Scaffold. While study provides a coordination function (knowledge preservation), it does not resolve the core problem of the unfulfilled obligation. It is not a Scaffold because it lacks a sunset clause and is not transitional; it is a permanent adaptation. It is not a Rope because the benefits are asymmetric and there is a clear victim (the unfulfilled command and the Jewish people bearing its burden). The persistence of the constraint is not due to inertia (Piton) but active maintenance of a binding, albeit unperformable, law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fulfillment_vs_preservation,
    'Does the act of studying the laws of sacrifice, in any meaningful sense, constitute a form of ''fulfillment'' of the divine command, or is it purely an act of preservation?',
    'Theological consensus shift or a new authoritative halakhic ruling that redefines the nature of fulfillment in the absence of the Temple.',
    'If study were deemed a form of fulfillment, the extractiveness from the ''unfulfilled_divine_command'' would decrease significantly, potentially reclassifying the constraint as a Rope or even a Mountain (if the fulfillment is seen as inherent to the act of study). If it remains purely preservation, the Snare classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fulfillment_vs_preservation, conceptual, 'Ambiguity regarding the spiritual efficacy of study in fulfilling the sacrifice obligation.').

omega_variable(
    messianic_era_impact,
    'How would the re-establishment of the Temple and the resumption of sacrifices impact the authority and role of rabbinic scholars whose careers are built on the study of unperformable laws?',
    'Observation of the actual dynamics following a hypothetical messianic restoration, or a detailed sociological study of how religious authority adapts to radical shifts in practice.',
    'If the authority of scholars diminishes significantly, it would reveal a greater extractive component in the current arrangement, as their status is tied to the unperformability. If their role adapts seamlessly, it would suggest a more genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_era_impact, empirical, 'Uncertainty about the future role of scholars if the Temple is rebuilt.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 70, 0.1).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 500, 0.1).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(temp_tr_t2024, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 70, 0.6).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1000, 0.63).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1500, 0.64).
narrative_ontology:measurement(temp_be_t2024, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 70, 0.85).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 500, 0.87).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1500, 0.89).
narrative_ontology:measurement(temp_su_t2024, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
