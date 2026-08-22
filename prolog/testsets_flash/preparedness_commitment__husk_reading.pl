% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes preparedness as a set of routines that
 *   prioritize formal compliance and visible performance over genuine
 *   operational competence. Drills and plans become memorial acts, retaining
 *   the form of readiness without its adaptive capacity. This 'husk reading'
 *   highlights how the D5 break (decoupling of form from function) manifests
 *   as a collapse of competence under novel stress, despite high formal
 *   adherence. The claimed type is 'piton' because the primary function
 *   (actual readiness) has atrophied, but the constraint persists due to
 *   institutional inertia and the theatrical maintenance of 'being prepared'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.65).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.7).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, '96c20902-1569-4404-8975-db5e24714248').
narrative_ontology:cs_kernel_codification('96c20902-1569-4404-8975-db5e24714248', formalized).
narrative_ontology:cs_authority_grounding('96c20902-1569-4404-8975-db5e24714248', extraction).
narrative_ontology:cs_interpretation_layer_present('96c20902-1569-4404-8975-db5e24714248').
narrative_ontology:cs_reading_relation('96c20902-1569-4404-8975-db5e24714248', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('96c20902-1569-4404-8975-db5e24714248', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('96c20902-1569-4404-8975-db5e24714248', foundational, appearance_of_readiness_is_sufficient).
narrative_ontology:cs_axiom_status(appearance_of_readiness_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('96c20902-1569-4404-8975-db5e24714248', appearance_of_readiness_is_sufficient, conventional).
narrative_ontology:cs_axiom('96c20902-1569-4404-8975-db5e24714248', secondary, formal_compliance_equals_competence).
narrative_ontology:cs_axiom_status(formal_compliance_equals_competence, holdable).
narrative_ontology:cs_axiom_grounding('96c20902-1569-4404-8975-db5e24714248', formal_compliance_equals_competence, conventional).
narrative_ontology:cs_reference_frame('96c20902-1569-4404-8975-db5e24714248', formal_compliance_framework).
narrative_ontology:cs_drift_state('96c20902-1569-4404-8975-db5e24714248', contemporary_complex_threat_environment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96c20902-1569-4404-8975-db5e24714248', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, public_relations_departments).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the agenda for preparedness, emphasizing compliance with established procedures and visible drills. Benefits from the appearance of readiness and avoids accountability for actual competence gaps. Their careers depend on maintaining the illusion of control.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, institutional_leadership, agenda_setter,
    institutional, biographical, constrained, national).

% Leverage preparedness drills and formal plans to project an image of competence and responsibility to the public and media. Their success is measured by positive media coverage and public reassurance, not by operational outcomes.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, public_relations_departments, beneficiary,
    organized, immediate, mobile, national).

% Participate in drills and maintain formal compliance, often aware of the gap between performance and actual readiness. They bear the cost of ineffective training and the risk of operational failure in real crises. Their professional identity is tied to the institution, limiting exit.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_responders, payer,
    moderate, immediate, constrained, local).

% Are the ultimate victims of preparedness failures. They rely on the promised competence of institutions and suffer disproportionately when memorial performance collapses under stress. They have no direct influence over preparedness policy.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% Review preparedness plans and drill reports for compliance with regulations. Their audits often focus on documentation and formal procedures, inadvertently reinforcing the memorial performance aspect rather than probing operational depth.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, external_auditors, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional actors around a shared set of procedures and visible demonstrations of readiness, creating a sense of order and accountability in the face of potential disaster.
% TRANSFER_FUNCTION: Transfers resources (time, budget, attention) from genuine operational capacity building to performative compliance and public relations, from frontline competence to institutional image.
% ABSENT_VOICES: Experienced operational experts who have left the system due to frustration with performative culture, and future victims of disaster who would demand genuine competence over theatrical displays.
% DISAPPEARANCE_RATIONALE: If this performative aspect vanished, institutions would either be forced to confront their actual competence gaps and invest in real readiness, or face immediate public and political backlash for their lack of demonstrable preparedness. The current equilibrium of 'looking prepared' would collapse.
% FOUNDING_PROBLEM: The need to demonstrate institutional readiness and accountability for disaster response, especially after past failures, and to reassure the public that risks are being managed.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership and public relations departments attest that the problem of demonstrating readiness is live. Frontline responders and external auditors, while critical of the current approach, corroborate the underlying need for demonstrable preparedness, but not the efficacy of the current system.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because resources are diverted from real capacity to performative displays, and the cost of actual failure is borne by frontline responders and vulnerable populations. Suppression is high because dissent about the efficacy of current preparedness is often marginalized or reframed as 'not understanding the process'. The theater ratio is very high (0.85) as the core activity is about appearing ready, not being ready. Accessibility collapse is moderate, as some alternatives (e.g., genuine, adaptive training) are theoretically available but institutionally suppressed. Resistance is low because the system is effective at absorbing or deflecting criticism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional leadership, the constraint is a necessary 'rope' for coordinating complex readiness efforts. From the perspective of frontline responders and vulnerable populations, it operates as a 'snare' or 'piton', extracting resources and trust while delivering insufficient actual protection. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership and public relations departments are beneficiaries, gaining legitimacy and positive image from the performative aspects. Frontline responders and vulnerable populations are payers, bearing the costs of inadequate preparation. External auditors, while observers, can inadvertently reinforce the performative aspect by focusing on formal compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring actual preparedness) has atrophied, replaced by a focus on the performance of preparedness. The classification as 'piton' prevents mislabeling this as a 'rope' (which would imply genuine coordination benefits for all) or a 'snare' (which would imply concentrated benefit for a party actively maintaining the extraction). Instead, it highlights the diffuse costs and the inertial persistence of a degraded function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_performative_budget_allocation,
    'What proportion of the preparedness budget is allocated to genuine operational capacity building versus performative drills and public relations?',
    'Detailed, independently audited financial analysis of preparedness spending, categorizing expenditures by their direct contribution to adaptive operational competence versus formal compliance and image management.',
    'If the majority of the budget is performative, it strengthens the ''piton'' classification and the ''husk_reading''. If it''s genuinely operational, it would shift towards a ''rope'' or ''tangled_rope'' classification, supporting the ''competence_reading'' or ''hybrid_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_vs_performative_budget_allocation, empirical, 'Distinguishes resource allocation between actual competence and theatrical display.').

omega_variable(
    accountability_for_failure,
    'To what extent are institutional leaders held accountable for actual operational failures during crises, as opposed to failures in formal compliance or public image?',
    'Analysis of post-crisis investigations and personnel changes: do leaders face consequences for competence gaps, or only for visible missteps in communication or procedure?',
    'If accountability is primarily for performance, it reinforces the ''husk_reading'' and the ''piton'' classification. If it''s for operational competence, it would push towards a ''snare'' (if leaders benefit from the extraction) or ''tangled_rope'' (if there''s genuine coordination with asymmetric costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_for_failure, empirical, 'Examines the true locus of accountability in preparedness systems.').

omega_variable(
    husk_vs_competence_framing,
    'Is preparedness fundamentally about maintaining a visible commitment (husk reading) or about ensuring adaptive capacity (competence reading)?',
    'Analysis of institutional discourse and resource allocation patterns over time: which framing consistently guides decision-making and investment?',
    'If the ''husk reading'' is the dominant frame, the constraint remains a piton. If the ''competence reading'' gains ascendancy, the constraint would shift towards a rope or tangled rope, with a lower theater ratio and higher genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_framing, conceptual, 'The core conceptual ambiguity between performative and functional preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.7).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__husk_reading, theater_ratio, 5, 0.75).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__husk_reading, theater_ratio, 10, 0.8).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__husk_reading, theater_ratio, 15, 0.83).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__husk_reading, theater_ratio, 20, 0.85).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__husk_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__husk_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__husk_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__husk_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__husk_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__husk_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__husk_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__husk_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__husk_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_commitment' kernel, focusing on the performative and competence-lacking aspects. It is linked to sibling readings that emphasize competence or a hybrid approach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
