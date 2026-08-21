% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Probable Cause Floor for FISA 702 Queries
 *   domain: Constitutional Law / National Security / Surveillance Policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'constitutional_floor_reading' of
 *   the FISA Section 702 kernel. It asserts that, regardless of statutory
 *   language, the Fourth Amendment mandates a probable cause warrant for any
 *   government search of U.S. person communications content, and that FISA
 *   702 database queries constitute such searches. This reading reframes the
 *   issue from a foreign intelligence statute to a criminal procedure
 *   question, requiring individualized judicial review prior to querying U.S.
 *   person data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.4).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, mountain).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Probable Cause Floor for FISA 702 Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "Constitutional Law / National Security / Surveillance Policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).
domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '079c93b7-82aa-4141-bdb6-be8c8a6e8d48').
narrative_ontology:cs_kernel_codification('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', fixed_text).
narrative_ontology:cs_authority_grounding('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', lineage).
narrative_ontology:cs_interpretation_layer_present('079c93b7-82aa-4141-bdb6-be8c8a6e8d48').
narrative_ontology:cs_reading_relation('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', foundational, fourth_amendment_applies_to_all_searches_of_us_persons).
narrative_ontology:cs_axiom_status(fourth_amendment_applies_to_all_searches_of_us_persons, holdable).
narrative_ontology:cs_axiom_grounding('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', fourth_amendment_applies_to_all_searches_of_us_persons, deontological).
narrative_ontology:cs_axiom('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', foundational, warrant_requirement_is_default_for_content_searches).
narrative_ontology:cs_axiom_status(warrant_requirement_is_default_for_content_searches, holdable).
narrative_ontology:cs_axiom_grounding('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', warrant_requirement_is_default_for_content_searches, deontological).
narrative_ontology:cs_reference_frame('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', original_fourth_amendment_intent).
narrative_ontology:cs_drift_state('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', contemporary_surveillance_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('079c93b7-82aa-4141-bdb6-be8c8a6e8d48', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, intelligence_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose communications content is protected by the Fourth Amendment, regardless of whether they are targeted or incidentally collected. They benefit from the warrant requirement as a safeguard against arbitrary government intrusion.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    powerless, biographical, trapped, national).

% Organizations and legal experts who champion the Fourth Amendment's protections and litigate to enforce them. They benefit from this reading as it aligns with their mission to limit government surveillance power.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocates, beneficiary,
    organized, generational, mobile, national).

% Government entities (e.g., NSA, FBI) that conduct surveillance under FISA Section 702. They bear the cost of this constraint by having to obtain probable cause warrants for queries of U.S. person communications, which they view as an impediment to operational speed and secrecy.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, intelligence_agencies, payer,
    institutional, generational, constrained, global).

% The branch of government responsible for national security and intelligence operations. It sets policy and directs agencies, and resists this reading due to perceived operational burdens and a preference for executive discretion in intelligence matters.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, national).

% The court established to oversee requests for surveillance warrants. Under this reading, its role would expand to include individualized probable cause review for U.S. person queries, acting as a check on executive power.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, observer).

% The legislative branch, which has the power to amend FISA 702. It observes the debate and could legislate to codify or reject this constitutional interpretation, though its actions are also subject to constitutional review.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, congress, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates government surveillance activities with the constitutional rights of U.S. persons, ensuring that national security interests are pursued within the bounds of individual liberty and judicial oversight.
% TRANSFER_FUNCTION: Transfers the burden of justifying surveillance from the individual (who would otherwise be subject to warrantless search) to the government, requiring a showing of probable cause to a neutral magistrate.
% ABSENT_VOICES: Those who prioritize unfettered executive power for national security above individual rights, arguing that any warrant requirement impedes intelligence gathering and creates unacceptable delays in responding to threats. They are often excluded from the judicial process that would enforce this reading.
% DISAPPEARANCE_RATIONALE: If this constitutional floor vanished, government surveillance of U.S. persons' communications content would expand significantly without judicial oversight, fundamentally altering the balance of power between the state and its citizens and eroding privacy protections.
% FOUNDING_PROBLEM: To protect individuals from unreasonable searches and seizures by the government, ensuring privacy and liberty against arbitrary state power, as enshrined in the Fourth Amendment.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil liberties organizations, and historical jurisprudence consistently corroborate the Fourth Amendment's intent to limit government search power, citing its origins in colonial grievances against general warrants.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, ExtMetricName, E),
    domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `claimed_type` is 'mountain' because this reading asserts a fundamental, unchangeable constitutional requirement. `Extractiveness` is low (0.25) because it represents the 'cost' of constitutional compliance for the executive (e.g., speed, secrecy preferences), not an extraction from citizens. `Suppression` is moderate (0.4) as this reading aims to *reduce* the suppression of Fourth Amendment rights, but the executive's resistance means the constraint is not fully effective. `Resistance` is high (0.8) due to strong opposition from intelligence agencies and the executive branch, who view the warrant requirement as an impediment. `Accessibility_collapse` is low (0.3) as this reading opens up the alternative of judicial review.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of U.S. persons and civil liberties advocates, this constraint is a vital protection of fundamental rights. From the perspective of intelligence agencies and the executive branch, it is an unnecessary bureaucratic hurdle that impedes national security operations. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons and civil liberties advocates are beneficiaries, as their rights are protected by this interpretation. Intelligence agencies and the executive branch are victims, as their operational preferences are constrained by the warrant requirement. The FISA Court, while an institutional actor, acts as an enforcer of this constitutional floor, aligning its role with the beneficiaries' interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by re-asserting the original, enduring mandate of the Fourth Amendment against evolving surveillance technologies and executive interpretations. It ensures the constraint's function remains tied to its founding purpose of protecting individual liberty, rather than atrophying into a mere procedural formality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_policy_ambiguity,
    'Is the requirement for a probable cause warrant for FISA 702 queries of U.S. person data a genuine constitutional mandate, or a policy preference dressed as constitutional law?',
    'Supreme Court ruling explicitly affirming or denying the Fourth Amendment''s applicability to such queries, or a constitutional amendment clarifying surveillance powers.',
    'If affirmed as constitutional, the constraint''s ''mountain'' classification is strengthened; if denied, it would be reclassified as a ''snare'' (if enforced by executive power) or ''piton'' (if merely a rhetorical claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_policy_ambiguity, conceptual, 'Ambiguity regarding the constitutional grounding of the warrant requirement.').

omega_variable(
    operational_impact_of_warrant_requirement,
    'To what extent would a probable cause warrant requirement for FISA 702 queries genuinely impede intelligence gathering and national security, versus merely requiring more rigorous justification?',
    'Empirical studies and declassified data on the impact of warrant requirements on intelligence operations in comparable contexts, or a pilot program implementing such a requirement.',
    'If the impediment is minimal, it strengthens the argument for constitutional compliance; if severe, it would highlight a tension between constitutional rights and perceived national security needs, potentially shifting the constraint''s effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_impact_of_warrant_requirement, empirical, 'The actual operational cost of implementing a warrant requirement.').

omega_variable(
    scope_of_search_definition_ambiguity,
    'What specific actions or data access points constitute a ''search'' of U.S. person communications content in the context of modern digital surveillance, triggering the warrant requirement?',
    'Judicial clarification through case law, or legislative definition of ''search'' tailored to digital communications and intelligence operations.',
    'A broad definition of ''search'' would expand the constraint''s scope and impact; a narrow definition would limit its application, potentially reducing its protective function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_search_definition_ambiguity, conceptual, 'Ambiguity in defining ''search'' for digital communications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(fisa_tr_t30, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(fisa_be_t30, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(fisa_su_t30, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_privacy_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the FISA 702 statutory text kernel, focusing on the Fourth Amendment's constitutional floor. It is linked to sibling readings that offer alternative interpretations of the same statutory framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
