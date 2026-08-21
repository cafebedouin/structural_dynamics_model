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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Fourth Amendment Probable Cause Warrant for 702 US Person Content Searches (Constitutional Floor Reading)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents the 'constitutional floor' reading of FISA
 *   Section 702, asserting that the Fourth Amendment mandates a probable
 *   cause warrant for any government search of U.S. person communications
 *   content, regardless of statutory interpretation or foreign/domestic
 *   distinctions. This reading classifies 702 database queries as searches,
 *   triggering a warrant requirement and individualized judicial review. The
 *   claimed type is 'mountain' because it asserts an unchangeable
 *   constitutional principle, even though its application to 702 is highly
 *   contested. The metrics reflect the costs imposed on executive speed and
 *   secrecy preferences, and the high suppression of unconstitutional
 *   alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.88).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, mountain).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Probable Cause Warrant for 702 US Person Content Searches (Constitutional Floor Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).
domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '3a395fe8-b065-44fa-92cd-c2947f129a9a').
narrative_ontology:cs_kernel_codification('3a395fe8-b065-44fa-92cd-c2947f129a9a', fixed_text).
narrative_ontology:cs_authority_grounding('3a395fe8-b065-44fa-92cd-c2947f129a9a', lineage).
narrative_ontology:cs_interpretation_layer_present('3a395fe8-b065-44fa-92cd-c2947f129a9a').
narrative_ontology:cs_reading_relation('3a395fe8-b065-44fa-92cd-c2947f129a9a', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('3a395fe8-b065-44fa-92cd-c2947f129a9a', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('3a395fe8-b065-44fa-92cd-c2947f129a9a', foundational, warrant_for_us_person_content_search).
narrative_ontology:cs_axiom_status(warrant_for_us_person_content_search, holdable).
narrative_ontology:cs_axiom_grounding('3a395fe8-b065-44fa-92cd-c2947f129a9a', warrant_for_us_person_content_search, deontological).
narrative_ontology:cs_axiom('3a395fe8-b065-44fa-92cd-c2947f129a9a', foundational, fourth_amendment_applies_to_702_queries).
narrative_ontology:cs_axiom_status(fourth_amendment_applies_to_702_queries, holdable).
narrative_ontology:cs_axiom_grounding('3a395fe8-b065-44fa-92cd-c2947f129a9a', fourth_amendment_applies_to_702_queries, deontological).
narrative_ontology:cs_reference_frame('3a395fe8-b065-44fa-92cd-c2947f129a9a', individual_privacy_as_constitutional_bedrock).
narrative_ontology:cs_drift_state('3a395fe8-b065-44fa-92cd-c2947f129a9a', post_9_11_surveillance_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3a395fe8-b065-44fa-92cd-c2947f129a9a', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, intelligence_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_protections).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, individual_privacy_rights).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, separation_of_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As individuals, they are the primary subjects of surveillance and benefit from the protection of a probable cause warrant requirement for searches of their communications content. Their ability to exit the surveillance system is negligible.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    powerless, biographical, trapped, global).

% Actively champion this interpretation, viewing it as essential for protecting fundamental rights. They benefit from its adoption as it aligns with their mission and legal arguments.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).

% Bear the cost of increased operational friction, speed, and secrecy compromises due to the warrant requirement. They argue it hinders their ability to protect national security. Their exit options are limited to seeking legislative changes or challenging the interpretation in court.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, intelligence_agencies, payer,
    institutional, immediate, constrained, global).

% As the branch overseeing intelligence operations, they bear the political and administrative costs of this interpretation, which limits their discretion in surveillance activities. They resist this reading, preferring broader authority.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Would be responsible for conducting individualized probable cause review for 702 queries, shifting its role from oversight of programmatic collection to pre-query warrant adjudication. This expands its judicial authority and workload.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, analytical, national).

% Holds legislative power over FISA 702 but, under this reading, would be constrained by the constitutional floor. They observe the legal contestation and could legislate to align statutory text with this constitutional interpretation, or attempt to circumvent it.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, constitutionally mandated standard for government searches of U.S. person communications content, coordinating the actions of intelligence agencies with Fourth Amendment requirements and judicial oversight.
% TRANSFER_FUNCTION: Transfers authority over U.S. person communications content searches from the executive branch to the judicial branch (FISA Court), imposing a cost of compliance (time, resources, disclosure) on intelligence operations.
% ABSENT_VOICES: Foreign intelligence targets are absent from this specific debate, as the constraint focuses on U.S. persons. Their interests in privacy and due process, while related, are not directly addressed by this U.S.-centric constitutional floor.
% DISAPPEARANCE_RATIONALE: If this constitutional floor vanished, intelligence agencies would likely revert to broader, warrantless searches of U.S. person communications content, significantly eroding privacy protections and shifting the balance of power towards the executive. The legal and operational landscape of surveillance would fundamentally reorganize.
% FOUNDING_PROBLEM: The Fourth Amendment was established to prevent arbitrary government searches and seizures, ensuring individual privacy and security against unchecked state power, particularly in the context of evolving surveillance technologies.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations, legal scholars, and a segment of the judiciary consistently attest that the founding problem of protecting against arbitrary government intrusion remains live and is exacerbated by modern surveillance capabilities. Intelligence agencies and the executive branch, while acknowledging the Fourth Amendment, dispute this specific interpretation's necessity for national security.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.25) because this constraint primarily extracts from the executive's preferred operational flexibility, not from citizens. Suppression is high (0.88) because this reading aims to completely suppress the alternative of warrantless searches of U.S. person content, enforcing a strict constitutional boundary. Theater ratio is low (0.05) as this is a direct legal interpretation, not a performative maintenance. Resistance is high (0.75) from intelligence agencies and the executive branch, who actively oppose this interpretation due to perceived operational impacts. Accessibility collapse is high (0.9) for the government's ability to conduct warrantless searches of U.S. person content.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of U.S. persons and civil liberties advocates, this constraint is a vital protection, a 'mountain' of fundamental rights. From the perspective of intelligence agencies and the executive, it is an onerous, 'snare-like' burden that impedes national security operations. The engine's classification will highlight this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons and civil liberties advocates are the primary beneficiaries, gaining enhanced privacy protections. Intelligence agencies and the executive branch are the victims, facing increased procedural burdens and limitations on their surveillance capabilities. The FISA Court acts as an agenda-setter, tasked with enforcing this constitutional standard through individualized review.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is this ''constitutional_floor_reading'' the inherent, unchangeable meaning of the Fourth Amendment as applied to modern surveillance, or a policy preference framed as a constitutional mandate?',
    'A definitive Supreme Court ruling affirming this interpretation, or a constitutional amendment explicitly codifying it.',
    'If affirmed as inherent, the constraint''s ''mountain'' classification is robust. If revealed as a policy preference, it would reclassify as a ''rope'' or ''tangled_rope'' reflecting its constructed nature and contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of this constitutional interpretation.').

omega_variable(
    operational_impact_on_national_security,
    'What is the actual, measurable impact of a probable cause warrant requirement on intelligence agencies'' ability to prevent terrorist attacks or counter foreign adversaries?',
    'Declassified studies, independent expert analysis, or a pilot program implementing the warrant requirement with transparent reporting on operational outcomes.',
    'If the impact is negligible, it strengthens the ''mountain'' claim by removing the ''cost'' justification for warrantless searches. If the impact is severe, it would fuel resistance and potentially lead to legislative attempts to circumvent the interpretation, challenging its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_impact_on_national_security, empirical, 'Uncertainty about the practical operational costs of implementing a warrant requirement.').

omega_variable(
    reading_forecloses_incidental_collection,
    'How does this ''constitutional_floor_reading'' structurally interact with the ''incidental_collection_reading'' of the FISA 702 kernel?',
    'Analysis of judicial opinions and legal scholarship on the direct contradiction between requiring warrants for U.S. person content searches and permitting warrantless query of incidentally collected U.S. person data.',
    'This reading directly forecloses the ''incidental_collection_reading'' by making its core premise (warrantless query of U.S. person data) constitutionally impermissible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_forecloses_incidental_collection, conceptual, 'This reading forecloses the ''incidental_collection_reading'' by direct contradiction on warrant requirement.').

omega_variable(
    reading_influences_foreign_target_strict,
    'How does this ''constitutional_floor_reading'' structurally interact with the ''foreign_target_strict_reading'' of the FISA 702 kernel?',
    'Analysis of how a warrant requirement for U.S. person content searches would constrain the utility and handling of data collected under a ''foreign_target_strict_reading'' if it contains U.S. person communications.',
    'This reading influences the ''foreign_target_strict_reading'' by imposing a warrant requirement on searches of U.S. person content, even if collected under foreign intelligence authority, thereby constraining the utility of such collection and potentially requiring changes to collection minimization procedures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_influences_foreign_target_strict, conceptual, 'This reading influences the ''foreign_target_strict_reading'' by imposing search constraints on U.S. person data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(fisa_tr_t2012, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2012, 0.05).
narrative_ontology:measurement(fisa_tr_t2016, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2016, 0.05).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2020, 0.05).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement(fisa_be_t2012, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2012, 0.18).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2016, 0.21).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2020, 0.23).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.75).
narrative_ontology:measurement(fisa_su_t2012, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2012, 0.8).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2016, 0.83).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2020, 0.86).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
