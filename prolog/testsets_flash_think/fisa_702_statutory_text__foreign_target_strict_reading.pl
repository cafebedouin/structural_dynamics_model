% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA Section 702: Strict Foreign Target Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint story represents a strict reading of the statutory
 *   'foreign target' language within FISA Section 702. Under this
 *   interpretation, collection is rigorously confined to communications where
 *   both the sender and the primary investigative interest are non-U.S.
 *   persons located abroad. Crucially, any incidental U.S. person data must
 *   be minimized to the point of inaccessibility for domestic purposes
 *   without a separate, individualized warrant. This reading emphasizes the
 *   protective aspects of the statute for U.S. persons, aiming to maintain a
 *   low extractiveness profile for them while enabling legitimate foreign
 *   intelligence collection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.4).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA Section 702: Strict Foreign Target Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '050d968e-2aa7-4c6e-976e-607780a9b724').
narrative_ontology:cs_kernel_codification('050d968e-2aa7-4c6e-976e-607780a9b724', fixed_text).
narrative_ontology:cs_authority_grounding('050d968e-2aa7-4c6e-976e-607780a9b724', lineage).
narrative_ontology:cs_interpretation_layer_present('050d968e-2aa7-4c6e-976e-607780a9b724').
narrative_ontology:cs_reading_relation('050d968e-2aa7-4c6e-976e-607780a9b724', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('050d968e-2aa7-4c6e-976e-607780a9b724', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('050d968e-2aa7-4c6e-976e-607780a9b724', foundational, statutory_text_limits_scope).
narrative_ontology:cs_axiom_status(statutory_text_limits_scope, holdable).
narrative_ontology:cs_axiom_grounding('050d968e-2aa7-4c6e-976e-607780a9b724', statutory_text_limits_scope, conventional).
narrative_ontology:cs_axiom('050d968e-2aa7-4c6e-976e-607780a9b724', foundational, minimization_as_exclusion).
narrative_ontology:cs_axiom_status(minimization_as_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('050d968e-2aa7-4c6e-976e-607780a9b724', minimization_as_exclusion, deontological).
narrative_ontology:cs_reference_frame('050d968e-2aa7-4c6e-976e-607780a9b724', original_fisa_702_intent_balance).
narrative_ontology:cs_drift_state('050d968e-2aa7-4c6e-976e-607780a9b724', contemporary_oversight_debates, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('050d968e-2aa7-4c6e-976e-607780a9b724', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_national_security_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_abroad).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_privacy_rights).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, statutory_construction_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the FISA Section 702 program, targeting non-U.S. persons abroad for foreign intelligence purposes. Under this strict reading, they are constrained to minimize and make inaccessible any incidentally collected U.S. person data for domestic law enforcement, ensuring the program serves its intended foreign intelligence mandate without becoming a domestic surveillance tool.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_national_security_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the strict interpretation of FISA 702, which ensures their communications are not intentionally targeted and that any incidentally collected data is minimized and inaccessible for domestic purposes without a warrant, thereby upholding their Fourth Amendment privacy rights. Their 'exit' is through legal challenge and political advocacy.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons, beneficiary,
    organized, biographical, constrained, national).

% Are the legitimate targets of foreign intelligence collection under FISA 702. Their communications are collected without a warrant, as they are outside U.S. jurisdiction and are not afforded Fourth Amendment protections. They bear the direct cost of surveillance under this statutory framework.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_abroad, payer,
    powerless, immediate, trapped, global).

% Interpret and enforce the statutory language of FISA 702, including the foreign targeting and minimization requirements. Under this strict reading, they would uphold robust protections for U.S. persons and ensure the program adheres to its statutory limits.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_courts, agenda_setter,
    institutional, generational, analytical, national).

% Enacts and reauthorizes FISA Section 702. This reading reflects the intent to balance national security with civil liberties, requiring clear statutory boundaries for surveillance activities.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Actively monitor and challenge the implementation of FISA 702, advocating for interpretations that maximize privacy protections for U.S. persons and minimize the scope of government surveillance. This strict reading aligns closely with their objectives.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collection of vital foreign intelligence on non-U.S. persons abroad while simultaneously coordinating the protection of U.S. persons' Fourth Amendment rights by strictly limiting the use of incidentally collected data.
% TRANSFER_FUNCTION: Transfers foreign intelligence information from non-U.S. persons abroad to U.S. national security agencies, while strictly limiting the transfer of U.S. person data to domestic law enforcement without a warrant.
% ABSENT_VOICES: Foreign governments and international human rights organizations would object to the surveillance of non-U.S. persons, arguing for universal privacy rights, but are largely outside the U.S. legal and political conversation regarding FISA 702.
% DISAPPEARANCE_RATIONALE: If this strict reading of FISA 702 vanished, the balance between national security and civil liberties would be fundamentally altered. U.S. person data would be at greater risk of warrantless domestic use, and the foreign intelligence community would lose a key tool, forcing a complete reorganization of surveillance policy and practice.
% FOUNDING_PROBLEM: The original problem was the need for a legal framework to collect foreign intelligence from non-U.S. persons located outside the United States, particularly in the context of evolving digital communications, without infringing on the constitutional rights of U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: U.S. national security agencies attest to the ongoing live nature of foreign intelligence threats. Civil liberties advocates and some members of Congress corroborate the ongoing need for a framework, but emphasize the persistent challenge of balancing it with privacy, supporting the need for strict interpretation.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set low (0.15) because this strict reading prioritizes the protection of U.S. persons' privacy, ensuring that the primary extraction is from legitimate foreign targets, not domestic individuals. Suppression (0.4) reflects the legal and technical mechanisms in place to enforce these limitations, preventing unauthorized access to U.S. person data. Theater ratio is low (0.1) as this reading assumes genuine adherence to the statutory intent, with minimal performative compliance masking other objectives. Accessibility collapse (0.6) is moderate; while it limits government action, it doesn't collapse all avenues for legitimate surveillance. Resistance (0.5) is moderate, reflecting ongoing legal and political debate over FISA 702's interpretation, with this reading being a point of advocacy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of U.S. persons and civil liberties advocates, this strict reading functions as a protective 'rope,' coordinating national security with constitutional rights. From the perspective of non-U.S. persons abroad, it is still a 'snare' of surveillance, albeit one operating within defined legal parameters. The engine's per-seat classification would reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. national security agencies are beneficiaries in that they gain a legal framework for foreign intelligence, but are also constrained by strict rules. U.S. persons are beneficiaries due to robust privacy protections. Non-U.S. persons abroad are the primary targets/victims, as their communications are collected for foreign intelligence purposes. U.S. courts and Congress act as agenda-setters, defining and enforcing the boundaries of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimization_effectiveness_ambiguity,
    'Is the statutory minimization requirement, even under a strict reading, truly effective in preventing warrantless domestic access to U.S. person data, or does it remain a technical/procedural rather than substantive barrier?',
    'Independent audits of minimization procedures, analysis of FBI query practices, and judicial review of specific cases where U.S. person data was accessed for domestic purposes.',
    'If minimization is found to be substantively ineffective, the effective extractiveness for U.S. persons would be higher, potentially shifting the classification towards a ''tangled_rope'' or ''snare'' for that seat. If effective, the ''rope'' classification for U.S. persons is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_effectiveness_ambiguity, empirical, 'Effectiveness of minimization procedures for U.S. person data.').

omega_variable(
    statutory_vs_constitutional_authority,
    'Does the statutory authority of FISA 702, even strictly interpreted, adequately address the Fourth Amendment''s constitutional floor for U.S. person searches, or is an individualized warrant always required for U.S. person communications content?',
    'Supreme Court rulings on the constitutionality of FISA 702''s U.S. person data handling, or legislative action to codify a warrant requirement.',
    'If the Supreme Court upholds the ''constitutional_floor_reading'', this ''foreign_target_strict_reading'' would be deemed insufficient to protect U.S. persons, leading to a reclassification of the constraint as more extractive for U.S. persons, or even a ''snare'' if the statutory framework is found to be unconstitutional without a warrant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_vs_constitutional_authority, conceptual, 'Relationship between statutory limits and constitutional requirements for U.S. person data.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(fisa_tr_t2012, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(fisa_tr_t2016, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement(fisa_be_t2012, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2012, 0.15).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2016, 0.15).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement(fisa_su_t2012, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2012, 0.4).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the FISA Section 702 statutory text kernel, each with different structural properties and implications for U.S. persons' rights and government surveillance powers. This reading emphasizes strict foreign targeting and minimization, contrasting with readings that permit broader incidental collection or demand a constitutional warrant floor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
