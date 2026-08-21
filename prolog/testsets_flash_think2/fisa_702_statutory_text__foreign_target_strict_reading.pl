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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: FISA Section 702 Foreign Target Strict Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint represents a strict interpretation of the FISA Section
 *   702 statutory language, focusing on the 'foreign target' requirement and
 *   robust minimization of U.S. person data. Under this reading, collection
 *   is strictly limited to non-U.S. persons abroad, and any incidental U.S.
 *   person data is minimized and inaccessible for domestic purposes without a
 *   warrant. This interpretation aims to ensure that FISA 702 operates as a
 *   foreign intelligence tool without becoming a backdoor for domestic
 *   surveillance. The low extractiveness and suppression reflect the strong
 *   safeguards for U.S. persons inherent in this strict reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA Section 702 Foreign Target Strict Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '6dcadeb1-bb7f-4e90-aef0-f85b581904b5').
narrative_ontology:cs_kernel_codification('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', fixed_text).
narrative_ontology:cs_authority_grounding('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', lineage).
narrative_ontology:cs_interpretation_layer_present('6dcadeb1-bb7f-4e90-aef0-f85b581904b5').
narrative_ontology:cs_reading_relation('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', foundational, warrantless_surveillance_of_us_persons_prohibited).
narrative_ontology:cs_axiom_status(warrantless_surveillance_of_us_persons_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', warrantless_surveillance_of_us_persons_prohibited, deontological).
narrative_ontology:cs_axiom('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', foundational, foreign_intelligence_purpose_must_be_primary).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_must_be_primary, holdable).
narrative_ontology:cs_axiom_grounding('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', foreign_intelligence_purpose_must_be_primary, conventional).
narrative_ontology:cs_reference_frame('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', fourth_amendment_privacy_protections).
narrative_ontology:cs_drift_state('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6dcadeb1-bb7f-4e90-aef0-f85b581904b5', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, national_security_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_abroad).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for collecting foreign intelligence under FISA Section 702. Under this strict reading, they benefit from a clear legal framework for collection while being constrained by robust minimization and targeting rules that protect U.S. persons.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, national_security_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from strong Fourth Amendment protections: their communications are not subject to warrantless collection, and any incidentally collected data is minimized and inaccessible for domestic law enforcement purposes without a warrant. Their privacy is largely preserved.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons, beneficiary,
    organized, generational, mobile, national).

% Monitor the implementation of FISA 702 to ensure compliance with statutory limits and constitutional rights. This strict reading largely aligns with their advocacy goals for privacy protection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Oversee the implementation of FISA 702, reviewing targeting and minimization procedures. Under this strict reading, their role is to ensure rigorous adherence to the statutory text, particularly regarding U.S. person protections.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisc_judges, agenda_setter,
    institutional, generational, constrained, national).

% Are the legitimate targets of foreign intelligence collection under FISA 702. Their communications are collected without a warrant, as they are outside U.S. jurisdiction and are not U.S. persons. They bear the direct cost of surveillance.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_abroad, payer,
    powerless, biographical, trapped, global).

% Enacts and reauthorizes FISA Section 702, setting the statutory boundaries for foreign intelligence collection. This strict reading reflects the intent of some members of Congress to balance national security with civil liberties.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congress, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__foreign_target_strict_reading, national_security_agencies).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__foreign_target_strict_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collection of vital foreign intelligence by national security agencies, ensuring that such collection is focused on non-U.S. persons abroad and adheres to strict minimization procedures for any incidentally collected U.S. person data.
% TRANSFER_FUNCTION: Transfers foreign intelligence information from non-U.S. persons abroad to U.S. national security agencies, while simultaneously transferring privacy protections to U.S. persons by limiting access to their data for domestic purposes.
% ABSENT_VOICES: Non-U.S. persons abroad are structurally absent from the U.S. legal and political discourse regarding FISA 702, despite being the primary targets of collection. They lack standing to challenge the constraint.
% DISAPPEARANCE_RATIONALE: If this statutory language and its strict interpretation vanished, U.S. foreign intelligence collection would lose its legal basis, potentially leading to a significant gap in national security capabilities. Simultaneously, U.S. person privacy protections would be severely weakened, leading to a reorganization of surveillance practices and a surge in civil liberties concerns.
% FOUNDING_PROBLEM: The need for an effective foreign intelligence collection tool to counter terrorism and other national security threats post-9/11, while simultaneously safeguarding the Fourth Amendment rights of U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: Intelligence community reports consistently highlight the ongoing need for FISA 702 for national security. Civil liberties organizations, while often critical of its implementation, acknowledge the foundational problem of balancing security and privacy. Congressional debates and legislative history also corroborate the dual nature of the founding problem.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.15) reflects that, under this strict reading, U.S. persons are largely protected from warrantless surveillance, and the 'extraction' is primarily from non-U.S. persons abroad, who are legitimate foreign intelligence targets. Suppression (0.10) is low because U.S. persons retain robust legal avenues to challenge any perceived overreach, and the constraint itself is designed to prevent suppression of their rights. Theater ratio (0.05) is minimal, as this reading assumes genuine adherence to the statutory limitations rather than performative compliance. The metrics are held constant over time to reflect the ideal, stable operation of the statute under this strict interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Under this strict reading, there is less of a perspectival gap for U.S. persons, as their rights are largely protected. However, non-U.S. persons abroad would still experience the constraint as purely extractive, lacking the protections afforded to U.S. persons. The primary perspectival gap exists between this strict reading and other, more expansive interpretations of FISA 702, which would show higher extraction and suppression for U.S. persons.
 *
 * DIRECTIONALITY LOGIC:
 *   National security agencies and U.S. persons are beneficiaries: agencies gain foreign intelligence, and U.S. persons gain privacy protections. Non-U.S. persons abroad are the targets/payers, as their communications are collected. The strict reading ensures that the benefits to U.S. persons are substantial and direct, preventing them from becoming victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimization_effectiveness_ambiguity,
    'Is the statutory minimization requirement, as strictly interpreted, truly effective in preventing access to U.S. person data for domestic purposes, or does it require deletion rather than mere access restriction?',
    'Independent audits of agency minimization practices, judicial rulings from the FISC, or legislative amendments mandating deletion of U.S. person data.',
    'If minimization as access restriction is found ineffective, the constraint''s effective extractiveness and suppression for U.S. persons would be higher, pushing it towards a Tangled Rope or Snare classification from the U.S. person seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_effectiveness_ambiguity, empirical, 'Ambiguity regarding the practical effectiveness of minimization procedures.').

omega_variable(
    fbi_query_prohibition_enforcement,
    'Are FBI queries of the 702 database for domestic criminal investigations truly ''categorically prohibited'' under this strict reading, or do operational realities and interpretive ambiguities create loopholes for such queries?',
    'Publicly released FISC opinions detailing instances of non-compliance or clarification of permissible query standards, or congressional oversight investigations.',
    'If domestic FBI queries are found to occur, the constraint''s effective extractiveness and suppression for U.S. persons would increase, indicating a shift towards a more extractive classification for U.S. persons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fbi_query_prohibition_enforcement, empirical, 'Ambiguity regarding the enforcement of prohibitions on domestic FBI queries.').

omega_variable(
    constitutional_compliance_ambiguity,
    'Does this strict reading of FISA 702, despite its safeguards, fully satisfy the Fourth Amendment''s constitutional floor for U.S. person privacy, or does the Fourth Amendment require an individualized probable cause warrant for any search of U.S. person communications?',
    'Supreme Court ruling on the constitutionality of FISA 702 as applied, or a constitutional amendment clarifying privacy rights in the digital age.',
    'If a court rules that even this strict reading falls short of the Fourth Amendment, the constraint would be reclassified as a Snare or Tangled Rope from the U.S. person seat, indicating a fundamental constitutional defect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_compliance_ambiguity, conceptual, 'Ambiguity regarding whether the statute, even strictly read, meets constitutional requirements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(fisa_tr_t2012, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2012, 0.05).
narrative_ontology:measurement(fisa_tr_t2016, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2016, 0.05).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2020, 0.05).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement(fisa_be_t2012, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2012, 0.15).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2016, 0.15).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2008, 0.1).
narrative_ontology:measurement(fisa_su_t2012, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2012, 0.1).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2016, 0.1).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2020, 0.1).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the FISA Section 702 statutory text, each with different structural implications for privacy and surveillance. This reading emphasizes strict adherence to statutory limits to protect U.S. persons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
