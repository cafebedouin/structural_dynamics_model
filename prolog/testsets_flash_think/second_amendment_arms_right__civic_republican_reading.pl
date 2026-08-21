% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Armed Citizenship for Republican Self-Governance (Civic Republican Reading)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the civic republican reading of the Second
 *   Amendment, which views the right to keep and bear arms as intrinsically
 *   linked to the duty of armed citizenship and the maintenance of a free
 *   state. It is neither purely an individual liberty nor solely a state
 *   prerogative, but a civic right with associated responsibilities. The
 *   'moderate ε on training/qualification requirements' reflects the costs
 *   borne by citizens to fulfill this civic duty, making it a Tangled Rope
 *   where coordination (self-governance) comes with asymmetric extraction
 *   (regulatory compliance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.45).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.3).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Armed Citizenship for Republican Self-Governance (Civic Republican Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, 'a0fed765-c182-4ed2-b5f4-e8a97d2124a5').
narrative_ontology:cs_kernel_codification('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', fixed_text).
narrative_ontology:cs_authority_grounding('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', lineage).
narrative_ontology:cs_interpretation_layer_present('a0fed765-c182-4ed2-b5f4-e8a97d2124a5').
narrative_ontology:cs_reading_relation('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', foundational, armed_citizenry_essential_for_republic).
narrative_ontology:cs_axiom_status(armed_citizenry_essential_for_republic, holdable).
narrative_ontology:cs_axiom_grounding('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', armed_citizenry_essential_for_republic, deontological).
narrative_ontology:cs_axiom('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', foundational, civic_duty_accompanies_right).
narrative_ontology:cs_axiom_status(civic_duty_accompanies_right, holdable).
narrative_ontology:cs_axiom_grounding('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', civic_duty_accompanies_right, conventional).
narrative_ontology:cs_reference_frame('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0fed765-c182-4ed2-b5f4-e8a97d2124a5', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizens_subject_to_qualification_requirements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, individual_citizens).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, republican_self_governance_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_virtue_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These citizens are beneficiaries of the right to bear arms for civic purposes, seeing it as essential to their role in self-governance. However, they also bear the costs and duties of training, organization, and qualification requirements, which are seen as necessary for responsible armed citizenship.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, payer).

% These authorities are tasked with establishing and enforcing reasonable regulations for armed citizens, such as training, licensing, and safe storage, consistent with the civic republican understanding of the right. Their authority is constrained by the need to facilitate, not impede, civic participation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% All citizens benefit from the existence of an armed citizenry capable of contributing to self-governance, as it theoretically provides a check on potential tyranny and ensures a robust civic sphere. They may or may not actively participate in militia activities.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, individual_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Advocates for stricter gun control or outright prohibition are largely excluded from the core framing of this reading, which prioritizes armed citizenship as a civic good. Their arguments are often framed as undermining the very foundation of republican self-governance within this perspective.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, anti_gun_advocates, excluded,
    organized, biographical, constrained, national).

% Judicial bodies interpret the scope and limits of the Second Amendment. From this reading's perspective, their role is to balance the individual right to bear arms with the collective civic purpose and the state's regulatory authority to ensure a well-ordered militia.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, judicial_interpreters, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the right of citizens to bear arms with their civic duty to participate in the defense and governance of the republic, ensuring a balance between individual liberty and collective security through a 'well-regulated militia' concept.
% TRANSFER_FUNCTION: Transfers the responsibility for civic defense and the maintenance of a free state to an armed citizenry, while imposing costs in terms of training, qualification, and adherence to regulations on those citizens. It also transfers a degree of regulatory authority to the state, constrained by the civic purpose.
% ABSENT_VOICES: Those who view the Second Amendment as solely an individual right (unfettered by civic duty) or solely a collective right (limited to state-organized militias) are largely absent from this reading's central discourse. Also, those who advocate for complete disarmament or view armed citizenry as inherently dangerous are excluded.
% DISAPPEARANCE_RATIONALE: If this civic republican understanding of the Second Amendment vanished, the foundational concept of armed citizenship as a prerequisite for self-governance would be lost. This would likely lead to a re-evaluation of the relationship between citizens, arms, and the state, potentially shifting towards either pure individual liberty or pure state control, fundamentally altering the constitutional landscape.
% FOUNDING_PROBLEM: The constraint was built to ensure the security of a free state by preventing both federal overreach and the need for a standing army, relying instead on a virtuous, armed citizenry capable of self-defense and civic participation.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists outside of direct beneficiary groups corroborate that the founding generation genuinely grappled with these concerns, viewing an armed citizenry as vital for a republic. While interpretations of 'militia' and 'well-regulated' vary, the underlying problem of balancing liberty and security remains live, though its contemporary manifestations are contested.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) due to the inherent costs of training, qualification, and regulatory compliance for armed citizens, which are seen as necessary for the 'well-regulated' aspect of the militia. Suppression is low (0.30) because the constraint aims to enable, rather than suppress, armed citizenship, albeit within a regulated framework. Theater ratio is low (0.10) as the civic function is generally considered genuine, not merely performative. Accessibility collapse is moderate (0.60) as alternatives to armed citizenship for self-governance are limited within this framework, but not entirely foreclosed. Resistance is moderate (0.40) reflecting ongoing debates about the scope of regulation and the definition of 'militia'.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of citizen-militia members, the constraint is a necessary (if sometimes burdensome) coordination mechanism for self-governance. From the perspective of regulatory authorities, it's a framework for managing a vital civic function. Those outside this civic republican frame, particularly individual-right advocates, would see the regulatory aspects as undue extraction or suppression of liberty.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen-militia members are dual beneficiaries (of the right) and payers (of the duties/requirements), placing them near the symmetric end but with a slight tilt towards extraction due to the 'moderate ε'. Regulatory authorities are agenda-setters, benefiting from the coordinated civic defense but also constrained by the civic purpose. Individual citizens are diffuse beneficiaries of the overall framework. Anti-gun advocates are structurally excluded from this reading's core premises.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_definition_ambiguity,
    'What constitutes a ''well-regulated militia'' in the contemporary context, and how does this definition impact the scope of civic duty and regulatory authority?',
    'Legislative clarification, evolving judicial precedent, or widespread civic consensus on the nature of modern armed citizenship.',
    'A narrow definition could increase the perceived extraction on individual citizens by limiting their participation or increasing regulatory burden; a broad definition could dilute the ''well-regulated'' aspect, increasing perceived risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''militia'' and ''well-regulated'' in modern society.').

omega_variable(
    civic_duty_enforcement_genuineness,
    'Is the ''civic duty'' aspect of armed citizenship genuinely enforced and practiced, or is it largely rhetorical cover for individual gun ownership without corresponding responsibilities?',
    'Empirical study of citizen participation in state-organized or civic defense activities, and analysis of the actual enforcement of training and qualification requirements.',
    'If the civic duty is largely rhetorical, the constraint leans more towards pure individual right (Rope) or even extraction (Snare) if regulations are burdensome without clear civic benefit. If genuine, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_enforcement_genuineness, empirical, 'The extent to which the civic duty aspect of armed citizenship is genuinely operative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1791, 0.3).
narrative_ontology:measurement(seco_be_t1850, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(seco_be_t1950, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement(seco_su_t1850, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1850, 0.22).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1900, 0.25).
narrative_ontology:measurement(seco_su_t1950, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2000, 0.29).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment kernel, each with different structural properties. This civic republican reading emphasizes the right as a civic duty for self-governance, distinct from purely individual or state-centered interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
