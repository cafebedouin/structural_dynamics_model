% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Dignified Death: Autonomy as Primary Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'autonomy_primary' reading of the
 *   'dignified_death' kernel. It describes a situation where dignity is
 *   understood to reside in self-determination, granting the suffering
 *   individual final authority over the timing and method of their death.
 *   However, this ideal is often constrained by state prohibitions and
 *   medical gatekeeping, leading to a 'tangled_rope' dynamic where the
 *   coordination function (facilitating autonomous choice) is entangled with
 *   extraction (prolonging suffering against will). The metrics reflect the
 *   gap between the claimed ideal and the operational reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.55).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.7).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Dignified Death: Autonomy as Primary Authority").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, 'd99b78de-7c4e-48a3-860d-bee02f92740d').
narrative_ontology:cs_kernel_codification('d99b78de-7c4e-48a3-860d-bee02f92740d', formalized).
narrative_ontology:cs_authority_grounding('d99b78de-7c4e-48a3-860d-bee02f92740d', practice).
narrative_ontology:cs_interpretation_layer_present('d99b78de-7c4e-48a3-860d-bee02f92740d').
narrative_ontology:cs_reading_relation('d99b78de-7c4e-48a3-860d-bee02f92740d', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('d99b78de-7c4e-48a3-860d-bee02f92740d', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('d99b78de-7c4e-48a3-860d-bee02f92740d', foundational, individual_sovereignty_over_body).
narrative_ontology:cs_axiom_status(individual_sovereignty_over_body, holdable).
narrative_ontology:cs_axiom_grounding('d99b78de-7c4e-48a3-860d-bee02f92740d', individual_sovereignty_over_body, deontological).
narrative_ontology:cs_axiom('d99b78de-7c4e-48a3-860d-bee02f92740d', secondary, suffering_as_justification_for_exit).
narrative_ontology:cs_axiom_status(suffering_as_justification_for_exit, holdable).
narrative_ontology:cs_axiom_grounding('d99b78de-7c4e-48a3-860d-bee02f92740d', suffering_as_justification_for_exit, empirically_contingent).
narrative_ontology:cs_reference_frame('d99b78de-7c4e-48a3-860d-bee02f92740d', enlightenment_individual_rights).
narrative_ontology:cs_drift_state('d99b78de-7c4e-48a3-860d-bee02f92740d', contemporary_bioethics_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d99b78de-7c4e-48a3-860d-bee02f92740d', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_agent).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_professionals).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, state_legal_framework).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individuals_denied_exit).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, advocates_for_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals experiencing intractable suffering whose requests for self-determined death are denied due to legal prohibitions or medical gatekeeping. They bear the cost of prolonged suffering against their will.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_individuals_denied_exit, payer,
    powerless, immediate, trapped, national).

% Represents the ideal of the self-determining individual whose will should be paramount in end-of-life decisions. This agent benefits from the theoretical recognition of autonomy, even if practical application is limited.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomous_agent, beneficiary,
    moderate, biographical, mobile, global).

% Physicians and healthcare systems who act as gatekeepers, interpreting and applying legal and ethical guidelines for end-of-life care. They benefit from maintaining control over the process, ensuring professional standards and legal compliance.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_professionals, agenda_setter,
    institutional, biographical, constrained, national).

% The legal and regulatory apparatus that prohibits or strictly controls self-determined death, often citing public policy, protection of vulnerable populations, or the sanctity of life. It benefits from maintaining its authority and control over life-and-death decisions.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legal_framework, agenda_setter,
    institutional, civilizational, analytical, national).

% Organizations and individuals campaigning for legal and medical reforms to expand access to self-determined death. They bear the costs of advocacy, litigation, and public education against entrenched legal and medical systems.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, advocates_for_autonomy, payer,
    organized, biographical, mobile, national).

% Groups and individuals who believe life has intrinsic value and oppose any form of intentional life-termination. While their views are often reflected in existing laws, they are structurally excluded from the core premise of this 'autonomy_primary' reading.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, sanctity_of_life_proponents, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework where the suffering individual's self-determination is the primary authority in end-of-life decisions, coordinating medical practice and legal permissions around this principle.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority over the timing and method of death from external authorities (state, medical) to the suffering individual, though this transfer is often mediated and constrained by gatekeeping mechanisms.
% ABSENT_VOICES: Those who prioritize the 'sanctity of life' or emphasize 'relational autonomy' (e.g., family, community, collective well-being) are often marginalized or excluded from the core logic of a purely individualistic self-determination framework.
% DISAPPEARANCE_RATIONALE: If the constraint (the norm of individual self-determination as primary authority in end-of-life decisions) vanished, the legal and medical landscape would fundamentally shift. It would likely revert to a more paternalistic or communitarian approach, or lead to a chaotic lack of guidance, as the central organizing principle for end-of-life care would be gone.
% FOUNDING_PROBLEM: To address the historical paternalism in medicine and law that denied individuals agency over their own bodies and lives, particularly in the face of intractable suffering and the desire for a dignified end.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, bioethicists, and legal scholars outside of state or medical institutions corroborate the ongoing struggle for individual self-determination in end-of-life decisions, highlighting persistent legal and medical barriers.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the cost borne by individuals whose autonomous choices are denied or delayed. Suppression (0.70) is high due to legal prohibitions and the medical system's control over end-of-life options. The theater ratio (0.40) indicates that while genuine care is provided, a significant portion of the system's activity involves maintaining the appearance of upholding life while denying self-determination. The increasing trend in all metrics over the interval reflects the growing tension as the demand for autonomy meets persistent institutional resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the suffering individual, the constraint is highly extractive and suppressive, denying their fundamental autonomy. From the perspective of the medical and legal institutions, it is a necessary coordination mechanism to ensure ethical practice and public safety. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'suffering_individuals_denied_exit' and 'advocates_for_autonomy' are clear targets (payers), bearing the costs of denied self-determination and advocacy. The 'autonomous_agent' (as an ideal) is a beneficiary. 'Medical_professionals' and the 'state_legal_framework' are agenda-setters and beneficiaries, as they maintain control and authority over the process, even while ostensibly serving autonomy. 'Sanctity_of_life_proponents' are excluded, as their core premise is outside this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_vulnerability_protection,
    'Is the emphasis on individual autonomy adequately protecting vulnerable individuals from coercion or undue influence in end-of-life decisions?',
    'Empirical studies on the incidence of coercion or regret in jurisdictions with liberalized self-determined death laws, compared to those with stricter regulations.',
    'If vulnerability is systematically exploited, the effective extractiveness of the constraint (even when framed as autonomy-enhancing) would be higher for vulnerable populations, potentially shifting the classification towards a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_vulnerability_protection, empirical, 'Ambiguity regarding the balance between individual autonomy and the protection of vulnerable persons.').

omega_variable(
    medical_gatekeeping_function,
    'Is medical gatekeeping a necessary safeguard to ensure informed, voluntary, and appropriate self-determined death, or does it primarily function as an extractive mechanism that limits access to autonomy?',
    'Analysis of medical protocols and their outcomes: if protocols primarily serve to delay or deny access without clear medical justification, it points to extraction. If they genuinely ensure patient safety and informed consent, it supports a coordination function.',
    'If primarily extractive, the ''tangled_rope'' classification leans more heavily towards ''snare'' due to higher effective extraction from individuals seeking self-determination. If primarily safeguarding, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_gatekeeping_function, conceptual, 'The dual nature of medical gatekeeping as both a safeguard and a potential barrier to self-determination.').

omega_variable(
    conceptual_boundary_of_dignity,
    'Does dignity inherently reside solely in self-determination, or is it also tied to other values such as the intrinsic value of life or relational well-being?',
    'Philosophical and ethical discourse, cross-cultural comparative analysis of dignity concepts, and legal precedent in different jurisdictions. This is a conceptual framing choice.',
    'If dignity is found to be multi-faceted, this ''autonomy_primary'' reading would be seen as incomplete, and its classification might shift if other dimensions of dignity (e.g., relational) are prioritized, potentially leading to a different constraint type for those alternative framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_boundary_of_dignity, conceptual, 'The foundational definition of dignity itself is contested, impacting the normative force of this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dign_tr_t4, dignified_death__autonomy_primary, theater_ratio, 4, 0.33).
narrative_ontology:measurement(dign_tr_t8, dignified_death__autonomy_primary, theater_ratio, 8, 0.36).
narrative_ontology:measurement(dign_tr_t12, dignified_death__autonomy_primary, theater_ratio, 12, 0.38).
narrative_ontology:measurement(dign_tr_t16, dignified_death__autonomy_primary, theater_ratio, 16, 0.39).
narrative_ontology:measurement(dign_tr_t20, dignified_death__autonomy_primary, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dign_be_t4, dignified_death__autonomy_primary, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(dign_be_t8, dignified_death__autonomy_primary, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(dign_be_t12, dignified_death__autonomy_primary, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(dign_be_t16, dignified_death__autonomy_primary, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(dign_be_t20, dignified_death__autonomy_primary, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(dign_su_t4, dignified_death__autonomy_primary, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(dign_su_t8, dignified_death__autonomy_primary, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(dign_su_t12, dignified_death__autonomy_primary, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(dign_su_t16, dignified_death__autonomy_primary, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(dign_su_t20, dignified_death__autonomy_primary, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, identity_coordination).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dignified_death' kernel, emphasizing individual autonomy. It is linked to sibling readings that offer alternative framings of dignity in end-of-life contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
