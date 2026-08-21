% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary Right (Mandate Legitimacy Scope Reading)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint instantiates the 'bodily_autonomy_primary' reading of the
 *   'mandate_legitimacy_scope' kernel. It asserts that individual bodily
 *   integrity is a fundamental right that cannot be overridden by claims of
 *   collective benefit, making any medical intervention without informed
 *   consent a violation. Sibling readings include 'public_health_primary' and
 *   'proportionality_reading'. From this perspective, medical mandates are
 *   inherently extractive and suppressive, regardless of their stated public
 *   health goals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.9).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.9).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Bodily Autonomy as Primary Right (Mandate Legitimacy Scope Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'ee06c820-4f9e-4168-9d3f-e04b824fab01').
narrative_ontology:cs_kernel_codification('ee06c820-4f9e-4168-9d3f-e04b824fab01', formalized).
narrative_ontology:cs_authority_grounding('ee06c820-4f9e-4168-9d3f-e04b824fab01', lineage).
narrative_ontology:cs_interpretation_layer_present('ee06c820-4f9e-4168-9d3f-e04b824fab01').
narrative_ontology:cs_reading_relation('ee06c820-4f9e-4168-9d3f-e04b824fab01', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('ee06c820-4f9e-4168-9d3f-e04b824fab01', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('ee06c820-4f9e-4168-9d3f-e04b824fab01', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ee06c820-4f9e-4168-9d3f-e04b824fab01', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('ee06c820-4f9e-4168-9d3f-e04b824fab01', foundational, consent_is_non_negotiable).
narrative_ontology:cs_axiom_status(consent_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ee06c820-4f9e-4168-9d3f-e04b824fab01', consent_is_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('ee06c820-4f9e-4168-9d3f-e04b824fab01', absolute_bodily_sovereignty).
narrative_ontology:cs_drift_state('ee06c820-4f9e-4168-9d3f-e04b824fab01', contemporary_public_health_crises, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ee06c820-4f9e-4168-9d3f-e04b824fab01', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who face severe penalties (e.g., job loss, exclusion from public life) for refusing medical interventions, despite their fundamental right to bodily integrity. Their consent is effectively coerced.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced, payer,
    powerless, immediate, trapped, national).

% Government bodies that issue and enforce medical mandates, claiming justification in collective health. From this reading's perspective, they are the agents of rights violation, even if they perceive their actions as beneficial.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Healthcare providers who are tasked with administering or enforcing mandates. They may face ethical dilemmas between professional duties, public health directives, and individual patient autonomy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_professionals, agenda_setter,
    organized, biographical, constrained, local).

% Advocates and groups who prioritize collective health outcomes and support mandates. From this reading's perspective, their arguments for collective benefit are morally irrelevant when fundamental individual rights are at stake.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, proponents_of_collective_benefit, excluded,
    organized, biographical, mobile, global).

% Legal scholars, civil liberties organizations, and activists who defend individual rights, including bodily autonomy, against state overreach. They analyze and challenge mandates based on fundamental legal and ethical principles.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, constitutional_rights_advocates, observer,
    analytical, generational, analytical, national).

narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, there is no legitimate coordination function that justifies violating bodily autonomy. The 'coordination' achieved through mandates is coercive and constitutes a rights violation.
% TRANSFER_FUNCTION: Transfers individual control over one's body and medical decisions to the state, ostensibly for the purpose of collective health, but fundamentally violating individual sovereignty.
% ABSENT_VOICES: Those who advocate for a purely utilitarian calculus where collective benefit always overrides individual rights are absent from this reading's core premise. Also, those who seek a proportional balance are excluded by this reading's absolute stance.
% DISAPPEARANCE_RATIONALE: If the principle of primary bodily autonomy were universally upheld and enforced, all medical mandates would be deemed illegitimate. Public health strategies would be forced to rely solely on voluntary measures, education, and less restrictive alternatives, fundamentally altering the state's role in health governance and individual medical decisions.
% FOUNDING_PROBLEM: The historical problem of state overreach, medical paternalism, and the violation of individual rights through non-consensual medical interventions or coerced participation in public health measures.
% FOUNDING_PROBLEM_CORROBORATION: Legal precedents establishing bodily integrity (e.g., Nuremberg Code, informed consent doctrines), international human rights declarations, and historical accounts of medical abuses (e.g., forced sterilizations, unethical experiments) corroborate the ongoing relevance of this founding problem. Independent legal scholars and human rights organizations consistently attest to its live status.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.9) because any non-consensual medical intervention is viewed as a fundamental violation of rights, extracting bodily autonomy. Suppression is also high (0.85) as mandates rely on coercion (e.g., penalties, exclusion) to enforce compliance, effectively removing the option of refusal. Theater ratio is low (0.1) because the violation is direct and functional; there is little performative maintenance masking a degraded function. The metrics reflect the direct impact of mandates on individual rights, as interpreted by this reading.
 *
 * PERSPECTIVAL GAP:
 *   The state and medical professionals, operating under a 'public_health_primary' or 'proportionality_reading', would perceive their actions as legitimate coordination for collective welfare. However, from the 'bodily_autonomy_primary' reading, these same actions constitute a snare, directly violating fundamental individual rights. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals subject to mandates (e.g., 'unvaccinated_coerced') are the direct targets and victims, experiencing full extraction. State public health authorities are the agenda-setters, wielding institutional power to enforce the mandates. From this reading's perspective, there are no legitimate beneficiaries of a rights violation, even if other readings claim collective benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent claim, or one reading of the ''mandate_legitimacy_scope'' kernel?',
    'Analysis of the structural differences in beneficiary/victim sets and extraction levels across different interpretations of mandate legitimacy.',
    'If it is a reading, its classification is context-dependent within the kernel''s contest; if independent, it stands alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''bodily_autonomy_primary'' reading of the ''mandate_legitimacy_scope'' kernel.').

omega_variable(
    public_health_primary_impact,
    'How would the classification of state mandates change if the ''public_health_primary'' reading of the kernel were adopted?',
    'Re-evaluate the constraint''s metrics and stakeholder roles from the ''public_health_primary'' perspective, where collective benefit is paramount.',
    'The victim set would likely shrink or disappear, and the extractiveness (ε) would significantly decrease, potentially reclassifying the constraint as a Rope or Tangled Rope, as the state''s actions would be seen as legitimate coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_health_primary_impact, conceptual, 'Impact of adopting the ''public_health_primary'' sibling reading.').

omega_variable(
    proportionality_reading_impact,
    'How would the classification of state mandates change if the ''proportionality_reading'' of the kernel were adopted?',
    'Re-evaluate the constraint''s metrics and stakeholder roles from the ''proportionality_reading'' perspective, where mandates are legitimate only if proportional to the threat and least restrictive.',
    'The victim set would become conditional on the proportionality assessment, and extractiveness (ε) would decrease for mandates deemed proportional, potentially reclassifying the constraint as a Scaffold or Tangled Rope, reflecting conditional legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_reading_impact, conceptual, 'Impact of adopting the ''proportionality_reading'' sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t1, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 1, 0.1).
narrative_ontology:measurement(mand_tr_t2, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 2, 0.1).
narrative_ontology:measurement(mand_tr_t3, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 3, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(mand_be_t1, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 1, 0.89).
narrative_ontology:measurement(mand_be_t2, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 2, 0.9).
narrative_ontology:measurement(mand_be_t3, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 3, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.83).
narrative_ontology:measurement(mand_su_t1, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 1, 0.84).
narrative_ontology:measurement(mand_su_t2, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 2, 0.85).
narrative_ontology:measurement(mand_su_t3, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 3, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
