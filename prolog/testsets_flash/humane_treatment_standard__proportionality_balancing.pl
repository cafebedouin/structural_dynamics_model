% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality balancing' reading of
 *   Common Article 3 of the Geneva Conventions, which requires states to
 *   balance detainee dignity against security needs. It rejects both absolute
 *   prohibitions on certain interrogation methods and unlimited state
 *   discretion, instead mandating a case-by-case judicial review of treatment
 *   permissibility. This reading positions courts as gatekeepers, imposing
 *   moderate constraints on interrogators through procedural safeguards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.45).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.6).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, 'bdac70ba-c1cd-4922-b197-9edf6685d548').
narrative_ontology:cs_kernel_codification('bdac70ba-c1cd-4922-b197-9edf6685d548', fixed_text).
narrative_ontology:cs_authority_grounding('bdac70ba-c1cd-4922-b197-9edf6685d548', lineage).
narrative_ontology:cs_interpretation_layer_present('bdac70ba-c1cd-4922-b197-9edf6685d548').
narrative_ontology:cs_reading_relation('bdac70ba-c1cd-4922-b197-9edf6685d548', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('bdac70ba-c1cd-4922-b197-9edf6685d548', humane_treatment_standard__contextual_necessity, coexists_with).
narrative_ontology:cs_axiom('bdac70ba-c1cd-4922-b197-9edf6685d548', foundational, dignity_and_security_are_reconcilable).
narrative_ontology:cs_axiom_status(dignity_and_security_are_reconcilable, holdable).
narrative_ontology:cs_axiom_grounding('bdac70ba-c1cd-4922-b197-9edf6685d548', dignity_and_security_are_reconcilable, deontological).
narrative_ontology:cs_axiom('bdac70ba-c1cd-4922-b197-9edf6685d548', foundational, judicial_review_is_necessary_safeguard).
narrative_ontology:cs_axiom_status(judicial_review_is_necessary_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('bdac70ba-c1cd-4922-b197-9edf6685d548', judicial_review_is_necessary_safeguard, conventional).
narrative_ontology:cs_reference_frame('bdac70ba-c1cd-4922-b197-9edf6685d548', post_geneva_conventions_era).
narrative_ontology:cs_drift_state('bdac70ba-c1cd-4922-b197-9edf6685d548', post_9_11_security_paradigm, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bdac70ba-c1cd-4922-b197-9edf6685d548', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detainees).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, international_legal_framework).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, state_security_agencies).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, interrogators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must justify interrogation methods and detention conditions against a proportionality standard, incurring legal and operational costs. They seek maximum flexibility for security operations but are constrained by judicial review and international scrutiny.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_security_agencies, payer,
    institutional, biographical, constrained, national).

% Operate under guidelines that require balancing security needs with detainee dignity, facing potential legal repercussions for violations. They must document their methods and justify their choices, which can be seen as an impediment to intelligence gathering.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, interrogators, payer,
    moderate, immediate, constrained, local).

% Benefit from a legal standard that requires their dignity to be weighed against security imperatives, offering a degree of protection against arbitrary or cruel treatment. Their ability to invoke this protection is often mediated by external actors.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees, beneficiary,
    powerless, immediate, trapped, local).

% The framework itself is strengthened by the application of a nuanced standard that seeks to reconcile competing values, demonstrating the adaptability and moral force of international law. It 'benefits' by maintaining its legitimacy and coherence.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_legal_framework, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(humane_treatment_standard__proportionality_balancing, international_legal_framework).

% Actively push for the interpretation and enforcement of Common Article 3 through judicial and political channels, advocating for the rights of detainees and challenging practices that violate the proportionality standard. They shape the discourse and legal application.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Serve as the primary gatekeepers, adjudicating cases involving detainee treatment and interpreting the proportionality standard. Their rulings set precedents that guide security agencies and interrogators, balancing state interests with individual rights.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, domestic_courts, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to conduct security operations while adhering to minimum standards of humane treatment for detainees, balancing competing imperatives through a judicialized process.
% TRANSFER_FUNCTION: Transfers some discretion from security agencies to judicial bodies, and some operational flexibility from interrogators to detainees' rights, in exchange for maintaining the legitimacy of state action under international law.
% ABSENT_VOICES: Detainees themselves, often unable to directly advocate for their rights due to detention conditions or lack of legal access, would argue for a stronger emphasis on dignity and less on security discretion. Victims of terrorism, who might advocate for more aggressive interrogation tactics, are also often excluded from the direct legal balancing process.
% DISAPPEARANCE_RATIONALE: If this proportionality balancing standard vanished, states would likely revert to more absolute positions (either unlimited discretion or strict prohibition), leading to increased legal challenges, human rights abuses, or a breakdown in international consensus on detention standards. The current equilibrium, however imperfect, depends on this standard.
% FOUNDING_PROBLEM: To establish a minimum, non-derogable standard of humane treatment for persons not taking an active part in hostilities, even in non-international armed conflicts, preventing abuses while acknowledging the realities of conflict.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and a broad consensus among states (even those that challenge specific applications) corroborate that the problem of balancing security and humanity in conflict remains live. The International Committee of the Red Cross (ICRC) consistently attests to the ongoing need for such standards.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).
:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because security agencies and interrogators bear the cost of justifying their actions and accepting judicial oversight, which limits their operational freedom. Suppression (0.6) is significant as states must actively enforce this standard against internal pressures for more aggressive tactics, and detainees' ability to resist is highly constrained. Theater ratio (0.2) is relatively low, as the judicial review process is generally functional, though there can be performative compliance. The temporal measurements reflect a period of increased pressure on the standard post-9/11 (around 2005), followed by some re-stabilization.
 *
 * PERSPECTIVAL GAP:
 *   State security agencies and interrogators experience this as an extractive constraint, limiting their operational effectiveness and imposing compliance costs. Detainees and human rights advocates, however, view it as a crucial protective mechanism, a beneficiary structure that prevents worse outcomes. Domestic courts, as agenda-setters, navigate this tension, aiming for a balanced application.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are clear beneficiaries (d=0.0) as the standard directly protects their rights. The international legal framework also benefits (d=0.0) by maintaining its legitimacy. State security agencies and interrogators are targets (d=1.0) as the constraint limits their discretion and imposes costs. Domestic courts and human rights advocates act as agenda-setters, influencing the application and enforcement of the standard, with their directionality closer to symmetric (d=0.5) as they both uphold the standard and bear the costs of its defense.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate remains live, as the core problem of balancing security and humane treatment in conflict persists. The proportionality balancing reading prevents mislabeling genuine coordination (the state's need for security) as pure extraction, while also preventing the state from claiming its actions are purely coordinative when they impose significant costs on detainees. It acknowledges the inherent tension and seeks to manage it through a structured process, rather than allowing either extreme to dominate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_independence_and_capacity,
    'To what extent do domestic courts possess the independence and capacity to effectively act as gatekeepers for the proportionality balancing standard, especially in states with weak rule of law or under severe security threats?',
    'Empirical studies of judicial review outcomes in diverse national contexts, assessing the frequency of challenges to state security practices and the proportion of rulings in favor of detainee rights.',
    'If judicial independence/capacity is low, the constraint''s effective suppression and extractiveness on detainees would be higher, as the primary safeguard is weakened, potentially reclassifying it closer to a Snare for detainees. If high, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_and_capacity, empirical, 'The actual effectiveness of judicial oversight in enforcing the proportionality standard.').

omega_variable(
    balancing_criteria_objectivity,
    'Are the criteria used for ''proportionality balancing'' sufficiently objective and transparent, or do they allow for subjective interpretations that consistently favor security needs over detainee dignity?',
    'Analysis of legal precedents and judicial reasoning across multiple jurisdictions to identify consistent application of balancing factors versus ad hoc justifications. Expert legal commentary on the clarity and predictability of the standard''s application.',
    'If criteria are highly subjective and consistently biased, the constraint''s extractiveness on detainees would be higher, as the ''balancing'' becomes a cover for discretion. This would push the classification closer to a Snare for detainees, as the coordination function is undermined by biased application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_criteria_objectivity, conceptual, 'The objectivity and transparency of proportionality balancing criteria.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''proportionality balancing'' reading of Common Article 3, or is it a ''contextual necessity'' reading dressed up with procedural safeguards?',
    'Analysis of judicial outcomes: if courts consistently defer to security claims without rigorous balancing, it suggests a drift towards ''contextual necessity''. If they actively weigh and sometimes reject security claims, it supports ''proportionality balancing''.',
    'If it''s effectively ''contextual necessity'', the extractiveness and suppression would be higher for detainees, and the constraint would be reclassified closer to a Snare for them, as the ''balancing'' becomes largely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Distinguishing proportionality balancing from contextual necessity in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__proportionality_balancing, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(huma_tr_t1970, humane_treatment_standard__proportionality_balancing, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(huma_tr_t1990, humane_treatment_standard__proportionality_balancing, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(huma_tr_t2005, humane_treatment_standard__proportionality_balancing, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(huma_tr_t2015, humane_treatment_standard__proportionality_balancing, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__proportionality_balancing, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement(huma_be_t1970, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(huma_be_t1990, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(huma_be_t2005, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(huma_be_t2015, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(huma_su_t1970, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(huma_su_t1990, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(huma_su_t2005, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(huma_su_t2015, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'humane_treatment_standard' kernel. Its proportionality balancing approach influences and coexists with other readings, such as 'absolute_prohibition' and 'contextual_necessity', by offering a middle ground that seeks to reconcile competing values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
