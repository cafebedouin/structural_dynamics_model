% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality-Based Vaccine Mandate
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a 'proportionality reading' of the broader
 *   'vaccine_mandate_balance' kernel. It asserts that vaccine mandates are
 *   permissible only when disease severity, transmission risk, and vaccine
 *   safety meet strict proportionality thresholds, and robust exemptions are
 *   provided. The constraint is claimed as a Tangled Rope, reflecting its
 *   dual function of coordinating public health while extracting from
 *   individuals, conditional on its proportionality. The metrics reflect a
 *   scenario where the mandate, while enforced, strives for proportionality,
 *   leading to moderate extraction and suppression, with low theatricality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.45).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.55).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality-Based Vaccine Mandate").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'b6327510-7c8f-41bf-8bed-e4bb1d712c93').
narrative_ontology:cs_kernel_codification('b6327510-7c8f-41bf-8bed-e4bb1d712c93', formalized).
narrative_ontology:cs_authority_grounding('b6327510-7c8f-41bf-8bed-e4bb1d712c93', lineage).
narrative_ontology:cs_interpretation_layer_present('b6327510-7c8f-41bf-8bed-e4bb1d712c93').
narrative_ontology:cs_reading_relation('b6327510-7c8f-41bf-8bed-e4bb1d712c93', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b6327510-7c8f-41bf-8bed-e4bb1d712c93', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('b6327510-7c8f-41bf-8bed-e4bb1d712c93', foundational, proportionality_principle_supreme).
narrative_ontology:cs_axiom_status(proportionality_principle_supreme, holdable).
narrative_ontology:cs_axiom_grounding('b6327510-7c8f-41bf-8bed-e4bb1d712c93', proportionality_principle_supreme, deontological).
narrative_ontology:cs_axiom('b6327510-7c8f-41bf-8bed-e4bb1d712c93', secondary, context_dependent_legitimacy).
narrative_ontology:cs_axiom_status(context_dependent_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b6327510-7c8f-41bf-8bed-e4bb1d712c93', context_dependent_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('b6327510-7c8f-41bf-8bed-e4bb1d712c93', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('b6327510-7c8f-41bf-8bed-e4bb1d712c93', contemporary_pandemic_response, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6327510-7c8f-41bf-8bed-e4bb1d712c93', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_seeking_exemptions).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, unvaccinated_individuals).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, public_health_ethics_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, constitutional_proportionality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing public health measures, including vaccine mandates, guided by scientific evidence and legal frameworks. They aim to protect the population but must navigate legal and ethical constraints, ensuring mandates meet proportionality thresholds.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced disease transmission due to mandates, as they face higher risks of severe illness or death. Their health and safety are directly improved by effective public health measures that are proportionally applied.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Bear the direct costs of mandates, such as restrictions on access to certain spaces or employment. They may face social pressure or economic penalties for non-compliance, even if the mandate is deemed proportional by authorities.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Seek to avoid vaccination due to medical, religious, or philosophical reasons. Even with robust exemption processes, they face administrative burdens, scrutiny, and potential denial, leading to significant personal costs and a feeling of coercion.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_seeking_exemptions, payer,
    powerless, immediate, constrained, local).

% Adjudicate challenges to vaccine mandates, assessing their legality and adherence to constitutional principles like proportionality and individual rights. Their rulings shape the application and enforcement of the constraint, often acting as a check on executive power.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Argue for individual autonomy and against state overreach in public health matters. While they may engage in litigation, their core arguments often challenge the very premise of mandates, even proportional ones, and they are often outside the direct decision-making process of public health authorities.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, civil_liberties_advocates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate disease transmission and protect public health, by setting conditions under which individual liberties can be temporarily constrained for the greater good.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy and freedom of movement from unvaccinated individuals to public health authorities and vulnerable populations, in exchange for reduced disease risk.
% ABSENT_VOICES: Those who advocate for absolute bodily autonomy, regardless of public health risk, are often excluded from the core policy-making discussions, their arguments framed as outside the scope of public health necessity. They would argue that no mandate, however proportional, is legitimate.
% DISAPPEARANCE_RATIONALE: If the ability to implement proportionality-based vaccine mandates vanished, public health authorities would lose a critical tool for managing epidemics, leading to potentially higher disease transmission, increased strain on healthcare systems, and greater risk for vulnerable populations. Society would need to find alternative, potentially less effective, means of collective protection.
% FOUNDING_PROBLEM: The challenge of balancing individual liberties with collective public health needs during infectious disease outbreaks, where voluntary measures alone are insufficient to prevent widespread harm.
% FOUNDING_PROBLEM_CORROBORATION: Medical experts, epidemiologists, and public health organizations consistently attest to the ongoing threat of infectious diseases and the necessity of a framework to balance individual rights with collective protection. Constitutional scholars and ethicists corroborate the persistent challenge of applying proportionality in practice, even when the principle is accepted.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) is moderate, reflecting that even a proportional mandate imposes costs on individuals, but these are deemed justified by the public health benefit. Suppression (0.55) is also moderate, as enforcement is active but tempered by the requirement for robust exemptions. The low theater ratio (0.2) indicates that the justification for the mandate is genuinely rooted in public health necessity, not merely a cover for other agendas. The temporal measurements show an initial period of higher extractiveness and suppression, which then decreases and stabilizes as the application of proportionality principles becomes more refined and exemptions clearer.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities perceive this constraint as a necessary and ethical tool for collective protection, a legitimate exercise of state power when proportional. Unvaccinated individuals and those seeking exemptions, however, experience it as a coercive imposition on their bodily autonomy, even if the proportionality is legally affirmed. Constitutional courts act as a mediating observer, attempting to reconcile these divergent perspectives through legal interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are the primary beneficiaries, gaining collective protection and reduced disease risk. Unvaccinated individuals and those seeking exemptions are the targets, bearing the costs of compliance or exclusion. Constitutional courts observe and adjudicate, while civil liberties advocates are excluded from the direct policy-making process, representing a voice that challenges the very premise of mandates.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'What objective criteria define ''strict proportionality thresholds'' for disease severity, transmission risk, and vaccine safety, and how are they applied consistently?',
    'Development of universally accepted, transparent, and empirically grounded metrics and a standardized, independent review process for mandate justification.',
    'Clearer thresholds would reduce contestation and perceived extraction for individuals, potentially lowering the effective suppression required. Ambiguity allows for arbitrary application, increasing perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in defining and applying proportionality thresholds.').

omega_variable(
    robust_exemptions_efficacy,
    'Are the declared ''robust exemptions'' genuinely accessible and effective in mitigating the coercive impact on individuals, or do they function as a performative gesture?',
    'Empirical study of exemption application rates, success rates, and the lived experience of individuals seeking exemptions, including administrative burden and appeal processes.',
    'If exemptions are not robust in practice, the constraint''s effective suppression and extraction are higher than intended, pushing it closer to a Snare for those seeking exit. If effective, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(robust_exemptions_efficacy, empirical, 'Effectiveness and accessibility of exemption processes.').

omega_variable(
    kernel_reading_bodily_autonomy_primary,
    'How would the classification of this constraint change if the ''bodily_autonomy_primary'' reading of the vaccine_mandate_balance kernel were adopted?',
    'By analyzing the ''bodily_autonomy_primary'' constraint story, which would likely classify any mandate as a Snare due to its absolute rejection of compelled medical intervention.',
    'This constraint would be reclassified as a Snare, with significantly higher extractiveness and suppression, as its core premise (conditional mandates) would be deemed illegitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_bodily_autonomy_primary, conceptual, 'Impact of adopting the ''bodily_autonomy_primary'' kernel reading.').

omega_variable(
    kernel_reading_public_health_primary,
    'How would the classification of this constraint change if the ''public_health_primary'' reading of the vaccine_mandate_balance kernel were adopted?',
    'By analyzing the ''public_health_primary'' constraint story, which would likely classify mandates as a Rope or Scaffold, with lower perceived extraction due to the prioritization of collective benefit.',
    'This constraint would be reclassified as a Rope or Scaffold, with lower extractiveness and suppression, as the conditions for mandates would be less stringent, and the collective benefit would be seen as overriding individual costs more readily.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_public_health_primary, conceptual, 'Impact of adopting the ''public_health_primary'' kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__proportionality_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__proportionality_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__proportionality_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
