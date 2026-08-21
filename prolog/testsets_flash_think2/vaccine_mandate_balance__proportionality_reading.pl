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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality-Conditioned Vaccine Mandate Framework
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint describes a framework for vaccine mandates that are
 *   permissible only when disease severity, transmission risk, and vaccine
 *   safety meet strict proportionality thresholds, and robust exemptions are
 *   provided. It is a specific reading of the broader
 *   'vaccine_mandate_balance' kernel, aiming to balance public health
 *   imperatives with individual liberties. The framework acknowledges the
 *   extractive nature of mandates but seeks to limit it through conditional
 *   application and safeguards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.65).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.7).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality-Conditioned Vaccine Mandate Framework").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '8eea6927-d2c2-4338-b241-5445ccd204a9').
narrative_ontology:cs_kernel_codification('8eea6927-d2c2-4338-b241-5445ccd204a9', formalized).
narrative_ontology:cs_authority_grounding('8eea6927-d2c2-4338-b241-5445ccd204a9', lineage).
narrative_ontology:cs_interpretation_layer_present('8eea6927-d2c2-4338-b241-5445ccd204a9').
narrative_ontology:cs_reading_relation('8eea6927-d2c2-4338-b241-5445ccd204a9', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('8eea6927-d2c2-4338-b241-5445ccd204a9', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_axiom('8eea6927-d2c2-4338-b241-5445ccd204a9', foundational, state_intervention_requires_proportionality).
narrative_ontology:cs_axiom_status(state_intervention_requires_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('8eea6927-d2c2-4338-b241-5445ccd204a9', state_intervention_requires_proportionality, conventional).
narrative_ontology:cs_axiom('8eea6927-d2c2-4338-b241-5445ccd204a9', foundational, individual_exemptions_are_fundamental).
narrative_ontology:cs_axiom_status(individual_exemptions_are_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('8eea6927-d2c2-4338-b241-5445ccd204a9', individual_exemptions_are_fundamental, deontological).
narrative_ontology:cs_reference_frame('8eea6927-d2c2-4338-b241-5445ccd204a9', liberal_democratic_public_health_governance).
narrative_ontology:cs_drift_state('8eea6927-d2c2-4338-b241-5445ccd204a9', contemporary_pandemic_response, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8eea6927-d2c2-4338-b241-5445ccd204a9', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, general_public).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_subject_to_mandate).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, businesses_enforcing_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for assessing disease severity, transmission risk, and vaccine safety to determine if proportionality thresholds are met. They design and implement mandate policies and manage exemption processes. They benefit from the framework's legitimacy in achieving public health goals.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Bear the direct burden of compliance with vaccine mandates, including vaccination itself or navigating exemption processes. While the framework aims for proportionality, the mandate still restricts their autonomy and imposes costs, even if justified by public health needs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_subject_to_mandate, payer,
    moderate, biographical, constrained, local).

% Benefit significantly from reduced disease transmission and increased herd immunity, which protects them from severe illness or death. They are often unable to protect themselves through individual action alone and rely on collective measures.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Benefits from the overall reduction in disease burden, healthcare system strain, and societal disruption. They experience a safer environment but also bear indirect costs of mandates and may face social pressure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).

% Are tasked with implementing and enforcing vaccine mandates within their operations, incurring administrative costs, potential legal challenges, and employee relations issues. Their compliance is often compelled by public health directives.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, businesses_enforcing_mandates, payer,
    organized, immediate, constrained, local).

% Monitor the application of proportionality thresholds and the robustness of exemptions, often challenging mandates they deem overly broad or insufficiently protective of individual rights. They provide critical oversight and legal challenges.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Hold a categorical position against any state-compelled medical intervention, regardless of proportionality. Their core premise is fundamentally foreclosed by this reading, which accepts mandates under conditions. They are structurally excluded from the internal logic of this constraint.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a legitimate framework for state intervention in public health crises, balancing collective protection with individual rights by setting clear, context-dependent thresholds for vaccine mandates and ensuring robust exemptions.
% TRANSFER_FUNCTION: Transfers a degree of individual medical autonomy to the collective good of public health, conditional on disease parameters. It also transfers the burden of assessing proportionality and managing exemptions to public health authorities and implementing entities.
% ABSENT_VOICES: Advocates for absolute bodily autonomy are structurally excluded from the conversation within this framework, as their core premise (no compulsion ever) is incompatible with the conditional permissibility of mandates. They would argue that no proportionality threshold can justify state-compelled medical intervention.
% DISAPPEARANCE_RATIONALE: If this proportionality framework vanished, public health responses would either become purely coercive (if the 'public_health_primary' reading became dominant without checks) or entirely ineffective (if 'bodily_autonomy_primary' became dominant, preventing any mandates). This would lead to significant societal reorganization in how infectious disease outbreaks are managed, with either severe rights infringements or uncontrolled epidemics.
% FOUNDING_PROBLEM: The historical and ongoing challenge of reconciling individual liberty with the state's legitimate interest in protecting public health during infectious disease outbreaks, particularly when voluntary measures are insufficient to control spread.
% FOUNDING_PROBLEM_CORROBORATION: This problem is attested by centuries of public health law, constitutional jurisprudence on state police powers, and ethical debates in bioethics. Public health experts, legal scholars, and ethicists from diverse perspectives corroborate the persistent tension between these values, supporting the need for a balancing framework.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderately high because even a proportional mandate imposes a significant burden on individuals, requiring them to undergo a medical procedure or navigate complex exemption processes. Suppression (0.70) is also high, reflecting the coercive power of the state to enforce mandates, though robust exemptions mitigate it slightly. The theater ratio (0.15) is low, as this framework is primarily concerned with the functional application of ethical and legal principles, not performative maintenance. Accessibility collapse (0.60) is moderate; while the option of non-vaccination is removed, the provision for robust exemptions offers an alternative path. Resistance (0.55) is moderate, as even proportional mandates often face opposition from those prioritizing individual autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this framework is a necessary and legitimate tool for coordination and protection. From the perspective of individuals subject to mandates, it is a coercive measure that, even if proportional, extracts from their autonomy. The engine's per-seat classification will reflect this divergence, showing a more extractive classification for individuals and a more coordinative one for public health bodies.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, vulnerable populations, and the general public are beneficiaries, gaining protection and legitimacy for public health interventions. Individuals subject to mandates and businesses enforcing them are payers, bearing the direct costs and burdens. Civil liberties advocates act as observers, scrutinizing the application of the framework. Bodily autonomy advocates are structurally excluded from this reading's core premise, as their categorical rejection of mandates cannot be reconciled with conditional permissibility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How are ''strict proportionality thresholds'' for disease severity, transmission risk, and vaccine safety objectively measured and applied in practice?',
    'Development of standardized, transparent, and independently verifiable metrics and methodologies for assessing proportionality, subject to judicial review and public scrutiny.',
    'If proportionality metrics are vague or inconsistently applied, the framework''s legitimacy erodes, and its effective extractiveness increases, potentially reclassifying it closer to a Snare. Clear, verifiable metrics would reinforce its Tangled Rope classification by ensuring the coordination function is genuinely tied to public health necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Ambiguity in the practical application and measurement of proportionality thresholds.').

omega_variable(
    exemption_robustness_implementation,
    'What constitutes ''robust exemptions'' in practice, and how effectively are they implemented to protect individual rights without undermining public health goals?',
    'Empirical studies on exemption rates, accessibility of exemption processes, and outcomes for individuals seeking exemptions, coupled with legal challenges to overly restrictive exemption policies.',
    'If exemptions are difficult to obtain, narrowly defined, or administratively burdensome, the effective suppression of individual choice increases, pushing the constraint towards a Snare. Genuinely robust and accessible exemptions would reinforce the coordinative aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_implementation, empirical, 'Ambiguity in the practical implementation and effectiveness of robust exemptions.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''proportionality_reading'' the most appropriate framing for vaccine mandates, or does it obscure a more fundamental conflict between ''bodily_autonomy_primary'' and ''public_health_primary''?',
    'Analysis of public discourse, legal arguments, and policy outcomes to determine if the proportionality framework genuinely mediates the conflict or merely serves as a rhetorical bridge over an irreconcilable normative divide.',
    'If the proportionality framework is found to be a weak or disingenuous mediation, the underlying conflict between the sibling readings becomes more salient, potentially leading to a re-evaluation of the constraint''s claimed type as a more purely extractive Snare (if the proportionality is merely cover) or a more purely coordinative Rope (if the conflict is genuinely resolved).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the proportionality framework genuinely mediates the underlying normative conflict or merely papers over it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__proportionality_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__proportionality_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_balance__proportionality_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__proportionality_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.7).


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
