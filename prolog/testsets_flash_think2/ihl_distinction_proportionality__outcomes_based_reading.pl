% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: IHL Outcomes-Based Compliance for Autonomous Systems
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'outcomes-based' reading of International
 *   Humanitarian Law (IHL) regarding autonomous weapons systems (AWS). It
 *   asserts that IHL obligations (distinction and proportionality) are
 *   satisfied if AWS demonstrably achieve performance equal to or exceeding
 *   human operators, adhering to a technology-neutral interpretation where
 *   law governs outcomes, not means. This reading provides a legal
 *   justification for the development and deployment of AWS, but it is highly
 *   contested by other interpretations of IHL.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.45).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.55).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Outcomes-Based Compliance for Autonomous Systems").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, 'adc31dd1-580c-45b8-938d-9f4e2e2661cb').
narrative_ontology:cs_kernel_codification('adc31dd1-580c-45b8-938d-9f4e2e2661cb', formalized).
narrative_ontology:cs_authority_grounding('adc31dd1-580c-45b8-938d-9f4e2e2661cb', lineage).
narrative_ontology:cs_interpretation_layer_present('adc31dd1-580c-45b8-938d-9f4e2e2661cb').
narrative_ontology:cs_reading_relation('adc31dd1-580c-45b8-938d-9f4e2e2661cb', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('adc31dd1-580c-45b8-938d-9f4e2e2661cb', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_axiom('adc31dd1-580c-45b8-938d-9f4e2e2661cb', foundational, ihl_is_technology_neutral).
narrative_ontology:cs_axiom_status(ihl_is_technology_neutral, holdable).
narrative_ontology:cs_axiom_grounding('adc31dd1-580c-45b8-938d-9f4e2e2661cb', ihl_is_technology_neutral, conventional).
narrative_ontology:cs_axiom('adc31dd1-580c-45b8-938d-9f4e2e2661cb', foundational, measurable_performance_is_sufficient_for_compliance).
narrative_ontology:cs_axiom_status(measurable_performance_is_sufficient_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('adc31dd1-580c-45b8-938d-9f4e2e2661cb', measurable_performance_is_sufficient_for_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('adc31dd1-580c-45b8-938d-9f4e2e2661cb', traditional_ihl_principles).
narrative_ontology:cs_drift_state('adc31dd1-580c-45b8-938d-9f4e2e2661cb', contemporary_laws_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('adc31dd1-580c-45b8-938d-9f4e2e2661cb', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_forces).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, proponents_of_autonomous_systems).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_custodians_traditional_interpretation).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_at_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to integrate autonomous systems for operational advantage and reduced risk to own personnel, while maintaining a claim of IHL compliance. This reading provides a legal pathway for such integration.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_forces, agenda_setter,
    institutional, generational, constrained, global).

% Develop and sell autonomous weapons systems. This reading opens a significant market by providing a framework for their lawful deployment, shifting focus from human-in-the-loop requirements to technical performance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Advocate for the ethical and strategic benefits of autonomous systems, emphasizing their potential for precision and reduced collateral damage. This reading aligns with their technology-neutral stance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, proponents_of_autonomous_systems, beneficiary,
    powerful, biographical, mobile, global).

% Responsible for interpreting and upholding International Humanitarian Law. This reading challenges their traditional emphasis on human moral judgment and interpretive authority, shifting it towards technical metrics.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_custodians_traditional_interpretation, payer,
    institutional, generational, constrained, global).

% Bear the ultimate risk of harm if autonomous systems fail to achieve or maintain the required distinction and proportionality performance in real-world combat scenarios, or if the metrics themselves are insufficient.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_at_risk, payer,
    powerless, immediate, trapped, local).

% Oppose the outcomes-based approach, arguing it risks dehumanizing warfare and eroding accountability. They advocate for stronger human control or outright prohibition of autonomous weapons.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, human_rights_advocates, payer,
    organized, biographical, constrained, global).

% Analyze the legal and ethical implications of this reading, contributing to the academic and policy debate without direct enforcement power.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to develop and deploy autonomous weapons systems (AWS) while claiming compliance with IHL, by focusing on measurable performance outcomes (distinction and proportionality) rather than human intervention.
% TRANSFER_FUNCTION: Transfers the burden of IHL compliance from irreducible human moral judgment in the loop to technical performance metrics, potentially transferring risk from military personnel to civilian populations and interpretive authority from IHL custodians to technical experts.
% ABSENT_VOICES: Those who categorically oppose autonomous weapons systems on moral or ethical grounds, or who believe human judgment is irreplaceable in lethal force decisions, are marginalized by an outcomes-based framework that prioritizes technical performance.
% DISAPPEARANCE_RATIONALE: If this outcomes-based reading vanished overnight, the legal pathway for autonomous weapons development would become much narrower or disappear entirely, forcing states to either halt development or pursue it outside a claimed IHL-compliant framework. The debate would revert to human agency or categorical prohibition, significantly reorganizing military R&D and international legal discourse.
% FOUNDING_PROBLEM: The challenge of integrating rapidly advancing autonomous weapons technology into existing IHL frameworks, which were designed for human-operated systems, while maintaining military effectiveness and a claim of legal compliance.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and defense technologists attest to the ongoing problem of legal clarity for AWS. Human rights organizations and some legal scholars contest the framing of this as a 'problem to be solved' by technology-neutrality, viewing it instead as a problem of delegating moral responsibility.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).
:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the shift in interpretive authority from traditional IHL custodians to technical experts and the potential for increased risk to civilian populations if performance metrics are insufficient. Suppression (0.55) is also moderate, as this reading actively pushes back against and marginalizes alternative interpretations that emphasize human agency or categorical prohibition. The theater ratio (0.15) is low because the core claim rests on demonstrable technical performance, implying a functional rather than performative justification. Accessibility collapse (0.60) is moderate as it makes arguments based on categorical prohibition or irreducible human judgment less accessible within this framework. Resistance (0.60) is high due to strong opposition from human rights advocates and some legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading view it as a pragmatic and necessary evolution of IHL to accommodate new technologies, potentially leading to more precise and humane warfare. Opponents, however, perceive it as an erosion of fundamental IHL principles, a dangerous delegation of moral responsibility, and a potential increase in risk for civilians. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Military forces and defense contractors are clear beneficiaries, gaining legal clarity and market opportunities for AWS. Proponents of autonomous systems also benefit from a framework that aligns with their technological optimism. IHL custodians (traditional interpretation) are payers, as their interpretive authority is challenged and the focus shifts from human judgment to technical metrics. Civilian populations are also payers, bearing the ultimate risk if AWS performance falls short. Human rights advocates are payers, as their concerns about human control and accountability are suppressed by this outcomes-based approach.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved here; the constraint is actively contested. The founding problem (integrating AWS into IHL) is still live, but its proposed solution (outcomes-based compliance) is itself the subject of intense debate, indicating that the constraint's mandate is far from settled or atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validity_of_performance_metrics,
    'Can autonomous systems truly achieve and reliably demonstrate ''equal to or exceeding human operators'' performance in complex, dynamic combat environments, and can such performance be adequately measured and verified?',
    'Extensive, independent empirical testing and validation of AWS in realistic combat simulations and real-world scenarios, with transparent reporting and peer review.',
    'If performance cannot be reliably demonstrated, the empirical grounding of this reading collapses, strengthening arguments for human agency or categorical prohibition. If it can, the outcomes-based approach gains significant legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validity_of_performance_metrics, empirical, 'The reliability and verifiability of AWS performance metrics.').

omega_variable(
    interpretive_authority_shift,
    'Is the shift from human moral judgment to technical metrics an appropriate evolution of IHL, or does it fundamentally erode IHL''s foundational principles and interpretive authority?',
    'Broad consensus among international legal bodies, states, and civil society on the appropriate balance between technological capability and human moral responsibility in IHL application.',
    'If deemed an appropriate evolution, this reading gains stronger normative grounding. If deemed an erosion, its legitimacy is severely undermined, potentially leading to its rejection or reinterpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_authority_shift, conceptual, 'The normative appropriateness of delegating IHL compliance to technical systems.').

omega_variable(
    martens_clause_compatibility,
    'Does an outcomes-based approach to AWS compliance align with or contradict the Martens Clause principles of humanity and public conscience, particularly regarding the delegation of life-and-death decisions to machines?',
    'Authoritative legal opinions from international courts or a widely accepted interpretive statement by states parties to IHL treaties.',
    'If found to contradict the Martens Clause, this reading would face significant legal and ethical challenges, potentially leading to its invalidation. If found compatible, it would gain stronger normative support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(martens_clause_compatibility, conceptual, 'Compatibility of outcomes-based AWS with Martens Clause principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.11).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'outcomes_based_reading' of the 'ihl_distinction_proportionality' kernel. It is one of three competing interpretations, alongside 'human_agency_reading' and 'categorical_prohibition_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
