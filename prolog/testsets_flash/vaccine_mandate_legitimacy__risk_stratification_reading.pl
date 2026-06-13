% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Vaccine Mandate Legitimacy: Risk Stratification Reading
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'risk-stratification' reading of vaccine
 *   mandate legitimacy, where mandates are deemed legitimate only if they are
 *   proportionate to an actuarial risk threshold. Blanket mandates are
 *   considered disproportionate, while targeted mandates for high-risk
 *   individuals or settings are permissible. This reading attempts to
 *   navigate between the extremes of absolute bodily autonomy and absolute
 *   public health primacy, seeking a middle ground based on evidence and
 *   proportionality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.6).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.7).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy: Risk Stratification Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '63b4d664-701c-4a8c-b477-ef38b6c9a2ac').
narrative_ontology:cs_kernel_codification('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', formalized).
narrative_ontology:cs_authority_grounding('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', expertise).
narrative_ontology:cs_interpretation_layer_present('63b4d664-701c-4a8c-b477-ef38b6c9a2ac').
narrative_ontology:cs_reading_relation('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_reading_relation('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, influences).
narrative_ontology:cs_axiom('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', foundational, mandates_must_be_proportionate_to_risk).
narrative_ontology:cs_axiom_status(mandates_must_be_proportionate_to_risk, holdable).
narrative_ontology:cs_axiom_grounding('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', mandates_must_be_proportionate_to_risk, empirically_contingent).
narrative_ontology:cs_axiom('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', secondary, blanket_mandates_are_disproportionate).
narrative_ontology:cs_axiom_status(blanket_mandates_are_disproportionate, holdable).
narrative_ontology:cs_axiom_grounding('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', blanket_mandates_are_disproportionate, deontological).
narrative_ontology:cs_reference_frame('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', proportionality_and_evidence_based_governance).
narrative_ontology:cs_drift_state('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('63b4d664-701c-4a8c-b477-ef38b6c9a2ac', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_below_risk_threshold).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, employers_with_blanket_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they seek to implement mandates that are proportionate to risk, balancing individual liberties with collective well-being. They face pressure from both sides of the debate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced disease transmission when mandates are applied to high-risk groups, as their health is disproportionately affected by outbreaks. They have limited direct agency in policy formulation but are the intended protective target.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Bear the burden of mandates (e.g., vaccination requirements for employment or travel) even when their individual risk profile or contribution to transmission is low. They argue for proportionality and individual liberty.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_below_risk_threshold, payer,
    moderate, biographical, constrained, national).

% Implement mandates to comply with public health directives or reduce workplace risk, but face legal challenges and employee resistance when mandates are not risk-stratified. They bear the cost of enforcement and potential litigation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, employers_with_blanket_mandates, payer,
    organized, immediate, constrained, national).

% Analyze the legal and ethical implications of vaccine mandates, particularly concerning proportionality, individual rights, and state power. They provide frameworks for judicial review and policy development.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% Examine the moral justifications for public health interventions, focusing on principles of justice, autonomy, and beneficence. They contribute to the ethical framing of risk-stratified mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, bioethicists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health interventions by establishing a framework for mandates that balances collective protection with individual rights, ensuring interventions are proportionate to actual risk.
% TRANSFER_FUNCTION: Transfers the burden of compliance (e.g., vaccination, testing) from the general population to specific individuals or groups whose actuarial risk profile justifies the intervention, while transferring reduced disease burden to vulnerable populations.
% ABSENT_VOICES: Individuals who are medically exempt but fall outside the narrowly defined risk thresholds for mandate exemption, and who would argue for broader individual accommodations based on their unique health circumstances.
% DISAPPEARANCE_RATIONALE: If this reading of mandate legitimacy vanished, public health policy would likely swing to either absolute bodily autonomy or absolute public health primacy, leading to either widespread disease or disproportionate coercion, respectively. The current balance would be lost.
% FOUNDING_PROBLEM: The challenge of implementing public health measures that effectively control disease spread without infringing excessively on individual liberties, particularly during widespread health crises.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and constitutional law scholars widely attest that balancing individual rights and collective health remains a live and complex problem, especially with emerging infectious diseases. Judicial rulings and legislative debates consistently reflect this ongoing tension.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is moderate-high because even targeted mandates impose costs on individuals, but it's lower than a blanket mandate. Suppression (0.7) is also moderate-high, as active enforcement is required to ensure compliance and prevent free-riding. Theater ratio (0.2) is low, as the justification for mandates is generally tied to real public health goals, though the proportionality argument can be performative if the risk threshold is not rigorously defined. Accessibility collapse (0.4) is moderate, as alternatives (e.g., testing, masking) may exist for some, but not for all situations where mandates apply. Resistance (0.75) is high, reflecting ongoing legal and social challenges to mandates, even when risk-stratified.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities view this as a necessary and balanced approach, while individuals below the risk threshold experience it as an extractive imposition on their autonomy. Vulnerable populations see it as a beneficial coordination mechanism, and employers grapple with the practicalities and costs of implementation and enforcement. Constitutional scholars and bioethicists analyze its theoretical coherence and practical justice.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters, benefiting from a framework that allows them to act. Vulnerable populations are beneficiaries, as their health is protected. Individuals below the risk threshold and employers with blanket mandates are payers, bearing the costs of compliance or enforcement. Constitutional scholars and bioethicists are observers, analyzing the constraint's operation without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading attempts to prevent mandatrophy by tying legitimacy to a live, empirically verifiable problem (actuarial risk). If the risk threshold becomes arbitrary or the problem it addresses ceases to be live, the constraint would lose its justification and risk becoming a piton or snare. The ongoing contestation around the 'founding problem status' (contested) highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_threshold_definition,
    'How is the ''actuarial risk threshold'' precisely defined and measured, and by whom?',
    'Establishment of independent, transparent, and publicly reviewable epidemiological and statistical criteria for risk assessment, with clear governance over data collection and analysis.',
    'If the threshold is arbitrary or politically influenced, the constraint''s legitimacy as a ''risk-stratified'' mandate collapses, increasing its effective extractiveness and suppression for those below the threshold. If robust, it strengthens the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_threshold_definition, empirical, 'Ambiguity in defining the risk threshold for mandate application.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''risk-stratification'' reading, or does it merely serve as a rhetorical bridge between the ''public health primacy'' and ''bodily autonomy primacy'' readings, without a coherent independent structural basis?',
    'Analysis of policy outcomes: if policies consistently reflect a nuanced, data-driven proportionality that satisfies neither extreme, it''s a genuine reading. If it collapses into one of the extremes under pressure, it''s a bridge.',
    'If a genuine reading, it offers a viable path for public health policy. If a rhetorical bridge, it''s unstable and prone to collapse, leading to higher extractiveness and suppression as one extreme dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether the risk-stratification reading is a structurally independent position or a rhetorical compromise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 10, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_legitimacy' kernel, focusing on risk stratification. It attempts to coordinate between the 'public_health_primacy_reading' and 'bodily_autonomy_primacy_reading' by introducing proportionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
