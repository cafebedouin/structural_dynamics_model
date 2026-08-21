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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Vaccine Mandate Legitimacy (Risk Stratification Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'risk stratification' reading of vaccine
 *   mandate legitimacy, where mandates are considered legitimate only if they
 *   are proportional to an individual's actuarial risk of transmitting or
 *   suffering severe outcomes from a disease. Blanket mandates are seen as
 *   disproportionate, while targeted mandates based on clear risk thresholds
 *   are permissible. This reading attempts to find a middle ground between
 *   absolute bodily autonomy and absolute public health primacy, but its
 *   implementation is complex and often contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.65).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.7).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy (Risk Stratification Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, 'f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5').
narrative_ontology:cs_kernel_codification('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', formalized).
narrative_ontology:cs_authority_grounding('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', lineage).
narrative_ontology:cs_interpretation_layer_present('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5').
narrative_ontology:cs_reading_relation('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', foundational, mandates_must_be_proportional_to_risk).
narrative_ontology:cs_axiom_status(mandates_must_be_proportional_to_risk, holdable).
narrative_ontology:cs_axiom_grounding('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', mandates_must_be_proportional_to_risk, deontological).
narrative_ontology:cs_axiom('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', secondary, blanket_mandates_fail_proportionality).
narrative_ontology:cs_axiom_status(blanket_mandates_fail_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', blanket_mandates_fail_proportionality, empirically_contingent).
narrative_ontology:cs_reference_frame('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', proportional_public_health_ethics).
narrative_ontology:cs_drift_state('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', contemporary_pandemic_response, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0dbace3-1b5c-4c64-bc5a-b3e377fa69d5', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_below_risk_threshold).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, employers_implementing_mandates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, employers_implementing_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for public health outcomes, they seek to implement mandates that are proportional to risk, avoiding blanket policies. They benefit from reduced disease transmission but bear the cost of legal challenges and public distrust if mandates are perceived as overreaching.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from targeted mandates that reduce their exposure to infectious diseases, as they are at higher risk of severe outcomes. They have limited individual agency to avoid exposure in public spaces.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Bear the burden of mandates when their individual risk of severe disease or transmission is actuarially low, making the mandate disproportionate to their personal benefit or public health contribution. They face restrictions on employment, travel, or public access.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_below_risk_threshold, payer,
    moderate, biographical, constrained, national).

% Are compelled to implement mandates to comply with public health directives or reduce workplace risk, incurring administrative costs and potential legal challenges from employees. They benefit from a healthier workforce and reduced liability.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, employers_implementing_mandates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, employers_implementing_mandates, beneficiary).

% Monitor mandate policies for proportionality and potential overreach, advocating for individual rights and challenging policies that do not meet a strict risk-benefit threshold. They influence public discourse and legal challenges.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate public health interventions by ensuring that coercive measures like mandates are applied only where demonstrably proportional to actuarial risk, thereby balancing collective protection with individual liberty and maintaining public trust.
% TRANSFER_FUNCTION: Transfers the burden of compliance (e.g., vaccination, testing) from the general population to specific risk-stratified groups, and transfers the cost of enforcement and legal defense from individuals to public health authorities and employers.
% ABSENT_VOICES: Individuals whose risk profile is genuinely negligible but are still subject to mandates due to broad categorization, and those who cannot access the necessary actuarial data to challenge their classification. Their voices are often subsumed by broader advocacy groups.
% DISAPPEARANCE_RATIONALE: If this reading of mandate legitimacy vanished, public health policy would likely swing towards either absolute bodily autonomy or absolute public health primacy, leading to either widespread non-compliance with mandates or blanket mandates with little proportionality, causing significant societal reorganization and conflict.
% FOUNDING_PROBLEM: The challenge of implementing public health measures that require individual compliance without infringing disproportionately on individual liberties, especially during widespread health crises where collective action is needed.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, bioethicists, and public health ethicists outside of direct government agencies corroborate the ongoing tension between individual rights and collective health, affirming the need for proportionality in mandate design. Public opinion polls also reflect this ongoing societal debate.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because even targeted mandates impose costs on individuals (e.g., medical procedures, restrictions) that may not be offset by their personal risk profile. Suppression (0.70) is high because mandates require active enforcement to ensure compliance, and alternatives for non-compliance are often severely constrained (e.g., loss of employment, travel restrictions). The theater ratio is low (0.10) as the debate is genuinely about the proportionality and necessity of the mandates, not about performative maintenance of an atrophied function. Resistance is high (0.75) due to the ongoing contestation of risk thresholds and the perceived infringement on individual liberties.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities view this as a necessary, nuanced approach to protect the vulnerable, while individuals below the risk threshold perceive it as an arbitrary imposition that disproportionately extracts from them. The engine's per-seat classification would reflect this divergence, with beneficiaries experiencing a more 'rope-like' or 'scaffold-like' constraint and payers experiencing a more 'snare-like' or 'tangled_rope-like' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are beneficiaries, as this reading aims to protect the latter while providing a legitimate framework for the former's actions. Individuals below the risk threshold and employers implementing mandates are payers, bearing the direct costs and burdens. Civil liberties advocates act as observers, challenging the application of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_threshold_definition,
    'How are ''actuarial risk thresholds'' defined and by whom? Is there a consensus on what constitutes a ''proportional'' risk for mandate justification?',
    'Establishment of independent, transparent, and publicly accepted epidemiological and bioethical panels to define and regularly update risk thresholds, with clear methodologies for data collection and analysis.',
    'If thresholds are clear and accepted, the legitimacy of targeted mandates increases, potentially reducing resistance and extractiveness for payers. If contested, the constraint remains highly extractive and suppressive due to ongoing disputes over its application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_threshold_definition, empirical, 'Ambiguity in defining and applying actuarial risk thresholds for mandate legitimacy.').

omega_variable(
    mandate_proportionality_measurement,
    'How is the ''proportionality'' of a mandate measured in practice? Does it account for individual circumstances, or only broad demographic categories?',
    'Development of granular, individualized risk assessment tools and legal frameworks that allow for exemptions or alternative measures for individuals whose specific circumstances fall outside the general risk categories.',
    'If proportionality can be measured and applied individually, the constraint moves closer to a ''rope'' for most, as it becomes more tailored and less broadly extractive. If only broad categories are used, it remains a ''tangled_rope'' or ''snare'' for those disproportionately affected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_proportionality_measurement, conceptual, 'Difficulty in measuring and applying mandate proportionality at an individual level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(vacc_tr_t15, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(vacc_be_t15, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(vacc_su_t15, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_legitimacy' kernel, focusing on risk stratification. It is linked to the 'public_health_primacy_reading' and 'bodily_autonomy_primacy_reading' as sibling interpretations of the same core issue.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
