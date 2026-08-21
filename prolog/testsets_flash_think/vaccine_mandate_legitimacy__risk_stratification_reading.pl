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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Vaccine Mandate Legitimacy: Risk Stratification Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'risk_stratification_reading' of
 *   the 'vaccine_mandate_legitimacy' kernel. This reading argues that vaccine
 *   mandates are legitimate only if they are proportionate to actuarial risk,
 *   making blanket mandates (which fail this proportionality test)
 *   illegitimate. It seeks to balance collective public health goals with
 *   individual liberties, particularly for those at low risk. The claimed
 *   type is 'scaffold' because, ideally, such a principle would provide
 *   temporary, proportionate support during a public health emergency.
 *   However, the metrics reflect the current reality where blanket mandates
 *   are often implemented, leading to high extraction and suppression for
 *   those below the risk threshold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.78).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.85).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, scaffold).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy: Risk Stratification Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).
narrative_ontology:has_sunset_clause(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '9f7cd9b6-d51f-449f-8e49-c07f80afd268').
narrative_ontology:cs_kernel_codification('9f7cd9b6-d51f-449f-8e49-c07f80afd268', formalized).
narrative_ontology:cs_authority_grounding('9f7cd9b6-d51f-449f-8e49-c07f80afd268', lineage).
narrative_ontology:cs_interpretation_layer_present('9f7cd9b6-d51f-449f-8e49-c07f80afd268').
narrative_ontology:cs_reading_relation('9f7cd9b6-d51f-449f-8e49-c07f80afd268', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f7cd9b6-d51f-449f-8e49-c07f80afd268', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('9f7cd9b6-d51f-449f-8e49-c07f80afd268', foundational, mandates_must_be_proportionate_to_actuarial_risk).
narrative_ontology:cs_axiom_status(mandates_must_be_proportionate_to_actuarial_risk, holdable).
narrative_ontology:cs_axiom_grounding('9f7cd9b6-d51f-449f-8e49-c07f80afd268', mandates_must_be_proportionate_to_actuarial_risk, deontological).
narrative_ontology:cs_axiom('9f7cd9b6-d51f-449f-8e49-c07f80afd268', foundational, blanket_mandates_fail_proportionality_for_low_risk).
narrative_ontology:cs_axiom_status(blanket_mandates_fail_proportionality_for_low_risk, holdable).
narrative_ontology:cs_axiom_grounding('9f7cd9b6-d51f-449f-8e49-c07f80afd268', blanket_mandates_fail_proportionality_for_low_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('9f7cd9b6-d51f-449f-8e49-c07f80afd268', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('9f7cd9b6-d51f-449f-8e49-c07f80afd268', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9f7cd9b6-d51f-449f-8e49-c07f80afd268', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_individuals).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_below_risk_threshold).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, employers_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with protecting public health, they seek effective interventions. From this reading's perspective, they benefit by having a legitimate framework for targeted mandates, avoiding legal challenges to blanket policies. They bear the cost of implementing nuanced risk assessments.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% These individuals are disproportionately affected by blanket mandates, facing job loss, travel restrictions, or social exclusion despite low personal risk. They bear the costs of compliance or non-compliance without a proportionate public health benefit, making them victims of policies that fail risk stratification.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_below_risk_threshold, payer,
    powerless, biographical, identity_locked, national).

% Benefit from targeted mandates that protect them from severe disease, as their higher risk justifies the intervention. They are coordinated into a protective framework that aligns with their health needs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_individuals, beneficiary,
    moderate, biographical, constrained, national).

% Analyze the legal and ethical implications of mandates, advocating for proportionality and constitutional limits on state power. They provide the intellectual framework for this reading.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, universal).

% Champion individual rights and oppose state overreach. This reading's emphasis on proportionality and targeted mandates aligns with their goals, as it limits coercive measures to only those demonstrably necessary.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, civil_liberties_advocates, beneficiary,
    organized, biographical, mobile, national).

% Often tasked with enforcing mandates, they face legal and operational complexities. From this reading's perspective, they are victims when forced to implement blanket mandates that lack proportionality, leading to employee resistance and legal challenges.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, employers_institutions, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, employers_institutions, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate public health interventions with constitutional principles of proportionality and individual liberty, ensuring that coercive measures like vaccine mandates are justified by actuarial risk and are temporary.
% TRANSFER_FUNCTION: Prevents the disproportionate transfer of compliance burdens and potential harms from low-risk individuals to the collective, while ensuring high-risk individuals receive appropriate protection. It shifts the burden of justification onto the state for any mandate.
% ABSENT_VOICES: Those who hold that bodily autonomy is absolute (bodily_autonomy_primacy_reading) would object to any mandate, even targeted ones. Those who prioritize collective public health above all (public_health_primacy_reading) would object to any limitation on blanket mandates. This reading seeks a middle ground, making both extremes 'absent' from its core framing.
% DISAPPEARANCE_RATIONALE: If the principle of risk stratification and proportionality vanished, public health policy would likely swing to either absolute bodily autonomy (no mandates) or absolute public health primacy (blanket mandates without proportionality), leading to significant shifts in individual freedoms, public health outcomes, and legal challenges.
% FOUNDING_PROBLEM: The challenge of implementing effective public health measures during emergencies (like pandemics) without infringing disproportionately on individual liberties, particularly for those at low risk of severe outcomes, and ensuring such measures are temporary and justified.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars, bioethicists, and civil liberties organizations widely attest to the ongoing tension between public health powers and individual rights, especially concerning proportionality and the temporary nature of emergency measures. Legislative hearings and court cases provide ample evidence of this live problem.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because, from this reading's perspective, blanket mandates impose significant costs (job loss, social exclusion) on individuals for whom the public health benefit is not proportionate to their personal risk. Suppression is also high (0.85) as mandates are legally enforced, limiting exit options. Resistance is high (0.75) due to ongoing legal challenges and public debate. Theater ratio is low (0.10) as the debate is substantive and directly impacts policy. Accessibility collapse is moderate-high (0.70) as alternatives to compliance are severely limited. The claimed type is 'scaffold' because the principle, if properly applied, would offer temporary, proportionate support during an emergency, but its violation leads to extractive outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This reading mediates between the 'public_health_primacy_reading' (which justifies broad mandates for collective good) and the 'bodily_autonomy_primacy_reading' (which rejects all mandates). Its core contribution is the introduction of proportionality and risk stratification as a necessary condition for legitimacy, creating a distinct analytical lens that highlights the extraction inherent in blanket mandates.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, when implementing targeted mandates, and high-risk individuals, who are protected, are beneficiaries. Civil liberties advocates also benefit as their principles are upheld. Individuals below the risk threshold, who bear disproportionate costs under blanket mandates, are victims. Employers and institutions, caught between public health directives and individual rights, also bear costs and are victims when forced to implement disproportionate policies.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_threshold_definition_ambiguity,
    'What constitutes an ''actuarial risk threshold'' that justifies a mandate, and who defines it (e.g., epidemiologists, ethicists, policymakers)?',
    'Consensus among interdisciplinary expert panels, validated by empirical data on disease transmission and severity, and subject to public and legal review.',
    'A clear, empirically grounded definition would strengthen the legitimacy of targeted mandates and reduce the perceived extraction from those below the threshold. An ambiguous definition would perpetuate contestation and perceived disproportionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_threshold_definition_ambiguity, empirical, 'Ambiguity in defining the actuarial risk threshold for mandate legitimacy.').

omega_variable(
    proportionality_assessment_methodology,
    'How is ''proportionality'' measured and assessed in practice, balancing public health benefits against individual burdens?',
    'Development of standardized, transparent proportionality frameworks that integrate epidemiological data, ethical considerations, and legal precedent, applied by independent review bodies.',
    'A robust methodology would provide a clear basis for policy, reducing arbitrary application and strengthening the ''scaffold'' function. Lack of a clear methodology would allow mandates to drift towards ''snare'' behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_assessment_methodology, conceptual, 'Methodological ambiguity in assessing proportionality for public health mandates.').

omega_variable(
    victim_set_size_variability,
    'How does the specific definition of the actuarial risk threshold impact the size and composition of the ''victim'' set (those disproportionately affected by mandates)?',
    'Quantitative modeling and demographic analysis of different risk threshold definitions, assessing their impact on various population subgroups.',
    'A threshold that minimizes the victim set while achieving public health goals would enhance the constraint''s legitimacy. A broad threshold would increase the victim set, amplifying perceived extraction and resistance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_size_variability, empirical, 'Impact of risk threshold definition on the size of the victim population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t1, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 1, 0.1).
narrative_ontology:measurement(vacc_tr_t2, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 2, 0.1).
narrative_ontology:measurement(vacc_tr_t3, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 3, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(vacc_be_t1, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 1, 0.75).
narrative_ontology:measurement(vacc_be_t2, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 2, 0.78).
narrative_ontology:measurement(vacc_be_t3, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 3, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(vacc_su_t1, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 1, 0.83).
narrative_ontology:measurement(vacc_su_t2, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 2, 0.85).
narrative_ontology:measurement(vacc_su_t3, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 3, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
