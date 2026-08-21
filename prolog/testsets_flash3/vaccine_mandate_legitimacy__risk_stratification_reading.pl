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
 *   This constraint represents a 'risk stratification' reading of vaccine
 *   mandate legitimacy, arguing that blanket mandates are disproportionate,
 *   but targeted mandates are permissible when justified by actuarial risk
 *   thresholds. It seeks to navigate the tension between public health
 *   imperatives and individual bodily autonomy by introducing a
 *   proportionality test based on scientific evidence of risk. This reading
 *   is one of several competing interpretations of the broader
 *   'vaccine_mandate_legitimacy' kernel.
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
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy: Risk Stratification Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '27080443-918a-4535-a2b4-fe2415aed5dd').
narrative_ontology:cs_kernel_codification('27080443-918a-4535-a2b4-fe2415aed5dd', formalized).
narrative_ontology:cs_authority_grounding('27080443-918a-4535-a2b4-fe2415aed5dd', lineage).
narrative_ontology:cs_interpretation_layer_present('27080443-918a-4535-a2b4-fe2415aed5dd').
narrative_ontology:cs_reading_relation('27080443-918a-4535-a2b4-fe2415aed5dd', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('27080443-918a-4535-a2b4-fe2415aed5dd', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('27080443-918a-4535-a2b4-fe2415aed5dd', foundational, mandates_must_be_proportional_to_risk).
narrative_ontology:cs_axiom_status(mandates_must_be_proportional_to_risk, holdable).
narrative_ontology:cs_axiom_grounding('27080443-918a-4535-a2b4-fe2415aed5dd', mandates_must_be_proportional_to_risk, empirically_contingent).
narrative_ontology:cs_axiom('27080443-918a-4535-a2b4-fe2415aed5dd', foundational, collective_safety_justifies_targeted_intervention).
narrative_ontology:cs_axiom_status(collective_safety_justifies_targeted_intervention, holdable).
narrative_ontology:cs_axiom_grounding('27080443-918a-4535-a2b4-fe2415aed5dd', collective_safety_justifies_targeted_intervention, instrumental).
narrative_ontology:cs_reference_frame('27080443-918a-4535-a2b4-fe2415aed5dd', proportional_public_health_governance).
narrative_ontology:cs_drift_state('27080443-918a-4535-a2b4-fe2415aed5dd', contemporary_pandemic_response, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27080443-918a-4535-a2b4-fe2415aed5dd', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_below_risk_threshold).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, employers_implementing_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for public health outcomes, they seek to implement mandates that are proportional to risk, balancing individual liberty with collective safety. They face legal challenges from both sides of the debate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced transmission rates due to targeted mandates, as they are at higher risk of severe illness or death from vaccine-preventable diseases. Their safety depends on the effectiveness of public health interventions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Bear the burden of mandates (e.g., vaccination, testing, exclusion from certain activities) despite their individual risk profile being low, leading to claims of disproportionality and infringement on bodily autonomy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_below_risk_threshold, payer,
    moderate, biographical, constrained, national).

% Are compelled to implement mandates to comply with public health directives or to protect their workforce, incurring administrative costs and facing potential legal challenges or employee resistance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, employers_implementing_mandates, payer,
    organized, biographical, constrained, national).

% Adjudicate the legality and proportionality of vaccine mandates, often weighing public health imperatives against individual rights. Their rulings shape the operational boundaries of this constraint.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for public health interventions that seeks to coordinate collective action against infectious disease while respecting individual rights through proportionality and risk stratification.
% TRANSFER_FUNCTION: Transfers the burden of compliance (vaccination, testing, exclusion) from the general population to specific individuals or groups whose risk profile or social role justifies the intervention, aiming to protect vulnerable populations.
% ABSENT_VOICES: Individuals who believe all mandates are an absolute violation of bodily autonomy, regardless of risk, are often excluded from the policy-making process, as this reading seeks a middle ground that they reject entirely.
% DISAPPEARANCE_RATIONALE: If this reading of mandate legitimacy vanished, public health authorities would either revert to blanket mandates (public_health_primacy_reading) or abandon mandates entirely (bodily_autonomy_primacy_reading), leading to a significant shift in public health policy and potentially different disease outcomes.
% FOUNDING_PROBLEM: The challenge of implementing public health measures that effectively control disease spread without unduly infringing on individual liberties, particularly in contexts where risks are unevenly distributed.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, bioethicists, and public health ethicists from outside the direct beneficiaries corroborate the ongoing nature of this problem, emphasizing the need for nuanced approaches to mandates.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is moderate-high because even targeted mandates impose significant burdens on individuals, including potential loss of employment or access to services. Suppression (0.70) is high due to the active enforcement required to ensure compliance and the legal mechanisms used to uphold mandates. The theater ratio is low (0.10) as the justification for mandates, while contested, is generally grounded in genuine public health concerns, not performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities view this as a necessary and balanced approach, while individuals subject to mandates may still perceive it as an overreach, even if targeted. The engine's classification will reflect the structural burdens, which are significant for payers, even if the intent is coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are the agenda-setters and beneficiaries, as this reading provides a framework for their interventions. Vulnerable populations are beneficiaries, as their health is protected. Individuals below the risk threshold and employers implementing mandates are payers, bearing the direct costs and burdens. Constitutional courts act as observers, shaping the legal landscape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_threshold_definition,
    'How is the ''actuarial risk threshold'' defined, and who determines it? Is it a fixed scientific standard or subject to political/social negotiation?',
    'Analysis of legislative and regulatory processes for setting thresholds, and expert consensus on scientific definitions of risk. If thresholds are fluid or politically influenced, the constraint''s stability is compromised.',
    'If the threshold is arbitrary or easily manipulated, the constraint''s legitimacy as a ''tangled_rope'' (coordination with extraction) is weakened, potentially reclassifying it closer to a ''snare'' due to arbitrary extraction. If it''s robustly scientific, it strengthens the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_threshold_definition, empirical, 'Ambiguity in defining the actuarial risk threshold for mandate legitimacy.').

omega_variable(
    proportionality_test_application,
    'How consistently and fairly is the proportionality test applied across different populations and contexts? Does it genuinely mitigate extraction for low-risk individuals?',
    'Empirical studies of mandate implementation outcomes, disaggregated by population demographics and risk profiles. Legal challenges and court rulings on proportionality.',
    'Inconsistent application or failure to mitigate burdens for low-risk individuals would increase effective extraction and suppression, pushing the constraint towards a ''snare''. Consistent application would reinforce its ''tangled_rope'' classification by demonstrating a genuine, albeit imperfect, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_application, empirical, 'Consistency and fairness of proportionality test application in vaccine mandates.').

omega_variable(
    reading_coexistence_stability,
    'Can the ''risk_stratification_reading'' genuinely coexist with the ''public_health_primacy_reading'' and ''bodily_autonomy_primacy_reading'' in a stable legal and social framework, or does it inevitably collapse into one of the extremes?',
    'Longitudinal study of legal precedents, public discourse, and policy outcomes in jurisdictions attempting to implement this reading. Does it maintain its distinct identity or get absorbed?',
    'If it collapses into an extreme, the ''tangled_rope'' classification would be unstable, and the constraint would reclassify to either a ''snare'' (if absorbed by bodily autonomy primacy) or a ''rope'' (if absorbed by public health primacy, assuming low extraction). Its ability to maintain a distinct, stable middle ground is key to its current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_stability, conceptual, 'Stability of the risk stratification reading''s position between extreme views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_legitimacy' kernel, which also includes 'public_health_primacy_reading' and 'bodily_autonomy_primacy_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
