% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality Principle for Public Health Mandates
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   mandate legitimacy. It asserts that mandates are legitimate only when
 *   they are necessary, effective, and the least restrictive means to address
 *   a severe public health threat. This reading attempts to balance
 *   individual rights with collective well-being, aiming for a 'tangled_rope'
 *   classification where coordination is achieved with justified, but
 *   acknowledged, extraction. The metrics reflect the inherent friction and
 *   imposition of mandates, even when applied proportionally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.45).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.6).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality Principle for Public Health Mandates").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '7a5a1054-a978-4b7e-aa12-2e45b547d60e').
narrative_ontology:cs_kernel_codification('7a5a1054-a978-4b7e-aa12-2e45b547d60e', formalized).
narrative_ontology:cs_authority_grounding('7a5a1054-a978-4b7e-aa12-2e45b547d60e', lineage).
narrative_ontology:cs_interpretation_layer_present('7a5a1054-a978-4b7e-aa12-2e45b547d60e').
narrative_ontology:cs_reading_relation('7a5a1054-a978-4b7e-aa12-2e45b547d60e', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('7a5a1054-a978-4b7e-aa12-2e45b547d60e', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('7a5a1054-a978-4b7e-aa12-2e45b547d60e', foundational, individual_liberty_is_defeasible_for_collective_good_if_proportional).
narrative_ontology:cs_axiom_status(individual_liberty_is_defeasible_for_collective_good_if_proportional, holdable).
narrative_ontology:cs_axiom_grounding('7a5a1054-a978-4b7e-aa12-2e45b547d60e', individual_liberty_is_defeasible_for_collective_good_if_proportional, deontological).
narrative_ontology:cs_axiom('7a5a1054-a978-4b7e-aa12-2e45b547d60e', foundational, state_power_to_mandate_is_limited_by_necessity_and_least_restrictive_means).
narrative_ontology:cs_axiom_status(state_power_to_mandate_is_limited_by_necessity_and_least_restrictive_means, holdable).
narrative_ontology:cs_axiom_grounding('7a5a1054-a978-4b7e-aa12-2e45b547d60e', state_power_to_mandate_is_limited_by_necessity_and_least_restrictive_means, deontological).
narrative_ontology:cs_reference_frame('7a5a1054-a978-4b7e-aa12-2e45b547d60e', liberal_democratic_constitutionalism).
narrative_ontology:cs_drift_state('7a5a1054-a978-4b7e-aa12-2e45b547d60e', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a5a1054-a978-4b7e-aa12-2e45b547d60e', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, medical_professionals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, anti_mandate_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting public health, they interpret and apply the proportionality principle to justify mandates. They benefit from a healthier population and clear legal frameworks for intervention.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Bear the direct costs of mandates, such as receiving a vaccine or adhering to restrictions, potentially against their personal preferences or beliefs. Their exit options are limited by legal penalties or social exclusion.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, individuals_subject_to_mandate, payer,
    powerless, immediate, constrained, national).

% Benefit significantly from mandates that reduce disease transmission, as they are at higher risk of severe illness or death. They are often trapped by their health status, making collective protection essential.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Scrutinize mandates for adherence to constitutional principles and individual rights, often challenging policies they deem disproportionate. They aim to ensure state power is exercised within legal bounds.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, constitutional_lawyers_civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Benefit from reduced disease burden and clear public health guidelines, which streamline their practice and protect healthcare systems. They also contribute expertise to the formulation of mandates.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, medical_professionals, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, medical_professionals, agenda_setter).

% Actively resist mandates, viewing them as infringements on fundamental bodily autonomy and personal liberty. While organized, their views are often excluded from mainstream policy-making, leading to protest and legal challenges.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, anti_mandate_activists, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a principled framework for state intervention in public health, balancing individual liberty with collective well-being by ensuring mandates are proportional to the threat and least restrictive.
% TRANSFER_FUNCTION: Transfers a degree of individual liberty and autonomy (e.g., medical choice, freedom of movement) to the collective good of public health, with the expectation that this transfer is justified by disease severity and lack of alternatives.
% ABSENT_VOICES: Individuals with rare medical contraindications, severe economic hardship from compliance, or deeply held religious objections may find their specific circumstances marginalized in the broad application of mandates, even when the principle is invoked.
% DISAPPEARANCE_RATIONALE: Without the proportionality principle, public health responses would either devolve into unchecked state coercion (ignoring individual rights) or become entirely ineffective (failing to protect the vulnerable), leading to a chaotic and unjust public health landscape.
% FOUNDING_PROBLEM: To prevent both arbitrary state overreach and uncontrolled disease spread during public health crises, by providing a rational and ethical basis for compulsory measures.
% FOUNDING_PROBLEM_CORROBORATION: Ongoing public health emergencies (e.g., pandemics, measles outbreaks) and persistent legal challenges to mandates demonstrate the continuous need for a robust and defensible proportionality framework. Legal scholars, ethicists, and public health experts outside of direct policy-making bodies corroborate this necessity.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).
:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) is moderate, reflecting the imposition on individual liberty, which is deemed justifiable under specific conditions. Suppression (0.6) is also moderate, as mandates are enforced but typically allow for legal challenge. The theater ratio is low (0.1) because the intent is genuinely public health, not performative. Accessibility collapse (0.5) is moderate, as alternatives exist but are often less effective. Resistance (0.6) is significant due to the inherent tension between individual autonomy and collective action.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this reading provides a necessary and ethical tool for governance. From the perspective of individuals subject to mandates, even proportional measures can feel highly extractive and suppressive, especially if they disagree with the assessment of severity or alternatives. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, vulnerable populations, and medical professionals are beneficiaries, gaining from disease control and a stable framework. Individuals subject to mandates and anti-mandate activists are the primary targets, bearing the costs of compliance or exclusion. The proportionality principle aims to minimize the burden on targets while maximizing collective benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''mandate_legitimacy_scope'' kernel, or merely a nuanced application of one of the sibling readings?',
    'Analysis of legal and ethical arguments: if the proportionality principle introduces distinct criteria not reducible to either pure bodily autonomy or pure public health necessity, it is a distinct reading.',
    'If distinct, it validates the decomposition of the kernel. If not, it suggests this reading should be subsumed under a sibling, altering the network structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the structural independence of the proportionality reading within the kernel.').

omega_variable(
    empirical_proportionality_assessment,
    'How reliably can ''disease severity'', ''vaccine safety/efficacy'', and ''availability of less restrictive alternatives'' be empirically determined and agreed upon by all parties?',
    'Longitudinal studies of public health crises, expert consensus formation, and judicial review outcomes. Divergence in these assessments indicates persistent empirical ambiguity.',
    'High empirical ambiguity would increase the effective extractiveness and suppression of mandates, as their justification would be perpetually contested, pushing the constraint towards a ''snare'' for targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_proportionality_assessment, empirical, 'Uncertainty in the empirical inputs for applying the proportionality principle.').

omega_variable(
    conceptual_proportionality_thresholds,
    'Are the thresholds for ''proportionality'' (e.g., what constitutes ''severe'' disease or ''least restrictive'' alternative) fixed ethical/legal standards, or are they subject to political and social negotiation?',
    'Comparative legal analysis across jurisdictions and historical periods, and philosophical inquiry into the nature of rights and state power. Persistent variation suggests conceptual fluidity.',
    'If thresholds are fluid, the constraint''s classification could drift significantly based on prevailing political winds, potentially shifting from ''tangled_rope'' to ''snare'' if thresholds are lowered to justify broader mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_proportionality_thresholds, conceptual, 'Ambiguity in the conceptual definition of proportionality thresholds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(mand_tr_t15, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(mand_be_t15, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(mand_su_t15, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'mandate_legitimacy_scope' kernel, alongside 'bodily_autonomy_primary' and 'public_health_primary'. Each reading offers a distinct framework for assessing the legitimacy of public health mandates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
