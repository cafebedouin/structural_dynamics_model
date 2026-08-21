% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary: Intervention Legitimacy from Morbidity/Mortality Reduction
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of
 *   legitimate health intervention, where legitimacy is derived from the
 *   measurable reduction of population-level morbidity and mortality.
 *   Individual refusal of interventions is framed as an externality imposed
 *   on the collective. This reading prioritizes collective well-being and the
 *   integrity of healthcare systems, leading to policies that enforce
 *   compliance through various mechanisms. The unvaccinated are categorized
 *   as victims due to the restrictions and penalties they face, while the
 *   immunocompromised and the general population are beneficiaries of the
 *   reduced disease burden.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.7).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.65).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary: Intervention Legitimacy from Morbidity/Mortality Reduction").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '02ce4624-40e4-4a59-970a-fd885842ffba').
narrative_ontology:cs_kernel_codification('02ce4624-40e4-4a59-970a-fd885842ffba', formalized).
narrative_ontology:cs_authority_grounding('02ce4624-40e4-4a59-970a-fd885842ffba', expertise).
narrative_ontology:cs_interpretation_layer_present('02ce4624-40e4-4a59-970a-fd885842ffba').
narrative_ontology:cs_reading_relation('02ce4624-40e4-4a59-970a-fd885842ffba', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('02ce4624-40e4-4a59-970a-fd885842ffba', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('02ce4624-40e4-4a59-970a-fd885842ffba', foundational, collective_health_supersedes_individual_choice).
narrative_ontology:cs_axiom_status(collective_health_supersedes_individual_choice, holdable).
narrative_ontology:cs_axiom_grounding('02ce4624-40e4-4a59-970a-fd885842ffba', collective_health_supersedes_individual_choice, deontological).
narrative_ontology:cs_axiom('02ce4624-40e4-4a59-970a-fd885842ffba', foundational, measurable_morbidity_mortality_reduction_is_legitimacy_metric).
narrative_ontology:cs_axiom_status(measurable_morbidity_mortality_reduction_is_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('02ce4624-40e4-4a59-970a-fd885842ffba', measurable_morbidity_mortality_reduction_is_legitimacy_metric, empirically_contingent).
narrative_ontology:cs_reference_frame('02ce4624-40e4-4a59-970a-fd885842ffba', population_health_maximization_framework).
narrative_ontology:cs_drift_state('02ce4624-40e4-4a59-970a-fd885842ffba', contemporary_era_of_individual_rights_advocacy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('02ce4624-40e4-4a59-970a-fd885842ffba', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_systems).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, general_population).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, religious_objectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, anti_vaccine_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates and enforces public health interventions (e.g., vaccination requirements, mask mandates) based on epidemiological data to reduce population-level morbidity and mortality. Justifies actions by prioritizing collective health outcomes over individual preferences.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from reduced disease transmission due to widespread adherence to public health measures, as they are highly vulnerable to severe outcomes. Their safety depends on the actions of others.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from reduced patient load during epidemics, preventing system collapse and ensuring capacity for other medical needs. They advocate for measures that flatten infection curves.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Benefits from overall lower disease burden, reduced risk of severe illness, and fewer disruptions to social and economic life. They generally comply with mandates for collective good.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, general_population, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of public health mandates through restrictions on employment, travel, or access to public spaces if they refuse vaccination. They are seen as imposing externalities on the collective.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Face significant personal and social costs for refusing interventions based on deeply held religious beliefs. Their identity is often fused with their objection, making exit unthinkable without compromising core values.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, religious_objectors, payer,
    powerless, generational, identity_locked, local).

% Actively resist public health mandates, viewing them as infringements on personal liberty. They face social stigma and legal challenges, but their organized efforts create friction for enforcement.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, anti_vaccine_advocates, payer,
    organized, biographical, constrained, national).

% Adjudicate challenges to public health mandates, balancing collective welfare against individual rights. Their rulings can affirm or constrain the authority of public health bodies.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to reduce the spread of infectious diseases, ensuring herd immunity and protecting vulnerable populations by establishing common standards of behavior and intervention.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from vulnerable populations and the collective (healthcare system capacity, economic stability) to individuals who refuse public health interventions, through restrictions on their activities or employment.
% ABSENT_VOICES: Individuals who are medically unable to receive interventions (e.g., due to allergies) are often overlooked in the public discourse, yet their safety is the primary justification for the mandates. Their voice would emphasize the necessity of collective action.
% DISAPPEARANCE_RATIONALE: If the legitimacy of public health interventions based on morbidity/mortality reduction vanished, collective action would cease, leading to increased disease outbreaks, overwhelmed healthcare systems, and significant social and economic disruption. The world would rapidly rearrange to a state of higher vulnerability and chaos.
% FOUNDING_PROBLEM: The problem of infectious disease outbreaks causing widespread death, disability, and societal collapse, requiring collective action beyond individual choice to protect the population.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists, public health historians, and international health organizations (e.g., WHO) corroborate that infectious diseases remain a live threat, and that collective interventions are essential for population health, citing historical pandemics and ongoing outbreaks. This corroboration comes from outside the direct beneficiaries of specific mandates.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because individuals are compelled to participate in interventions that may not directly benefit them, or which they oppose, for the sake of the collective. Suppression (0.65) is substantial due to the enforcement mechanisms (e.g., employment termination, access restrictions) required to ensure compliance and manage resistance. Theater ratio is low (0.1) as the interventions are generally functional in achieving their stated public health goals, with minimal performative overhead. Accessibility collapse is moderate (0.4) as alternatives to compliance (e.g., avoiding vaccination) exist but come with significant social and economic costs. Resistance is high (0.75) due to strong individual liberty and anti-mandate movements.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities perceive this as a necessary and legitimate coordination mechanism for collective survival, while individuals facing mandates experience it as a coercive imposition on their bodily autonomy and personal freedom. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a 'rope' or 'scaffold' and targets experiencing a 'snare' or 'tangled_rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and healthcare systems are beneficiaries (low d) as they achieve their mandate and maintain system integrity. Immunocompromised individuals and the general population are also beneficiaries (low d) due to direct protection and reduced societal disruption. Unvaccinated individuals, religious objectors, and anti-vaccine advocates are targets (high d) as they bear the costs of compliance or face penalties for non-compliance. Religious objectors are identity_locked, facing the highest d due to the deep personal cost of exit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_risk_vs_collective_benefit,
    'How should the individual risks of an intervention (e.g., vaccine side effects) be weighed against the collective benefit of reduced morbidity/mortality?',
    'Development of a universally accepted ethical framework for risk-benefit analysis that integrates individual and population-level data, or judicial precedent establishing clear thresholds.',
    'If individual risks are weighted more heavily, the extractiveness of this constraint would be re-evaluated as higher, potentially shifting its classification towards a Snare. If collective benefit remains paramount, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_risk_vs_collective_benefit, conceptual, 'The ethical weighting of individual vs. collective health outcomes.').

omega_variable(
    efficacy_of_less_coercive_measures,
    'To what extent could public health goals be achieved through less coercive measures (e.g., education, incentives, voluntary compliance) compared to mandates?',
    'Comparative studies across jurisdictions with different policy approaches, evaluating public health outcomes and compliance rates under varying levels of coercion.',
    'If less coercive measures are shown to be equally effective, the suppression metric would be re-evaluated as artificially high, suggesting the constraint is more extractive than necessary and pushing it towards a Snare. If coercion is demonstrably necessary, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_less_coercive_measures, empirical, 'The necessity of coercive enforcement for public health outcomes.').

omega_variable(
    definition_of_externality_imposition,
    'Is individual refusal of a public health intervention always an ''externality imposition,'' or are there contexts where it is a legitimate exercise of autonomy without significant collective harm?',
    'Refined epidemiological models that quantify the precise collective risk posed by individual non-compliance in different contexts (e.g., endemic vs. pandemic, high vs. low transmissibility).',
    'If non-compliance is not always a significant externality, the justification for high extractiveness and suppression weakens, potentially reclassifying the constraint as a Snare or a more extractive Tangled Rope. If it is consistently an externality, the current classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_externality_imposition, conceptual, 'The scope and nature of ''externality imposition'' in public health.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.05).
narrative_ontology:measurement(legi_tr_t5, legitimate_health_intervention__public_health_primary, theater_ratio, 5, 0.08).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__public_health_primary, theater_ratio, 15, 0.09).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__public_health_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__public_health_primary, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__public_health_primary, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__public_health_primary, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__public_health_primary, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__public_health_primary, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__public_health_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__public_health_primary, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__public_health_primary, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, healthcare_resource_allocation).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, employment_discrimination_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
