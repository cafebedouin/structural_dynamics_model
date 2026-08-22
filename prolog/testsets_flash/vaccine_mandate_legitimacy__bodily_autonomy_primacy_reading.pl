% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Bodily Autonomy Primacy Reading of Vaccine Mandate Legitimacy
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents a specific reading of vaccine mandate
 *   legitimacy, where individual bodily autonomy is considered an absolute
 *   right, rendering state coercion for public health purposes categorically
 *   impermissible. This reading emerged strongly during the COVID-19
 *   pandemic, leading to significant societal debate and legal challenges
 *   against public health measures. The constraint's high extractiveness and
 *   suppression reflect the severe costs borne by vulnerable populations and
 *   public health systems when this principle is applied absolutely, and the
 *   active enforcement (legal and social) required to maintain this
 *   interpretation against competing public health imperatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.85).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.9).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, snare).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Bodily Autonomy Primacy Reading of Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '35dbc3f4-b60e-4803-a2e6-0d3c07e9505a').
narrative_ontology:cs_kernel_codification('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', fixed_text).
narrative_ontology:cs_authority_grounding('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', lineage).
narrative_ontology:cs_interpretation_layer_present('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a').
narrative_ontology:cs_reading_relation('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', vaccine_mandate_legitimacy__risk_stratification_reading, coexists_with).
narrative_ontology:cs_axiom('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', foundational, bodily_autonomy_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', bodily_autonomy_absolute, deontological).
narrative_ontology:cs_axiom('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', foundational, state_coercion_categorically_impermissible_in_medical_matters).
narrative_ontology:cs_axiom_status(state_coercion_categorically_impermissible_in_medical_matters, holdable).
narrative_ontology:cs_axiom_grounding('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', state_coercion_categorically_impermissible_in_medical_matters, deontological).
narrative_ontology:cs_reference_frame('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', individual_rights_supremacy_framework).
narrative_ontology:cs_drift_state('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', contemporary_pandemic_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('35dbc3f4-b60e-4803-a2e6-0d3c07e9505a', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_opposed_to_mandates).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, healthcare_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These movements gain legitimacy and support by championing absolute bodily autonomy, framing any state health intervention as an overreach. They benefit from the constraint's persistence as it provides a clear ideological battleground.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, mobile, national).

% These individuals benefit from the constraint by having their personal choices prioritized over collective health measures, avoiding mandatory vaccination or other interventions. Their benefit is the preservation of their perceived self-sovereignty.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_opposed_to_mandates, beneficiary,
    moderate, biographical, constrained, local).

% These individuals bear a disproportionate burden of exposure risk when vaccine mandates are deemed illegitimate. Their health and safety are directly compromised by the lack of collective immunity, with no viable exit from the increased risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% Similar to immunocompromised individuals, other vulnerable groups (e.g., elderly, those with specific comorbidities) face heightened risks of severe illness and death due to reduced herd immunity, bearing the costs of individual autonomy without protection.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% These authorities bear the cost of diminished capacity to implement effective public health interventions. Their mandate to protect collective health is undermined, leading to increased disease burden and strain on healthcare systems. They are constrained by legal challenges and public resistance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, payer,
    institutional, generational, constrained, national).

% Healthcare systems face increased patient loads, resource strain, and burnout among staff when preventable diseases spread due to lack of mandates. They bear the operational and human costs of a less healthy population.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, healthcare_systems, payer,
    institutional, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts that the primary coordination function is to protect individual liberty and self-determination against state overreach, ensuring that medical decisions remain solely with the individual.
% TRANSFER_FUNCTION: This reading transfers the burden of collective health risks (e.g., disease transmission, healthcare system strain) from individuals making autonomous choices to vulnerable populations and public health infrastructure.
% ABSENT_VOICES: The voices of future generations, who might inherit a society with weakened public health protections, are absent. Also, the collective voice of the 'common good' or 'societal welfare' is systematically de-prioritized in this framework.
% DISAPPEARANCE_RATIONALE: If this constraint (the absolute primacy of bodily autonomy over public health mandates) vanished, public health authorities would gain significant power to implement mandates, potentially leading to a rapid increase in vaccination rates and a decrease in disease transmission, fundamentally altering the balance between individual rights and collective welfare.
% FOUNDING_PROBLEM: The founding problem this reading addresses is the historical and potential for state coercion in medical matters, including forced sterilization, unethical experimentation, and mandatory treatments, which infringe upon individual dignity and autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and historical accounts of medical abuses corroborate the ongoing concern about state overreach. While public health authorities might argue the context has changed, the underlying principle of protecting individual bodily integrity remains a live concern for many, supported by legal scholars focused on individual rights.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) stems from the direct and indirect costs imposed on vulnerable groups and public health infrastructure due to reduced collective immunity. Suppression (0.9) is high because this reading actively suppresses alternative public health strategies and the voices advocating for them, often through legal challenges and strong social pressure. The low accessibility collapse (0.2) indicates that alternative public health strategies are well-understood and available, but actively resisted. Resistance (0.8) is high due to ongoing efforts by public health authorities and advocates for vulnerable groups to challenge this absolute interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of liberty advocates, this constraint is a 'rope' or even a 'mountain' of fundamental rights, ensuring individual freedom. From the perspective of immunocompromised individuals and public health authorities, it operates as a 'snare,' trapping them in a high-risk environment with limited recourse. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberty advocacy movements and individuals opposed to mandates are clear beneficiaries, as their core values are upheld. Immunocompromised individuals, vulnerable populations, and public health authorities are victims, bearing the direct and systemic costs of this interpretation. Healthcare systems also bear significant costs. The directionality for beneficiaries is low (subsidized), while for victims it is high (targeted).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_vs_proportional_autonomy,
    'Is bodily autonomy an absolute right, or is it subject to proportionality tests when collective harm is at stake?',
    'Judicial rulings establishing a clear legal framework for balancing individual rights against public health imperatives, or a societal consensus shift on the limits of individual liberty in a pandemic context.',
    'If autonomy is deemed proportional, the extractiveness on vulnerable populations would decrease, and the constraint might reclassify from a snare to a tangled rope or even a rope, depending on the balance struck.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_proportional_autonomy, conceptual, 'Ambiguity regarding the scope and limits of bodily autonomy in public health.').

omega_variable(
    causal_link_unvaccinated_to_vulnerable_harm,
    'What is the precise causal link and magnitude of harm from unvaccinated individuals to immunocompromised and vulnerable populations?',
    'Epidemiological studies and public health data providing robust, peer-reviewed evidence on transmission rates, severity, and healthcare burden attributable to unvaccinated populations.',
    'Stronger evidence of direct and severe harm would weaken the ''bodily autonomy primacy'' argument by highlighting the externality, potentially shifting public and legal opinion towards collective responsibility. Weaker evidence would reinforce the current reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_link_unvaccinated_to_vulnerable_harm, empirical, 'Empirical uncertainty about the direct harm caused by unvaccinated individuals to others.').

omega_variable(
    mandate_effectiveness_vs_social_cohesion,
    'Does the enforcement of vaccine mandates achieve sufficient public health benefits to outweigh the social costs of coercion and erosion of trust in public institutions?',
    'Longitudinal studies comparing health outcomes and social cohesion in jurisdictions with and without mandates, alongside qualitative research on public trust and compliance.',
    'If mandates are shown to be highly effective with minimal social cost, the ''bodily autonomy primacy'' reading would face significant challenge. If social costs are high and benefits marginal, this reading would gain further traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_effectiveness_vs_social_cohesion, empirical, 'Trade-off between mandate effectiveness and social cohesion/trust.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2022, 0.83).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2023, 0.84).
narrative_ontology:measurement(vacc_be_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2021, 0.85).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2022, 0.88).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2023, 0.89).
narrative_ontology:measurement(vacc_su_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, identity_coordination).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_policy__collective_action_problem).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, healthcare_resource_allocation__ethical_framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
