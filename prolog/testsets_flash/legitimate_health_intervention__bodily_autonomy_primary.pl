% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'bodily autonomy primary' reading of the
 *   legitimate_health_intervention kernel. It asserts that informed consent
 *   is paramount for medical interventions, and state coercion, regardless of
 *   public benefit, violates bodily integrity. This reading places
 *   individuals asserting autonomy and civil liberties advocates as
 *   beneficiaries, while mandate-coerced individuals and those facing access
 *   restrictions become victims. The state, when enforcing mandates, acts as
 *   an extractor. The structural delta from the kernel context is reflected
 *   in the moderate-to-high extractiveness and suppression, particularly
 *   during periods of active mandate enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.65).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.7).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily Autonomy as Primary in Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '8a6acdf5-4a8c-41fa-a60e-5e7f949877c2').
narrative_ontology:cs_kernel_codification('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', formalized).
narrative_ontology:cs_authority_grounding('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', lineage).
narrative_ontology:cs_interpretation_layer_present('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2').
narrative_ontology:cs_reading_relation('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', foundational, bodily_integrity_is_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', bodily_integrity_is_absolute, deontological).
narrative_ontology:cs_axiom('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', foundational, informed_consent_is_prerequisite_for_legitimacy).
narrative_ontology:cs_axiom_status(informed_consent_is_prerequisite_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', informed_consent_is_prerequisite_for_legitimacy, deontological).
narrative_ontology:cs_reference_frame('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', nuremberg_code_principles).
narrative_ontology:cs_drift_state('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', contemporary_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8a6acdf5-4a8c-41fa-a60e-5e7f949877c2', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, individuals_asserting_autonomy).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_advocates).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, employers_enforcing_mandates).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, access_restricted_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from the legal and ethical framework that prioritizes their right to make decisions about their own bodies, free from state coercion. Their ability to exercise this autonomy is often constrained by social or economic pressures.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, individuals_asserting_autonomy, beneficiary,
    moderate, biographical, constrained, national).

% Organizations and legal professionals who champion individual rights and freedoms, including bodily integrity. They benefit from the legal precedents and public discourse that reinforce this reading, using it to challenge state overreach.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).

% Individuals who face direct or indirect coercion (e.g., job loss, denial of access to services) if they do not comply with medical mandates. They bear the direct costs of non-compliance, often feeling trapped between their principles and their livelihoods.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, immediate, identity_locked, local).

% Government bodies tasked with protecting public health. Under this reading, their ability to implement broad public health measures is severely constrained by individual autonomy, making them potential extractors when they attempt to enforce mandates.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Private or public employers who implement medical mandates (e.g., vaccine requirements) due to state pressure or perceived liability. They bear the costs of enforcement, potential legal challenges, and workforce disruption, often acting as an intermediary for state coercion.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, employers_enforcing_mandates, payer,
    powerful, biographical, constrained, local).

% Individuals who are denied access to public spaces, transportation, or essential services due to non-compliance with medical mandates. Their daily lives are severely impacted, and their options for avoiding the constraint are minimal.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, access_restricted_individuals, payer,
    powerless, immediate, trapped, local).

% Academics and practitioners who analyze the ethical implications of public health policies. They observe the tension between individual rights and collective well-being, providing critical analysis of the constraint's operation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_ethicists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates individual actions by establishing a clear boundary around personal medical decisions, ensuring that interventions are based on voluntary, informed consent. It aims to prevent medical paternalism and state overreach into private bodily matters.
% TRANSFER_FUNCTION: It transfers the burden of proof and justification for medical interventions from the individual to the state or medical authority. It also transfers the risk of non-compliance (e.g., disease spread) from the individual to the collective, as individual autonomy is prioritized.
% ABSENT_VOICES: Those who prioritize collective well-being and public health outcomes above individual autonomy are often marginalized in this discourse. They would argue that individual refusal imposes externalities on the vulnerable and that the state has a legitimate role in protecting the population.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal and ethical landscape around medical interventions would fundamentally shift. State and institutional actors would have significantly more leeway to implement mandatory health measures, potentially leading to widespread public health campaigns with less individual consent, and a redefinition of individual rights in medical contexts.
% FOUNDING_PROBLEM: The constraint emerged from historical abuses of medical power, forced sterilizations, unethical human experimentation, and a desire to protect individuals from involuntary medical procedures, establishing the principle that a person's body is their own.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and patient advocacy groups consistently attest that the founding problem of protecting individuals from medical coercion remains live, citing ongoing debates about medical mandates, data privacy, and the rights of vulnerable populations. This corroboration comes from outside the direct beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is driven by the costs imposed on individuals who refuse mandates, including job loss or denial of services. Suppression (0.70) is high because the state actively enforces these mandates through legal and economic leverage, limiting alternatives for non-compliant individuals. The theater ratio (0.10) is low, as the constraint's operation is largely direct and functional, with little performative maintenance. The increase in extractiveness and suppression around 2020 reflects the heightened enforcement of public health mandates during the COVID-19 pandemic, which brought this reading into sharp conflict with public health imperatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals asserting autonomy, this constraint is a vital protection against state overreach. From the perspective of public health authorities, this same constraint can be seen as an impediment to effective disease control, forcing them into an extractive role when they attempt to protect the collective. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals asserting autonomy and civil liberties advocates are beneficiaries (low d) as the constraint protects their core values and rights. Mandate-coerced individuals and those facing access restrictions are clear targets (high d), bearing the direct costs of non-compliance. State public health authorities, when acting to enforce mandates, become extractors (high d) from the perspective of this reading, as they leverage state power to compel action. Employers enforcing mandates are also payers, acting as intermediaries for state coercion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_coercion_definition,
    'What constitutes ''state coercion'' in the context of medical interventions? Does it include indirect pressures (e.g., employment mandates) or only direct physical force?',
    'Legal precedent and judicial interpretation of constitutional rights and administrative law regarding public health powers.',
    'A broad definition of coercion would increase the victim set and perceived extractiveness of state actions under this reading; a narrow definition would reduce it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_coercion_definition, conceptual, 'Ambiguity in the scope of ''state coercion'' for medical interventions.').

omega_variable(
    autonomy_vs_externality,
    'To what extent does an individual''s refusal of a medical intervention impose an unchosen externality on others, and how should this be weighed against bodily autonomy?',
    'Empirical epidemiological data on disease transmission and severity, combined with ethical frameworks for collective responsibility.',
    'If externalities are deemed significant and unmitigable, the ''public_health_primary'' reading gains stronger justification, potentially influencing the perceived legitimacy of this reading''s absolute stance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_externality, empirical, 'The tension between individual autonomy and the imposition of externalities on the collective.').

omega_variable(
    identity_lock_mechanism,
    'For ''mandate_coerced_individuals'', is the ''identity_locked'' exit option primarily due to deeply held personal beliefs (ideological identity) or career path dependence (professional identity)?',
    'Qualitative sociological studies and individual testimony exploring the motivations for non-compliance and the perceived costs of exit.',
    'If primarily ideological, the suppression is more internalized and resistant to external changes; if primarily professional, structural changes to employment conditions could more easily resolve the lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity lock for mandate-coerced individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1947, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(legi_tr_t1970, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(legi_tr_t1990, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(legi_tr_t2010, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(legi_tr_t2020, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(legi_tr_t2024, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t1947, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 1947, 0.4).
narrative_ontology:measurement(legi_be_t1970, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(legi_be_t1990, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(legi_be_t2010, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(legi_be_t2020, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(legi_be_t2024, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1947, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 1947, 0.3).
narrative_ontology:measurement(legi_su_t1970, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(legi_su_t1990, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(legi_su_t2010, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(legi_su_t2020, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(legi_su_t2024, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__bodily_autonomy_primary, 0.08).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel, focusing on bodily autonomy. It is linked to sibling readings that prioritize public health or proportionality, as they represent competing interpretations of the same core ethical and legal dilemma.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
