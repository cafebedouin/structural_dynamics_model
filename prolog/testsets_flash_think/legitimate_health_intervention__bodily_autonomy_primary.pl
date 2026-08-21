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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary for Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'legitimate_health_intervention' kernel. From this perspective,
 *   the legitimacy of medical interventions hinges on informed consent, and
 *   state coercion (e.g., mandates tied to employment or access) constitutes
 *   a violation of bodily integrity, irrespective of any claimed public
 *   health benefit. The constraint is classified as a Snare because the
 *   coordination story (public health) is explicitly rejected as a
 *   justification for the coercive extraction of individual autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.9).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily Autonomy as Primary for Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '51630bb8-0a8e-4bd9-ac70-8fba89bae011').
narrative_ontology:cs_kernel_codification('51630bb8-0a8e-4bd9-ac70-8fba89bae011', formalized).
narrative_ontology:cs_authority_grounding('51630bb8-0a8e-4bd9-ac70-8fba89bae011', lineage).
narrative_ontology:cs_interpretation_layer_present('51630bb8-0a8e-4bd9-ac70-8fba89bae011').
narrative_ontology:cs_reading_relation('51630bb8-0a8e-4bd9-ac70-8fba89bae011', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('51630bb8-0a8e-4bd9-ac70-8fba89bae011', legitimate_health_intervention__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('51630bb8-0a8e-4bd9-ac70-8fba89bae011', foundational, bodily_autonomy_is_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('51630bb8-0a8e-4bd9-ac70-8fba89bae011', bodily_autonomy_is_absolute, deontological).
narrative_ontology:cs_axiom('51630bb8-0a8e-4bd9-ac70-8fba89bae011', foundational, informed_consent_is_prerequisite).
narrative_ontology:cs_axiom_status(informed_consent_is_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('51630bb8-0a8e-4bd9-ac70-8fba89bae011', informed_consent_is_prerequisite, conventional).
narrative_ontology:cs_reference_frame('51630bb8-0a8e-4bd9-ac70-8fba89bae011', individual_sovereignty_framework).
narrative_ontology:cs_drift_state('51630bb8-0a8e-4bd9-ac70-8fba89bae011', contemporary_public_health_crises, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51630bb8-0a8e-4bd9-ac70-8fba89bae011', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, healthcare_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, healthcare_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who face penalties (loss of employment, access to services, social participation) for refusing medical interventions mandated by the state. Their bodily integrity is violated, and their autonomy is suppressed. Exit means severe personal and economic cost, often sacrificing career or social life.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, immediate, identity_locked, national).

% Government bodies responsible for public health, which issue and enforce mandates for medical interventions. From this reading's perspective, they act as extractors, leveraging state power to compel compliance, regardless of individual consent. Their legitimacy is derived from a public health mandate, which this reading contests as overriding individual rights.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Legal experts who analyze the constitutional and ethical implications of state-mandated medical interventions, often advocating for robust protections of individual rights and bodily autonomy. They observe the structural dynamics without direct participation in enforcement or compliance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% Medical professionals who are often required to administer mandated interventions or enforce compliance with mandates (e.g., verifying vaccination status). They face ethical dilemmas and potential professional penalties for non-compliance, making them targets of the state's coercive power, even as they may benefit from a healthier population.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, healthcare_providers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, healthcare_providers, beneficiary).

% Advocacy groups that champion individual rights and bodily autonomy, often challenging state mandates in court or through public discourse. From this reading's perspective, their arguments are often marginalized or dismissed by state authorities in favor of collective public health goals, making them 'excluded' from the decision-making process that imposes mandates.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_organizations, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint's *legitimate* coordination function is limited to facilitating voluntary medical interventions based on informed consent. State coercion, however, imposes a collective preference without genuine coordination, thereby undermining the very basis of ethical medical practice.
% TRANSFER_FUNCTION: Transfers individual bodily integrity and autonomy from mandate-coerced individuals to the state's public health goals, often through the leverage of employment, education, or access to public spaces.
% ABSENT_VOICES: Individuals who refuse mandates and civil liberties organizations advocating for them are often excluded from the policy-making process, their concerns dismissed as secondary to public health imperatives. They would argue for the primacy of individual rights and the unacceptability of coercion.
% DISAPPEARANCE_RATIONALE: If the state's power to coerce medical interventions vanished overnight, individuals would regain full autonomy over their bodies. Public health strategies would need to rely solely on voluntary participation, education, and incentives, fundamentally reorganizing the relationship between the state and its citizens regarding health decisions.
% FOUNDING_PROBLEM: The historical and ongoing challenge of balancing individual liberty and bodily autonomy with the collective good of public health, particularly during epidemics or health crises.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, civil liberties organizations, and historical legal precedents (e.g., Nuremberg Code, common law on informed consent) consistently attest to the live and contested nature of this foundational problem, often in opposition to state public health claims.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because individuals are compelled to undergo interventions against their will, sacrificing fundamental rights. Suppression is very high (0.90) due to the state's power to impose severe penalties for non-compliance, effectively trapping individuals. Theater ratio is low (0.10) because the enforcement of mandates is direct and functional, not performative; the state genuinely intends to compel compliance. Accessibility collapse is high (0.80) as alternatives to compliance are severely limited or carry prohibitive costs. Resistance is moderate (0.70) reflecting ongoing legal challenges and public protests against mandates.
 *
 * PERSPECTIVAL GAP:
 *   The state public health authorities perceive their actions as legitimate coordination for the collective good, while mandate-coerced individuals experience the same structure as pure extraction and suppression. This divergence is central to the kernel contest, with this reading emphasizing the individual's experience of coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Mandate-coerced individuals are the primary targets (high d), bearing the direct cost of violated autonomy and facing severe exit penalties. State public health authorities are the beneficiaries (low d), achieving their public health objectives through coercion. Healthcare providers are also targets, compelled to enforce mandates, but may also benefit from a healthier population. Civil liberties organizations are excluded, their arguments for individual rights often dismissed by the agenda-setters.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''bodily_autonomy_primary'' reading of the ''legitimate_health_intervention'' kernel?',
    'Comparison with canonical texts and legal interpretations that prioritize individual bodily autonomy in medical ethics and constitutional law.',
    'If misidentified, the analysis of the kernel contest would be flawed, potentially misrepresenting the structural differences between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated.').

omega_variable(
    coercion_severity_impact,
    'How does the severity of state coercion (e.g., job loss vs. minor fine) quantitatively impact the perceived extractiveness and suppression from the individual''s perspective?',
    'Empirical studies on the psychological and economic impact of different levels of mandate enforcement on affected individuals.',
    'Higher severity of coercion would increase the measured extractiveness and suppression, potentially shifting the classification further into the Snare category or intensifying its severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_severity_impact, empirical, 'Quantifies the impact of coercive measures on individual experience.').

omega_variable(
    public_benefit_vs_individual_harm,
    'From an objective standpoint, what is the measurable public health benefit achieved by state coercion, and how does it compare to the individual harm (violation of autonomy, economic loss) incurred?',
    'Independent epidemiological studies on the efficacy of mandates versus voluntary measures, coupled with economic and social impact assessments on affected individuals.',
    'While this reading explicitly disregards public benefit as a justification for coercion, an empirical finding of negligible public benefit would strengthen the Snare classification by removing any residual ''coordination'' cover story, even if rejected by this reading. Conversely, a massive, undeniable public benefit might highlight the tension with other readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_benefit_vs_individual_harm, empirical, 'Assesses the empirical trade-off between public benefit and individual harm, which this reading rejects as a valid justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 6, 0.11).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.1).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 18, 0.1).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.1).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
