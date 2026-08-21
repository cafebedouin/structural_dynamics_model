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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Health Interventions
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'legitimate_health_intervention' kernel. It asserts that informed
 *   consent is a non-negotiable prerequisite for medical interventions, and
 *   that state coercion, regardless of public benefit, constitutes a
 *   violation of bodily integrity. This reading places individuals facing
 *   mandates into the victim set, and the state (when it coerces) into the
 *   extractor role. The metrics reflect a moderate-to-high extractiveness due
 *   to the perceived violation of rights, and significant resistance from
 *   those who uphold this principle.
 *
 * KEY AGENTS:
 *   - individuals_asserting_autonomy: Primary beneficiary (moderate/constrained) — benefits from the protection of their right.
 *   - mandate_coerced_individuals: Primary target (powerless/identity_locked) — bears the extraction of bodily violation or severe costs of non-compliance.
 *   - state_public_health_authorities: Primary target (institutional/constrained) — bears the cost of curtailed public health efficacy.
 *   - medical_professionals: Agenda setter (organized/constrained) — enforces the consent requirement.
 *   - public_health_advocates: Excluded (organized/constrained) — would object to this framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.7).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.6).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.6).
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
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '00396df7-5e0c-4edb-89c9-55e6d49f9079').
narrative_ontology:cs_kernel_codification('00396df7-5e0c-4edb-89c9-55e6d49f9079', formalized).
narrative_ontology:cs_authority_grounding('00396df7-5e0c-4edb-89c9-55e6d49f9079', lineage).
narrative_ontology:cs_interpretation_layer_present('00396df7-5e0c-4edb-89c9-55e6d49f9079').
narrative_ontology:cs_reading_relation('00396df7-5e0c-4edb-89c9-55e6d49f9079', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('00396df7-5e0c-4edb-89c9-55e6d49f9079', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('00396df7-5e0c-4edb-89c9-55e6d49f9079', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('00396df7-5e0c-4edb-89c9-55e6d49f9079', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('00396df7-5e0c-4edb-89c9-55e6d49f9079', foundational, informed_consent_non_derogable).
narrative_ontology:cs_axiom_status(informed_consent_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('00396df7-5e0c-4edb-89c9-55e6d49f9079', informed_consent_non_derogable, deontological).
narrative_ontology:cs_reference_frame('00396df7-5e0c-4edb-89c9-55e6d49f9079', post_nuremberg_ethical_consensus).
narrative_ontology:cs_drift_state('00396df7-5e0c-4edb-89c9-55e6d49f9079', contemporary_pandemic_response, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('00396df7-5e0c-4edb-89c9-55e6d49f9079', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, individuals_asserting_autonomy).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from the principle that their bodily integrity cannot be violated without informed consent, even if it means foregoing public health measures. Their 'benefit' is the protection of a right, which often comes at the cost of social or economic exclusion if they refuse mandates.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, individuals_asserting_autonomy, beneficiary,
    moderate, biographical, constrained, local).

% Individuals who face direct or indirect coercion (e.g., loss of employment, access to services) if they do not comply with medical interventions. They bear the direct cost of bodily violation or the severe cost of non-compliance. Their identity is locked into their bodily autonomy, making 'exit' from this position a violation of self.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, immediate, identity_locked, local).

% These authorities are constrained by the requirement for informed consent, even when they believe population-level health benefits are at stake. They bear the 'cost' of reduced efficacy for public health measures that rely on broad compliance, and face legal challenges if they overstep. They are victims in the sense that their mandate to protect public health is curtailed.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, state_public_health_authorities, payer,
    institutional, generational, constrained, national).

% Bound by ethical codes requiring informed consent, they must navigate the tension between individual autonomy and public health directives. They are the primary enforcers of the consent requirement at the point of care.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medical_professionals, agenda_setter,
    organized, biographical, constrained, local).

% Advocate for policies that prioritize population health outcomes, often viewing individual refusal as an externality. They are excluded from the decision-making process when bodily autonomy is held as an absolute, and would argue for a different balance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates medical practice around a foundational ethical principle, ensuring that all interventions respect individual self-determination and prevent medical paternalism.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over one's body from external authorities (state, medical establishment) to the individual, at the potential cost of collective health outcomes.
% ABSENT_VOICES: Public health advocates and those who prioritize collective well-being are often excluded from the conversation when bodily autonomy is framed as an absolute, arguing that individual choices have population-level consequences.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, medical ethics would fundamentally shift, potentially allowing for state-mandated interventions without consent, leading to a complete reorganization of patient rights, public health policy, and the relationship between individuals and the state regarding health.
% FOUNDING_PROBLEM: The historical problem of medical paternalism, non-consensual experimentation, and state-sanctioned bodily violations, particularly in vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Legal precedents (e.g., Nuremberg Code, Belmont Report), patient advocacy groups, and international human rights organizations consistently corroborate the ongoing need to protect individual autonomy against potential state or medical overreach. This is attested by sources outside the direct beneficiaries of the constraint.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.7) because any coerced intervention is seen as a fundamental violation, regardless of its medical efficacy. Suppression (0.6) is present through the social and economic pressures applied to enforce mandates. Resistance (0.75) is high due to the deeply held nature of bodily autonomy. The claimed type is 'snare' because the coordination story (public health) is seen as a cover for extraction (violation of individual rights) when coercion is applied, and identifiable victims exist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mandate-coerced individuals, the constraint operates as a snare, extracting their bodily autonomy. From the perspective of state public health authorities, it operates as a rope, coordinating ethical medical practice, but also as a constraint on their ability to achieve population-level health goals. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals asserting autonomy are beneficiaries (d near 0.0) as the constraint protects their rights. Mandate-coerced individuals are full targets (d near 1.0) as they bear the direct cost of bodily violation or severe consequences for refusal. State public health authorities are also targets (d near 0.8) as their public health mandate is curtailed. Medical professionals are agenda-setters (d near 0.5) balancing ethical duties.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling state coercion as mere coordination. By identifying mandate-coerced individuals as victims and the state as an extractor (when it coerces), it highlights the extractive nature of interventions that violate informed consent, even if they claim public benefit. The constraint's mandate (protecting autonomy) is seen as live, but its application in a coercive context reveals a snare-like structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_threshold_ambiguity,
    'At what point does ''strong encouragement'' or ''incentivization'' cross the line into ''coercion'' that violates bodily integrity?',
    'Legal precedent, ethical consensus, and empirical studies on perceived freedom of choice under various pressures. A clear definition of ''undue influence'' in medical contexts.',
    'If the threshold for coercion is low, more state actions would be classified as extractive snares. If high, more actions would be seen as legitimate coordination, shifting the classification towards a rope or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_threshold_ambiguity, conceptual, 'Ambiguity in defining coercion in health interventions.').

omega_variable(
    public_benefit_weighting,
    'Does the magnitude of public health benefit ever justify a limited infringement on individual bodily autonomy, and if so, how is that magnitude measured and balanced?',
    'Societal consensus, legislative action, and judicial review establishing a framework for balancing individual rights against collective goods. This is the core contest with the ''public_health_primary'' and ''proportionality_reading'' siblings.',
    'If public benefit is given any weight, the extractiveness of state action might be re-evaluated downward, potentially shifting the constraint towards a tangled rope or even a rope. If it holds no weight, the snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_benefit_weighting, preference, 'The role of public benefit in justifying autonomy infringements.').

omega_variable(
    identity_lock_mechanism,
    'Is the ''identity_locked'' exit option for mandate-coerced individuals primarily due to professional identity (career path dependence), relational identity (self-concept constituted through the relationship), ideological identity (worldview that makes exit unthinkable), or institutional identity (the organization has ''become'' its function)?',
    'Qualitative sociological studies and individual testimony exploring the specific mechanisms of identity fusion and the perceived costs of non-compliance. This would differentiate the nature of the ''lock''.',
    'If the lock is primarily ideological, the resistance to mandates is more fundamental and less amenable to material incentives. If it''s professional, economic remedies might be more effective. The classification itself might not change, but the resolution path would.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Specific mechanism binding identity-locked agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
