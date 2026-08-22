% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary Reading: Medical Coercion Impermissibility
 *   domain: medical_ethics/constitutional_law/public_health_policy
 *
 * SUMMARY:
 *   This constraint instantiates the bodily autonomy primary reading of the
 *   coercion legitimacy boundary kernel. The reading asserts that medical
 *   intervention without consent is categorically impermissible, regardless
 *   of collective benefit—that is, regardless of herd immunity thresholds,
 *   disease severity, or transmission dynamics. The constraint coordinates a
 *   foundational right (bodily integrity) by establishing it as prior to all
 *   public health claims. The structural cost is substantial:
 *   immunocompromised individuals who cannot be vaccinated are structurally
 *   exposed to transmission risk because the refusal principle protects those
 *   who could vaccinate but decline to do so. This reading differs
 *   fundamentally from the public_health_primary and proportionality_reading
 *   siblings, which subordinate autonomy to collective harm or scale coercion
 *   with disease severity. The claim/metric divergence is intentional: the
 *   constraint is CLAIMED as tangled rope (genuine coordination of a
 *   foundational right + asymmetric extraction from the unvaccinated
 *   vulnerable) while the metrics describe moderate-to-high extractiveness
 *   (the unvaccinated bear little cost while the immunocompromised bear the
 *   exposure), moderate suppression (courts actively suppress mandate
 *   attempts but do not suppress the refusal choice itself), and low theater
 *   (the enforcement is genuine non-coercion, not performative).
 *
 * KEY AGENTS:
 *   - individuals_refusing_medical_intervention: beneficiaries, retain choice veto protected by courts
 *   - autonomy_doctrine_holders: institutional beneficiaries, ground authority in the principle itself
 *   - immunocompromised_unprotected_by_herd_immunity: victims, structurally exposed to transmission
 *   - public_health_authorities: agenda_setters, prevented from using mandate tool
 *   - constitutional_courts: observers, enforce the autonomy boundary against mandate attempts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.41).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy Primary Reading: Medical Coercion Impermissibility").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "medical_ethics/constitutional_law/public_health_policy").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '13f65752-d2b5-44ce-997b-66e7f7e1a106').
narrative_ontology:cs_kernel_codification('13f65752-d2b5-44ce-997b-66e7f7e1a106', fixed_text).
narrative_ontology:cs_authority_grounding('13f65752-d2b5-44ce-997b-66e7f7e1a106', lineage).
narrative_ontology:cs_interpretation_layer_present('13f65752-d2b5-44ce-997b-66e7f7e1a106').
narrative_ontology:cs_reading_relation('13f65752-d2b5-44ce-997b-66e7f7e1a106', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_reading_relation('13f65752-d2b5-44ce-997b-66e7f7e1a106', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_axiom('13f65752-d2b5-44ce-997b-66e7f7e1a106', foundational, bodily_autonomy_foundational).
narrative_ontology:cs_axiom_status(bodily_autonomy_foundational, holdable).
narrative_ontology:cs_axiom_grounding('13f65752-d2b5-44ce-997b-66e7f7e1a106', bodily_autonomy_foundational, deontological).
narrative_ontology:cs_axiom('13f65752-d2b5-44ce-997b-66e7f7e1a106', foundational, state_cannot_override_autonomy_for_collective_benefit).
narrative_ontology:cs_axiom_status(state_cannot_override_autonomy_for_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('13f65752-d2b5-44ce-997b-66e7f7e1a106', state_cannot_override_autonomy_for_collective_benefit, deontological).
narrative_ontology:cs_reference_frame('13f65752-d2b5-44ce-997b-66e7f7e1a106', bodily_autonomy_as_limit_on_state_power).
narrative_ontology:cs_drift_state('13f65752-d2b5-44ce-997b-66e7f7e1a106', contemporary_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13f65752-d2b5-44ce-997b-66e7f7e1a106', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_refusing_medical_intervention).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, autonomy_doctrine_holders).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_unprotected_by_herd_immunity).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, populations_exposed_to_communicable_disease).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_institutions).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain the legal right to refuse any medical intervention without state coercion. The autonomy principle protects their choice regardless of public health consequences. They may face social stigma and institutional screening but no legal penalty or forced compliance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_refusing_medical_intervention, beneficiary,
    moderate, biographical, mobile, national).

% Medical, legal, and bioethical institutions that ground legitimacy in bodily autonomy as foundational. They gain institutional authority from adjudicating the principle; they gain predictability from its stable application; they avoid reputational cost of complicity in medical coercion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, autonomy_doctrine_holders, beneficiary,
    institutional, generational, arbitrage, national).

% Cannot be vaccinated or have reduced efficacy (cancer, transplant, HIV+ with low CD4). Depend on others' vaccination for protection. Under this reading, they have no recourse: vaccination is voluntary, and their vulnerability is the structural cost of protecting others' autonomy. Geographic exit does not help; the principle is built in.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_unprotected_by_herd_immunity, payer,
    powerless, biographical, trapped, national).

% Face elevated transmission risk when vaccination rates fall below herd immunity thresholds. They can pay for private healthcare, seek higher-vaccination jurisdictions, or bear the increased risk. None of these constitute genuine exit from the constraint itself.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, populations_exposed_to_communicable_disease, payer,
    organized, biographical, constrained, national).

% Must enforce non-coercion; they cannot issue legally enforceable vaccine mandates. They can recommend, educate, and incentivize, but the most epidemiologically effective tool (mandatory vaccination) is barred. They must maintain the institutional apparatus (courts, regulatory bodies) that blocks mandate attempts.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of maintaining informed consent infrastructure and patient choice documentation. They also gain liability protection by honoring refusals and documenting them. The cost (administrative burden, inability to mandate compliance) and benefit (legal safety) are institution-specific.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_institutions, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_institutions, beneficiary).

% Unvaccinated individuals whose choice to refuse is protected. They are excluded from the constraint's framing as 'parties' because the reading treats them as agents with protected choice, not as stakeholders whose interests conflict with others. They would argue for accommodations but the constraint does not engage with their case.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, disease_vector_individuals, excluded,
    moderate, biographical, identity_locked, national).

% Adjudicate challenges to medical mandates and enforce the bodily autonomy principle as a limit on state power. They would strike down any coercive intervention and block mandate attempts through judicial review. They are observers to the underlying constraint, not stakeholders in it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects bodily autonomy as a foundational right by establishing that the state may never compel medical intervention, even when the refusal creates public health risk. This solves a coordination problem between state police power and individual integrity: the principle subordinates collective benefit to the autonomy claim.
% TRANSFER_FUNCTION: Transfers the burden of disease prevention from individual refusers (who face no legal penalty) to vulnerable populations (who face exposure risk) and to public health authorities (who lose their most effective tool). It also transfers authority from health officials to individuals and courts.
% ABSENT_VOICES: Immunocompromised individuals are structurally present but lack standing to demand coercive intervention to protect themselves. Public health epidemiologists and vaccine researchers can testify but carry no legal force; their empirical case for mandates is excluded by the axiom.
% DISAPPEARANCE_RATIONALE: If bodily autonomy primacy disappeared, states could mandate medical intervention; informed consent would become advisory; the entire structure of medical law and bioethics would reorganize around compliance. Vulnerable populations would gain protection through mandate-enabled vaccination; individual refusers would lose their veto.
% FOUNDING_PROBLEM: Historical and ongoing medical coercion: forced sterilization, experimentation on prisoners and enslaved people, coercive treatment in detention and psychiatric systems. The principle was developed to prevent state and medical power from overriding bodily integrity.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethics commissions, constitutional law scholars, and survivors of coercive practices attest the problem is live. Public health authorities contest whether the principle should remain absolute when disease causes measurable harm; this disagreement is recorded testimony, not external corroboration of the founding problem's urgency.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.68 at interval end) reflects the asymmetry: individuals who refuse vaccination incur minimal cost (legal protection, some social stigma but no state penalty) while immunocompromised individuals incur high risk exposure. The rising trajectory (0.48→0.68) models increasing awareness of the herd immunity deficit and growing pressure on vulnerable populations as vaccine uptake falls below protective thresholds. Suppression is moderate and stable (0.41) because the constraint's operation does not suppress the refusal itself—rather, it suppresses mandate enforcement. Courts actively block mandate attempts (that is the enforcement action), not individuals choosing refusal. Theater is low (0.22) because informed consent processes and refusal documentation are functionally real, not performative—the constraint genuinely instantiates choice, it just assigns the cost asymmetrically. The measurement series is authored on a single time grid so every metric is present at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (individuals and autonomy institutions) and the victim seat (immunocompromised populations) experience fundamentally different constraint types from the same structure. From the beneficiary perspective, the arrangement is a foundational protection—a rope coordinating the right to bodily integrity. From the victim perspective, the same structure is extractive—a tangled rope that coordinates others' refusal rights at the cost of their own vulnerability. This divergence is the engine's computation of per-seat classification; the story does not resolve it but authoritatively states the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals refusing intervention occupy the beneficiary end of directionality (d near 0.0): they face no coercion, courts protect their choice, and institutional frameworks are built to honor it. Autonomy doctrine holders also benefit (institutional authority, coherence of the principle they adjudicate). Immunocompromised individuals occupy the target end (d near 1.0): they bear exposure cost, have trapped exit options (cannot move geographically to escape transmission), and face increasing risk as vaccination rates decline. The asymmetry is structural to the reading itself: the principle protects refusers from coercion while leaving vulnerable populations unprotected from the exposure that refusal creates. Public health authorities sit at moderate extraction (d around 0.5): they must enforce non-coercion (bearing institutional cost) while remaining unable to deploy the epidemiologically most effective tool.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not suffer mandatrophy in the sense of a dead founding problem persisting theatrically. The founding problem (preventing medical coercion) is live. However, the constraint exhibits an extraction-from-inaction pattern: the cost it imposes (on vulnerable populations) flows from what the constraint prevents (mandate enforcement) rather than from what it requires (it requires only non-interference). This is the reverse of traditional mandatrophy. The theater ratio remains low because the constraint's enforcement is real—courts genuinely block mandates, they do not perform blocking. The extractiveness rises as herd immunity falls, indicating the constraint's cost structure is sensitive to external conditions (vaccination rates, disease prevalence) rather than stable. This is diagnostically clean: the constraint genuinely exists (courts enforce it, refusal rights are respected) but its distribution of harms is asymmetric and grows as voluntary compliance declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immunocompromised_exposure_cost_quantification,
    'What is the actual additional mortality and morbidity risk imposed on immunocompromised populations when vaccination rates fall below herd immunity thresholds under non-enforcement of mandates?',
    'Epidemiological cohort studies comparing outcomes across jurisdictions with varying mandate policies and vaccination rates; disease surveillance data tracking immunocompromised infection rates.',
    'High quantified cost would establish the extraction as substantial and potentially unjustifiable within even the autonomy framework; low quantified cost would support the principle''s legitimacy. The magnitude of harm is empirically resolvable; the principle''s priority is not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_exposure_cost_quantification, empirical, 'The measurable health cost of the autonomy principle to unvaccinated vulnerable populations.').

omega_variable(
    axiom_foreclosure_vs_coexistence,
    'Within a single legal framework, can bodily autonomy be foundational (the reading''s axiom) AND can the state have authority to mandate medical intervention when collective benefit is at stake (public_health_primary axiom)?',
    'Jurisprudential analysis of constitutional doctrine: either case law establishes autonomy as an absolute limit on police power (foreclosure), or case law acknowledges the state''s competing authority and trades off between them (coexistence). Historical review of how courts have actually resolved these cases.',
    'True foreclosure means public_health_primary cannot be law-valid in any jurisdiction that adopts this reading—they are mutually exclusive readings of the same kernel. Coexistence means both readings can cohabitate as competing claims within a pluralistic legal system, which undermines the foundational claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_foreclosure_vs_coexistence, conceptual, 'Whether bodily autonomy primacy forecloses or coexists with public health authority.').

omega_variable(
    voluntary_compliance_sustainability,
    'Is voluntary vaccination uptake sufficient to maintain herd immunity for high-transmission diseases (measles R₀ ~12, COVID-19 R₀ ~2-3) without mandate enforcement?',
    'Long-term surveillance of vaccination rates in jurisdictions with strong autonomy protections and no mandates; game-theoretic modeling of incentives to free-ride on herd immunity.',
    'If voluntary compliance cannot sustain herd immunity, the constraint''s cost structure becomes unsustainable: immunocompromised populations face uncontrollable exposure. If voluntary compliance suffices, the constraint is compatible with population health.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_sustainability, empirical, 'Whether voluntary vaccination rates remain sufficient for herd immunity.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.41) purely structural (institutional barriers preventing mandate enforcement) or is it partially internalized in refusers'' identity-locked resistance to the legitimacy of coercion?',
    'Post-mandate thought experiments: if the principle were overturned and mandates became legal, would refusers accept them as legitimate (structural suppression) or continue to resist on principle even with legal force removed (internalized suppression)? Survey and interview data on whether refusal is contingent on autonomy protection or independent of it.',
    'High internalization would mean suppression persists even if the principle is formally abandoned; refusers would carry the suppression identity with them. Low internalization would mean suppression is contingent on legal enforcement of autonomy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of mandate enforcement is structural or identity-fused.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 5, 0.13).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 10, 0.16).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 15, 0.19).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.21).
narrative_ontology:measurement(coer_tr_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(coer_be_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(coer_su_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 25, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the coercion_legitimacy_boundary kernel. All three readings—bodily_autonomy_primary, proportionality_reading, and public_health_primary—share the same referent (what legitimizes medical coercion) but author different ε values and different beneficiary/victim structures because they instantiate different frameworks. Each reading has its own ε: autonomy_primary has moderate-to-high extraction (asymmetric cost to vulnerable populations), proportionality_reading has lower extraction (coercion scaled to disease severity), public_health_primary has lowest extraction (coercion justified by collective benefit). The three stories are linked via network.affects_constraints to enable comparative analysis of how reading choice determines classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__bodily_autonomy_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
