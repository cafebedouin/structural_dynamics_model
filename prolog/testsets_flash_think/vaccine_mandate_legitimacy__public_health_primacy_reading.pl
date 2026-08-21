% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy (Public Health Primacy Reading)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'public health primacy' reading of vaccine
 *   mandate legitimacy, where the state's duty to prevent collective harm
 *   justifies mandate authority, and unvaccinated status is framed as an
 *   externality. It is one reading of the 'vaccine_mandate_legitimacy'
 *   kernel, which is contested by 'bodily_autonomy_primacy_reading' and
 *   'risk_stratification_reading'. This reading asserts a strong state role
 *   in public health, leading to high extraction from and suppression of
 *   those who do not comply with mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.78).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.85).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '64de43fb-6626-4277-8acb-617414b78a99').
narrative_ontology:cs_kernel_codification('64de43fb-6626-4277-8acb-617414b78a99', formalized).
narrative_ontology:cs_authority_grounding('64de43fb-6626-4277-8acb-617414b78a99', lineage).
narrative_ontology:cs_interpretation_layer_present('64de43fb-6626-4277-8acb-617414b78a99').
narrative_ontology:cs_reading_relation('64de43fb-6626-4277-8acb-617414b78a99', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('64de43fb-6626-4277-8acb-617414b78a99', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('64de43fb-6626-4277-8acb-617414b78a99', foundational, collective_welfare_primacy).
narrative_ontology:cs_axiom_status(collective_welfare_primacy, holdable).
narrative_ontology:cs_axiom_grounding('64de43fb-6626-4277-8acb-617414b78a99', collective_welfare_primacy, deontological).
narrative_ontology:cs_axiom('64de43fb-6626-4277-8acb-617414b78a99', foundational, unvaccinated_status_as_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_as_externality, holdable).
narrative_ontology:cs_axiom_grounding('64de43fb-6626-4277-8acb-617414b78a99', unvaccinated_status_as_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('64de43fb-6626-4277-8acb-617414b78a99', state_police_power_doctrine).
narrative_ontology:cs_drift_state('64de43fb-6626-4277-8acb-617414b78a99', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('64de43fb-6626-4277-8acb-617414b78a99', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, general_public).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_systems).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting collective health, they assert the state's duty to prevent collective harm justifies mandates. They gain authority and legitimacy from the successful implementation of public health measures, and control over public health policy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from reduced disease transmission, lower healthcare burden, and a sense of collective safety and order. They generally comply with mandates, seeing them as a necessary measure for societal well-being.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Bear the direct costs of mandates, facing restrictions on employment, travel, and access to public spaces. Their choices are limited to compliance, social/economic exclusion, or active resistance, often leading to significant personal and professional disruption.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals, payer,
    powerless, immediate, constrained, local).

% Actively resist mandates on grounds of individual rights and bodily autonomy. They bear the costs of litigation, public advocacy, and reputational damage, but their analytical exit allows them to articulate alternative framings and challenge the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_advocates, payer,
    organized, generational, analytical, national).

% Benefit from reduced patient load, increased operational stability, and protection of healthcare workers. They are often instrumental in implementing and enforcing mandates, aligning with public health goals.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Implement mandates to ensure workplace safety, reduce liability, and maintain business continuity. They benefit from a healthier workforce and reduced operational disruptions, but also bear the costs of enforcement and potential employee attrition.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_institutions, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_institutions, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate the spread of infectious disease, ensuring public safety and reducing strain on healthcare infrastructure by asserting state authority over individual health choices when collective harm is present.
% TRANSFER_FUNCTION: Transfers individual autonomy and choice regarding medical interventions to state authority, in exchange for collective health benefits and reduced societal risk. The unvaccinated bear the direct costs of this transfer.
% ABSENT_VOICES: Those who prioritize absolute medical self-sovereignty or who believe mandates are disproportionate to the actual risk are actively suppressed or excluded from the policy-making conversation, often relegated to protest movements or legal challenges.
% DISAPPEARANCE_RATIONALE: If the state's authority to mandate vaccines for collective harm vanished, public health responses to future pandemics would be severely hampered, leading to fragmented and less effective disease control. Individual choices would dominate, potentially increasing disease burden and societal disruption, forcing a complete reorganization of public health policy and emergency response.
% FOUNDING_PROBLEM: The problem of managing infectious diseases that pose a significant threat to collective health, where individual actions (or inactions) create negative externalities for the broader population.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations, medical professionals, and international health bodies consistently attest to the ongoing threat of infectious diseases and the necessity of collective action. This is corroborated by epidemiological data and historical public health crises, from outside the immediate beneficiaries of mandate authority.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because mandates impose significant costs on the unvaccinated, limiting their freedoms and economic opportunities. Suppression is very high (0.85) due to the coercive nature of state mandates, backed by legal and institutional enforcement, which actively limits alternatives for non-compliance. Theater ratio is low (0.15) as the public health function is genuinely pursued, though its methods are contested. Accessibility collapse is moderate-high (0.70) as alternatives to compliance are severely restricted. Resistance is high (0.75) reflecting significant public and legal challenges to mandates.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and the general public perceive this constraint as a necessary and legitimate 'rope' for collective well-being, where the benefits of coordination outweigh individual costs. Unvaccinated individuals and civil liberties advocates, however, experience it as a 'snare' or 'tangled_rope' due to the high extraction of autonomy and severe suppression of choice, viewing the collective harm justification as cover for state overreach.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and healthcare systems are primary beneficiaries, gaining enhanced authority and operational stability (low d). The general public is also a beneficiary, receiving collective health protection (low d). Unvaccinated individuals are clear targets, bearing direct costs and facing severe restrictions (high d). Civil liberties advocates are also targets, as their core principles are challenged and they expend resources in resistance (high d). Employers and institutions act as agenda-setters and beneficiaries, enforcing mandates for their own operational stability.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate is actively asserted and enforced in response to a perceived live problem (infectious disease externalities). The contest is over its legitimacy and proportionality, not its function's atrophy. The classification as a Tangled Rope reflects the genuine coordination function (public health) combined with asymmetric extraction (from the unvaccinated) and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_quantification_ambiguity,
    'Is the ''unvaccinated status as externality'' claim accurately quantified, and does it justify the magnitude of state intervention?',
    'Independent epidemiological modeling and cost-benefit analysis comparing the actual risk posed by unvaccinated individuals to the societal costs of mandates, considering varying levels of transmissibility and severity.',
    'If the externality is found to be minor or disproportionate to the mandates, the justification for high extraction and suppression weakens, potentially reclassifying the constraint closer to a Snare. If robustly quantified and significant, it strengthens the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_quantification_ambiguity, empirical, 'Uncertainty regarding the empirical basis and proportionality of the externality claim.').

omega_variable(
    state_duty_scope_ambiguity,
    'What are the legitimate boundaries of the state''s duty to prevent collective harm, and at what point does it infringe on fundamental individual rights?',
    'Constitutional jurisprudence and public discourse establishing clear legal precedents and societal consensus on the balance between collective welfare and individual autonomy in public health crises.',
    'A narrower interpretation of state duty would reduce the perceived legitimacy of mandates, shifting the constraint towards a Snare. A broader interpretation would reinforce the current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_duty_scope_ambiguity, conceptual, 'Conceptual ambiguity regarding the scope of state authority versus individual rights.').

omega_variable(
    reading_legitimacy_contest,
    'Is this ''public health primacy'' reading the most legitimate interpretation of vaccine mandate authority, or do sibling readings offer superior frameworks?',
    'Ongoing legal challenges, public debate, and evolving scientific understanding that may shift societal consensus towards ''bodily_autonomy_primacy_reading'' or ''risk_stratification_reading''.',
    'If a sibling reading gains dominance, this constraint would be reclassified or superseded, reflecting a fundamental shift in the underlying commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, preference, 'This constraint is one reading of the ''vaccine_mandate_legitimacy'' kernel, contested by alternative framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(vacc_be_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2, 0.7).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 4, 0.75).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 6, 0.77).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 8, 0.78).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vacc_su_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2, 0.75).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 4, 0.8).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
