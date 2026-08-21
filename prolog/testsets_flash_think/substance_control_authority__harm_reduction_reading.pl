% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: Harm Reduction Public Health Policy for Substance Use
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the state's authority to implement harm
 *   reduction policies for substance use, shifting from a punitive criminal
 *   justice approach to a public health model. This involves
 *   decriminalization, providing health services, and minimizing associated
 *   harms, while not fully legalizing or regulating drug markets. It is one
 *   reading of the broader 'substance_control_authority' kernel, distinct
 *   from prohibitionist or full legalization approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.45).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "Harm Reduction Public Health Policy for Substance Use").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, 'a7466e03-c796-4feb-9097-7b35c45dce41').
narrative_ontology:cs_kernel_codification('a7466e03-c796-4feb-9097-7b35c45dce41', formalized).
narrative_ontology:cs_authority_grounding('a7466e03-c796-4feb-9097-7b35c45dce41', expertise).
narrative_ontology:cs_interpretation_layer_present('a7466e03-c796-4feb-9097-7b35c45dce41').
narrative_ontology:cs_reading_relation('a7466e03-c796-4feb-9097-7b35c45dce41', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('a7466e03-c796-4feb-9097-7b35c45dce41', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('a7466e03-c796-4feb-9097-7b35c45dce41', foundational, public_health_priority).
narrative_ontology:cs_axiom_status(public_health_priority, holdable).
narrative_ontology:cs_axiom_grounding('a7466e03-c796-4feb-9097-7b35c45dce41', public_health_priority, empirically_contingent).
narrative_ontology:cs_axiom('a7466e03-c796-4feb-9097-7b35c45dce41', foundational, individual_autonomy_and_dignity).
narrative_ontology:cs_axiom_status(individual_autonomy_and_dignity, holdable).
narrative_ontology:cs_axiom_grounding('a7466e03-c796-4feb-9097-7b35c45dce41', individual_autonomy_and_dignity, deontological).
narrative_ontology:cs_reference_frame('a7466e03-c796-4feb-9097-7b35c45dce41', public_health_governance_model).
narrative_ontology:cs_drift_state('a7466e03-c796-4feb-9097-7b35c45dce41', contemporary_policy_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a7466e03-c796-4feb-9097-7b35c45dce41', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, drug_users).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_systems).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, communities).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, taxpayers).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, criminal_justice_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops, implements, and oversees harm reduction policies, including decriminalization, safe consumption sites, and needle exchange programs. Justifies these policies based on public health evidence and human rights principles. Bears the administrative and political costs of policy implementation and public debate.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, state_public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from reduced criminal penalties, access to health services, and overdose prevention. Still bears the health harms of drug use, social stigma, and potential barriers to accessing comprehensive care. Their options are limited by the availability and accessibility of services.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, drug_users, beneficiary,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, drug_users, payer).

% Benefits from reduced drug-related crime, improved public safety, and decreased disease transmission rates. Bears residual social harms, public nuisance issues, and the costs associated with increased public health service provision. Their ability to exit is limited by geographic ties and collective action problems.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, communities, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, communities, payer).

% Experiences a reduced burden of drug-related arrests and incarceration, freeing up resources. However, it also faces a shift in its traditional role and potential funding reallocations, leading to internal resistance to policy changes. Its exit options are constrained by legislative mandates.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, criminal_justice_system, payer,
    institutional, biographical, constrained, national).

% Bears the financial cost of funding public health interventions, treatment programs, and social services associated with harm reduction policies. Their ability to influence policy is through democratic processes and advocacy.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, taxpayers, payer,
    moderate, biographical, mobile, national).

% Strongly objects to harm reduction policies, viewing them as normalizing drug use, undermining public morality, and increasing social disorder. They are excluded from the policy-making process for harm reduction, but actively lobby against it. Their identity is deeply tied to a punitive approach to drug control.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, identity_locked, national).

% Argues that harm reduction does not go far enough, maintaining black markets and stigma by not fully legalizing and regulating drug commerce. They are excluded from the current harm reduction policy framework but advocate for a more comprehensive market-based approach.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health, social services, and law enforcement to manage substance use as a health issue, reducing criminal justice involvement and mitigating associated health and social harms.
% TRANSFER_FUNCTION: Transfers resources from criminal justice enforcement to public health and social services; transfers reduced criminal penalties and health support to drug users; transfers some social costs (e.g., crime) from the criminal justice system to public health management.
% ABSENT_VOICES: Prohibition advocates would argue that harm reduction normalizes drug use and increases social disorder. Legalization advocates would argue that it maintains stigma and black markets by not fully regulating drug commerce.
% DISAPPEARANCE_RATIONALE: If harm reduction policies vanished, drug use would revert to being primarily a criminal justice issue, leading to increased incarceration, higher rates of disease transmission, overdose deaths, and social disorder, requiring a complete reorganization of state response and significant social costs.
% FOUNDING_PROBLEM: The failure of punitive prohibition to reduce drug use or associated harms, leading to mass incarceration, public health crises (e.g., HIV, hepatitis, overdose epidemics), and destabilized communities, while failing to address the root causes of addiction.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations, medical professionals, international bodies (e.g., WHO, UNODC reports), and independent academic research consistently corroborate the failures of prohibition and the ongoing benefits of harm reduction approaches in mitigating these problems.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).
:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) reflects the costs borne by taxpayers for public health services and the residual social harms in communities, even as criminal justice extraction is reduced. Suppression (0.45) is moderate; while criminal penalties are reduced, social stigma, barriers to service access, and the need to enforce policy boundaries (e.g., where decriminalization ends) still exert pressure. Theater ratio is low (0.15) as the focus is on evidence-based interventions rather than performative enforcement. Accessibility collapse (0.6) is moderate, as alternatives to harmful use are more available than under prohibition but not universally accessible. Resistance (0.5) is present from both prohibitionist and legalization camps.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this is a necessary and effective coordination mechanism to manage a complex social problem. From the perspective of prohibition advocates, it is a dangerous loosening of control. From legalization advocates, it is an insufficient half-measure. The engine will compute these divergences based on the structural positions and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Drug users and public health systems are primary beneficiaries, experiencing reduced criminalization and improved health outcomes. Communities are mixed, benefiting from reduced crime but bearing some residual social harms and service costs. Taxpayers and the criminal justice system are payers, funding services and adapting to new roles. Prohibition and legalization advocates are excluded, as their core premises are not fully adopted by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope acknowledges both the genuine coordination function (public health management, harm reduction) and the asymmetric extraction (taxpayer funding, residual community harms). It prevents mislabeling it as a pure Rope (ignoring costs) or a Snare (ignoring benefits and coordination), reflecting its complex, often contested, nature as a policy intervention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_cost_of_services,
    'What is the actual marginal cost of providing harm reduction services (e.g., safe supply, treatment) versus the public health benefits achieved?',
    'Longitudinal studies and cost-benefit analyses comparing health outcomes and societal costs in jurisdictions with robust harm reduction programs versus those without.',
    'If costs significantly outweigh benefits, the extractiveness of the policy is higher than currently estimated, potentially shifting the classification towards a more extractive type. If benefits far outweigh costs, the extractiveness is lower, strengthening the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_cost_of_services, empirical, 'Empirical assessment of the economic efficiency of harm reduction interventions.').

omega_variable(
    boundary_with_legalization,
    'At what point does ''accepting drug use while minimizing harms'' transition into ''regulating drug markets as legal commerce''?',
    'Conceptual analysis of policy frameworks, legal definitions, and market structures in jurisdictions that have implemented both harm reduction and some form of legalization.',
    'A clearer boundary would help distinguish the specific coordination and extraction mechanisms unique to harm reduction, preventing conflation with full market regulation. Ambiguity could lead to misattribution of costs or benefits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_with_legalization, conceptual, 'Defining the conceptual and practical boundary between harm reduction and full legalization.').

omega_variable(
    social_acceptance_vs_normalization,
    'Does the public acceptance of harm reduction policies lead to an undesirable normalization of drug use, or does it merely reduce stigma and encourage help-seeking?',
    'Sociological studies on public attitudes, drug use initiation rates, and treatment seeking behaviors in areas with long-standing harm reduction policies.',
    'If normalization leads to increased use and associated harms, the policy''s effective extractiveness (social costs) is higher. If it primarily reduces stigma and improves public health, the policy''s coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_acceptance_vs_normalization, empirical, 'Impact of harm reduction on social norms around drug use.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine, distinct reading of ''substance_control_authority'', or is it merely an intermediate stage towards ''legalization_reading'' or a less punitive ''prohibition_reading''?',
    'Analysis of policy stability and stated long-term goals in jurisdictions implementing harm reduction. If policies consistently remain at this stage without progressing to full legalization or reverting to prohibition, it supports its distinct identity.',
    'If it''s a distinct reading, its classification stands. If it''s merely a transitional stage, its long-term stability and independent function are questionable, potentially affecting its classification as a stable ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing harm reduction as a stable policy reading versus a transitional phase.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__harm_reduction_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__harm_reduction_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(subs_tr_t18, substance_control_authority__harm_reduction_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__harm_reduction_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(subs_tr_t30, substance_control_authority__harm_reduction_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__harm_reduction_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__harm_reduction_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(subs_be_t18, substance_control_authority__harm_reduction_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__harm_reduction_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(subs_be_t30, substance_control_authority__harm_reduction_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__harm_reduction_reading, suppression_requirement, 6, 0.41).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__harm_reduction_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(subs_su_t18, substance_control_authority__harm_reduction_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__harm_reduction_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(subs_su_t30, substance_control_authority__harm_reduction_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, criminal_justice_system_funding).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, public_health_funding).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, drug_market_dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
