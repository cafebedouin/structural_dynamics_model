% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Substance Control Legitimacy (Harm Reduction Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'harm reduction' reading of substance
 *   control legitimacy, where state authority is derived from a duty to
 *   minimize harm without resorting to criminalization. It frames substance
 *   use as a public health issue, leading to medicalization of users and
 *   diversion to treatment. While reducing the overt extraction of
 *   prohibition, it still involves moderate extractiveness through mandated
 *   treatment and persistent suppression of a black market. The claimed type
 *   is 'tangled_rope' because it genuinely coordinates public health efforts
 *   while still extracting costs from users and maintaining a degree of
 *   coercive control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.6).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Substance Control Legitimacy (Harm Reduction Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '8e509f94-2b0b-4fcb-925a-d6928e475774').
narrative_ontology:cs_kernel_codification('8e509f94-2b0b-4fcb-925a-d6928e475774', formalized).
narrative_ontology:cs_authority_grounding('8e509f94-2b0b-4fcb-925a-d6928e475774', lineage).
narrative_ontology:cs_interpretation_layer_present('8e509f94-2b0b-4fcb-925a-d6928e475774').
narrative_ontology:cs_reading_relation('8e509f94-2b0b-4fcb-925a-d6928e475774', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e509f94-2b0b-4fcb-925a-d6928e475774', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('8e509f94-2b0b-4fcb-925a-d6928e475774', foundational, substance_use_is_public_health_issue).
narrative_ontology:cs_axiom_status(substance_use_is_public_health_issue, holdable).
narrative_ontology:cs_axiom_grounding('8e509f94-2b0b-4fcb-925a-d6928e475774', substance_use_is_public_health_issue, empirically_contingent).
narrative_ontology:cs_axiom('8e509f94-2b0b-4fcb-925a-d6928e475774', foundational, state_duty_to_minimize_harm_without_criminalization).
narrative_ontology:cs_axiom_status(state_duty_to_minimize_harm_without_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('8e509f94-2b0b-4fcb-925a-d6928e475774', state_duty_to_minimize_harm_without_criminalization, deontological).
narrative_ontology:cs_reference_frame('8e509f94-2b0b-4fcb-925a-d6928e475774', public_health_first_approach).
narrative_ontology:cs_drift_state('8e509f94-2b0b-4fcb-925a-d6928e475774', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8e509f94-2b0b-4fcb-925a-d6928e475774', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers harm reduction programs, allocates funding for treatment, and advocates for non-criminalization policies. Benefits from expanded mandate and funding for public health approaches.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receives funding and referrals for substance use disorder treatment, including medication-assisted treatment and counseling. Benefits from the medicalization of substance use and increased demand for services.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    organized, biographical, mobile, local).

% Are directed into treatment programs, often under legal or social pressure, rather than facing criminal charges. Bear the costs of treatment, social stigma, and limited autonomy, but avoid incarceration. Still face a persistent black market for non-prescribed substances.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users, payer,
    powerless, immediate, identity_locked, local).

% Shifts focus from incarceration to diversion programs and treatment mandates, reducing prison populations but maintaining a role in enforcement for related offenses. Bears the cost of retraining and reorienting its mission.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system, payer).

% Continue to operate due to unmet demand for non-medicalized substances, but face ongoing enforcement pressure. Their existence undermines the public health approach by providing unregulated access.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_actors, excluded,
    organized, biographical, constrained, regional).

% Argue that harm reduction, while better than prohibition, still infringes on individual autonomy and fails to eliminate the black market. Their proposals for full legalization are not adopted under this reading.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, advocacy_groups_for_legalization, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state efforts to address substance use by framing it as a public health issue, directing resources towards treatment and prevention, and reducing reliance on criminalization, thereby aligning public health and justice systems.
% TRANSFER_FUNCTION: Transfers individuals from the criminal justice system to the public health system, shifting resources from incarceration to treatment and support services. It also transfers a degree of autonomy from substance users to public health authorities.
% ABSENT_VOICES: Advocacy groups for full legalization are largely excluded from the policy-making process under this reading, as their proposals go beyond the 'harm minimization without criminalization' framework. Black market actors are also excluded, as their activities are deemed illegitimate.
% DISAPPEARANCE_RATIONALE: If this harm reduction framework vanished, the vacuum would likely be filled by a return to more punitive prohibitionist policies, leading to increased incarceration rates for substance-related offenses, or a rapid, unregulated shift towards full legalization, with unpredictable public health consequences. The current system's balance would be lost.
% FOUNDING_PROBLEM: The punitive approach to substance use created significant social harms, including mass incarceration, disproportionate impact on marginalized communities, and a failure to address underlying health issues, while failing to reduce substance use effectively.
% FOUNDING_PROBLEM_CORROBORATION: Public health experts, social justice advocates, and many former criminal justice officials corroborate that the punitive approach was deeply problematic and that the harms it sought to address are still present, albeit mitigated by harm reduction. Data on incarceration rates and public health outcomes from jurisdictions adopting harm reduction policies provide further corroboration.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).
:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate: users are not criminalized but face mandated treatment and reduced autonomy. Suppression (0.6) is present through legal and social pressures to enter treatment, and continued enforcement against black market activities. Theater ratio (0.2) is low, as the public health and treatment functions are genuinely active, though the 'without criminalization' aspect is often imperfectly realized. Accessibility collapse (0.4) is moderate, as alternatives to state-sanctioned treatment (e.g., self-management, black market) are constrained but not entirely eliminated. Resistance (0.3) is moderate, coming from both users seeking full autonomy and black market actors.
 *
 * PERSPECTIVAL GAP:
 *   Public health agencies and treatment providers experience this as a legitimate and beneficial coordination mechanism, expanding their mandate and resources. Substance users, while avoiding criminalization, experience it as a coercive system that limits their autonomy and imposes costs, even if those costs are less severe than outright prohibition. The criminal justice system experiences a shift in its role and a reduction in its traditional enforcement powers, but maintains a presence in diversion and related offenses.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and treatment providers are beneficiaries (d near 0.0) as their roles and funding expand. Substance users are targets (d near 1.0) as they bear the costs of treatment and reduced autonomy. The criminal justice system is an agenda-setter with a mixed directionality, shifting from direct extraction to a more supportive but still controlling role. Black market actors are excluded and targeted by remaining enforcement, placing them at the high end of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling harm reduction as pure extraction (snare) by acknowledging its genuine coordination function in public health. However, it also prevents mislabeling it as pure coordination (rope) by highlighting the persistent, albeit reduced, extraction from substance users and the continued suppression of alternatives. The 'tangled_rope' classification captures the hybrid nature where a genuine public good (harm reduction) is delivered through a structure that still imposes significant costs and limits on a specific population, requiring active enforcement to maintain its boundaries against both prohibitionist and legalizationist pressures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treatment_mandate_coercion,
    'To what extent are ''voluntary'' treatment mandates truly voluntary, or do they function as a form of soft coercion, effectively replacing criminalization with medicalization as a control mechanism?',
    'Longitudinal studies tracking post-mandate outcomes, user satisfaction, and perceived autonomy, compared to truly voluntary treatment pathways. Analysis of legal frameworks for diversion programs.',
    'If mandates are found to be highly coercive, the effective suppression and extractiveness for substance users would be higher, pushing the classification closer to a Snare for that seat. If genuinely voluntary, it would reinforce the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_mandate_coercion, empirical, 'Ambiguity of ''voluntary'' treatment mandates.').

omega_variable(
    black_market_persistence,
    'Is the persistence of the black market under harm reduction a failure of the harm reduction model, or an inevitable consequence of not fully legalizing and regulating all substances?',
    'Comparative analysis with jurisdictions that have fully legalized and regulated substances, examining the size and nature of their illicit markets. Economic modeling of demand elasticity for regulated vs. unregulated substances.',
    'If persistence is due to the model''s limitations, it suggests the harm reduction reading cannot fully achieve its stated goals without further policy shifts, potentially increasing the ''theater_ratio'' as enforcement against the black market becomes performative. If inevitable, it highlights the inherent limits of any non-prohibitionist but non-legalizationist approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, conceptual, 'Role of black market in harm reduction efficacy.').

omega_variable(
    legitimacy_framing_contest,
    'Is the ''public health issue'' framing a genuine shift in state duty, or a rhetorical reframing to maintain state control over substance use without the political costs of explicit prohibition?',
    'Analysis of legislative intent, funding allocations, and enforcement practices over time. Examination of public discourse and expert consensus on the ''true'' nature of state authority in this domain.',
    'If primarily rhetorical, the ''extraction'' component of the constraint is higher than measured, as the coordination story is cover. If genuine, it reinforces the ''tangled_rope'' classification as a legitimate, albeit imperfect, coordination effort.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_framing_contest, conceptual, 'Rhetorical vs. substantive shift in state legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1980, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(subs_tr_t1990, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(subs_tr_t2000, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(subs_tr_t2010, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(subs_tr_t2020, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(subs_tr_t2024, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t1980, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(subs_be_t1990, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(subs_be_t2000, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(subs_be_t2010, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(subs_be_t2020, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement(subs_be_t2024, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1980, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(subs_su_t1990, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(subs_su_t2000, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(subs_su_t2010, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(subs_su_t2020, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(subs_su_t2024, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
