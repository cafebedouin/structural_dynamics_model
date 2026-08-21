% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Approach to Substance Use
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'harm reduction' reading of the substance
 *   control kernel. It frames substance use as a public health issue,
 *   prioritizing interventions to reduce negative consequences (e.g.,
 *   overdose, disease) over criminalizing individual users. While it reduces
 *   direct extraction from users by law enforcement, it maintains a degree of
 *   paternalistic intervention and leaves the illicit supply chain largely
 *   intact, leading to a hybrid coordination/extraction dynamic. The state
 *   shifts from purely punitive to a service provider role, but still exerts
 *   control over individuals and the market.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.6).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Approach to Substance Use").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, 'a40132ce-a64f-4583-a071-0d8bf932bbf4').
narrative_ontology:cs_kernel_codification('a40132ce-a64f-4583-a071-0d8bf932bbf4', formalized).
narrative_ontology:cs_authority_grounding('a40132ce-a64f-4583-a071-0d8bf932bbf4', expertise).
narrative_ontology:cs_interpretation_layer_present('a40132ce-a64f-4583-a071-0d8bf932bbf4').
narrative_ontology:cs_reading_relation('a40132ce-a64f-4583-a071-0d8bf932bbf4', substance_control_kernel__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('a40132ce-a64f-4583-a071-0d8bf932bbf4', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('a40132ce-a64f-4583-a071-0d8bf932bbf4', foundational, substance_use_is_health_condition).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('a40132ce-a64f-4583-a071-0d8bf932bbf4', substance_use_is_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('a40132ce-a64f-4583-a071-0d8bf932bbf4', foundational, pragmatic_harm_reduction_is_ethical_imperative).
narrative_ontology:cs_axiom_status(pragmatic_harm_reduction_is_ethical_imperative, holdable).
narrative_ontology:cs_axiom_grounding('a40132ce-a64f-4583-a071-0d8bf932bbf4', pragmatic_harm_reduction_is_ethical_imperative, deontological).
narrative_ontology:cs_reference_frame('a40132ce-a64f-4583-a071-0d8bf932bbf4', public_health_first_response).
narrative_ontology:cs_drift_state('a40132ce-a64f-4583-a071-0d8bf932bbf4', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a40132ce-a64f-4583-a071-0d8bf932bbf4', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, criminal_supply_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and advocate for harm reduction policies, providing services like needle exchanges, overdose prevention, and safe consumption sites. They benefit from a public health mandate and funding, but are constrained by political will and existing legal frameworks.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced risk of overdose, disease transmission, and criminalization for personal use. However, they remain subject to paternalistic health interventions and the dangers of a criminalized supply chain, bearing costs through stigma and limited access to safe supply.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, payer).

% Bear the costs of continued law enforcement efforts against drug production and distribution, even as individual users are decriminalized. They adapt to enforcement pressure, maintaining a dangerous and unregulated supply.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, criminal_supply_networks, payer,
    organized, biographical, constrained, global).

% Shifts focus from individual user arrests to targeting large-scale trafficking and production. They are constrained by evolving public policy and resource allocation, often resisting full decriminalization due to perceived threats to public order.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, national).

% Advocate for full legalization and regulation of substances, arguing that harm reduction, while beneficial, does not address the root causes of harm from criminalized supply. They are excluded from the core policy-making process of harm reduction, which maintains a criminalized supply.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, advocacy_groups_for_legalization, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health efforts to mitigate the negative health and social consequences of substance use, providing services and information to reduce harm for individuals and communities, while navigating existing legal frameworks.
% TRANSFER_FUNCTION: Transfers resources (funding, personnel) from criminal justice enforcement of individual use to public health services. It also transfers some risk from individual users to the state (e.g., through safe consumption sites) while maintaining a transfer of profits to criminal supply networks due to continued prohibition on production/distribution.
% ABSENT_VOICES: Advocacy groups for full legalization are absent from the core policy-making table, as their proposals for regulated supply chains fundamentally challenge the harm reduction framework's implicit acceptance of continued criminalization of production and distribution.
% DISAPPEARANCE_RATIONALE: If the harm reduction approach vanished overnight, there would be an immediate surge in overdose deaths, disease transmission, and criminal justice involvement for individual users. Public health infrastructure would collapse, and the social costs of substance use would dramatically increase, forcing a rapid reorganization of state and community responses.
% FOUNDING_PROBLEM: The 'War on Drugs' led to escalating rates of overdose, HIV/HCV transmission, and mass incarceration, with disproportionate impacts on marginalized communities, without reducing substance use.
% FOUNDING_PROBLEM_CORROBORATION: Public health data, medical professionals, and international health organizations consistently corroborate the ongoing public health crisis related to substance use, validating the continued need for harm reduction interventions. Law enforcement agencies also acknowledge the limitations of purely punitive approaches.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).
:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the reduction in direct criminal penalties for users, but acknowledges the continued costs of a criminalized supply and paternalistic health interventions. Suppression (0.6) is moderate, as individual users face less direct coercion but the illicit market remains suppressed. Theater ratio (0.2) is low, as harm reduction services are genuinely functional, though some 'performance' may exist in balancing public health goals with political pressures to maintain some level of prohibition. Accessibility collapse (0.4) is moderate, as some alternatives (e.g., safe supply) are still constrained, but others (e.g., needle exchanges) are more accessible. Resistance (0.3) is present from both those advocating for full legalization and those resisting any decriminalization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health agencies, this is a necessary and beneficial coordination mechanism. From the perspective of people who use drugs, it's a mixed bag: relief from criminalization but continued control and an unsafe market. From the perspective of legalization advocates, it's a 'tangled rope' that fails to address the fundamental problem of prohibition.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies are beneficiaries (damped extraction) as they gain a clear mandate and resources. People who use drugs are both beneficiaries (reduced criminalization, access to services) and payers (continued stigma, unsafe supply, paternalistic oversight). Criminal supply networks are payers (continued enforcement pressure). Law enforcement is an agenda-setter, shifting its focus but still maintaining control. Advocacy groups for legalization are excluded, as their vision is not fully integrated into this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_chain_criminalization_impact,
    'To what extent does the continued criminalization of the substance supply chain undermine the harm reduction goals for individual users?',
    'Comparative analysis of jurisdictions with varying degrees of supply chain decriminalization/legalization on overdose rates, purity, and access to safe supply.',
    'If criminalization significantly undermines harm reduction, the effective extractiveness on users is higher than measured, and the constraint leans more towards a Snare due to the unaddressed root cause of harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_criminalization_impact, empirical, 'Impact of criminalized supply on harm reduction efficacy.').

omega_variable(
    paternalism_vs_autonomy,
    'Is the ''paternalistic health intervention'' aspect of harm reduction an acceptable trade-off for reduced criminalization, or does it constitute a new form of control over people who use drugs?',
    'Qualitative research on the lived experiences of people who use drugs under harm reduction regimes, focusing on perceived autonomy and agency.',
    'If perceived as a new form of control, the effective extractiveness on users is higher, and the constraint''s coordination function is weaker, pushing it closer to a Snare or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy, conceptual, 'Balancing state intervention with individual autonomy in harm reduction.').

omega_variable(
    political_economy_of_harm_reduction,
    'Does the harm reduction framework, by not challenging the fundamental prohibitionist paradigm, inadvertently stabilize the political economy of criminalized substances, benefiting certain state actors or criminal enterprises?',
    'Political economic analysis of funding flows, institutional power dynamics, and market structures under harm reduction policies, compared to full legalization or strict prohibition.',
    'If it stabilizes the criminalized political economy, the constraint''s ''beneficiaries'' are more diffuse and hidden, and its overall extractiveness is higher, potentially reclassifying it as a Snare from a broader systemic view.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_economy_of_harm_reduction, empirical, 'Systemic effects of harm reduction on the political economy of drugs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__harm_reduction_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__harm_reduction_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__harm_reduction_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__harm_reduction_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__harm_reduction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__harm_reduction_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'harm_reduction_reading' of the 'substance_control_kernel', which also includes 'prohibition_reading' and 'legalization_reading'. Each reading represents a distinct structural claim about how substance use is governed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
