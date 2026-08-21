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
 *   control kernel. It frames substance use as a health condition requiring
 *   pragmatic intervention to reduce negative consequences, independent of
 *   use cessation. This shifts the primary focus from criminal punishment of
 *   users to public health services, while still maintaining some level of
 *   state control and enforcement against the illicit supply chain. The
 *   claimed type is 'tangled_rope' because it genuinely coordinates public
 *   health benefits for users while still extracting costs through continued
 *   criminalization of supply and paternalistic interventions.
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
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '624d6f02-758e-460d-b2d4-5649c26a4ea6').
narrative_ontology:cs_kernel_codification('624d6f02-758e-460d-b2d4-5649c26a4ea6', formalized).
narrative_ontology:cs_authority_grounding('624d6f02-758e-460d-b2d4-5649c26a4ea6', expertise).
narrative_ontology:cs_interpretation_layer_present('624d6f02-758e-460d-b2d4-5649c26a4ea6').
narrative_ontology:cs_reading_relation('624d6f02-758e-460d-b2d4-5649c26a4ea6', substance_control_kernel__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('624d6f02-758e-460d-b2d4-5649c26a4ea6', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('624d6f02-758e-460d-b2d4-5649c26a4ea6', foundational, substance_use_is_health_condition).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('624d6f02-758e-460d-b2d4-5649c26a4ea6', substance_use_is_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('624d6f02-758e-460d-b2d4-5649c26a4ea6', foundational, harm_reduction_is_ethical_imperative).
narrative_ontology:cs_axiom_status(harm_reduction_is_ethical_imperative, holdable).
narrative_ontology:cs_axiom_grounding('624d6f02-758e-460d-b2d4-5649c26a4ea6', harm_reduction_is_ethical_imperative, deontological).
narrative_ontology:cs_reference_frame('624d6f02-758e-460d-b2d4-5649c26a4ea6', public_health_first_approach).
narrative_ontology:cs_drift_state('624d6f02-758e-460d-b2d4-5649c26a4ea6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('624d6f02-758e-460d-b2d4-5649c26a4ea6', '').
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

% Bear the costs of continued law enforcement efforts against drug production and distribution, even as individual users are decriminalized. They adapt to enforcement pressure, maintaining a dangerous and profitable black market.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, criminal_supply_networks, payer,
    organized, biographical, constrained, global).

% Shifts focus from individual user arrests to targeting large-scale drug trafficking. They maintain enforcement capacity but face pressure to reallocate resources and adapt to a public health-oriented approach, which can be resisted due to established institutional norms.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, national).

% Advocate for strict criminal penalties for all substance use, viewing harm reduction as condoning drug use. Their moral framework is marginalized in this reading, but they continue to exert political pressure against policy expansion.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health efforts to mitigate the negative consequences of substance use (overdose, disease, criminalization) by providing services and shifting legal focus, while maintaining some control over illicit markets.
% TRANSFER_FUNCTION: Transfers resources from criminal justice enforcement (for users) to public health services, and transfers some risk from individual users to the state (via safe consumption sites, naloxone distribution). It also transfers profits to criminal supply networks by maintaining prohibition on production/distribution.
% ABSENT_VOICES: Advocates for full legalization, who would argue that the harm reduction approach, while an improvement, still perpetuates a criminalized supply chain and paternalistic state control, are largely excluded from the policy-making table.
% DISAPPEARANCE_RATIONALE: If the harm reduction framework vanished overnight, the public health infrastructure supporting people who use drugs would collapse, leading to increased overdose deaths, disease transmission, and a return to full criminalization of users. The illicit drug market would remain, but without any mitigating state intervention.
% FOUNDING_PROBLEM: The 'War on Drugs' led to mass incarceration, high rates of overdose, and HIV/HCV transmission among people who use drugs, without significantly reducing substance use.
% FOUNDING_PROBLEM_CORROBORATION: Public health data, medical professionals, and international health organizations corroborate that the problems of overdose, disease transmission, and the negative impacts of criminalization remain live, even if mitigated by harm reduction. Law enforcement and prohibition advocates contest the extent to which harm reduction addresses the 'root cause' of drug use.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate because while users are less criminalized, they remain subject to a dangerous black market and health interventions that can feel coercive. Suppression (0.6) is significant because the state actively enforces laws against drug production and distribution, and public health agencies exert control over users' choices. Theater ratio (0.2) is low, as harm reduction services are generally functional and directly address stated goals, though some performative aspects may exist in balancing public perception with pragmatic action. The metrics show a slight decrease in extractiveness and suppression over time as harm reduction policies mature.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health agencies, this is a progressive and effective coordination mechanism. From the perspective of people who use drugs, it's a mixed bag: better than prohibition, but still extractive due to lack of safe supply and continued paternalism. From the perspective of criminal supply networks, it's a persistent, albeit adapted, enforcement challenge. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies are beneficiaries and agenda-setters, gaining mandate and resources. People who use drugs are both beneficiaries (reduced harm) and payers (continued stigma, criminalized supply). Criminal supply networks are payers due to ongoing enforcement. Law enforcement shifts from primarily targeting users to targeting supply, making them a complex actor with both costs and benefits. Prohibition advocates are excluded, bearing the cost of their moral framework being sidelined.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_chain_criminalization_impact,
    'To what extent does the continued criminalization of the drug supply chain undermine the harm reduction goals for individual users?',
    'Comparative analysis of jurisdictions with varying degrees of supply chain decriminalization/legalization on overdose rates, purity, and user safety outcomes.',
    'If continued criminalization significantly undermines harm reduction, the effective extractiveness on users is higher than measured, pushing the constraint closer to a Snare. If the impact is minimal, the current Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_criminalization_impact, empirical, 'Assesses the net effect of a partially criminalized system on user harm.').

omega_variable(
    paternalism_vs_autonomy,
    'Is the ''health condition'' framing of substance use inherently paternalistic, limiting user autonomy even in a harm reduction context?',
    'Qualitative research on user experiences with harm reduction services, focusing on perceived coercion, choice, and self-determination, compared to models emphasizing full user autonomy.',
    'If paternalism is a significant and unacknowledged feature, the effective extractiveness on people who use drugs is higher, and the constraint''s coordination function is more coercive than intended, pushing it towards a Snare for that seat. If autonomy is largely respected, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy, conceptual, 'Examines the balance between public health intervention and individual liberty within harm reduction.').

omega_variable(
    mandatrophy_of_prohibition_enforcement,
    'Has the enforcement apparatus originally built for full prohibition truly adapted to harm reduction, or does it retain vestigial functions that extract from users?',
    'Audits of law enforcement budgets and activities, comparing resource allocation to stated harm reduction goals versus continued low-level user harassment or asset forfeiture.',
    'If significant vestigial prohibition enforcement persists, the ''suppression'' metric is higher than measured, and the ''theater_ratio'' for law enforcement''s role is higher, indicating a Piton-like element within the broader Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_prohibition_enforcement, empirical, 'Assesses the functional shift of law enforcement under harm reduction policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__harm_reduction_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__harm_reduction_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__harm_reduction_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__harm_reduction_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__harm_reduction_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__harm_reduction_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'substance_control_kernel', focusing on harm reduction. It influences and is influenced by the 'prohibition_reading' and 'legalization_reading' of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
