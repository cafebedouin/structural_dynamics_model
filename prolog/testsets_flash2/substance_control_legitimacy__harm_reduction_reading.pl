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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm Reduction Approach to Substance Control
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'harm reduction' reading of substance
 *   control legitimacy, where substance use is primarily a public health
 *   issue. State authority is seen as deriving from a duty to minimize harm
 *   without resorting to criminalization. This reading medicalizes users,
 *   leading to moderate extractiveness through mandated treatment and a
 *   persistent black market for those outside the formal system. It is one
 *   reading of the broader 'substance_control_legitimacy' kernel, distinct
 *   from prohibitionist or legalization readings.
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
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Approach to Substance Control").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'a13b9cad-c40d-4a97-8702-2ce7c113a227').
narrative_ontology:cs_kernel_codification('a13b9cad-c40d-4a97-8702-2ce7c113a227', formalized).
narrative_ontology:cs_authority_grounding('a13b9cad-c40d-4a97-8702-2ce7c113a227', lineage).
narrative_ontology:cs_interpretation_layer_present('a13b9cad-c40d-4a97-8702-2ce7c113a227').
narrative_ontology:cs_reading_relation('a13b9cad-c40d-4a97-8702-2ce7c113a227', substance_control_legitimacy__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('a13b9cad-c40d-4a97-8702-2ce7c113a227', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('a13b9cad-c40d-4a97-8702-2ce7c113a227', foundational, substance_use_is_public_health_issue).
narrative_ontology:cs_axiom_status(substance_use_is_public_health_issue, holdable).
narrative_ontology:cs_axiom_grounding('a13b9cad-c40d-4a97-8702-2ce7c113a227', substance_use_is_public_health_issue, empirically_contingent).
narrative_ontology:cs_axiom('a13b9cad-c40d-4a97-8702-2ce7c113a227', foundational, state_duty_to_minimize_harm_without_criminalization).
narrative_ontology:cs_axiom_status(state_duty_to_minimize_harm_without_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('a13b9cad-c40d-4a97-8702-2ce7c113a227', state_duty_to_minimize_harm_without_criminalization, deontological).
narrative_ontology:cs_reference_frame('a13b9cad-c40d-4a97-8702-2ce7c113a227', public_health_first_approach).
narrative_ontology:cs_drift_state('a13b9cad-c40d-4a97-8702-2ce7c113a227', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a13b9cad-c40d-4a97-8702-2ce7c113a227', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, some_substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users_with_mandated_treatment).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and implements policies that treat substance use as a health issue, focusing on reducing negative consequences rather than criminalizing users. Benefits from increased funding and legitimacy for public health interventions.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive funding and referrals for substance use treatment programs. Benefit from the medicalization of substance use, which expands their client base and institutional role. They are coordinated into the public health framework.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    organized, biographical, mobile, local).

% Are diverted from criminal justice into mandatory treatment programs. While avoiding incarceration, they bear the costs of treatment, loss of autonomy, and social stigma. Their choices are constrained by legal mandates.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users_with_mandated_treatment, payer,
    powerless, immediate, constrained, local).

% Continue to operate in an illicit market due to partial decriminalization, facing ongoing legal risks and violence. They bear the costs of enforcement and lack legal protections, but provide access for users unwilling or unable to access legal channels.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_participants, payer,
    moderate, immediate, trapped, regional).

% Argue that any approach short of criminalization fails to address the moral harm of substance use. They are excluded from the policy-making process under this reading, but continue to exert political pressure.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% Argue that harm reduction does not go far enough in respecting individual autonomy and still perpetuates a black market. They are excluded from the policy-making process under this reading, but continue to exert political pressure.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health, social services, and a modified criminal justice system to manage substance use as a health issue, providing treatment and support services while reducing incarceration rates.
% TRANSFER_FUNCTION: Transfers resources from criminal justice enforcement (partially) to public health and treatment services. Transfers autonomy from substance users to medical and social service professionals through mandated interventions.
% ABSENT_VOICES: Advocates for full legalization, who would argue for greater individual autonomy and market freedom, are marginalized. Advocates for strict prohibition, who would argue for moral condemnation and criminal penalties, are also excluded from the core policy-making.
% DISAPPEARANCE_RATIONALE: If this harm reduction framework vanished, the system would likely revert to either a more punitive prohibitionist approach (increasing incarceration) or a more liberal legalization approach (creating new regulatory challenges), fundamentally altering how society manages substance use.
% FOUNDING_PROBLEM: The criminalization of substance use led to mass incarceration, disproportionate impacts on marginalized communities, and failed to reduce substance use or related harms.
% FOUNDING_PROBLEM_CORROBORATION: Public health experts, social justice advocates, and international health organizations corroborate that criminalization was a failed approach and that the harms it caused are still live problems. Law enforcement agencies also acknowledge the limitations of a purely punitive approach.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) comes from the loss of autonomy for users mandated into treatment and the continued existence of a black market that extracts from users and participants. Suppression (0.6) is present through legal mandates for treatment and ongoing (though reduced) enforcement against illicit markets. The theater ratio (0.2) reflects that while the public health framing is genuine, some enforcement resources are still dedicated to maintaining control rather than purely health outcomes. The claimed type is Tangled Rope because it genuinely coordinates public health efforts but also extracts from users through mandated treatment and from black market participants through continued suppression.
 *
 * PERSPECTIVAL GAP:
 *   Public health agencies perceive this as a progressive, effective coordination mechanism. Substance users, particularly those mandated into treatment, experience it as a coercive system that, while better than incarceration, still limits their freedom. Black market participants experience it as a persistent, though shifted, form of suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and treatment providers are beneficiaries, gaining resources and legitimacy. Substance users with mandated treatment are payers, losing autonomy and bearing treatment costs. Black market participants are also payers, facing ongoing risks. Prohibition and legalization advocates are excluded, as their core tenets are not central to this policy framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling harm reduction as pure extraction by acknowledging its genuine coordination function (reducing incarceration, providing health services). However, it also highlights the extractive elements (mandated treatment, persistent black market) that prevent it from being a pure Rope. The 'contested' status of the founding problem indicates ongoing debate about whether the original problem of mass incarceration has been adequately addressed without creating new forms of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''harm_reduction_reading'' of the ''substance_control_legitimacy'' kernel?',
    'Analysis of policy documents, legislative intent, and public discourse to confirm the primary framing aligns with harm reduction principles (medicalization, non-criminalization, harm minimization).',
    'If misidentified, the classification would be inaccurate, and its relationship to sibling readings (prohibition, legalization) would be structurally misaligned.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint is the intended reading of the kernel.').

omega_variable(
    black_market_persistence_driver,
    'Is the persistence of the black market primarily due to the limitations of the harm reduction framework (e.g., access barriers to legal treatment, user preference for illicit substances), or external factors (e.g., international drug trade dynamics)?',
    'Empirical studies on user access to and satisfaction with harm reduction services, combined with analysis of global illicit drug supply chains.',
    'If primarily due to harm reduction limitations, the extractiveness and suppression metrics for this reading would be higher, indicating a more Snare-like quality. If external, the black market''s contribution to extraction is less attributable to this specific policy framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence_driver, empirical, 'Determines the primary cause of black market persistence under harm reduction.').

omega_variable(
    mandated_treatment_autonomy_cost,
    'What is the true cost to individual autonomy of mandated treatment, and how does it compare to the benefits of avoiding criminal justice involvement?',
    'Qualitative studies with individuals undergoing mandated treatment, comparing their experiences and perceived autonomy with those in voluntary treatment or those who faced criminal penalties.',
    'If the autonomy cost is perceived as very high, the extractiveness for ''substance_users_with_mandated_treatment'' would be higher, potentially shifting their seat classification towards a Snare. If the benefits of avoiding criminalization strongly outweigh the autonomy cost, the extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandated_treatment_autonomy_cost, preference, 'Assesses the balance of costs and benefits for individuals in mandated treatment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
