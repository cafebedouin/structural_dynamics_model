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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Approach to Substance Use
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the harm reduction reading of substance
 *   control policy, framing substance use as a health condition requiring
 *   pragmatic intervention to reduce negative consequences, independent of
 *   use cessation. It shifts policy away from criminalization towards public
 *   health, but still involves state control and paternalism. The claimed
 *   type is Tangled Rope because it genuinely coordinates public health
 *   outcomes (benefiting users and society) while still extracting some
 *   autonomy from users and maintaining an illicit supply chain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.35).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Approach to Substance Use").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '7537c2c9-b4f9-4f2d-8634-fede0b1c4d96').
narrative_ontology:cs_kernel_codification('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', formalized).
narrative_ontology:cs_authority_grounding('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', expertise).
narrative_ontology:cs_interpretation_layer_present('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96').
narrative_ontology:cs_reading_relation('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', foundational, substance_use_is_health_condition).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', substance_use_is_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', foundational, pragmatic_intervention_reduces_harm).
narrative_ontology:cs_axiom_status(pragmatic_intervention_reduces_harm, holdable).
narrative_ontology:cs_axiom_grounding('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', pragmatic_intervention_reduces_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', public_health_paradigm).
narrative_ontology:cs_drift_state('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7537c2c9-b4f9-4f2d-8634-fede0b1c4d96', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, healthcare_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, general_public).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, illicit_supply_networks).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, public_health_approach).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, evidence_based_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, implement, and oversee harm reduction policies and programs, including safe supply, needle exchanges, and overdose prevention sites. They gain expanded mandate, funding, and legitimacy by addressing substance use as a health issue.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from reduced risk of overdose, disease transmission, and criminalization. However, they remain subject to paternalistic health interventions, potential stigma, and limited autonomy in their choices, as the supply chain remains largely illicit.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, substance_users, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, substance_users, payer).

% Receive funding and a clearer mandate to treat substance use as a chronic health condition, integrating users into mainstream healthcare. They gain professional legitimacy and expanded practice areas.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, healthcare_providers, beneficiary,
    organized, biographical, mobile, national).

% Benefits from reduced public health burdens (e.g., lower rates of infectious disease, fewer emergency room visits for overdose) and safer communities due to less visible street-level drug activity and crime associated with illicit markets.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, general_public, beneficiary,
    moderate, generational, mobile, national).

% Their role shifts from punitive enforcement against users to supporting public health initiatives, focusing on disrupting large-scale illicit supply networks rather than individual possession. This can be a contested shift within the institution.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, national).

% Experience disruption to their markets due to safe supply programs and increased law enforcement focus on large-scale trafficking. While not eliminated, their profitability and reach are challenged by harm reduction strategies.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, illicit_supply_networks, payer,
    powerful, immediate, trapped, global).

% Strongly oppose harm reduction, viewing substance use as a moral failing or criminal act requiring abstinence-only approaches and punitive measures. They are excluded from the policy-making process under a harm reduction framework.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, identity_locked, national).

% Advocate for full decriminalization and regulated legal markets, arguing that harm reduction, while better than prohibition, still maintains state control and paternalism. They are excluded from the current policy framing which stops short of full legalization.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate public health responses to substance use, reduce disease transmission, prevent overdose deaths, and integrate substance users into mainstream healthcare systems, thereby mitigating the negative health and social impacts of illicit drug use.
% TRANSFER_FUNCTION: Transfers public funds to health services and infrastructure for substance users; transfers some autonomy and decision-making power from substance users to health authorities; transfers risk from individual users to public health systems; and shifts enforcement costs from individual criminalization to supply-side disruption.
% ABSENT_VOICES: Prohibition advocates would object, arguing for abstinence-only and punitive measures based on moral or criminal justice framings. Legalization advocates would object, arguing for full individual liberty and regulated markets, seeing harm reduction as insufficient and still paternalistic.
% DISAPPEARANCE_RATIONALE: If harm reduction policies vanished overnight, overdose rates would likely surge, disease transmission (HIV/HCV) would increase, and substance users would revert to purely criminalized status, overwhelming emergency services and increasing social costs. The public health system would be unable to cope with the consequences.
% FOUNDING_PROBLEM: High rates of overdose deaths, disease transmission (HIV/HCV), and criminal justice involvement among substance users, driven by the failures of prohibitionist policies to address the health and social dimensions of drug use.
% FOUNDING_PROBLEM_CORROBORATION: Public health data, medical professionals, international health organizations (e.g., WHO), and substance user advocacy groups consistently corroborate the ongoing public health crisis and the efficacy of harm reduction in mitigating its worst effects. This corroboration comes from outside the direct beneficiaries of the policy's administrative structure.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness (0.45) is moderate: while it reduces the severe extraction of criminalization, it still involves significant public spending and a degree of paternalistic control over users' choices. Suppression (0.35) is lower than prohibition but remains active, focusing on regulating access to harm reduction services and disrupting illicit supply. Theater ratio (0.20) is relatively low, as harm reduction is largely evidence-based and functionally oriented. Accessibility collapse (0.40) reflects that while some alternatives (e.g., unregulated illicit markets) are constrained, others (e.g., full legalization) are not foreclosed by this framework. Resistance (0.30) comes from both prohibitionist and legalizationist camps.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health agencies, this is a functional coordination mechanism. From the perspective of substance users, it's a significant improvement over prohibition but still involves a degree of state control and paternalism. From the perspective of prohibition advocates, it's a dangerous enablement, while legalization advocates see it as an insufficient compromise.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and healthcare providers are beneficiaries, gaining mandate and resources. Substance users are both beneficiaries (reduced harm) and payers (loss of autonomy, continued stigma). Illicit supply networks are targets, as their operations are disrupted. The general public benefits from reduced social costs. Law enforcement's role shifts, making them a complex agenda-setter.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''harm_reduction_reading'' of the ''substance_control_kernel''?',
    'Comparison with policy documents, legislative intent, and public health guidelines to confirm alignment with harm reduction principles (e.g., focus on reducing negative consequences, non-judgmental approach, independence from cessation).',
    'If misidentified, the analysis of its structural relationship to other readings (prohibition, legalization) would be flawed, leading to incorrect classification of inter-reading dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated from the kernel.').

omega_variable(
    paternalism_vs_autonomy_balance,
    'To what extent does the ''paternalistic health intervention'' aspect of harm reduction infringe on individual autonomy, and is this infringement justified by public health outcomes?',
    'Qualitative studies on user experience and autonomy within harm reduction programs, combined with ethical analysis of state intervention in health choices. Comparison with outcomes in fully legalized, less paternalistic models.',
    'If paternalism is found to be excessive or unjustified, the ''extractiveness'' metric for substance users would be higher, potentially shifting the constraint closer to a Snare for that seat. If justified, the current extractiveness is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy_balance, preference, 'Assesses the ethical balance between state intervention for health and individual liberty within harm reduction.').

omega_variable(
    supply_chain_criminalization_impact,
    'What is the true impact of maintaining a criminalized illicit supply chain alongside harm reduction services on user safety and public health outcomes?',
    'Empirical studies comparing jurisdictions with varying degrees of supply chain criminalization alongside harm reduction, focusing on overdose rates, adulterated supply, and user engagement with services.',
    'If the criminalized supply chain significantly undermines harm reduction goals, the ''suppression'' and ''extractiveness'' metrics would be higher than currently assessed, indicating a more coercive and less effective system. This could push the constraint towards a more extractive Tangled Rope or even Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_criminalization_impact, empirical, 'Examines the tension between harm reduction for users and continued criminalization of supply.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subs_tr_t6, substance_control_kernel__harm_reduction_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__harm_reduction_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(subs_tr_t18, substance_control_kernel__harm_reduction_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(subs_tr_t24, substance_control_kernel__harm_reduction_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__harm_reduction_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t6, substance_control_kernel__harm_reduction_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__harm_reduction_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(subs_be_t18, substance_control_kernel__harm_reduction_reading, base_extractiveness, 18, 0.46).
narrative_ontology:measurement(subs_be_t24, substance_control_kernel__harm_reduction_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__harm_reduction_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(subs_su_t6, substance_control_kernel__harm_reduction_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__harm_reduction_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(subs_su_t18, substance_control_kernel__harm_reduction_reading, suppression_requirement, 18, 0.36).
narrative_ontology:measurement(subs_su_t24, substance_control_kernel__harm_reduction_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__harm_reduction_reading, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
