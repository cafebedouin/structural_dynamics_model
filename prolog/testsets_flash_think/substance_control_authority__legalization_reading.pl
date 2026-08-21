% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint describes the state's authority to regulate drug markets
 *   as legal commerce, with a focus on quality and access controls. It is a
 *   specific reading of the broader 'substance_control_authority' kernel,
 *   distinct from prohibition or pure harm reduction. The core function is to
 *   replace illicit markets with a regulated system that prioritizes public
 *   health and safety while generating tax revenue. The metrics reflect a
 *   system designed for functional coordination, with moderate extraction
 *   (taxes) and significant suppression of the illegal market.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.35).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.7).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, 'f8d161a9-f1c4-4746-b61d-d78943ad34d9').
narrative_ontology:cs_kernel_codification('f8d161a9-f1c4-4746-b61d-d78943ad34d9', formalized).
narrative_ontology:cs_authority_grounding('f8d161a9-f1c4-4746-b61d-d78943ad34d9', lineage).
narrative_ontology:cs_interpretation_layer_present('f8d161a9-f1c4-4746-b61d-d78943ad34d9').
narrative_ontology:cs_reading_relation('f8d161a9-f1c4-4746-b61d-d78943ad34d9', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('f8d161a9-f1c4-4746-b61d-d78943ad34d9', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('f8d161a9-f1c4-4746-b61d-d78943ad34d9', foundational, individual_autonomy_and_public_safety_balance).
narrative_ontology:cs_axiom_status(individual_autonomy_and_public_safety_balance, holdable).
narrative_ontology:cs_axiom_grounding('f8d161a9-f1c4-4746-b61d-d78943ad34d9', individual_autonomy_and_public_safety_balance, deontological).
narrative_ontology:cs_axiom('f8d161a9-f1c4-4746-b61d-d78943ad34d9', foundational, evidence_based_policy_efficacy).
narrative_ontology:cs_axiom_status(evidence_based_policy_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('f8d161a9-f1c4-4746-b61d-d78943ad34d9', evidence_based_policy_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('f8d161a9-f1c4-4746-b61d-d78943ad34d9', rational_public_policy_framework).
narrative_ontology:cs_drift_state('f8d161a9-f1c4-4746-b61d-d78943ad34d9', contemporary_regulatory_challenges, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f8d161a9-f1c4-4746-b61d-d78943ad34d9', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_treasury).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, regulated_businesses).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, adult_consumers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_agencies).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, criminal_organizations).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, unregulated_suppliers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These bodies establish and enforce the legal framework for substance production, distribution, and sale, including licensing, quality control, and taxation. They aim to ensure public safety and generate revenue.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% These entities operate legally within the regulated market, paying taxes and adhering to quality and safety standards. They gain legitimate market access and protection from criminal competition.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, regulated_businesses, beneficiary,
    organized, biographical, mobile, national).

% These individuals gain access to tested, regulated, and safely distributed substances, reducing health risks associated with black markets. They pay taxes on purchases.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, adult_consumers, beneficiary,
    moderate, immediate, mobile, national).

% These agencies benefit from increased data collection on substance use, enabling evidence-based public health campaigns, treatment programs, and harm reduction initiatives within a legal framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_agencies, beneficiary,
    institutional, generational, analytical, national).

% These groups lose significant market share and revenue as legal alternatives become available. They face continued law enforcement pressure for any remaining illicit activities.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, criminal_organizations, payer,
    powerless, immediate, trapped, global).

% These individuals or small groups operating outside the legal framework lose their customer base and face enforcement actions, as the state actively suppresses the illicit market.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, unregulated_suppliers, payer,
    powerless, immediate, trapped, local).

% These groups fundamentally oppose the legalization of substances, arguing for continued criminalization. Their policy preferences are excluded from the framework of this particular reading of state authority.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal, regulated market for substances, ensuring product quality, controlling access, and generating tax revenue, thereby replacing an unregulated, criminal market and its associated harms.
% TRANSFER_FUNCTION: Transfers tax revenue from legal sales to the state treasury; transfers market control from criminal organizations to regulated businesses; transfers health and safety risks from consumers to regulatory oversight and public health programs.
% ABSENT_VOICES: Criminal organizations and unregulated suppliers are structurally excluded from the legal market, though their continued existence as a black market poses a challenge. Hardline prohibition advocates are also excluded from the policy-making process for this reading, as their core premise is rejected.
% DISAPPEARANCE_RATIONALE: If state authority to regulate legal drug markets vanished overnight, the market would immediately revert to an unregulated, criminalized, or chaotic state. This would lead to a resurgence of black market harms, loss of tax revenue, and a collapse of public health and safety controls, fundamentally reorganizing the social and economic landscape around substance use.
% FOUNDING_PROBLEM: The harms associated with unregulated, criminalized drug markets, including violence, lack of product safety, disease transmission, mass incarceration, and lost tax revenue, which prohibition failed to solve.
% FOUNDING_PROBLEM_CORROBORATION: Public health experts, economists, and criminal justice reform advocates corroborate the persistent problems of prohibition and the potential benefits of regulation, citing empirical data from jurisdictions that have implemented legalization. This corroboration comes from independent academic research and international policy analyses, not solely from benefiting parties.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).
:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it aims to solve a genuine collective action problem (the harms of unregulated markets) by establishing a functional, regulated system where participants (businesses, consumers, public health) are net beneficiaries. Extraction is moderate, primarily through taxation for public services, not rent-seeking. Suppression is high, but directed at the illegal market, not at legal participants. The low theater ratio indicates a system intended to be highly functional and evidence-based.
 *
 * PERSPECTIVAL GAP:
 *   While the legalization reading aims for broad public benefit, some stakeholders (e.g., prohibition advocates) would perceive this constraint as a Snare, arguing it legitimizes harmful activities. However, from the perspective of this reading, the constraint is a functional Rope, coordinating a safer market. The engine's classification will reflect the structural data provided, not the excluded perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory bodies, regulated businesses, adult consumers, and public health agencies are beneficiaries, gaining stability, safety, and resources. Criminal organizations and unregulated suppliers are the targets, as the constraint actively suppresses their market. Prohibition advocates are excluded from this policy framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    primary_goal_ambiguity,
    'Is the primary goal of this authority public health and safety, or tax revenue generation?',
    'Analysis of budget allocations (e.g., proportion of tax revenue dedicated to public health vs. general fund) and legislative intent over time.',
    'If primarily revenue-driven, the constraint might drift towards higher extractiveness and less public health focus; if primarily public health, it would prioritize access to treatment and harm reduction, potentially accepting lower revenue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_goal_ambiguity, preference, 'Ambiguity in the core purpose of legalization.').

omega_variable(
    black_market_persistence,
    'Will a significant black market persist for cheaper or unregulated products, undermining the legal market''s goals?',
    'Longitudinal empirical studies comparing legal market prices, product availability, and illicit market activity in legalized jurisdictions.',
    'If a substantial black market persists, the constraint''s effective suppression of criminal activity is lower, and its coordination function is less complete, potentially requiring adjustments to regulatory policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Effectiveness of legalization in eliminating illegal markets.').

omega_variable(
    use_volume_vs_harm_reduction,
    'Will the potential increase in substance use volume (due to increased accessibility) outweigh the public health benefits of regulation and harm reduction?',
    'Epidemiological studies tracking rates of substance use, addiction, and related health outcomes in legalized vs. non-legalized jurisdictions over time.',
    'If increased use leads to net public health harms, the constraint''s overall societal benefit is reduced, challenging its ''rope'' classification and potentially necessitating policy adjustments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_vs_harm_reduction, empirical, 'Net public health impact of increased accessibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t5, substance_control_authority__legalization_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__legalization_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(subs_tr_t15, substance_control_authority__legalization_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(subs_be_t5, substance_control_authority__legalization_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__legalization_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(subs_su_t5, substance_control_authority__legalization_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__legalization_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
