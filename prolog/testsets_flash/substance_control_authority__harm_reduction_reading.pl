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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Authority for Harm Reduction in Substance Use
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint describes the state's authority to implement harm
 *   reduction policies for substance use, a reading of the broader
 *   'substance_control_authority' kernel. It focuses on minimizing health and
 *   social harms associated with drug use, rather than criminalizing users or
 *   fully legalizing markets. This reading accepts drug use as a reality and
 *   intervenes with public health services, decriminalizing possession while
 *   maintaining controls on supply. Users remain in a partial victim set due
 *   to health harms but exit the criminal justice system for simple
 *   possession. Third parties (communities) bear some residual risks (e.g.,
 *   visible use, disease transmission) but benefit from reduced crime and
 *   public health crises.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.3).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Authority for Harm Reduction in Substance Use").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, '0484b1fc-557c-4182-92ab-c79b5711ffe1').
narrative_ontology:cs_kernel_codification('0484b1fc-557c-4182-92ab-c79b5711ffe1', formalized).
narrative_ontology:cs_authority_grounding('0484b1fc-557c-4182-92ab-c79b5711ffe1', expertise).
narrative_ontology:cs_interpretation_layer_present('0484b1fc-557c-4182-92ab-c79b5711ffe1').
narrative_ontology:cs_reading_relation('0484b1fc-557c-4182-92ab-c79b5711ffe1', substance_control_authority__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('0484b1fc-557c-4182-92ab-c79b5711ffe1', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('0484b1fc-557c-4182-92ab-c79b5711ffe1', foundational, minimizing_harm_is_primary_goal).
narrative_ontology:cs_axiom_status(minimizing_harm_is_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('0484b1fc-557c-4182-92ab-c79b5711ffe1', minimizing_harm_is_primary_goal, instrumental).
narrative_ontology:cs_axiom('0484b1fc-557c-4182-92ab-c79b5711ffe1', foundational, drug_use_is_a_health_issue_not_a_criminal_one).
narrative_ontology:cs_axiom_status(drug_use_is_a_health_issue_not_a_criminal_one, holdable).
narrative_ontology:cs_axiom_grounding('0484b1fc-557c-4182-92ab-c79b5711ffe1', drug_use_is_a_health_issue_not_a_criminal_one, conventional).
narrative_ontology:cs_reference_frame('0484b1fc-557c-4182-92ab-c79b5711ffe1', public_health_first_approach).
narrative_ontology:cs_drift_state('0484b1fc-557c-4182-92ab-c79b5711ffe1', contemporary_opioid_crisis_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0484b1fc-557c-4182-92ab-c79b5711ffe1', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, drug_users_receiving_services).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, drug_users_with_health_harms).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, communities_with_unmanaged_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, law_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and advocates for harm reduction policies, including needle exchanges, overdose prevention sites, and treatment programs. Benefits from reduced disease transmission and improved public health outcomes, but operates within political and budgetary constraints.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from access to clean supplies, medical care, and overdose reversal drugs, reducing immediate health risks and avoiding criminal penalties for possession. Still faces stigma and potential legal issues for acquisition or distribution.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, drug_users_receiving_services, beneficiary,
    powerless, immediate, constrained, local).

% Continues to experience health harms from drug use, even with harm reduction services, due to the inherent risks of substances, lack of full treatment access, or inability to access services. Bears the direct health costs and social stigma.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, drug_users_with_health_harms, payer,
    powerless, biographical, trapped, local).

% Bears some social costs associated with visible drug use, public disorder, and potential disease transmission, even as harm reduction mitigates some of these. May resist harm reduction initiatives due to perceived negative impacts.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, communities_with_unmanaged_use, payer,
    moderate, biographical, constrained, local).

% Shifts focus from prosecuting users to addressing drug trafficking and related violent crime. May experience reduced workload from minor drug offenses but faces challenges in adapting to new public health-oriented approaches and managing public perception.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, law_enforcement, payer).

% Opposes harm reduction as condoning drug use and undermining moral order. Would advocate for stricter prohibition and criminalization, but their views are marginalized in this policy framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, conservative_political_factions, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health efforts to mitigate the negative consequences of drug use (e.g., HIV/HCV transmission, overdose deaths) by providing services to a population that would otherwise be marginalized and hidden.
% TRANSFER_FUNCTION: Transfers resources (funding, personnel, medical supplies) from the state to public health agencies and directly to drug users, in exchange for reduced public health burdens and criminal justice costs. Transfers some social costs (e.g., visible drug use) to communities.
% ABSENT_VOICES: Conservative political factions and some community groups who prioritize abstinence-only approaches or criminal justice solutions are largely excluded from the policy-making process, as their views are incompatible with the harm reduction framework.
% DISAPPEARANCE_RATIONALE: If harm reduction authority vanished, overdose deaths would surge, disease transmission rates would increase, and the criminal justice system would be overwhelmed by minor drug offenses. Public health infrastructure would collapse, and drug users would face immediate, severe health and legal consequences.
% FOUNDING_PROBLEM: The criminalization of drug use led to widespread health crises (HIV/HCV epidemics), high overdose rates, and an overburdened criminal justice system, without significantly reducing drug use itself.
% FOUNDING_PROBLEM_CORROBORATION: Public health experts, medical professionals, and international health organizations consistently corroborate that the problems of overdose, disease transmission, and criminal justice strain remain live, and that harm reduction is an evidence-based response. This is attested by epidemiological data and public health outcomes from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).

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
 *   The extractiveness (0.45) is moderate, reflecting the ongoing health harms and social costs that persist even with harm reduction, but significantly lower than a prohibitionist approach due to reduced criminal penalties. Suppression (0.30) is also moderate, as it still involves state control over substances and some enforcement, but aims to reduce coercive measures against users. Theater ratio (0.10) is low, indicating that the policies are genuinely aimed at their stated public health goals, with minimal performative elements. Accessibility collapse (0.40) is moderate, as alternatives to illicit drug use (e.g., safe supply, full treatment) are still constrained, but alternatives to criminalization are available. Resistance (0.25) is moderate, coming from both those who desire full legalization and those who advocate for stricter prohibition.
 *
 * PERSPECTIVAL GAP:
 *   Public health agencies and drug users receiving services experience this as a beneficial coordination mechanism, reducing severe harms. However, communities with unmanaged use and drug users still experiencing health harms perceive residual costs and unmet needs. Law enforcement's perspective shifts from punitive to public safety, but with internal friction. Conservative factions view it as a betrayal of moral order.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and drug users receiving services are beneficiaries, as the constraint channels resources and reduces immediate harms. Drug users with health harms and communities with unmanaged use are payers, bearing the residual health and social costs. Law enforcement is an agenda-setter with some payer aspects, as their role shifts and they bear the costs of adapting. Conservative political factions are excluded, as their preferred policies are not implemented.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively addresses the mandatrophy of prohibitionist policies, which failed to reduce drug use and instead created severe public health and social crises. By shifting from criminalization to public health, it attempts to realign the constraint with a live problem (minimizing harm) rather than maintaining an ineffective, extractive system. The 'founding_problem_status: live' reflects this ongoing adaptation to a persistent problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_decriminalization,
    'What is the precise scope of decriminalization under this authority, and how does it impact the black market for drug acquisition?',
    'Analysis of legal frameworks and enforcement practices across jurisdictions, coupled with economic studies of illicit drug market dynamics post-decriminalization.',
    'If decriminalization is too narrow, it may not significantly reduce criminal justice costs or black market harms. If too broad, it might inadvertently strengthen illicit supply chains or increase public disorder, shifting the balance of extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_decriminalization, empirical, 'Ambiguity in the extent of decriminalization and its market effects.').

omega_variable(
    community_acceptance_vs_harm_reduction_efficacy,
    'To what extent does community resistance to visible harm reduction services (e.g., overdose prevention sites) impede their efficacy, and how is this balanced against public health benefits?',
    'Longitudinal studies comparing health outcomes and community perceptions in areas with and without visible harm reduction services, alongside qualitative research on community engagement strategies.',
    'High community resistance could lead to underutilization of services, reducing their public health impact and potentially increasing social costs, pushing the constraint towards higher extractiveness for communities and lower efficacy for users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_acceptance_vs_harm_reduction_efficacy, empirical, 'Tension between community acceptance and the operational effectiveness of harm reduction.').

omega_variable(
    natural_law_vs_constructed_harm_reduction,
    'Is the principle of harm reduction a self-evident ethical imperative (akin to a natural law), or a constructed policy choice reflecting a specific set of values and empirical evidence?',
    'Philosophical analysis of ethical frameworks for public health, and cross-cultural comparison of drug policy rationales. This is a conceptual question, not empirically resolvable.',
    'If viewed as a natural law, its legitimacy is inherent, making resistance appear irrational. If a constructed choice, its persistence depends on ongoing political will and evidence, making it vulnerable to shifts in public opinion or scientific consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_harm_reduction, conceptual, 'Whether harm reduction is a fundamental principle or a policy construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1980, substance_control_authority__harm_reduction_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(subs_tr_t1990, substance_control_authority__harm_reduction_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(subs_tr_t2000, substance_control_authority__harm_reduction_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__harm_reduction_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(subs_tr_t2020, substance_control_authority__harm_reduction_reading, theater_ratio, 2020, 0.11).
narrative_ontology:measurement(subs_tr_t2024, substance_control_authority__harm_reduction_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(subs_be_t1980, substance_control_authority__harm_reduction_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(subs_be_t1990, substance_control_authority__harm_reduction_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(subs_be_t2000, substance_control_authority__harm_reduction_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__harm_reduction_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(subs_be_t2020, substance_control_authority__harm_reduction_reading, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement(subs_be_t2024, substance_control_authority__harm_reduction_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1980, substance_control_authority__harm_reduction_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(subs_su_t1990, substance_control_authority__harm_reduction_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(subs_su_t2000, substance_control_authority__harm_reduction_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__harm_reduction_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(subs_su_t2020, substance_control_authority__harm_reduction_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(subs_su_t2024, substance_control_authority__harm_reduction_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'substance_control_authority' kernel. This 'harm_reduction_reading' focuses on public health interventions and decriminalization, distinct from 'prohibition_reading' (criminalization) and 'legalization_reading' (regulated markets).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
