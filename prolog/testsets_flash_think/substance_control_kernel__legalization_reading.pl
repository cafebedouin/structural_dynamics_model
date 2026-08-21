% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Substance Legalization Framework (Liberty Reading)
 *   domain: Public Health Policy / Criminal Justice / Political Economy
 *
 * SUMMARY:
 *   This constraint represents the 'legalization_reading' of the
 *   'substance_control_kernel', where substance use is framed as an
 *   individual liberty issue. The state's role is limited to preventing
 *   direct third-party harm and capturing externality costs through taxation
 *   and regulation. This reading aims to dismantle the harms of prohibition
 *   by creating a legal, regulated market. The structural delta from
 *   prohibition includes users exiting the victim set, third parties entering
 *   as victims via unmitigated externalities, the emergence of a legal
 *   industry, and the state becoming a revenue collector. The claimed type is
 *   'tangled_rope' because it coordinates a legal market and generates
 *   revenue, but also extracts from consumers (taxes) and imposes unmitigated
 *   costs on third parties (externalities).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.45).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.2).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Substance Legalization Framework (Liberty Reading)").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "Public Health Policy / Criminal Justice / Political Economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, 'f011c126-5a0e-400a-a4cb-e22155ffcc76').
narrative_ontology:cs_kernel_codification('f011c126-5a0e-400a-a4cb-e22155ffcc76', formalized).
narrative_ontology:cs_authority_grounding('f011c126-5a0e-400a-a4cb-e22155ffcc76', practice).
narrative_ontology:cs_interpretation_layer_present('f011c126-5a0e-400a-a4cb-e22155ffcc76').
narrative_ontology:cs_reading_relation('f011c126-5a0e-400a-a4cb-e22155ffcc76', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('f011c126-5a0e-400a-a4cb-e22155ffcc76', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('f011c126-5a0e-400a-a4cb-e22155ffcc76', foundational, individual_autonomy_over_substance_use).
narrative_ontology:cs_axiom_status(individual_autonomy_over_substance_use, holdable).
narrative_ontology:cs_axiom_grounding('f011c126-5a0e-400a-a4cb-e22155ffcc76', individual_autonomy_over_substance_use, deontological).
narrative_ontology:cs_axiom('f011c126-5a0e-400a-a4cb-e22155ffcc76', foundational, state_intervention_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_intervention_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('f011c126-5a0e-400a-a4cb-e22155ffcc76', state_intervention_limited_to_third_party_harm, conventional).
narrative_ontology:cs_reference_frame('f011c126-5a0e-400a-a4cb-e22155ffcc76', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('f011c126-5a0e-400a-a4cb-e22155ffcc76', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f011c126-5a0e-400a-a4cb-e22155ffcc76', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_treasury).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, adult_consumers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_party_citizens).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, black_market_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, adult_consumers).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, legal_substance_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances, exercising individual liberty and benefiting from regulated product safety. They pay taxes on purchases and may bear some indirect costs of regulation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, adult_consumers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, adult_consumers, payer).

% Establishes and enforces the regulatory framework, collects tax revenue from legal sales, and is responsible for mitigating third-party harms. Benefits directly from increased revenue.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_treasury, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_treasury, beneficiary).

% Gains legal market access and the ability to operate openly, generating profits. Bears the costs of taxation, licensing, and regulatory compliance.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, legal_substance_industry, payer).

% Bear the unmitigated costs of externalities (e.g., impaired driving incidents, secondhand exposure, public nuisance) that are not fully captured or prevented by state intervention. Their ability to avoid these harms is limited.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_party_citizens, payer,
    moderate, immediate, constrained, local).

% Are displaced from the market by legal competition and face continued criminalization for operating outside the regulated framework. They bear the costs of enforcement actions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_actors, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, black_market_actors, payer).

% Analyze the public health outcomes of legalization, focusing on rates of use, addiction, and harm reduction services. They advocate for policy adjustments to minimize negative health impacts.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, state_treasury).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a regulated market for substances, ensuring product safety, controlling distribution, and generating tax revenue, while upholding individual autonomy and limiting state intervention to preventing third-party harm.
% TRANSFER_FUNCTION: Transfers tax revenue from consumers and the legal industry to the state treasury. Transfers profits to the legal substance industry. Transfers unmitigated externality costs (e.g., healthcare, public safety) to third-party citizens.
% ABSENT_VOICES: Prohibition advocates would argue for a moral framework that prioritizes abstinence and social order through criminalization. Harm reduction advocates would argue for a more comprehensive public health approach, prioritizing treatment and support over market regulation and taxation.
% DISAPPEARANCE_RATIONALE: If the legalization framework vanished overnight, the market would likely revert to either prohibition (with its associated black market harms) or a chaotic, unregulated free-for-all. This would lead to significant social disruption, loss of tax revenue, and potentially increased public health and safety issues.
% FOUNDING_PROBLEM: The harms of prohibition (e.g., black markets, criminalization of users, lack of product quality control, lost tax revenue) and the desire to uphold individual liberty and limit state overreach.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations, some economists, and former law enforcement officials corroborate the problems of prohibition and the benefits of a liberty-based approach. Public opinion polls often show significant support for legalization based on these principles.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) reflects the costs of taxation on consumers and industry, as well as the uncaptured externality costs borne by third parties. Suppression (0.20) is significantly lower than prohibition, focusing on regulating the legal market and enforcing against third-party harms (e.g., DUI), rather than suppressing use itself. The theater ratio (0.15) is low, indicating that the state's actions are largely functional in managing the legal market and public safety. Accessibility collapse (0.35) is moderate, as legal access exists but is still constrained by age limits, licensing, and distribution rules. Resistance (0.20) is lower than under prohibition, primarily from those who oppose legalization on moral grounds or from remnants of the black market.
 *
 * PERSPECTIVAL GAP:
 *   Adult consumers largely perceive this as a beneficial framework due to increased liberty and safety, despite paying taxes. Third-party citizens, however, may experience it as extractive if externality costs are not adequately addressed. The state views it as a functional regulatory and revenue-generating mechanism. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The state treasury and legal substance industry are primary beneficiaries, gaining revenue and market access, respectively. Adult consumers are beneficiaries of liberty and safety, but also payers through taxes. Third-party citizens are victims of unmitigated externalities. Black market actors are excluded and victimized by the legal framework's enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a complete and accurate representation of the ''legalization_reading'' of the ''substance_control_kernel''?',
    'Comparison with other generated readings of the same kernel and expert review of the structural delta.',
    'If the reading is incomplete or misrepresents the core tenets, the classification of this specific constraint and its network relationships would be inaccurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as one specific reading of a contested kernel.').

omega_variable(
    externality_cost_capture_efficacy,
    'How effectively does the state''s taxation and regulation capture and mitigate the full range of third-party externality costs associated with substance use?',
    'Longitudinal empirical studies comparing public health and safety costs with tax revenues and regulatory expenditures in legalized jurisdictions.',
    'If externality costs are significantly under-captured, the effective extraction from third-party citizens is higher than estimated, potentially shifting the constraint closer to a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_cost_capture_efficacy, empirical, 'Assesses the gap between externality costs and state mitigation/revenue capture.').

omega_variable(
    black_market_persistence,
    'To what extent does a black market for substances persist or adapt within a legalized framework, and what are its associated harms?',
    'Empirical analysis of black market activity (e.g., illicit sales, arrests, product seizures) in legalized jurisdictions, comparing pre- and post-legalization trends.',
    'Significant black market persistence would indicate higher suppression requirements and continued victimhood for those involved, potentially increasing the overall extractiveness and suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Examines the resilience of illicit markets post-legalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__legalization_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__legalization_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(subs_tr_t25, substance_control_kernel__legalization_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__legalization_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__legalization_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__legalization_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(subs_be_t25, substance_control_kernel__legalization_reading, base_extractiveness, 25, 0.43).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__legalization_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__legalization_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__legalization_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(subs_su_t25, substance_control_kernel__legalization_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__legalization_reading, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'substance_control_kernel', each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
