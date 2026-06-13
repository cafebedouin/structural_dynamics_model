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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Substance Control: Legalization Reading (Individual Liberty)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'legalization' reading of the substance
 *   control kernel, where substance use is primarily an individual liberty
 *   issue. State intervention is limited to preventing direct third-party
 *   harm and capturing externality costs through taxation and regulation.
 *   Users are no longer victims of the state's enforcement, but third parties
 *   may bear unmitigated externality costs. A legal industry emerges as a new
 *   beneficiary, and the state becomes a revenue collector.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.3).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.2).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Substance Control: Legalization Reading (Individual Liberty)").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '47ec1c38-3337-472d-b733-3042f6a17547').
narrative_ontology:cs_kernel_codification('47ec1c38-3337-472d-b733-3042f6a17547', formalized).
narrative_ontology:cs_authority_grounding('47ec1c38-3337-472d-b733-3042f6a17547', lineage).
narrative_ontology:cs_interpretation_layer_present('47ec1c38-3337-472d-b733-3042f6a17547').
narrative_ontology:cs_reading_relation('47ec1c38-3337-472d-b733-3042f6a17547', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('47ec1c38-3337-472d-b733-3042f6a17547', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('47ec1c38-3337-472d-b733-3042f6a17547', foundational, individual_autonomy_over_body).
narrative_ontology:cs_axiom_status(individual_autonomy_over_body, holdable).
narrative_ontology:cs_axiom_grounding('47ec1c38-3337-472d-b733-3042f6a17547', individual_autonomy_over_body, deontological).
narrative_ontology:cs_axiom('47ec1c38-3337-472d-b733-3042f6a17547', foundational, state_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('47ec1c38-3337-472d-b733-3042f6a17547', state_limited_to_third_party_harm, deontological).
narrative_ontology:cs_reference_frame('47ec1c38-3337-472d-b733-3042f6a17547', classical_liberal_autonomy).
narrative_ontology:cs_drift_state('47ec1c38-3337-472d-b733-3042f6a17547', contemporary_public_health_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('47ec1c38-3337-472d-b733-3042f6a17547', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, adult_consumers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_revenue_agencies).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_parties_affected_by_externalities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances, avoiding criminal penalties and black markets. They bear the costs of taxation and potential health risks, but their liberty to choose is protected. Exit options are to abstain or seek unregulated sources.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, adult_consumers, beneficiary,
    moderate, biographical, mobile, national).

% Operates legally, paying taxes and adhering to regulations. Profits from the sale of substances, creating jobs and economic activity. Benefits from the elimination of black market competition and access to legitimate financial systems.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    organized, generational, arbitrage, national).

% Collects tax revenue from legal substance sales, which can be used to fund public services or mitigate externality costs. Sets and enforces regulations on production, distribution, and sale, focusing on public safety and revenue generation.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_revenue_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Bear the costs of negative externalities such as impaired driving incidents, public intoxication, or secondhand exposure, which are not fully captured by taxation or enforcement. They have limited direct recourse against individual users or the industry.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_parties_affected_by_externalities, payer,
    powerless, immediate, trapped, local).

% Are pushed out of the legal market, losing revenue and facing continued law enforcement pressure if they persist in illicit activities. They may adapt by moving into gray markets or other illegal ventures.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, black_market_operators, excluded,
    powerful, biographical, constrained, global).

% Monitor public health outcomes, advocate for harm reduction measures, and assess the effectiveness of taxation and regulation in mitigating negative impacts. They provide data and policy recommendations but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual liberty with public safety by allowing legal access to substances while establishing a framework for taxation and regulation to manage third-party harms and generate revenue.
% TRANSFER_FUNCTION: Transfers tax revenue from legal substance sales to the state, and transfers the burden of managing externalities (e.g., public safety costs) to the state and, to some extent, to affected third parties.
% ABSENT_VOICES: The prohibitionist perspective, which views substance use as a moral failing and demands state punishment, is largely excluded from the policy-making process under this reading. They would argue for stricter controls and criminalization.
% DISAPPEARANCE_RATIONALE: If this legalization framework vanished, the legal substance industry would collapse, tax revenues would disappear, and a black market would likely re-emerge to meet demand, leading to a significant reorganization of economic and criminal justice systems.
% FOUNDING_PROBLEM: The founding problem was the conflict between individual autonomy over personal choices and the state's interest in public order and safety, exacerbated by the failures and costs of prohibition.
% FOUNDING_PROBLEM_CORROBORATION: Economists and civil liberties organizations corroborate that the tension between individual liberty and state control over substances remains a live issue, and that prohibition has historically failed to achieve its goals while creating significant social costs. Public health data also corroborates the ongoing challenge of managing substance-related harms.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).

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
 *   Extractiveness is low (0.3) because the primary goal is not to extract from users but to manage externalities and generate revenue. Suppression is also low (0.2) as the state's coercive power is redirected from users to regulating the legal industry and preventing specific harms. Theater ratio is low (0.1) as the system is designed to be functional and transparent, with less need for performative enforcement against users. The metrics reflect a system that aims for efficient coordination and limited extraction, consistent with a Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adult consumers and the legal industry, this constraint is a clear Rope, facilitating commerce and individual choice. From the perspective of third parties affected by externalities, it may feel more extractive, as they bear costs that are not fully mitigated. The state, as agenda-setter, views it as a balanced approach to public policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Adult consumers and the legal substance industry are clear beneficiaries (low d) as they gain legal access and profit, respectively. State revenue agencies are agenda-setters and beneficiaries, collecting taxes. Third parties affected by externalities are victims (high d) as they bear unmitigated costs. Black market operators are excluded, facing continued suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a system designed for liberty and revenue generation as pure extraction. While some extraction occurs (taxes, unmitigated externalities), the primary function is coordination of a legal market and protection of individual choice, rather than coercive control. The low theater ratio indicates that the system's stated purpose aligns with its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_capture_completeness,
    'To what extent do taxation and regulation fully capture and mitigate the negative externalities associated with substance use, or do significant unmitigated costs remain for third parties?',
    'Comprehensive economic and social impact studies comparing tax revenue dedicated to externality mitigation with the actual costs borne by third parties (e.g., healthcare, public safety, environmental impact).',
    'If externalities are largely unmitigated, the effective extractiveness on third parties is higher than currently measured, potentially shifting their seat classification towards Snare. If fully mitigated, the system is more purely a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_completeness, empirical, 'Assesses the completeness of externality cost capture under legalization.').

omega_variable(
    black_market_persistence,
    'Does a significant black market persist in ''gray areas'' (e.g., unregulated products, underage sales) despite legalization, and what is its impact on the constraint''s effectiveness and the state''s enforcement burden?',
    'Market analysis and law enforcement data tracking the size and scope of illicit substance markets post-legalization, including product quality and consumer demographics.',
    'Persistent black markets would indicate higher effective suppression is still required to maintain the legal framework, and that the ''excluded'' status of black market operators is not fully effective, potentially increasing the overall extractiveness and suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Examines the extent of black market persistence post-legalization.').

omega_variable(
    reading_framing_legitimacy,
    'Is the framing of substance use as primarily an ''individual liberty issue'' a legitimate and comprehensive lens, or does it obscure underlying public health or social justice concerns that would be better addressed by alternative readings?',
    'Comparative policy analysis across jurisdictions adopting different readings (legalization, prohibition, harm reduction) to assess differential outcomes in public health, crime rates, and social equity. Deliberative democracy processes to gauge public consensus on primary framing.',
    'If the individual liberty framing is found to systematically obscure significant unaddressed harms or inequities, it could suggest a conceptual flaw in the constraint''s design, potentially leading to a re-evaluation of its claimed type or a shift in policy towards a harm reduction or more nuanced approach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Examines the conceptual legitimacy and comprehensiveness of the individual liberty framing for substance control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__legalization_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__legalization_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__legalization_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.29).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__legalization_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__legalization_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__legalization_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'legalization_reading' of the 'substance_control_kernel', focusing on individual liberty and externality management. It is distinct from the 'prohibition_reading' (criminalization) and 'harm_reduction_reading' (public health pragmatism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
