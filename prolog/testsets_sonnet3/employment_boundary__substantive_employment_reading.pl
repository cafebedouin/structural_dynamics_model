% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Reading of the Platform Worker Boundary
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested employment-boundary
 *   kernel: the substantive/economic-realities reading, which holds that
 *   employment status is defined by economic dependence and algorithmic
 *   control rather than by contract form. Under this reading, platform
 *   delivery drivers, ridehail drivers, and gig taskers are
 *   employees-in-fact, and platforms that have structured their contracts to
 *   claim independent-contractor status are, in substance, misclassifying
 *   employees to externalize the cost of social insurance and job security.
 *   The sibling readings (formalist: contract form controls; hybrid: a
 *   tailored third category) are NOT part of this constraint — they are
 *   separate constraints with their own ε and stakeholder structures, linked
 *   via network edges. This story's ε (0.62) reflects the extraction the
 *   substantive reading identifies in the standing arrangement it is
 *   contesting: platforms currently operating under the formalist shield
 *   while exercising employment-level control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.62).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.58).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Reading of the Platform Worker Boundary").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '973b3563-7077-4f96-b910-2b1f4cfa8f78').
narrative_ontology:cs_kernel_codification('973b3563-7077-4f96-b910-2b1f4cfa8f78', distributed).
narrative_ontology:cs_authority_grounding('973b3563-7077-4f96-b910-2b1f4cfa8f78', distributed).
narrative_ontology:cs_reading_relation('973b3563-7077-4f96-b910-2b1f4cfa8f78', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('973b3563-7077-4f96-b910-2b1f4cfa8f78', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('973b3563-7077-4f96-b910-2b1f4cfa8f78', foundational, economic_dependence_and_control_define_employment_regardless_of_label).
narrative_ontology:cs_axiom_status(economic_dependence_and_control_define_employment_regardless_of_label, holdable).
narrative_ontology:cs_axiom_grounding('973b3563-7077-4f96-b910-2b1f4cfa8f78', economic_dependence_and_control_define_employment_regardless_of_label, conventional).
narrative_ontology:cs_axiom('973b3563-7077-4f96-b910-2b1f4cfa8f78', foundational, algorithmic_management_is_functionally_equivalent_to_direct_supervision).
narrative_ontology:cs_axiom_status(algorithmic_management_is_functionally_equivalent_to_direct_supervision, holdable).
narrative_ontology:cs_axiom_grounding('973b3563-7077-4f96-b910-2b1f4cfa8f78', algorithmic_management_is_functionally_equivalent_to_direct_supervision, empirically_contingent).
narrative_ontology:cs_reference_frame('973b3563-7077-4f96-b910-2b1f4cfa8f78', common_law_control_and_dependence_test).
narrative_ontology:cs_drift_state('973b3563-7077-4f96-b910-2b1f4cfa8f78', platform_economy_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('973b3563-7077-4f96-b910-2b1f4cfa8f78', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_operators_under_formalist_shield).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_delivery_drivers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_ridehail_drivers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_gig_taskers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, consumers_and_merchants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work under an app that sets prices, routes, acceptance-rate thresholds, and deactivation criteria unilaterally. Under this reading they are already employees in substance — economically dependent on a single algorithmic dispatcher and subject to its behavioral control — but are denied the minimum wage floor, unemployment insurance, workers' compensation, and collective bargaining rights that formal employee status would trigger. Exit means leaving the income entirely; multi-apping does not restore bargaining power because the same algorithmic logic governs every platform.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_delivery_drivers, payer,
    powerless, biographical, constrained, national).

% Subject to algorithmic fare-setting, ride-matching, and rating-based deactivation with no appeal process comparable to due process. Under the substantive reading their dependence on the platform's dispatch algorithm for essentially all income, combined with the platform's real-time behavioral control, satisfies the functional test for employment regardless of the independent-contractor label in their signed agreement.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_ridehail_drivers, payer,
    powerless, biographical, constrained, national).

% Perform tasks assigned and priced by a platform algorithm, often with no direct human supervisor but pervasive algorithmic scoring and ranking that functions as supervision by other means. They bear full liability for equipment, insurance, and downtime with no employer-side safety net.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_gig_taskers, payer,
    powerless, biographical, constrained, national).

% Design the contract terms, algorithmic dispatch and control systems, and public-policy lobbying strategy that together keep workers classified as independent contractors. Currently capture the cost savings of avoiding payroll tax, minimum wage, unemployment insurance, and benefits obligations. Under the substantive reading, they become the obligated party who must fund full social insurance and job security once economic dependence and algorithmic control are recognized as employment-defining. They resist reclassification through litigation, ballot initiatives, and contract redesign aimed at diluting the control indicators this reading relies on.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_operators_under_formalist_shield, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, platform_operators_under_formalist_shield, beneficiary).

% Adjudicate misclassification claims and enforcement actions using economic-dependence and control tests. Their rulings determine whether the substantive reading's employee-in-fact standard is applied against particular platforms, and their capacity is contested terrain between worker advocates and platform lobbying.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_regulators_and_courts, observer,
    institutional, generational, analytical, national).

% Receive low-cost, on-demand delivery and transport services whose price partly reflects the absence of employer-side labor costs. Under the substantive reading, if reclassification succeeds, they may see price increases as platforms internalize social insurance costs, but they are not parties to the enforcement contest and are not represented in it directly.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, consumers_and_merchants, beneficiary,
    organized, biographical, mobile, national).

% Taxi companies, courier firms, and other incumbents that must classify comparable workers as employees under existing law compete against platforms that externalize the same costs by relying on the formalist contract label. They would testify that the substantive reading simply restores a level playing field, but are rarely named parties in misclassification litigation.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, displaced_traditional_employers, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_operators_under_formalist_shield).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The underlying platform-dispatch system does solve a real coordination problem — matching fragmented, intermittent labor supply to fragmented, intermittent demand in real time at a scale and speed prior brokerage models could not achieve.
% TRANSFER_FUNCTION: The independent-contractor label, layered on top of that coordination function, moves the cost of unemployment insurance, workers' compensation, minimum-wage guarantees, payroll tax, and equipment/liability risk from the platform onto the worker, while the platform retains algorithmic control comparable to direct supervision.
% ABSENT_VOICES: Individual drivers and taskers rarely have standing or resources to litigate misclassification alone; most substantive-reading enforcement depends on class actions, regulatory agencies, or legislative reclassification acting on their behalf, meaning the workers whose classification is contested are frequently not the ones arguing the case.
% DISAPPEARANCE_RATIONALE: If the substantive-employment standard were adopted and enforced overnight, platforms would owe payroll taxes, minimum wage floors, unemployment insurance contributions, and collective bargaining exposure across their entire workforce; pricing, staffing algorithms, and business models built on the contractor classification would have to be restructured, and many platforms have stated this would force market exit or fundamental redesign.
% FOUNDING_PROBLEM: Labor law's employee/contractor line was built to distinguish genuine independent business owners (who set their own prices, own their client relationships, and bear real entrepreneurial risk) from workers who are economically dependent on and behaviorally controlled by a single employer — so that the latter receive the protections the former do not need.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying platform work, several state and national labor regulators (e.g. rulings finding algorithmic control satisfies traditional control tests), and displaced traditional-sector employers who must classify comparable workers as employees all corroborate that the economic-dependence/control problem the doctrine was built to address remains live in the platform context — this corroboration comes from outside the platform operators, who are the beneficiaries of the contrary formalist reading.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) captures the gap between the cost platforms currently avoid (payroll tax, unemployment insurance, workers' comp, minimum wage floors) and what the substantive reading holds they owe given the level of algorithmic control exercised. Suppression (0.58) reflects active platform resistance to reclassification via contract redesign, ballot initiatives, and litigation — this is enforcement-dependent, not passive. Theater ratio (0.40) captures the growing share of platform activity devoted to reclassification defense (legal argument, PR campaigns framing 'flexibility' as worker-serving) rather than to the underlying dispatch coordination function, which remains real. Resistance (0.72) is high because workers and regulators actively contest the formalist label through litigation and organizing; accessibility_collapse (0.45) is moderate because the contractor arrangement has not fully foreclosed alternatives — worker organizing, regulatory enforcement, and legislative reclassification remain live paths, which is precisely why this reading exists as a live contest rather than a settled fact.
 *
 * PERSPECTIVAL GAP:
 *   From the platform operator seat, the current contractor arrangement is efficient, flexible coordination the platform built and that many workers say they prefer. From the driver/tasker seat under this reading, the same structure is misclassification: employment-level control without employment-level protection. The engine computes this divergence from the structural data (power, exit_options, beneficiary/victim declarations) — this story does not adjudicate between the seats, it authors the substantive reading's structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform delivery drivers, ridehail drivers, and gig taskers are the victims under this reading: economically dependent on single algorithmic dispatchers with no meaningful multi-homing escape (d near the full-target end). Platform operators are named beneficiaries of the CURRENT formalist shield they operate under, but under this reading's logic they become obligated beneficiaries — once reclassified, they owe the social insurance and job-security costs the current arrangement lets them avoid. This is why extraction is authored as moderate rather than extreme: the reading identifies real extraction in the standing arrangement without pretending platforms have zero coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing genuine independent contractors from economically dependent, behaviorally controlled workers) remains live per corroboration from regulators, economists, and displaced traditional employers outside the platform beneficiary set — this is not a mandatrophy case where the underlying problem has vanished. The mandatrophy risk runs the other direction: if the formalist contract label persists after the economic-dependence facts it was designed to track have shifted, the LABEL becomes the zombie, not the underlying employment-protection function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_employment_boundary,
    'Which of the three readings of the employment_boundary kernel (formalist, hybrid_security, substantive) will prevail in a given jurisdiction''s law, and does the answer vary by platform business model (delivery vs. ridehail vs. task marketplaces)?',
    'Track jurisdiction-by-jurisdiction legislative and judicial outcomes (e.g. ABC test adoption, misclassification rulings, hybrid-category statutes) over a multi-year window; the sibling reading that accumulates the most binding precedent structurally displaces the others in that jurisdiction without any single reading being conceptually ''refuted.''',
    'If the formalist reading prevails broadly, the victim set authored here (platform_delivery_drivers etc.) never receives standing to claim employee status, and this constraint''s real-world extraction persists uncorrected. If the substantive reading prevails, platforms shift from beneficiaries-of-the-shield to obligated beneficiaries bearing the social-insurance costs this story anticipates. If hybrid prevails, a different, intermediate constraint (hybrid_security_reading) becomes the operative structure and this story''s victim/beneficiary framing is partially superseded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_employment_boundary, preference, 'Which kernel reading of the employment boundary becomes legally operative, and where the disagreement is located (the legal test for ''control'' and ''dependence'').').

omega_variable(
    algorithmic_control_as_supervision_equivalence,
    'Does algorithmic dispatch, pricing, and deactivation constitute ''control'' in the same legal and functional sense as direct human supervision, or is it a structurally distinct form of coordination that the traditional employment test was not built to evaluate?',
    'Comparative case analysis of misclassification rulings that examine algorithmic control indicators (acceptance-rate thresholds, automated deactivation, real-time price-setting) against the traditional common-law control test; convergence or divergence across rulings would indicate whether courts treat the two as functionally equivalent.',
    'If algorithmic control is treated as equivalent to direct supervision, the substantive reading''s core premise is strongly corroborated and extraction estimates in this story likely understate the true figure. If treated as categorically distinct, the substantive reading''s foundational axiom weakens and the hybrid_security_reading''s tailored-category logic gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_as_supervision_equivalence, conceptual, 'Whether algorithmic control is structurally equivalent to traditional supervisory control for employment-classification purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__substantive_employment_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__substantive_employment_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__substantive_employment_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__substantive_employment_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__substantive_employment_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__substantive_employment_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(empl_be_t4, employment_boundary__substantive_employment_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(empl_be_t8, employment_boundary__substantive_employment_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(empl_be_t12, employment_boundary__substantive_employment_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(empl_be_t16, employment_boundary__substantive_employment_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(empl_be_t20, employment_boundary__substantive_employment_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(empl_be_t24, employment_boundary__substantive_employment_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(empl_su_t4, employment_boundary__substantive_employment_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(empl_su_t8, employment_boundary__substantive_employment_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(empl_su_t12, employment_boundary__substantive_employment_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(empl_su_t16, employment_boundary__substantive_employment_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(empl_su_t20, employment_boundary__substantive_employment_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(empl_su_t24, employment_boundary__substantive_employment_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the employment_boundary kernel. formalist_employment_reading holds that contract form controls and authors much lower ε (workers are genuine independent contractors, no misclassification extraction). hybrid_security_reading holds that platform workers occupy a tailored third category and authors intermediate ε with a different, narrower beneficiary/victim structure (some but not full employment protections). substantive_employment_reading (this file) holds employment is defined by economic dependence and algorithmic control regardless of contract form, and authors moderate-to-high ε reflecting the misclassification extraction it identifies in the standing formalist arrangement. All three share the same underlying platform-dispatch coordination mechanism but diverge on which legal test should govern it and therefore on who counts as victim/beneficiary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
