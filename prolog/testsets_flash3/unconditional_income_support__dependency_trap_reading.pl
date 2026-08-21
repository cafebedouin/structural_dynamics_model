% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support (Dependency Trap Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'dependency trap' reading of
 *   unconditional income support, where the policy is viewed as an
 *   incentive-distorting subsidy that rewards idleness, crowds out more
 *   effective targeted aid, and redistributes wealth upward to non-needy
 *   recipients. It is classified as a Snare due to its high extractiveness
 *   from the working poor and taxpayers, and its reliance on active
 *   enforcement (e.g., defunding targeted programs) to maintain its
 *   structure. This reading emphasizes the negative consequences for labor
 *   market participation and fiscal responsibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.85).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.7).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support (Dependency Trap Reading)").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '04b3f820-9ac4-4abd-a4cc-ffd32acb43d5').
narrative_ontology:cs_kernel_codification('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', formalized).
narrative_ontology:cs_authority_grounding('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', extraction).
narrative_ontology:cs_interpretation_layer_present('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5').
narrative_ontology:cs_reading_relation('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', foundational, incentives_drive_productivity).
narrative_ontology:cs_axiom_status(incentives_drive_productivity, holdable).
narrative_ontology:cs_axiom_grounding('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', incentives_drive_productivity, empirically_contingent).
narrative_ontology:cs_axiom('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', foundational, targeted_aid_is_efficient).
narrative_ontology:cs_axiom_status(targeted_aid_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', targeted_aid_is_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', meritocratic_welfare_state).
narrative_ontology:cs_drift_state('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', contemporary_policy_debate, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('04b3f820-9ac4-4abd-a4cc-ffd32acb43d5', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income transfers despite not needing them for basic subsistence, effectively a net subsidy from the working poor and taxpayers. They benefit from the universality of the program without bearing its full costs or experiencing its negative incentives.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients, beneficiary,
    powerful, biographical, mobile, national).

% Gain political capital and validation for their policy agenda through the implementation of unconditional income support. Their ideological commitment to universality is vindicated, even if the practical outcomes are seen as perverse by critics.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocates, beneficiary,
    organized, generational, analytical, national).

% Lose access to targeted welfare programs (e.g., housing assistance, food stamps, job training) that provided greater net benefit than the unconditional income amount. They are incentivized to reduce work effort due to the income floor, but without sufficient support to escape poverty, leading to a dependency trap. Their alternatives are suppressed by the replacement of existing aid structures.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, immediate, trapped, local).

% Bear the net fiscal cost of the program, estimated at $1.4 trillion after offsets. They see their tax contributions redistributed to individuals who do not require the support, while also funding a system that, from this reading, distorts labor incentives.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Their programs are defunded or replaced by the universal scheme, leading to a loss of institutional capacity and expertise in addressing specific poverty needs. They would argue for the efficacy of means-tested, conditional aid but are sidelined by the push for universality.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, targeted_welfare_agencies, excluded,
    institutional, biographical, constrained, national).

% Analyze the program's impact on labor markets, fiscal sustainability, and individual responsibility. They highlight evidence of reduced employment and upward redistribution, using these findings to argue against the policy's efficacy and fairness.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, economic_conservatives, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simplifies welfare administration by providing a single, unconditional transfer, theoretically reducing bureaucratic overhead and stigma associated with means-tested programs.
% TRANSFER_FUNCTION: Transfers a fixed income amount to all citizens, funded by general taxation, resulting in a net transfer from taxpayers (including the working poor) to all recipients, including the non-needy.
% ABSENT_VOICES: Advocates for targeted, conditional welfare programs are marginalized, as their expertise and program structures are deemed unnecessary or inefficient under a universal scheme. The working poor, whose specific needs are unmet by the universal transfer, lack a strong collective voice to advocate for their lost targeted benefits.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, the welfare landscape would revert to a more targeted, conditional system. Labor market participation would likely increase, and the fiscal burden on taxpayers would decrease, leading to a significant reorganization of social policy and economic incentives.
% FOUNDING_PROBLEM: The perceived complexity, inefficiency, and stigmatizing nature of existing welfare systems, alongside concerns about future job displacement due to automation.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of unconditional income support argue the founding problems (welfare stigma, administrative burden) remain live. Critics, including many economists and social policy experts outside the UBI advocacy community, argue that while these problems exist, unconditional income support exacerbates other issues (dependency, fiscal unsustainability) and that the original problem of poverty alleviation is better addressed through targeted means.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant net cost to taxpayers and the opportunity cost for the working poor who lose more valuable targeted aid. Suppression (0.70) is present through the active replacement of existing welfare structures, limiting alternatives for those who would benefit more from conditional support. The theater ratio (0.20) is relatively low, as the program's stated goals of administrative simplicity and stigma reduction are genuinely pursued, but its primary function, from this reading, has shifted to an extractive transfer.
 *
 * PERSPECTIVAL GAP:
 *   The 'dependency trap' reading highlights a significant perspectival gap with other readings. While proponents (freedom_floor_reading) see UBI as empowering, this reading sees it as disempowering the working poor by creating a trap. The engine's classification as a Snare from this perspective directly contrasts with a potential Rope or Scaffold classification from a 'freedom floor' perspective, demonstrating how different structural interpretations lead to divergent classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Middle/upper-class recipients and UBI advocates are beneficiaries, as they receive transfers or political validation without significant cost. The working poor are victims, as they face reduced employment incentives and lose more beneficial targeted aid. Taxpayers are also victims, bearing the fiscal burden. Targeted welfare agencies are excluded, as their function is undermined.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_impact_ambiguity,
    'What is the true long-term impact of unconditional income support on labor market participation and employment rates, disaggregated by income level and pre-existing employment status?',
    'Longitudinal studies with robust control groups, tracking employment and income trajectories over multiple years, specifically designed to isolate the UBI effect from other economic factors.',
    'If employment impacts are negligible or positive for the working poor, the ''dependency trap'' claim weakens, potentially shifting the constraint towards a less extractive classification. If negative impacts are confirmed, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_impact_ambiguity, empirical, 'Uncertainty regarding the empirical evidence for UBI''s impact on employment and work incentives.').

omega_variable(
    fiscal_sustainability_ambiguity,
    'What are the full, long-term fiscal implications of unconditional income support, considering dynamic economic effects, tax base changes, and potential offsets from reduced social program costs?',
    'Comprehensive macroeconomic modeling that incorporates behavioral responses (e.g., labor supply, consumption patterns) and accounts for all direct and indirect fiscal impacts over several decades.',
    'If the program proves fiscally sustainable or even beneficial due to unforeseen economic growth or reduced social costs, the ''taxpayer victim'' claim weakens. If it leads to unsustainable deficits, the Snare classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_ambiguity, empirical, 'Uncertainty regarding the long-term fiscal sustainability and true cost of unconditional income support.').

omega_variable(
    program_replacement_efficacy,
    'Does the unconditional income transfer genuinely replace the value and efficacy of the targeted welfare programs it displaces, particularly for the most vulnerable populations?',
    'Comparative analysis of outcomes for specific vulnerable groups (e.g., disabled, single parents, homeless) under targeted programs versus unconditional income support, measuring health, housing, and educational attainment.',
    'If targeted programs are found to be more effective for specific groups, the claim that UBI ''crowds out targeted aid'' is reinforced, strengthening the Snare classification. If UBI proves equally or more effective, the victim status of the working poor is mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_replacement_efficacy, empirical, 'Uncertainty about whether UBI effectively replaces targeted welfare programs for vulnerable populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__dependency_trap_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__dependency_trap_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__dependency_trap_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__dependency_trap_reading, base_extractiveness, 10, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__dependency_trap_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__dependency_trap_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unconditional_income_support' kernel. Its high extractiveness and Snare classification contrast with the 'freedom_floor_reading' (which emphasizes autonomy and may classify as Rope/Scaffold) and the 'universality_paradox_reading' (which focuses on political dynamics and fiscal convergence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
