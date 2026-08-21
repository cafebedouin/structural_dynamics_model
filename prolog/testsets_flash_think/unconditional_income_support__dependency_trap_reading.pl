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
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint is the 'dependency trap' reading of the
 *   'unconditional_income_support' kernel. It frames UBI as an
 *   incentive-distorting subsidy that rewards idleness, crowds out more
 *   effective targeted aid, and redistributes upward to non-needy
 *   populations, leading to a net fiscal burden and increased social
 *   dependency. This reading contrasts sharply with the
 *   'freedom_floor_reading' (autonomy-enabling) and the
 *   'universality_paradox_reading' (politically ambiguous). The high
 *   extractiveness and suppression reflect the view that UBI, despite its
 *   stated goals, functions to extract resources from taxpayers and the
 *   working poor while creating a new form of dependency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.85).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.75).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'eddb1f13-62e7-4d7f-9300-f9108e9a40d2').
narrative_ontology:cs_kernel_codification('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', formalized).
narrative_ontology:cs_authority_grounding('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', extraction).
narrative_ontology:cs_interpretation_layer_present('eddb1f13-62e7-4d7f-9300-f9108e9a40d2').
narrative_ontology:cs_reading_relation('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', unconditional_income_support__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', foundational, universal_transfers_distort_incentives).
narrative_ontology:cs_axiom_status(universal_transfers_distort_incentives, holdable).
narrative_ontology:cs_axiom_grounding('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', universal_transfers_distort_incentives, empirically_contingent).
narrative_ontology:cs_axiom('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', foundational, targeted_aid_is_superior_for_poverty_reduction).
narrative_ontology:cs_axiom_status(targeted_aid_is_superior_for_poverty_reduction, holdable).
narrative_ontology:cs_axiom_grounding('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', targeted_aid_is_superior_for_poverty_reduction, empirically_contingent).
narrative_ontology:cs_reference_frame('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', traditional_welfare_state_principles).
narrative_ontology:cs_drift_state('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', contemporary_ubi_pilot_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eddb1f13-62e7-4d7f-9300-f9108e9a40d2', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor_losing_targeted_aid).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Government agencies or NGOs tasked with implementing and managing unconditional income support programs. They manage the distribution of funds and the phasing out of other welfare programs, often operating under a mandate to simplify welfare and reduce administrative overhead.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_program_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Individuals and households in higher income brackets who receive unconditional income support. For them, it represents a net gain or a minor subsidy, as they do not rely on it for basic needs and may pay less in taxes than they receive, or benefit from reduced social friction. They are not the intended primary beneficiaries but receive the transfer due to universality.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients, beneficiary,
    moderate, immediate, mobile, local).

% Academics, activists, and political groups who champion unconditional income support. They gain political capital, research funding, and influence from the implementation and discussion of UBI, seeing it as a progressive policy regardless of its specific distributional outcomes.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocates, beneficiary,
    organized, generational, analytical, global).

% Low-income individuals and families who previously relied on targeted welfare programs (e.g., housing assistance, food stamps, childcare subsidies) that are reduced or eliminated in favor of unconditional income support. They often find the universal payment insufficient to cover the loss of specific, higher-value targeted benefits, effectively leaving them worse off and trapped in a cycle of dependency or underemployment.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor_losing_targeted_aid, payer,
    powerless, immediate, trapped, local).

% Citizens who contribute to the tax base funding unconditional income support. They bear the net fiscal cost of the program, especially when it results in significant upward redistribution or disincentivizes labor force participation, leading to a perception of subsidizing idleness.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Organizations and policymakers who argue for the efficacy and necessity of means-tested, conditional welfare programs. They are often sidelined or actively opposed by the universalist UBI framework, as their preferred solutions are replaced or de-emphasized, despite evidence of their effectiveness for specific vulnerable populations.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, targeted_aid_advocates, excluded,
    organized, generational, constrained, national).

% Economists, policy analysts, and political groups who critically evaluate unconditional income support from a fiscal responsibility and incentive-based perspective. They analyze its impact on labor markets, government budgets, and economic growth, often highlighting its costs and disincentive effects.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, fiscal_conservatives_analysts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to simplify welfare administration, reduce bureaucracy, and provide a universal safety net that ensures basic income security for all citizens, regardless of employment status.
% TRANSFER_FUNCTION: Transfers income from the general tax base to all citizens, including those who do not need it, while simultaneously replacing or reducing existing targeted welfare programs, thereby shifting resources away from the most vulnerable.
% ABSENT_VOICES: Advocates for targeted, conditional aid and those who emphasize work requirements are often excluded from the core policy debate, as the universalist framework of UBI inherently de-prioritizes their concerns about incentives and specific needs.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, the fiscal burden on taxpayers would significantly decrease, and the labor market would likely see an increase in participation. However, the social safety net would need to be entirely re-evaluated and rebuilt with targeted programs to address the needs of the working poor who lost their previous aid, leading to substantial social and economic reorganization.
% FOUNDING_PROBLEM: The founding problem was to simplify complex welfare systems, reduce poverty, and provide a universal safety net that could adapt to future economic changes like automation.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (UBI advocates) attest that the founding problem of poverty and welfare complexity is still live. Critics (fiscal conservatives, targeted aid advocates) argue that while the original problems may persist, UBI, as implemented, creates new, more severe problems related to work incentives, fiscal sustainability, and equitable redistribution, with economic studies and social impact assessments from outside UBI advocacy groups corroborating these negative impacts.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.85) is high due to the significant fiscal cost and the negative impact on labor force participation, as evidenced by studies showing employment reductions in large pilots. Suppression (0.75) is high because the universal nature of UBI replaces more effective targeted programs, trapping the working poor in a less beneficial system and disincentivizing work. The theater ratio (0.20) is low because, from this reading's perspective, the stated coordination function (poverty reduction, welfare simplification) is largely undermined by the actual outcomes of dependency and upward redistribution. The measurement series show a trend of increasing extractiveness and suppression as the policy matures, with the performative aspect diminishing.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of UBI proponents (not represented in this specific reading's classification, but acknowledged as a sibling reading), the same policy would be seen as a 'rope' or 'scaffold' providing a vital safety net. However, from the 'dependency trap' reading, the structural outcomes are clearly extractive, creating a snare for the working poor and a fiscal burden for taxpayers. The engine's computation of per-seat classification will highlight this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'middle_upper_class_recipients' and 'ubi_advocates' are beneficiaries, as they either receive transfers without needing them or gain political capital. The 'working_poor_losing_targeted_aid' are clear victims, as they lose more valuable targeted benefits. 'Taxpayers' are also victims, bearing the fiscal cost. 'Targeted_aid_advocates' are excluded, as their policy preferences are actively undermined by the UBI framework. 'Fiscal_conservatives_analysts' serve as observers, analyzing the negative impacts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disincentive_mechanism_ambiguity,
    'Is the observed disincentive to work a fundamental human response to unconditional income, or a consequence of specific policy design flaws (e.g., benefit cliffs, insufficient complementary services)?',
    'Comparative analysis of UBI pilots with varying design parameters (e.g., phase-out rates, integration with social services) and longitudinal studies tracking labor market behavior post-implementation.',
    'If fundamental, the high extractiveness and suppression are inherent to UBI''s structure. If design-dependent, the constraint could be re-engineered to reduce negative incentives, potentially lowering extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disincentive_mechanism_ambiguity, empirical, 'Whether work disincentives are inherent to UBI or modifiable by design.').

omega_variable(
    crowding_out_inevitability,
    'Is the ''crowding out'' of targeted aid an inevitable consequence of UBI''s universality and fiscal demands, or a policy choice driven by ideological preferences for simplification over specificity?',
    'Policy analysis comparing jurisdictions that implement UBI alongside robust targeted programs versus those that replace them, and examining the stated rationales for program consolidation.',
    'If inevitable, the victim status of the working poor is a structural feature. If a policy choice, UBI could theoretically coexist with or complement targeted aid, altering the beneficiary/victim structure and potentially lowering extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_inevitability, conceptual, 'Whether UBI inherently replaces targeted aid or if this is a policy choice.').

omega_variable(
    upward_redistribution_function,
    'Is the upward redistribution to non-needy recipients an unavoidable feature of UBI''s universality, or a ''bug'' that could be mitigated through progressive taxation or clawback mechanisms?',
    'Modeling the net fiscal impact of UBI under different tax and transfer regimes, and analyzing the political feasibility of implementing highly progressive clawback mechanisms.',
    'If unavoidable, the beneficiary status of the middle/upper class is a structural consequence. If mitigable, the constraint''s extractiveness could be reduced by re-directing benefits more effectively, shifting the beneficiary profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(upward_redistribution_function, preference, 'Whether upward redistribution is inherent to UBI or can be mitigated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(unco_tr_t6, unconditional_income_support__dependency_trap_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(unco_tr_t18, unconditional_income_support__dependency_trap_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__dependency_trap_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__dependency_trap_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(unco_be_t6, unconditional_income_support__dependency_trap_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(unco_be_t18, unconditional_income_support__dependency_trap_reading, base_extractiveness, 18, 0.8).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__dependency_trap_reading, base_extractiveness, 24, 0.83).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__dependency_trap_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(unco_su_t6, unconditional_income_support__dependency_trap_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(unco_su_t18, unconditional_income_support__dependency_trap_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(unco_su_t24, unconditional_income_support__dependency_trap_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__dependency_trap_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
