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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   This constraint describes unconditional income support from the
 *   'dependency trap' perspective, arguing it acts as an incentive-distorting
 *   subsidy. It rewards idleness, crowds out more effective targeted aid, and
 *   redistributes wealth upward to non-needy populations. This reading
 *   highlights the negative employment effects (e.g., AEI meta-analysis
 *   showing -3.2% employment in large pilots) and the significant fiscal
 *   burden on taxpayers, leading to a classification as a Snare.
 *
 * KEY AGENTS:
 *   - government_agencies: Agenda setter (institutional/constrained)
 *   - middle_upper_class: Primary beneficiary (powerful/mobile)
 *   - ubi_advocates: Secondary beneficiary (organized/mobile)
 *   - working_poor: Primary payer/victim (powerless/trapped)
 *   - taxpayers: Secondary payer/victim (moderate/constrained)
 *   - economists_critics: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.85).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.75).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'fea95618-d150-44ca-811a-fe17e3091673').
narrative_ontology:cs_kernel_codification('fea95618-d150-44ca-811a-fe17e3091673', formalized).
narrative_ontology:cs_authority_grounding('fea95618-d150-44ca-811a-fe17e3091673', extraction).
narrative_ontology:cs_interpretation_layer_present('fea95618-d150-44ca-811a-fe17e3091673').
narrative_ontology:cs_reading_relation('fea95618-d150-44ca-811a-fe17e3091673', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('fea95618-d150-44ca-811a-fe17e3091673', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('fea95618-d150-44ca-811a-fe17e3091673', foundational, work_incentives_are_paramount).
narrative_ontology:cs_axiom_status(work_incentives_are_paramount, holdable).
narrative_ontology:cs_axiom_grounding('fea95618-d150-44ca-811a-fe17e3091673', work_incentives_are_paramount, empirically_contingent).
narrative_ontology:cs_axiom('fea95618-d150-44ca-811a-fe17e3091673', foundational, targeted_aid_is_efficient).
narrative_ontology:cs_axiom_status(targeted_aid_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('fea95618-d150-44ca-811a-fe17e3091673', targeted_aid_is_efficient, instrumental).
narrative_ontology:cs_reference_frame('fea95618-d150-44ca-811a-fe17e3091673', incentive_compatible_welfare_state).
narrative_ontology:cs_drift_state('fea95618-d150-44ca-811a-fe17e3091673', contemporary_policy_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fea95618-d150-44ca-811a-fe17e3091673', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the unconditional income support program, manage its fiscal implications, and defend its policy rationale. They are constrained by political pressures, public opinion, and the need to maintain fiscal stability.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive unconditional transfers, which for them represent a net gain or a minor subsidy, as they do not rely on means-tested benefits. They benefit from the universality without needing the poverty-alleviation aspect, contributing to upward redistribution.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class, beneficiary,
    powerful, biographical, mobile, national).

% Gain political capital and validation for their policy agenda. They benefit from the implementation of unconditional income support, regardless of its specific outcomes, as it advances their ideological goals of universal provision.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocates, beneficiary,
    organized, generational, mobile, global).

% Experience a net loss as targeted, often more generous, aid programs are replaced by a universal but insufficient unconditional income. They are trapped by the loss of alternatives and the disincentive effects on low-wage work, leading to a dependency trap.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, immediate, trapped, local).

% Bear the significant fiscal cost of the universal program through general taxation, often without receiving commensurate benefits or seeing the intended social improvements. Their exit options are limited to political action or emigration.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Analyze the economic impacts of unconditional income support, often highlighting disincentive effects, fiscal unsustainability, and unintended upward redistribution. They provide the empirical basis for the 'dependency trap' reading.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, economists_critics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_upper_class).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint primarily coordinates a universal transfer mechanism, but its claimed coordination of poverty reduction or administrative simplification is undermined by its incentive-distorting and crowding-out effects.
% TRANSFER_FUNCTION: Transfers a fixed income amount to all citizens, funded by general taxation. This results in a net transfer from taxpayers (including the working poor via lost targeted aid) to all recipients, including those who do not need it, and to UBI advocates via policy validation.
% ABSENT_VOICES: Advocates for targeted welfare programs, who would argue for means-tested aid that maximizes impact on the truly needy. Fiscal conservatives, who would object to the massive unfunded mandate and its long-term economic distortions.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, the massive fiscal burden on taxpayers would disappear, targeted welfare programs might be restored, and labor market incentives would shift, potentially increasing labor supply among low-income groups. The social safety net would need to be entirely re-evaluated.
% FOUNDING_PROBLEM: The stated founding problem was to alleviate poverty, reduce administrative burden of complex welfare systems, and provide a safety net against automation. This reading argues these problems are either not solved or exacerbated by the proposed solution.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., UBI advocates) claim it addresses persistent poverty and future job displacement. Critics (e.g., AEI, CATO Institute, some labor economists) provide meta-analyses of pilot programs showing negative employment effects and fiscal analyses demonstrating unsustainable costs, corroborating that the founding problem is either not solved or worsened by the proposed solution.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) due to the significant fiscal cost and the negative impact on labor supply among the working poor. Suppression is high (0.75) because the program replaces more valuable targeted aid, effectively trapping the working poor in a less beneficial system, and imposes costs on taxpayers with limited exit. Theater ratio is low (0.15) as the mechanism is direct transfer and policy implementation, not performative maintenance. Accessibility collapse is moderate-high (0.65) as alternatives (targeted aid) are explicitly crowded out. Resistance is high (0.7) from fiscal conservatives and some welfare advocates.
 *
 * PERSPECTIVAL GAP:
 *   The 'dependency trap' reading fundamentally diverges from 'freedom floor' readings. While proponents see UBI as empowering, this reading views it as disempowering for the working poor by removing more effective support and distorting incentives. The engine's classification as a Snare reflects the structural extraction and suppression identified by this reading, contrasting with the Rope classification that might emerge from a 'freedom floor' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Government agencies are the agenda setters, implementing the policy. The middle/upper class and UBI advocates are beneficiaries, receiving transfers or political validation. The working poor are primary victims, losing more valuable aid and facing disincentives. Taxpayers are also victims, bearing the fiscal cost. Economists and critics serve as analytical observers, documenting the negative impacts.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a potentially extractive system as pure coordination. The 'dependency trap' reading argues that while UBI is presented as solving social problems, its actual operation creates new forms of dependency and extraction, particularly from the working poor and taxpayers. The high extractiveness and suppression, coupled with the contested founding problem status, indicate that the mandate has either shifted or was flawed from the outset, making it a Snare rather than a genuine Rope or Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    is_disincentive_effect_structural,
    'Is the observed disincentive effect on labor supply a structural property of unconditional transfers, or a transient effect of pilot programs and specific design choices?',
    'Longitudinal studies of large-scale, permanent unconditional income implementations across diverse economic contexts, comparing labor market outcomes with control groups.',
    'If structural, the ''dependency trap'' reading is strongly validated, reinforcing the Snare classification. If transient or context-dependent, the extractiveness and suppression metrics might be lower, potentially shifting classification towards a Tangled Rope or even a Rope if benefits outweigh costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(is_disincentive_effect_structural, empirical, 'Structural vs. transient nature of UBI''s labor disincentive.').

omega_variable(
    crowding_out_as_policy_choice,
    'Is the ''crowding out'' of more valuable targeted aid programs an inevitable consequence of unconditional income''s universality and fiscal constraints, or a policy choice that could be avoided with different program design?',
    'Comparative policy analysis of unconditional income proposals that explicitly integrate or preserve targeted aid, alongside fiscal modeling of their feasibility and impact.',
    'If inevitable, the Snare classification is strengthened as the harm to the working poor is inherent. If avoidable, the constraint''s extractiveness might be reduced by alternative designs, potentially shifting its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_as_policy_choice, conceptual, 'Inevitable vs. avoidable crowding out of targeted aid.').

omega_variable(
    upward_redistribution_intent,
    'Is the upward redistribution of funds to the non-needy (who receive unconditional income but pay less in taxes than they receive) an unintended side effect, or an implicit design feature to secure broader political support for the program?',
    'Analysis of legislative history, policy advocacy documents, and public statements from unconditional income proponents regarding the political strategy behind universal, rather than means-tested, transfers.',
    'If an implicit design feature, it reinforces the ''snare'' aspect by highlighting a deliberate mechanism for political capture at the expense of fiscal efficiency and targeted poverty reduction. If unintended, it points to a design flaw rather than a deliberate extractive mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(upward_redistribution_intent, preference, 'Intentional vs. unintentional upward redistribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unco_tr_t6, unconditional_income_support__dependency_trap_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(unco_tr_t18, unconditional_income_support__dependency_trap_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__dependency_trap_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__dependency_trap_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(unco_be_t6, unconditional_income_support__dependency_trap_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(unco_be_t18, unconditional_income_support__dependency_trap_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__dependency_trap_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__dependency_trap_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(unco_su_t6, unconditional_income_support__dependency_trap_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(unco_su_t18, unconditional_income_support__dependency_trap_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(unco_su_t24, unconditional_income_support__dependency_trap_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__dependency_trap_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'dependency trap' reading of the 'unconditional_income_support' kernel, focusing on disincentive effects and perverse redistribution. It is distinct from the 'freedom_floor_reading' (autonomy-enabling) and 'universality_paradox_reading' (political ambiguity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
