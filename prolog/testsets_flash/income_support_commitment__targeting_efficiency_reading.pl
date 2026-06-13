% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeting Efficiency Principle for Income Support
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'targeting efficiency' reading of the
 *   broader 'income_support_commitment' kernel. It asserts that income
 *   support should be concentrated on individuals demonstrating specific
 *   needs, rather than distributed universally. This reading often frames
 *   universal programs as inefficient or wasteful, arguing that resources are
 *   better spent on those 'most in need.' However, in practice, implementing
 *   this principle can lead to significant extraction from the very
 *   populations it claims to help, especially when universal programs (like
 *   UBI) are proposed as replacements for existing targeted benefits, leading
 *   to net losses for the poorest.
 *
 * KEY AGENTS:
 *   - low_income_households_losing_targeted_benefits: Primary victims (powerless/constrained) — bear extraction from program restructuring.
 *   - taxpayers_averse_to_universal_programs: Primary beneficiaries (organized/mobile) — benefit from perceived fiscal responsibility.
 *   - welfare_state_administrators: Agenda setters (institutional/constrained) — administer and defend the targeted system.
 *   - advocates_for_universal_basic_income: Excluded voices (organized/mobile) — argue for alternative distribution models.
 *   - social_policy_researchers: Analytical observers (analytical/analytical) — analyze the distributional impacts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.75).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.65).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeting Efficiency Principle for Income Support").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '96ca5332-43a4-406c-b87d-7b300a930491').
narrative_ontology:cs_kernel_codification('96ca5332-43a4-406c-b87d-7b300a930491', formalized).
narrative_ontology:cs_authority_grounding('96ca5332-43a4-406c-b87d-7b300a930491', lineage).
narrative_ontology:cs_interpretation_layer_present('96ca5332-43a4-406c-b87d-7b300a930491').
narrative_ontology:cs_reading_relation('96ca5332-43a4-406c-b87d-7b300a930491', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('96ca5332-43a4-406c-b87d-7b300a930491', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('96ca5332-43a4-406c-b87d-7b300a930491', foundational, resources_are_scarce_and_must_be_optimized).
narrative_ontology:cs_axiom_status(resources_are_scarce_and_must_be_optimized, holdable).
narrative_ontology:cs_axiom_grounding('96ca5332-43a4-406c-b87d-7b300a930491', resources_are_scarce_and_must_be_optimized, empirically_contingent).
narrative_ontology:cs_axiom('96ca5332-43a4-406c-b87d-7b300a930491', secondary, universal_provision_is_wasteful).
narrative_ontology:cs_axiom_status(universal_provision_is_wasteful, holdable).
narrative_ontology:cs_axiom_grounding('96ca5332-43a4-406c-b87d-7b300a930491', universal_provision_is_wasteful, empirically_contingent).
narrative_ontology:cs_reference_frame('96ca5332-43a4-406c-b87d-7b300a930491', post_war_welfare_state_targeting_consensus).
narrative_ontology:cs_drift_state('96ca5332-43a4-406c-b87d-7b300a930491', contemporary_ubi_discourse, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('96ca5332-43a4-406c-b87d-7b300a930491', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, taxpayers_averse_to_universal_programs).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, welfare_state_administrators).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, low_income_households_losing_targeted_benefits).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, marginalized_groups_facing_stigma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These households currently rely on targeted welfare programs. Under a policy shift driven by the 'targeting efficiency' principle, where universal programs (like UBI) are funded by cutting existing targeted benefits, they face a net loss of income, becoming victims of the 'efficiency' argument. Their options are limited to navigating complex application processes or facing destitution.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, low_income_households_losing_targeted_benefits, payer,
    powerless, immediate, trapped, local).

% This group perceives universal programs as wasteful and inefficient, preferring that public funds be directed only to those 'demonstrably in need.' They benefit from the political rhetoric and policy outcomes that prioritize targeting, believing it reduces their tax burden or ensures 'responsible' spending. They can organize politically to defend this principle.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, taxpayers_averse_to_universal_programs, beneficiary,
    organized, biographical, mobile, national).

% These are the civil servants and agencies responsible for designing, implementing, and managing targeted income support programs. Their institutional mandate, expertise, and budgets are often tied to the complexity of means-testing and administering these programs. They defend the 'targeting efficiency' principle as essential for their function and the integrity of the welfare state.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, welfare_state_administrators, agenda_setter,
    institutional, generational, constrained, national).

% These groups argue for unconditional income support as a means to reduce poverty, enhance autonomy, and simplify welfare administration. Their proposals directly challenge the 'targeting efficiency' principle and are often dismissed or actively opposed by its proponents, effectively excluding them from the policy-making conversation where this constraint is dominant.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, advocates_for_universal_basic_income, excluded,
    organized, generational, mobile, global).

% Beyond direct financial losses, these groups bear the social and psychological costs of means-testing and the 'demonstrated need' requirement. The process can be stigmatizing, intrusive, and disempowering, leading to reduced participation even among eligible individuals. Their identity is often fused with their status as 'recipients,' making exit from the system difficult without also exiting their social context.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, marginalized_groups_facing_stigma, payer,
    powerless, biographical, identity_locked, local).

% These academics and think tanks analyze the effectiveness, costs, and social impacts of different income support models. They provide evidence that can either support or challenge the 'targeting efficiency' principle, but their influence is mediated by political will and public discourse. They are not directly affected by the constraint's operation but provide critical analysis.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, social_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, welfare_state_administrators).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinates the allocation of public funds for income support by establishing criteria for eligibility, aiming to direct resources to those deemed 'most in need' and avoid perceived waste.
% TRANSFER_FUNCTION: It transfers public funds from the general tax base to specific low-income households, but also transfers administrative power and institutional legitimacy to the welfare state apparatus, while potentially extracting net resources from the poorest when universal alternatives are foreclosed.
% ABSENT_VOICES: Advocates for universal basic income and other universal programs are largely absent from the core policy-making discussions where this principle is dominant. They would argue that the administrative costs and social stigma of targeting outweigh any efficiency gains, and that universal provision offers greater dignity and economic security.
% DISAPPEARANCE_RATIONALE: If the 'targeting efficiency' principle vanished overnight, the entire structure of means-tested welfare programs would collapse. Funds would either be distributed universally (if an alternative principle took hold) or cease to be distributed at all, leading to a complete reorganization of social safety nets and public finance.
% FOUNDING_PROBLEM: The founding problem was the perceived inefficiency and potential for 'moral hazard' (disincentives to work) associated with untargeted or overly generous welfare provisions, particularly in the context of limited public resources.
% FOUNDING_PROBLEM_CORROBORATION: Welfare state administrators and some taxpayers attest that the problem of efficient resource allocation remains live, citing ongoing fiscal pressures and the need to prevent dependency. However, social policy researchers and advocates for universal programs argue that the original problem has been reframed to justify an extractive system, and that the 'efficiency' claims are often not borne out by evidence of administrative costs or poverty reduction, with corroboration from independent economic analyses and comparative studies of welfare systems.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness (0.75) is high because, while ostensibly 'efficient,' it often results in net losses for the poorest when universal programs are funded by cannibalizing existing targeted benefits. For example, a Queens parent receiving $31,100 in targeted benefits might lose $19,100 under a UBI replacement, making them a victim of the 'efficiency' argument. Suppression (0.65) arises from the administrative burden, means-testing, and social stigma associated with demonstrating need, which can deter eligible individuals. The theater ratio (0.20) is low, as the administrative machinery for targeting is genuinely active, though its 'efficiency' claims are often performative cover for distributional choices. The increasing extractiveness and suppression over time reflect a hardening of the targeted approach and a growing resistance to universal alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'taxpayers_averse_to_universal_programs,' this constraint is a Rope, ensuring fiscal responsibility and efficient use of public funds. From the perspective of 'low_income_households_losing_targeted_benefits,' it operates as a Snare, trapping them in a system that extracts from them under the guise of efficiency, especially when universal alternatives are foreclosed. Welfare state administrators experience it as a Tangled Rope, balancing the coordination of aid with the administrative burden and political defense of targeting.
 *
 * DIRECTIONALITY LOGIC:
 *   Taxpayers averse to universal programs are beneficiaries (d=0.1) as they perceive reduced tax burdens or more 'responsible' spending. Welfare state administrators are also beneficiaries (d=0.2) as their institutional mandate and budgets are tied to administering complex targeted programs. Low-income households losing targeted benefits are clear victims (d=0.9) as they face a net reduction in support. Marginalized groups facing stigma are also victims (d=0.8) due to the social costs and barriers to access. Advocates for universal basic income are excluded (d=0.95) as their proposals are actively suppressed by the 'targeting efficiency' argument.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy in the traditional sense, as the 'problem' of inefficient resource allocation is perpetually 'live' for its beneficiaries. However, the analysis reveals that the 'efficiency' mandate often serves as a cover for a transfer of resources away from the poorest, preventing the mislabeling of this as a benign coordination mechanism. The persistence of the constraint is driven by the concentrated benefits to certain taxpayer groups and the institutional inertia of the administrative apparatus, rather than a genuine, universally beneficial coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_policy_choice,
    'Is the principle of targeting income support to demonstrated need a natural law of efficient resource allocation, or a policy choice with distributional consequences?',
    'Comparative analysis of welfare states with universal vs. targeted programs on metrics of poverty reduction, administrative cost, and social cohesion.',
    'If a natural law, the constraint is a Mountain; if a policy choice, its extractive and suppressive properties are subject to political contestation and re-design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_or_policy_choice, conceptual, 'Ambiguity between economic efficiency principle and political choice.').

omega_variable(
    targeting_efficiency_vs_universal_provision,
    'This constraint is the ''targeting_efficiency_reading'' of the ''income_support_commitment'' kernel. How would the classification change under the ''freedom_floor_reading'' or ''dependency_trap_reading''?',
    'Analyze the structural properties (beneficiaries, victims, extractiveness) of the sibling readings as separate constraints.',
    'The ''freedom_floor_reading'' would likely compute as a Rope (coordination for dignity/autonomy), while the ''dependency_trap_reading'' would likely compute as a Snare (extraction via work disincentives). This reading computes as a Snare due to the extraction from the poor to fund universal distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeting_efficiency_vs_universal_provision, conceptual, 'Impact of alternative readings on constraint classification.').

omega_variable(
    stigma_as_suppression_mechanism,
    'To what extent does the ''demonstrated need'' requirement create social stigma that acts as an internalized suppression mechanism, discouraging eligible individuals from seeking support?',
    'Sociological studies on welfare program participation rates, self-reported stigma, and mental health outcomes among recipients of targeted vs. universal programs.',
    'If stigma is a significant internalized suppression mechanism, the effective suppression for marginalized groups is higher than the structural measure suggests, amplifying their effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_as_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism from ''demonstrated need'' requirements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__targeting_efficiency_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__targeting_efficiency_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_commitment' kernel, focusing on targeting efficiency. It is structurally distinct from the 'freedom_floor_reading' and 'dependency_trap_reading' of the same kernel, which emphasize different normative goals and produce different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
