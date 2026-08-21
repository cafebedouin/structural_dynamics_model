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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeted Income Support as Efficiency Constraint
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'targeting efficiency' reading of the
 *   broader commitment to income support, asserting that aid should be
 *   concentrated on demonstrated need rather than universally distributed.
 *   While proponents frame this as fiscally responsible and effective, the
 *   structural analysis reveals it operates as a Snare, extracting from
 *   low-income individuals through administrative burden, stigma, and
 *   conditionalities, while suppressing alternative, less conditional forms
 *   of support. The high extractiveness and Snare classification reflect a
 *   critical perspective on the *structural effects* of this commitment,
 *   rather than its stated intent.
 *
 * KEY AGENTS:
 *   - welfare_administrators: Agenda setter / Beneficiary (institutional/constrained)
 *   - fiscal_conservatives: Beneficiary (powerful/mobile)
 *   - low_income_individuals: Payer / Victim (powerless/trapped)
 *   - targeted_program_recipients: Beneficiary / Payer (powerless/identity_locked)
 *   - universal_basic_income_advocates: Excluded (organized/constrained)
 *   - social_policy_researchers: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.85).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.75).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeted Income Support as Efficiency Constraint").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660').
narrative_ontology:cs_kernel_codification('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', formalized).
narrative_ontology:cs_authority_grounding('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', extraction).
narrative_ontology:cs_interpretation_layer_present('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660').
narrative_ontology:cs_reading_relation('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', income_support_commitment__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', foundational, resource_scarcity_justifies_targeting).
narrative_ontology:cs_axiom_status(resource_scarcity_justifies_targeting, holdable).
narrative_ontology:cs_axiom_grounding('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', resource_scarcity_justifies_targeting, empirically_contingent).
narrative_ontology:cs_axiom('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', secondary, universal_distribution_is_wasteful).
narrative_ontology:cs_axiom_status(universal_distribution_is_wasteful, holdable).
narrative_ontology:cs_axiom_grounding('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', universal_distribution_is_wasteful, empirically_contingent).
narrative_ontology:cs_reference_frame('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', efficient_resource_allocation_framework).
narrative_ontology:cs_drift_state('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', contemporary_welfare_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb5c5bbe-bfdd-41f4-b6a0-bc973fd77660', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, welfare_administrators).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, fiscal_conservatives).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, low_income_individuals).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, marginalized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the complex means-testing and eligibility verification processes for targeted income support programs. They benefit from the institutional stability and employment generated by the system's complexity, and wield significant power over recipients' access to aid.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, welfare_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Advocate for concentrating income support on demonstrated need, framing it as fiscally responsible and efficient. They gain political capital and legitimacy from maintaining a system that limits overall welfare spending and reinforces conditional aid.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, fiscal_conservatives, beneficiary,
    powerful, generational, mobile, national).

% Are subjected to the administrative burdens, intrusive means-testing, and social stigma associated with targeted welfare programs. Many are excluded due to complex eligibility criteria or inability to navigate the system, effectively paying with their time, dignity, and lost opportunities.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, low_income_individuals, payer,
    powerless, immediate, trapped, local).

% Receive direct financial benefits from targeted programs (e.g., the Queens parent with $31,100 in benefits). However, they also bear the costs of the system's conditionalities, administrative hurdles, and the constant threat of benefit reduction or loss, making them beneficiaries of the aid but payers of the system's extractive mechanisms.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients, beneficiary,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients, payer).

% Propose alternative models of unconditional, universal income support. Their proposals are actively suppressed by the 'targeting efficiency' commitment, which frames universal distribution as wasteful and inefficient, thereby excluding their policy alternatives from mainstream consideration.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, universal_basic_income_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the effectiveness, costs, and social impacts of both targeted and universal income support programs. They provide evidence that can challenge or support the 'targeting efficiency' commitment, but their findings are often selectively used by political actors.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, social_policy_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To allocate limited public resources to individuals and families deemed most in need, based on specific eligibility criteria and means-testing, thereby ensuring fiscal responsibility and preventing 'undeserved' aid.
% TRANSFER_FUNCTION: Moves public funds from taxpayers to eligible low-income individuals, but also transfers significant administrative burden, social stigma, and conditionalities onto recipients, while consolidating power within welfare bureaucracies and political factions advocating for limited welfare states.
% ABSENT_VOICES: Advocates for universal, unconditional income support are structurally excluded from the policy debate by the framing that prioritizes 'targeting efficiency.' Also, many low-income individuals who are excluded by complex eligibility criteria or who face severe administrative hurdles are effectively silenced.
% DISAPPEARANCE_RATIONALE: If the commitment to 'targeting efficiency' vanished overnight, the existing architecture of means-tested welfare programs would likely collapse or be fundamentally reformed. Resources would either be reallocated to universal programs or left unspent, leading to a significant reorganization of social safety nets and public finance.
% FOUNDING_PROBLEM: The perceived problem of inefficient allocation of public funds, where universal programs might provide aid to those not in 'demonstrated need,' leading to fiscal waste and potential disincentives to work.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., fiscal conservatives, some welfare administrators) attest that the problem of resource scarcity and potential waste is still live. Critics (e.g., UBI advocates, some social policy researchers) argue that the 'founding problem' is largely a cover for limiting welfare spending and that the administrative costs and social harms of targeting outweigh any efficiency gains; independent economic analyses often highlight the high administrative overhead of targeted programs.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) stems from the significant non-monetary costs imposed on recipients: the time and effort required for complex applications, the psychological burden of means-testing, the stigma of receiving 'welfare,' and the conditionalities that limit autonomy. Suppression (0.75) is high because the commitment actively delegitimizes and excludes universal alternatives, maintaining the existing targeted system through political and bureaucratic inertia. Theater ratio (0.45) reflects that while some administrative effort genuinely targets aid, a substantial portion is performative, serving to justify the system's existence and its limitations. Accessibility collapse (0.65) is due to the complexity and digital divide, making it hard for many to access aid. Resistance (0.55) is present through individual appeals and advocacy groups, but often fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The prompt's specific examples of 'current targeted-program recipients' as beneficiaries and 'same recipients under UBI replacement' as victims highlight the *proponents'* argument for targeting efficiency: that it protects the truly needy from losing existing benefits under a universal system. However, the Snare classification and high extractiveness for *this constraint* (the commitment to targeting efficiency) reflect a structural analysis of the *system it creates*. From this critical perspective, while recipients may receive monetary benefits, the *system itself* extracts non-monetary costs (stigma, burden, loss of autonomy) from them. The 'victim under UBI' scenario is a counterfactual argument against an alternative, not a direct structural victim of the *current* targeting constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeting_efficiency_vs_administrative_burden,
    'Does the administrative overhead and social cost of means-testing truly result in a more efficient allocation of resources than universal alternatives, or does it primarily serve to limit overall welfare spending?',
    'Comparative empirical studies of administrative costs, take-up rates, and poverty reduction outcomes across targeted vs. universal programs in different jurisdictions.',
    'If administrative burden outweighs efficiency gains, the extractiveness of the ''targeting efficiency'' commitment is confirmed as high; if genuine efficiency is demonstrated, the extractiveness might be lower, reclassifying towards a Tangled Rope or even Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_efficiency_vs_administrative_burden, empirical, 'Whether targeting genuinely improves efficiency or is a cover for austerity.').

omega_variable(
    stigma_as_extraction_mechanism,
    'To what extent does the social stigma associated with means-tested welfare programs function as an intentional or unintentional mechanism of extraction, deterring eligible individuals from accessing aid?',
    'Sociological studies on welfare stigma, recipient interviews, and analysis of program take-up rates compared to eligibility pools.',
    'If stigma is a significant deterrent, it confirms high suppression and extractiveness, reinforcing the Snare classification. If stigma is negligible, the constraint''s extractiveness might be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_as_extraction_mechanism, empirical, 'Role of stigma in welfare system extraction.').

omega_variable(
    prompt_victim_contradiction_resolution,
    'How should the prompt''s victim example (''same recipients under UBI replacement lose $19,100'') be reconciled with the Snare classification of the ''targeting efficiency'' constraint itself?',
    'Clarification from the prompt author on whether the victim declaration refers to the structural effects of *this constraint* or the *consequences of its alternative* from this reading''s perspective.',
    'If the victim example is a structural declaration for *this constraint*, it would contradict the Snare classification and high extractiveness, forcing a re-evaluation of the constraint''s type and metrics. If it''s a rhetorical point about an alternative, the current Snare classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prompt_victim_contradiction_resolution, conceptual, 'Reconciling prompt''s victim example with Snare classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t1970, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(inco_tr_t1980, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(inco_tr_t1990, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(inco_tr_t2000, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(inco_tr_t2010, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(inco_tr_t2025, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(inco_be_t1970, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(inco_be_t1980, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(inco_be_t1990, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(inco_be_t2000, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(inco_be_t2010, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(inco_be_t2025, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t1970, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(inco_su_t1980, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(inco_su_t1990, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(inco_su_t2000, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement(inco_su_t2010, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(inco_su_t2025, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
