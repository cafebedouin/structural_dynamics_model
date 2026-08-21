% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support: Universality Paradox Reading
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the political ambiguity surrounding
 *   unconditional income support, where its cross-ideological appeal masks
 *   incompatible implementation paths that often converge on similar fiscal
 *   outcomes. This reading highlights how the policy functions as a 'Trojan
 *   horse,' allowing political entrepreneurs to build coalitions and policy
 *   designers to achieve reforms, while obscuring ideological clarity and
 *   potentially harming targeted program recipients. The constraint is
 *   claimed as a Tangled Rope because it genuinely coordinates diverse
 *   political interests but does so through a mechanism that extracts
 *   ideological coherence and potentially shifts burdens onto vulnerable
 *   groups.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: Primary beneficiary (powerful/mobile) — exploit ambiguity for coalition building.
 *   - policy_designers: Secondary beneficiary (organized/constrained) — use taxing-back mechanisms for rhetorical flexibility.
 *   - ideological_clarity: Primary victim (analytical/identity_locked) — suffers from policy ambiguity.
 *   - targeted_program_recipients: Secondary victim (powerless/trapped) — potentially harmed by cuts to existing programs.
 *   - academic_researchers: Analytical observer (analytical/analytical) — studies the policy's effects and political dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.25).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.4).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support: Universality Paradox Reading").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '652d9bdf-43ed-497c-9c1b-5279738f1823').
narrative_ontology:cs_kernel_codification('652d9bdf-43ed-497c-9c1b-5279738f1823', distributed).
narrative_ontology:cs_authority_grounding('652d9bdf-43ed-497c-9c1b-5279738f1823', extraction).
narrative_ontology:cs_interpretation_layer_present('652d9bdf-43ed-497c-9c1b-5279738f1823').
narrative_ontology:cs_reading_relation('652d9bdf-43ed-497c-9c1b-5279738f1823', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('652d9bdf-43ed-497c-9c1b-5279738f1823', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('652d9bdf-43ed-497c-9c1b-5279738f1823', foundational, political_ambiguity_as_policy_tool).
narrative_ontology:cs_axiom_status(political_ambiguity_as_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('652d9bdf-43ed-497c-9c1b-5279738f1823', political_ambiguity_as_policy_tool, instrumental).
narrative_ontology:cs_axiom('652d9bdf-43ed-497c-9c1b-5279738f1823', secondary, fiscal_convergence_across_designs).
narrative_ontology:cs_axiom_status(fiscal_convergence_across_designs, holdable).
narrative_ontology:cs_axiom_grounding('652d9bdf-43ed-497c-9c1b-5279738f1823', fiscal_convergence_across_designs, empirically_contingent).
narrative_ontology:cs_reference_frame('652d9bdf-43ed-497c-9c1b-5279738f1823', pragmatic_policy_making_framework).
narrative_ontology:cs_drift_state('652d9bdf-43ed-497c-9c1b-5279738f1823', contemporary_political_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('652d9bdf-43ed-497c-9c1b-5279738f1823', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploit the cross-ideological ambiguity of unconditional income support to build broad political coalitions, advancing their careers and policy agendas by appealing to diverse groups with different interpretations of the policy's intent and outcome.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary,
    powerful, biographical, mobile, national).

% Utilize 'taxing-back' mechanisms to achieve similar fiscal outcomes across different UBI designs, allowing for rhetorical flexibility and political maneuvering. They benefit from the policy's ambiguity, which enables them to navigate political opposition and implement reforms.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    organized, generational, constrained, national).

% Suffers from the policy's inherent ambiguity, as incompatible normative commitments are entangled within the same policy vehicle. This prevents coherent evaluation and debate, leading to a lack of clear understanding of the policy's true goals and effects.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    analytical, generational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Are victims when the universality of unconditional income support is used to justify cuts to existing targeted welfare programs. They may experience a net loss of support or a reduction in the quality of services, despite the promise of a universal safety net.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, local).

% Analyze the fiscal and social outcomes of various unconditional income support designs, often finding that different implementation paths converge on similar distributional effects due to taxing-back mechanisms. They observe the political ambiguity and its consequences for policy coherence.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, academic_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate diverse political factions and policy goals under a single, rhetorically flexible policy umbrella, allowing for incremental reform in social welfare systems.
% TRANSFER_FUNCTION: Transfers political capital and rhetorical advantage to political entrepreneurs and policy designers by obscuring the true fiscal and distributional impacts of unconditional income support, while transferring the cost of ideological incoherence and potential program cuts to targeted recipients and the clarity of public debate.
% ABSENT_VOICES: Advocates for clear, ideologically consistent policy frameworks are often marginalized, as the political utility of ambiguity outweighs the demand for coherence. They would argue for transparent policy design that explicitly states its normative goals and avoids hidden trade-offs.
% DISAPPEARANCE_RATIONALE: If the political ambiguity surrounding unconditional income support vanished, the cross-ideological coalitions supporting it would likely fragment. Policy debates would become more polarized, and the current incremental reform path would be replaced by more direct, ideologically driven conflicts over welfare state design.
% FOUNDING_PROBLEM: The political challenge of building consensus for significant welfare state reform, given entrenched ideological divisions and the difficulty of enacting policies with clear, unambiguous distributional consequences.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and policy analysts, from outside the immediate beneficiaries, corroborate that building consensus for welfare reform remains a live and complex problem, and that ambiguous policy framing is a common strategy to overcome political gridlock.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because, from this reading's perspective, the policy's primary 'extraction' is not fiscal but rather the obfuscation of ideological clarity and the potential for cuts to targeted programs, which are diffuse and often indirect. Suppression (0.4) reflects the active political maneuvering required to maintain the ambiguity and prevent clear ideological challenges. Theater ratio is high (0.6) because a significant portion of the policy's public presentation and debate is performative, designed to appeal to different ideological camps without resolving underlying contradictions. The fiscal outcomes, per taxing-back research, are often similar across different designs, making the 'universality' aspect more of a rhetorical tool than a fundamental shift in distribution.
 *
 * PERSPECTIVAL GAP:
 *   Political entrepreneurs and policy designers experience this as a beneficial coordination mechanism, enabling political progress. In contrast, those seeking ideological clarity or representing targeted program recipients experience it as a form of extraction, where genuine policy debate is suppressed by strategic ambiguity. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are beneficiaries (low d) as they gain political leverage and flexibility. Ideological clarity and targeted program recipients are victims (high d) as they bear the costs of ambiguity and potential program erosion. Academic researchers are observers (d=0.5) as they analyze the system without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling this as a pure Rope (ignoring the extraction of ideological clarity and potential harm to targeted recipients) or a pure Snare (ignoring the genuine coordination function for political actors). It highlights how the policy's mandate to achieve broad political consensus is entangled with its function of obscuring trade-offs, leading to a persistent, actively enforced ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_outcome_convergence_robustness,
    'How robust is the empirical finding that different UBI implementation paths converge on similar fiscal outcomes, especially under varying economic conditions or political priorities?',
    'Longitudinal studies of UBI pilots in diverse economic contexts, and comparative analysis of fiscal models under different policy assumptions.',
    'If convergence is less robust, the ''universality paradox'' weakens, and the policy''s true distributional effects become more salient, potentially altering its political appeal and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_outcome_convergence_robustness, empirical, 'Uncertainty regarding the stability of fiscal outcome convergence across UBI designs.').

omega_variable(
    ideological_ambiguity_sustainability,
    'Can the political ambiguity of unconditional income support be sustained indefinitely, or will increasing scrutiny force a clearer articulation of its underlying normative commitments?',
    'Analysis of public discourse, media framing, and legislative debates over time, particularly in response to policy implementation or challenges.',
    'If ambiguity becomes unsustainable, the constraint''s ''theater ratio'' would likely decrease, and its ''suppression'' of clear ideological debate would weaken, potentially leading to a reclassification towards a more ideologically defined type (Rope or Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_ambiguity_sustainability, conceptual, 'The long-term viability of political ambiguity as a policy-making strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__universality_paradox_reading, theater_ratio, 5, 0.53).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.56).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__universality_paradox_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__universality_paradox_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__universality_paradox_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__universality_paradox_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__universality_paradox_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__universality_paradox_reading, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unconditional_income_support' kernel, focusing on the political ambiguity and convergence of fiscal outcomes. It is linked to sibling readings that emphasize different aspects of the policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
