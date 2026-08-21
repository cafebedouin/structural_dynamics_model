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
 *   This constraint describes the political ambiguity inherent in
 *   'unconditional income support' (UIS) when viewed as a 'Trojan horse.' It
 *   highlights how the policy's cross-ideological appeal masks fundamentally
 *   incompatible implementation paths, which often converge on similar fiscal
 *   outcomes due to 'taxing-back' mechanisms. This reading focuses on the
 *   political dynamics and the 'paradox of universality' where universal
 *   rhetoric can undermine targeted support. This is one reading of the
 *   'unconditional_income_support' kernel, distinct from
 *   'freedom_floor_reading' and 'dependency_trap_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.35).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.45).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.45).
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
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '7c81c0e6-9547-48dd-8b9a-99a4eb007f6b').
narrative_ontology:cs_kernel_codification('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', formalized).
narrative_ontology:cs_authority_grounding('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', distributed).
narrative_ontology:cs_reading_relation('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', foundational, policy_ambiguity_is_a_political_tool).
narrative_ontology:cs_axiom_status(policy_ambiguity_is_a_political_tool, holdable).
narrative_ontology:cs_axiom_grounding('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', policy_ambiguity_is_a_political_tool, conventional).
narrative_ontology:cs_axiom('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', foundational, universal_rhetoric_masks_targeted_outcomes).
narrative_ontology:cs_axiom_status(universal_rhetoric_masks_targeted_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', universal_rhetoric_masks_targeted_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', ideological_polarization_as_policy_barrier).
narrative_ontology:cs_drift_state('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', contemporary_policy_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7c81c0e6-9547-48dd-8b9a-99a4eb007f6b', '2024-07-30T12:00:00Z').
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

% Exploit the cross-ideological ambiguity of unconditional income support to build broad, temporary coalitions for policy adoption. They benefit from the rhetorical flexibility that allows different factions to project their own goals onto the policy.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary,
    powerful, biographical, mobile, national).

% Utilize 'taxing-back' mechanisms to reconcile universal provision with fiscal realities, allowing the policy to be presented as both universal (appealing to the left) and fiscally responsible (appealing to the right). They benefit from the ability to design policies that bridge ideological divides, even if the practical outcomes are similar to targeted programs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    institutional, generational, constrained, national).

% Suffers from the policy's inherent ambiguity. The lack of a clear, consistent ideological grounding prevents coherent evaluation and debate, leading to policy drift and a 'mushy middle' where fundamental disagreements are obscured rather than resolved.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Are often the unacknowledged victims. The push for universality, framed as reducing stigma, can be used to justify dismantling existing targeted programs that provided higher net benefits to the most vulnerable, leading to a net loss for these groups once 'taxing-back' mechanisms are applied.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, local).

% Support unconditional income support if it can replace existing, more complex welfare programs and be fiscally neutral or reduce overall spending through 'taxing-back' mechanisms. They observe the policy's implementation for evidence of fiscal discipline.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_conservatives, observer,
    organized, biographical, mobile, national).

% Support unconditional income support as a means to reduce poverty and enhance individual autonomy, often overlooking or downplaying the 'taxing-back' mechanisms that can dilute its redistributive impact. They observe for evidence of genuine poverty reduction and equity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, social_justice_advocates, observer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse political factions around a single policy vehicle by allowing each to interpret 'unconditional income support' through their own ideological lens, thus enabling legislative action that would otherwise be impossible due to fundamental disagreements.
% TRANSFER_FUNCTION: Transfers political capital and rhetorical flexibility to political entrepreneurs and policy designers, by obscuring the actual fiscal and distributional outcomes of universal programs that are effectively means-tested through the tax system. It also transfers resources to all citizens, but then claws back from higher earners, making the net transfer less universal than the rhetoric suggests.
% ABSENT_VOICES: Advocates for clear, ideologically consistent policy frameworks are often marginalized, as their insistence on definitional precision threatens the cross-ideological appeal. Similarly, those who would highlight the specific losses to targeted program recipients are often drowned out by the universalist rhetoric.
% DISAPPEARANCE_RATIONALE: If the political ambiguity of unconditional income support vanished, the fragile cross-ideological coalitions supporting it would collapse. The policy would likely fail to pass, and the political landscape would revert to more traditional, ideologically polarized debates over welfare reform, forcing political entrepreneurs to find new, less ambiguous policy vehicles.
% FOUNDING_PROBLEM: The problem of political gridlock and ideological polarization preventing comprehensive welfare reform, alongside the desire to find a policy that could appeal to both left-leaning desires for social safety nets and right-leaning desires for administrative simplicity.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and policy analysts from across the spectrum corroborate that political gridlock over welfare reform remains a live problem. Think tanks and academic studies, independent of the benefiting political entrepreneurs, document the persistent challenge of building consensus on social policy.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.35) is moderate because while the policy itself might not be highly extractive in fiscal terms (due to taxing-back), it extracts ideological clarity and can lead to a net loss for vulnerable groups. Suppression (0.45) reflects the difficulty of challenging the universalist rhetoric, which suppresses critical analysis of its actual distributional effects. The high theater ratio (0.60) indicates that the policy's rhetorical performance (universal, stigma-free) often outweighs its actual functional impact, especially when compared to its fiscal outcomes. The metrics reflect the political and ideological costs of this ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of political entrepreneurs, the ambiguity is a feature, enabling policy adoption. From the perspective of ideological clarity, it's a bug, preventing meaningful debate. The engine's classification will highlight how the same policy structure is experienced as a coordination mechanism by some and an extractive force by others, due to the inherent paradox of its universality.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are beneficiaries, as the ambiguity provides them with flexibility and coalition-building opportunities. Ideological clarity and targeted program recipients are victims, as the ambiguity obscures real policy impacts and can lead to the erosion of specific support. Fiscal conservatives and social justice advocates are observers, each looking for evidence that aligns with their pre-existing ideological commitments, often contributing to the ambiguity rather than resolving it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_outcome_convergence,
    'To what extent do different implementation paths of unconditional income support (e.g., flat grant vs. negative income tax) truly converge on similar fiscal and distributional outcomes after ''taxing-back'' mechanisms are applied?',
    'Comparative empirical studies across jurisdictions with different UIS designs, analyzing net transfers and poverty reduction impacts after all tax and benefit interactions.',
    'If convergence is strong, it reinforces the ''universality paradox'' reading, highlighting the theatricality of ideological debates over design. If divergence is significant, it weakens this reading, suggesting that design choices have more substantive impact than currently assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_outcome_convergence, empirical, 'Empirical verification of fiscal outcome convergence across different UIS designs.').

omega_variable(
    ideological_ambiguity_sustainability,
    'Can the political ambiguity of unconditional income support be sustained long-term, or will one dominant ideological interpretation eventually prevail, leading to a clearer (and potentially more contested) policy framework?',
    'Longitudinal analysis of political discourse, legislative debates, and public opinion trends in countries implementing UIS, tracking the evolution of its dominant framing.',
    'If ambiguity persists, it strengthens the ''tangled rope'' classification, as the constraint''s function relies on maintaining this entanglement. If a dominant interpretation emerges, the constraint might reclassify towards a ''snare'' (if extractive) or ''rope'' (if genuinely coordinative) as the underlying ideological conflict is resolved or suppressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_ambiguity_sustainability, conceptual, 'The long-term sustainability of UIS''s cross-ideological ambiguity.').

omega_variable(
    impact_on_targeted_programs,
    'Does the implementation of universal unconditional income support consistently lead to the dismantling or reduction of existing targeted welfare programs, and what is the net effect on the most vulnerable populations?',
    'Case studies and quantitative analysis of welfare state reforms in countries adopting UIS, specifically tracking changes in targeted program budgets and outcomes for low-income groups.',
    'If targeted programs are consistently eroded with negative impacts on the vulnerable, it strengthens the ''victim'' status of targeted program recipients and increases the perceived extractiveness of the ''universality paradox'' reading. If targeted programs remain robust or are enhanced, it weakens this aspect of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_targeted_programs, empirical, 'The actual impact of UIS on existing targeted welfare programs and vulnerable populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__universality_paradox_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__universality_paradox_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__universality_paradox_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__universality_paradox_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__universality_paradox_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__universality_paradox_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__universality_paradox_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unconditional_income_support' kernel. This 'universality_paradox_reading' focuses on the political ambiguity and its consequences, distinct from the 'freedom_floor_reading' (positive framing) and 'dependency_trap_reading' (negative framing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
