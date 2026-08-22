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
 *   unconditional income support (UIS) proposals, where cross-ideological
 *   appeal masks incompatible implementation paths that often converge on
 *   similar fiscal outcomes due to 'taxing-back' mechanisms. This reading
 *   highlights how the policy functions as a 'Trojan horse,' allowing
 *   different factions to support it for contradictory reasons, ultimately
 *   benefiting political actors who can leverage this ambiguity. The claimed
 *   type is 'tangled_rope' because it genuinely coordinates diverse political
 *   interests but does so through a mechanism that extracts ideological
 *   clarity and potentially harms targeted beneficiaries.
 *
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
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '04268dc5-62e9-483e-a05a-f63fce561a37').
narrative_ontology:cs_kernel_codification('04268dc5-62e9-483e-a05a-f63fce561a37', distributed).
narrative_ontology:cs_authority_grounding('04268dc5-62e9-483e-a05a-f63fce561a37', distributed).
narrative_ontology:cs_reading_relation('04268dc5-62e9-483e-a05a-f63fce561a37', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('04268dc5-62e9-483e-a05a-f63fce561a37', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('04268dc5-62e9-483e-a05a-f63fce561a37', foundational, policy_ambiguity_as_political_tool).
narrative_ontology:cs_axiom_status(policy_ambiguity_as_political_tool, holdable).
narrative_ontology:cs_axiom_grounding('04268dc5-62e9-483e-a05a-f63fce561a37', policy_ambiguity_as_political_tool, conventional).
narrative_ontology:cs_axiom('04268dc5-62e9-483e-a05a-f63fce561a37', secondary, fiscal_convergence_across_design).
narrative_ontology:cs_axiom_status(fiscal_convergence_across_design, holdable).
narrative_ontology:cs_axiom_grounding('04268dc5-62e9-483e-a05a-f63fce561a37', fiscal_convergence_across_design, empirically_contingent).
narrative_ontology:cs_reference_frame('04268dc5-62e9-483e-a05a-f63fce561a37', political_consensus_through_ambiguity).
narrative_ontology:cs_drift_state('04268dc5-62e9-483e-a05a-f63fce561a37', contemporary_policy_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('04268dc5-62e9-483e-a05a-f63fce561a37', '2024-07-30T12:00:00Z').
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

% Utilize 'taxing-back' mechanisms to achieve similar fiscal and distributional outcomes across different UBI designs, allowing for rhetorical flexibility and masking the underlying ideological compromises. They benefit from the policy's adaptability to various political demands.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    institutional, generational, constrained, national).

% Suffers from the policy's inherent ambiguity. The lack of a clear, consistent ideological grounding prevents coherent public debate and evaluation, making it difficult to assess the policy's true intent or long-term effects. This 'victim' is an abstract concept, not an agent.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Are at risk of losing existing, often more generous, targeted welfare benefits as universality is used to justify cuts or consolidation of social programs. While theoretically receiving a universal income, their net benefit may decrease, and their specific needs may be overlooked by a 'one-size-fits-all' approach.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, local).

% Advocate for UBI as a means to streamline the welfare state, reduce administrative overhead, and potentially replace existing targeted programs, aligning with their goals of fiscal austerity and smaller government. They are key in shaping the 'taxing-back' mechanisms.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_conservatives, agenda_setter,
    organized, biographical, mobile, national).

% Support UBI as a tool for poverty reduction, economic security, and social equality, seeing it as a step towards a more just society. They often overlook the 'Trojan horse' aspect, focusing on the universal provision rather than the potential for cuts to targeted aid.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, social_justice_advocates, agenda_setter,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse political factions and policy goals under a single policy umbrella, allowing for the advancement of a seemingly universal income support system despite underlying ideological disagreements on its purpose and implementation.
% TRANSFER_FUNCTION: Transfers political capital and rhetorical flexibility to political entrepreneurs and policy designers by obscuring the true fiscal and social impacts of unconditional income support, while potentially transferring existing benefits away from targeted recipients.
% ABSENT_VOICES: The voices of those who demand clear, ideologically consistent policy frameworks are often drowned out by the broad, ambiguous appeal of UBI. Also, the specific needs and vulnerabilities of targeted program recipients, who may be harmed by the shift to universality, are frequently marginalized in the broader debate.
% DISAPPEARANCE_RATIONALE: If the political ambiguity surrounding unconditional income support vanished, the cross-ideological coalitions supporting it would likely fragment. Policy debates would become more polarized, and the current momentum for UBI, which relies on this ambiguity, would dissipate, leading to a significant rearrangement of social policy discourse and legislative priorities.
% FOUNDING_PROBLEM: The problem of building broad political consensus for significant social policy reform in ideologically fragmented societies, where direct appeals to specific ideological goals often fail.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and policy analysts, independent of the direct beneficiaries, corroborate that political consensus-building for social policy remains a live and challenging problem, and that ambiguous policy framing is a common strategy to overcome it. Historical legislative analyses also support this reading.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.25) is low because the fiscal outcomes, after 'taxing-back,' are often similar to existing systems, meaning the direct monetary extraction from the overall economy is not dramatically higher. However, it extracts ideological clarity and potentially existing benefits from vulnerable groups. Suppression (0.4) is moderate, as the ambiguity itself suppresses clear opposition by allowing multiple interpretations. The theater ratio (0.6) is high because a significant portion of the political discourse around UIS is performative, focusing on its universal appeal while obscuring the actual distributional impacts and ideological compromises embedded in its design. The metrics show a slight increase in extractiveness and theater over time as the political strategy matures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of political entrepreneurs, the ambiguity is a feature, enabling broad support. From the perspective of ideological clarity, it's a bug, preventing honest assessment. The engine will compute different classifications for these seats based on their declared roles and structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are beneficiaries (low d) as they gain flexibility and coalition-building power. Ideological clarity (an abstract victim) and targeted program recipients are payers (high d) as they bear the costs of ambiguity and potential benefit erosion. Fiscal conservatives and social justice advocates act as agenda-setters, each pushing for UIS from their own perspective, contributing to the ambiguity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_outcome_convergence,
    'To what extent do different unconditional income support designs, after ''taxing-back'' mechanisms, truly converge on similar fiscal and distributional outcomes?',
    'Comprehensive, independent fiscal modeling and empirical analysis of pilot programs across diverse economic contexts, comparing net transfers and administrative costs.',
    'If convergence is strong, it reinforces the ''Trojan horse'' reading, highlighting the performative nature of ideological debates. If divergence is significant, it would weaken this reading, suggesting that implementation paths do have distinct, measurable impacts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_outcome_convergence, empirical, 'Empirical verification of fiscal convergence across UBI designs.').

omega_variable(
    ideological_ambiguity_utility,
    'Is the political utility of ideological ambiguity in social policy a sustainable strategy, or does it eventually lead to policy incoherence and public disillusionment?',
    'Longitudinal studies of public trust in ambiguous social policies, tracking voter engagement and policy stability over multiple electoral cycles.',
    'If ambiguity proves unsustainable, the ''Trojan horse'' mechanism would eventually fail, forcing clearer ideological alignment. If sustainable, it suggests a persistent feature of political systems, reinforcing the constraint''s current operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_ambiguity_utility, conceptual, 'Sustainability of political ambiguity as a policy-making strategy.').

omega_variable(
    targeted_aid_displacement,
    'Does the implementation of universal income support consistently lead to the displacement or reduction of existing targeted welfare programs, and what is the net impact on vulnerable populations?',
    'Comparative policy analysis across jurisdictions implementing UBI, specifically tracking changes in targeted aid budgets and outcomes for previously targeted recipients.',
    'If displacement is widespread and detrimental, it strengthens the ''victim'' status of targeted program recipients and the extractive nature of the ''universality paradox.'' If targeted aid remains robust or is effectively integrated, it would mitigate this aspect of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeted_aid_displacement, empirical, 'Impact of UBI on existing targeted welfare programs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t2010, unconditional_income_support__universality_paradox_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(unco_tr_t2015, unconditional_income_support__universality_paradox_reading, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(unco_tr_t2020, unconditional_income_support__universality_paradox_reading, theater_ratio, 2020, 0.55).
narrative_ontology:measurement(unco_tr_t2025, unconditional_income_support__universality_paradox_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(unco_be_t2010, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(unco_be_t2015, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2015, 0.2).
narrative_ontology:measurement(unco_be_t2020, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2020, 0.23).
narrative_ontology:measurement(unco_be_t2025, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t2010, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(unco_su_t2015, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(unco_su_t2020, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(unco_su_t2025, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unconditional_income_support' kernel, focusing on its political ambiguity. It is linked to sibling readings that emphasize its potential as a 'freedom floor' or a 'dependency trap,' as these different interpretations are all part of the same overarching policy debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
