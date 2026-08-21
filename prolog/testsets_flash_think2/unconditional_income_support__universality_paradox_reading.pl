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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support: Universality Paradox Reading
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes unconditional income support as a politically
 *   ambiguous 'Trojan horse'. Its cross-ideological appeal masks incompatible
 *   implementation paths that, through mechanisms like 'taxing-back',
 *   converge on similar fiscal outcomes. This reading focuses on the
 *   political dynamics of this ambiguity, where the rhetoric of universality
 *   serves to build coalitions while obscuring the actual distributional and
 *   ideological compromises. The constraint is claimed as a Tangled Rope
 *   because it genuinely coordinates political support but does so through
 *   asymmetric extraction of ideological clarity and potential harm to
 *   targeted program recipients.
 *
 * KEY AGENTS:
 *   - Political entrepreneurs: Primary agenda-setters and beneficiaries (powerful/mobile) — exploit ambiguity for coalition building.
 *   - Policy designers: Agenda-setters and beneficiaries (institutional/constrained) — craft mechanisms to maintain ambiguity.
 *   - Ideological clarity advocates: Payers (moderate/constrained) — bear the cost of obscured policy goals.
 *   - Targeted program recipients: Payers (powerless/trapped) — vulnerable to cuts or dilution of benefits.
 *   - Fiscal conservatives: Beneficiaries (organized/mobile) — support for fiscal convergence.
 *   - Social justice advocates: Beneficiaries (organized/constrained) — support for universal rhetoric.
 *   - Analytical observers: Observers (analytical/analytical) — highlight the paradox.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.4).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.65).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support: Universality Paradox Reading").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '313310ca-e917-4461-ada0-8239c6fd8f29').
narrative_ontology:cs_kernel_codification('313310ca-e917-4461-ada0-8239c6fd8f29', formalized).
narrative_ontology:cs_authority_grounding('313310ca-e917-4461-ada0-8239c6fd8f29', extraction).
narrative_ontology:cs_interpretation_layer_present('313310ca-e917-4461-ada0-8239c6fd8f29').
narrative_ontology:cs_reading_relation('313310ca-e917-4461-ada0-8239c6fd8f29', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('313310ca-e917-4461-ada0-8239c6fd8f29', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('313310ca-e917-4461-ada0-8239c6fd8f29', foundational, universality_as_political_tool).
narrative_ontology:cs_axiom_status(universality_as_political_tool, holdable).
narrative_ontology:cs_axiom_grounding('313310ca-e917-4461-ada0-8239c6fd8f29', universality_as_political_tool, conventional).
narrative_ontology:cs_axiom('313310ca-e917-4461-ada0-8239c6fd8f29', foundational, fiscal_convergence_via_taxback).
narrative_ontology:cs_axiom_status(fiscal_convergence_via_taxback, holdable).
narrative_ontology:cs_axiom_grounding('313310ca-e917-4461-ada0-8239c6fd8f29', fiscal_convergence_via_taxback, empirically_contingent).
narrative_ontology:cs_reference_frame('313310ca-e917-4461-ada0-8239c6fd8f29', cross_ideological_consensus).
narrative_ontology:cs_drift_state('313310ca-e917-4461-ada0-8239c6fd8f29', contemporary_policy_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('313310ca-e917-4461-ada0-8239c6fd8f29', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, fiscal_conservatives).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, social_justice_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploit the cross-ideological appeal of unconditional income support to build broad political coalitions, leveraging its ambiguity to secure diverse support without fully committing to a single ideological vision. They benefit from the political capital gained.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary).

% Develop and implement the 'taxing-back' mechanisms that allow for rhetorical flexibility (universal payout) while achieving specific fiscal outcomes. They benefit from the ability to craft politically viable, if ambiguous, policy.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary).

% Bear the cost of the policy's inherent ambiguity, finding it difficult to coherently evaluate or critique due to its shifting justifications and outcomes. They are forced to engage with a policy whose true intent is obscured.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity_advocates, payer,
    moderate, generational, constrained, national).

% Are vulnerable to the universality paradox, as the push for universal programs can be used to justify cuts or reductions in existing targeted welfare programs that previously provided more substantial support to specific needy populations. Their benefits may be diluted or reduced.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, local).

% Support unconditional income support due to its potential for fiscal convergence with existing welfare spending via taxing-back mechanisms, seeing it as a way to streamline or reduce overall welfare costs, despite its universal rhetoric.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_conservatives, beneficiary,
    organized, biographical, mobile, national).

% Support unconditional income support for its rhetorical commitment to universality and its potential to reduce poverty and enhance autonomy, often overlooking or downplaying the fiscal convergence and potential impact on targeted programs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, social_justice_advocates, beneficiary,
    organized, generational, constrained, national).

% Study the policy's implementation, fiscal outcomes, and political dynamics, often highlighting the paradox between its universal rhetoric and its actual distributional effects. They provide independent analysis but have no direct power over the constraint.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unite disparate political factions (e.g., fiscal conservatives and social justice advocates) under a common policy label, allowing for coalition building and the passage of significant welfare reform by leveraging the ambiguity of 'universality'.
% TRANSFER_FUNCTION: Rhetorically promises universal income distribution to all citizens, but fiscally, through 'taxing-back' mechanisms, it effectively transfers resources in a way that may converge with existing targeted welfare spending, potentially shifting resources from specific needy populations to a broader, less impactful distribution.
% ABSENT_VOICES: Those who demand clear, non-ambiguous policy goals and transparent fiscal accounting for welfare programs, as well as advocates for strictly targeted, high-impact programs that serve specific vulnerable populations. Their calls for clarity are obscured by the cross-ideological appeal.
% DISAPPEARANCE_RATIONALE: If the political ambiguity and the policy structure of unconditional income support vanished overnight, the political coalitions built on this ambiguity would dissolve. The debate over welfare policy would be forced into clearer, potentially more confrontational, ideological lines, and the welfare state would reorganize around more explicit policy goals.
% FOUNDING_PROBLEM: The challenge of building broad political consensus for significant welfare reform across ideological divides, and the need to design such programs with perceived fiscal sustainability.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, public policy analysts, and non-partisan fiscal bodies corroborate the ongoing challenge of achieving broad consensus for welfare reform and the necessity of fiscal mechanisms like taxing-back to manage costs. Legislative hearing testimony and academic research from outside the directly benefiting political parties support this assessment.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness (0.4) is set to reflect the political and ideological costs of ambiguity, rather than a high fiscal extraction, consistent with the prompt's 'low epsilon' for fiscal outcomes. Suppression (0.65) is high because the constraint actively obscures clear debate and coherent alternatives, requiring continuous political effort to maintain the ambiguity. The theater ratio (0.5) reflects the significant gap between the universal rhetoric and the actual, often less universal, fiscal and distributional outcomes. Accessibility collapse (0.5) indicates that clear alternatives (e.g., purely targeted programs or truly universal, non-taxed-back schemes) are partly obscured or politically difficult to pursue. Resistance (0.55) comes from those who demand ideological clarity or defend existing targeted programs.
 *
 * PERSPECTIVAL GAP:
 *   Political entrepreneurs and policy designers experience this constraint as a successful coordination mechanism, enabling broad political support for welfare reform. In contrast, ideological clarity advocates and targeted program recipients experience it as an extractive mechanism that obscures policy intent and potentially harms vulnerable groups. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a Rope-like function and victims experiencing a Snare-like extraction of clarity and resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are beneficiaries, as they gain political capital and flexibility from the ambiguity. Fiscal conservatives and social justice advocates are also beneficiaries, as the policy allows them to claim ideological victories (fiscal responsibility or universal provision, respectively). Ideological clarity advocates are targets, as the constraint extracts their ability to engage in coherent policy debate. Targeted program recipients are also targets, as the policy's universality can be used to justify cuts to their specific benefits. The active enforcement of the 'taxing-back' mechanisms and the political narrative maintains this asymmetric structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to provide unconditional income support and build political consensus. However, the 'Trojan horse' aspect suggests a form of mandatrophy where the stated function (universal benefit) is partially replaced by a latent function (political coalition building and fiscal convergence via hidden mechanisms). The ambiguity prevents a clear resolution of its true purpose, allowing it to persist by satisfying disparate, often contradictory, political agendas. The high theater ratio and suppression reflect this performative maintenance of ambiguity over clear functional outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_resolution_path,
    'Will the political ambiguity inherent in unconditional income support eventually resolve towards one dominant ideological interpretation, or will it persist as a permanent feature of the policy?',
    'Longitudinal analysis of policy discourse, legislative amendments, and public perception over several decades. If one ideological framing consistently gains dominance, the ambiguity is resolving.',
    'If the ambiguity resolves, the constraint''s classification would likely shift towards a clearer Rope (if the coordination function becomes transparent and beneficial) or Snare (if the extractive aspects become undeniable). If it persists, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_resolution_path, empirical, 'Whether the political ambiguity of UBI is a transient or permanent feature.').

omega_variable(
    fiscal_outcome_divergence,
    'To what extent do the actual fiscal and distributional outcomes of ''taxing-back'' universal income support truly converge with or diverge from those of targeted programs, beyond rhetorical claims?',
    'Independent, long-term econometric studies comparing the net fiscal impact and distributional effects of implemented universal schemes (with taxing-back) against counterfactual targeted programs.',
    'If significant divergence is found, the ''low epsilon'' claim (fiscal) would be challenged, potentially increasing the measured extractiveness and reinforcing a Snare-like classification. If strong convergence is confirmed, the political extraction of clarity remains the primary concern.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_outcome_divergence, empirical, 'Empirical verification of fiscal convergence claims for UBI.').

omega_variable(
    impact_on_targeted_programs,
    'Does the political momentum for universal income support, driven by its cross-ideological appeal, lead to a net reduction or weakening of existing targeted welfare programs?',
    'Comparative policy analysis across jurisdictions implementing UBI, tracking changes in funding, scope, and eligibility for targeted welfare programs before and after UBI implementation.',
    'If a clear pattern of reduction or weakening is observed, the ''victims'' group (targeted_program_recipients) would experience higher effective extraction, pushing the constraint closer to a Snare. If targeted programs remain robust, the extraction is primarily ideological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_targeted_programs, empirical, 'Whether UBI implementation negatively impacts existing targeted welfare programs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__universality_paradox_reading, theater_ratio, 4, 0.47).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__universality_paradox_reading, theater_ratio, 8, 0.49).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__universality_paradox_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__universality_paradox_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__universality_paradox_reading, base_extractiveness, 4, 0.37).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__universality_paradox_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__universality_paradox_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__universality_paradox_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__universality_paradox_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__universality_paradox_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__universality_paradox_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__universality_paradox_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unconditional_income_support' kernel, focusing on the political ambiguity and paradoxical universality. It is linked to the 'freedom_floor_reading' and 'dependency_trap_reading' as part of a constraint family where different ideological framings instantiate distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
