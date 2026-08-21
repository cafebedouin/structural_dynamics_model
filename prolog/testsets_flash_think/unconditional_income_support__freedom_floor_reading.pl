% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support (Freedom Floor Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story analyzes 'Unconditional Income Support' from the
 *   'freedom_floor_reading' perspective. This reading posits UBI as a
 *   mechanism to enhance individual autonomy by removing the coercive
 *   necessity to accept exploitative labor, eliminating welfare stigma, and
 *   providing a buffer against economic shocks. It is framed as a Pareto
 *   improvement, where the benefits of increased autonomy and social
 *   stability outweigh the costs, with minimal negative impact on labor
 *   supply. The constraint is claimed as a Rope, reflecting its function as a
 *   coordination mechanism that benefits participants by enabling voluntary
 *   action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.25).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support (Freedom Floor Reading)").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '79166ca1-e16f-409f-99a7-b5788e8824bc').
narrative_ontology:cs_kernel_codification('79166ca1-e16f-409f-99a7-b5788e8824bc', formalized).
narrative_ontology:cs_authority_grounding('79166ca1-e16f-409f-99a7-b5788e8824bc', practice).
narrative_ontology:cs_interpretation_layer_present('79166ca1-e16f-409f-99a7-b5788e8824bc').
narrative_ontology:cs_reading_relation('79166ca1-e16f-409f-99a7-b5788e8824bc', unconditional_income_support__dependency_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('79166ca1-e16f-409f-99a7-b5788e8824bc', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('79166ca1-e16f-409f-99a7-b5788e8824bc', foundational, human_autonomy_is_foundational).
narrative_ontology:cs_axiom_status(human_autonomy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('79166ca1-e16f-409f-99a7-b5788e8824bc', human_autonomy_is_foundational, deontological).
narrative_ontology:cs_axiom('79166ca1-e16f-409f-99a7-b5788e8824bc', foundational, economic_security_enables_flourishing).
narrative_ontology:cs_axiom_status(economic_security_enables_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('79166ca1-e16f-409f-99a7-b5788e8824bc', economic_security_enables_flourishing, instrumental).
narrative_ontology:cs_reference_frame('79166ca1-e16f-409f-99a7-b5788e8824bc', human_flourishing_framework).
narrative_ontology:cs_drift_state('79166ca1-e16f-409f-99a7-b5788e8824bc', contemporary_policy_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('79166ca1-e16f-409f-99a7-b5788e8824bc', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, taxpayers_general_public).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpayers_general_public).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, employers_low_wage_sectors).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, human_autonomy_principle).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, dignity_of_labor_redefined).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains a baseline income that removes the immediate coercion to accept exploitative labor conditions, enabling them to seek better work, education, or care for family.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Receives recognition and financial support for essential, often unpaid, care work, reducing financial strain and increasing autonomy in their caregiving roles.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, caregivers, beneficiary,
    powerless, biographical, constrained, national).

% Gains financial stability to pursue creative work without immediate market pressure, fostering cultural production and innovation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists, beneficiary,
    moderate, biographical, constrained, national).

% Acquires financial independence, enabling them to leave abusive relationships or situations where economic dependency is a primary barrier to exit.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims, beneficiary,
    powerless, immediate, trapped, local).

% Actively champions the implementation and defense of unconditional income support, framing it as a fundamental human right and economic necessity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, advocacy_groups, agenda_setter,
    organized, biographical, mobile, national).

% Provides theoretical and empirical support for UBI, analyzing its economic impacts, social benefits, and feasibility, often engaging in policy debates.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, economists_proponents, observer,
    analytical, biographical, analytical, global).

% Contributes to the funding of UBI through taxes, but also benefits from a more stable, less coercive society with reduced social costs associated with poverty and precarity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers_general_public, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, taxpayers_general_public, beneficiary).

% Faces increased labor costs and reduced bargaining power as workers gain the ability to refuse exploitative wages and conditions, potentially leading to shifts in business models.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, employers_low_wage_sectors, payer,
    powerful, biographical, mobile, national).

% Their role shifts from means-testing and conditional aid to universal distribution and potentially complementary social services, requiring adaptation of existing bureaucratic structures.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, traditional_welfare_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a baseline of economic security and dignity for all citizens, enabling voluntary participation in the labor market and other social activities, and reducing the administrative overhead and stigma of means-tested welfare.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to all citizens, regardless of employment status or income, creating a universal income floor.
% ABSENT_VOICES: Those who benefit from the current coercive labor market dynamics (e.g., employers relying on cheap, desperate labor) would object to the shift in power dynamics, but their voices are often framed as economic necessity rather than a preference for coercion.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, many would revert to precarious employment, face increased welfare stigma, and be more vulnerable to economic shocks, leading to a less autonomous and more coercive labor market. The social fabric would experience significant stress.
% FOUNDING_PROBLEM: The existing welfare state creates poverty traps and stigma, while the labor market increasingly offers precarious, low-wage work that fails to provide dignity or security, leading to widespread precarity and limited autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Social policy researchers, labor economists, and human rights organizations consistently document the ongoing issues of precarious work, welfare stigma, and economic insecurity, corroborating the problem's persistence from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set at 0.25, reflecting the moderate cost of funding UBI through taxation, which is offset by the significant benefits of increased autonomy and reduced social costs. Suppression is very low (0.10) because the core function of this reading is to *reduce* coercion in the labor market and welfare system. Theater ratio is minimal (0.05) as the policy is a direct intervention with clear, measurable outcomes. Accessibility collapse is low (0.10) because UBI *expands* alternatives for individuals. Resistance is moderate (0.40) due to political opposition from those who benefit from the existing labor market structure or hold different ideological views on welfare.
 *
 * PERSPECTIVAL GAP:
 *   While this reading frames UBI as a net benefit and a Rope, other readings (e.g., 'dependency_trap_reading') would perceive it as extractive or suppressive, leading to a different classification. The engine's computation of per-seat classification will highlight these divergences based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers, caregivers, artists, and abuse victims are clear beneficiaries, experiencing a significant increase in autonomy and security. The general public, while contributing through taxes, also benefits from a more stable and equitable society. Employers in low-wage sectors are positioned as payers due to the anticipated increase in labor costs and reduced bargaining power. Advocacy groups and economists act as agenda-setters and observers, shaping the discourse and policy implementation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''freedom_floor_reading'' of the ''unconditional_income_support'' kernel?',
    'Analysis of policy proposals and advocacy literature to confirm the framing of UBI as primarily an autonomy-enabling mechanism.',
    'If misidentified, the entire analysis of this constraint''s structural properties and its relation to other readings would be invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed.').

omega_variable(
    dependency_trap_contradiction,
    'Does the ''freedom_floor_reading'' genuinely foreclose the ''dependency_trap_reading'', or can elements of both coexist in practice?',
    'Empirical studies on long-term UBI pilots measuring labor force participation, entrepreneurial activity, and psychological well-being, specifically looking for evidence of ''dependency traps'' vs. ''autonomy gains''.',
    'If the dependency trap is found to be a significant outcome, the ''freedom_floor_reading''s'' core premise is undermined, potentially shifting its classification towards a ''tangled_rope'' or ''snare'' from certain perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_trap_contradiction, empirical, 'Examines the direct contradiction with the dependency trap reading.').

omega_variable(
    universality_paradox_implications,
    'How do the political and fiscal ambiguities highlighted by the ''universality_paradox_reading'' affect the practical implementation and long-term stability of the ''freedom_floor_reading''?',
    'Comparative policy analysis of UBI implementation strategies across different political economies, tracking fiscal sustainability and political coalition stability.',
    'If the paradox leads to significant implementation failures or political instability, the ''freedom_floor_reading''s'' practical viability as a ''rope'' could be challenged, even if its normative goals remain valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_paradox_implications, conceptual, 'Explores the practical implications of the universality paradox.').

omega_variable(
    labor_supply_effects_magnitude,
    'What is the actual magnitude and nature of labor supply changes (reduction in hours, shift to informal work, entrepreneurial activity) resulting from unconditional income support?',
    'Large-scale, long-term randomized controlled trials of UBI in diverse economic contexts, with robust data collection on labor market outcomes.',
    'Significant, widespread reduction in labor supply could challenge the ''freedom_floor_reading''s'' claim of minimal labor market distortion and shift the perception of its ''extractiveness'' from the perspective of the broader economy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_supply_effects_magnitude, empirical, 'Quantifies the impact on labor market participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__freedom_floor_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__freedom_floor_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__freedom_floor_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__freedom_floor_reading, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__freedom_floor_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__freedom_floor_reading, base_extractiveness, 25, 0.25).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__freedom_floor_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__freedom_floor_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__freedom_floor_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__freedom_floor_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__freedom_floor_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__freedom_floor_reading, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__freedom_floor_reading, suppression_requirement, 30, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, precarious_labor_market).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, welfare_stigma).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, social_safety_net_design).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
