% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Freedom Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'freedom floor' reading of unconditional
 *   income support, where the primary goal is to enhance individual autonomy,
 *   dignity, and labor market exit capacity. It is framed as a Rope, solving
 *   a genuine coordination problem (universal economic security) with minimal
 *   extraction, as universality eliminates the administrative overhead and
 *   stigma associated with means-tested programs. The low extractiveness
 *   reflects the view that the costs are primarily coordination costs (tax
 *   collection, distribution) rather than asymmetric transfers. The metrics
 *   reflect a system that, once established, becomes less suppressive and
 *   more efficient over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.15).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.05).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support as Freedom Floor").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, 'e7f6e8c4-33b9-491e-9375-3328e7b2706c').
narrative_ontology:cs_kernel_codification('e7f6e8c4-33b9-491e-9375-3328e7b2706c', formalized).
narrative_ontology:cs_authority_grounding('e7f6e8c4-33b9-491e-9375-3328e7b2706c', lineage).
narrative_ontology:cs_interpretation_layer_present('e7f6e8c4-33b9-491e-9375-3328e7b2706c').
narrative_ontology:cs_reading_relation('e7f6e8c4-33b9-491e-9375-3328e7b2706c', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7f6e8c4-33b9-491e-9375-3328e7b2706c', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('e7f6e8c4-33b9-491e-9375-3328e7b2706c', foundational, universal_dignity_and_autonomy).
narrative_ontology:cs_axiom_status(universal_dignity_and_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('e7f6e8c4-33b9-491e-9375-3328e7b2706c', universal_dignity_and_autonomy, deontological).
narrative_ontology:cs_axiom('e7f6e8c4-33b9-491e-9375-3328e7b2706c', foundational, economic_security_as_precondition_for_freedom).
narrative_ontology:cs_axiom_status(economic_security_as_precondition_for_freedom, holdable).
narrative_ontology:cs_axiom_grounding('e7f6e8c4-33b9-491e-9375-3328e7b2706c', economic_security_as_precondition_for_freedom, instrumental).
narrative_ontology:cs_reference_frame('e7f6e8c4-33b9-491e-9375-3328e7b2706c', post_industrial_social_contract).
narrative_ontology:cs_drift_state('e7f6e8c4-33b9-491e-9375-3328e7b2706c', contemporary_precarity_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e7f6e8c4-33b9-491e-9375-3328e7b2706c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, all_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, human_dignity_as_foundational).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, autonomy_as_social_good).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, labor_market_power_rebalancing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a baseline income, enabling greater freedom in career choices, caregiving, and civic participation. Reduces precarity and enhances bargaining power in the labor market. Experiences reduced stress and improved health outcomes.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, all_citizens, beneficiary,
    organized, generational, mobile, national).

% Receives financial recognition for unpaid care work, allowing them to sustain their families and contribute to society without being forced into exploitative labor. Enhances their autonomy and reduces economic vulnerability.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, local).

% Gains a safety net that allows them to refuse exploitative work, seek better opportunities, or pursue education/training. Reduces the immediate pressure to accept low-wage, insecure employment, increasing their labor market exit capacity.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, local).

% Receives the financial means to leave abusive situations, reducing economic dependency on their abuser. Provides a critical pathway to safety and independence, which is often foreclosed by financial precarity.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, identity_locked, local).

% Gains the financial stability to pursue creative endeavors or start new businesses without the immediate pressure of market success. Fosters innovation and cultural production by de-risking non-traditional career paths.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Faces a labor market with increased worker bargaining power, potentially leading to higher wages and improved working conditions. May experience increased tax burden to fund the program. Their ability to rely on a desperate labor pool is constrained.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, payer,
    institutional, biographical, mobile, national).

% Contributes to the funding of the unconditional income support through taxes. Benefits indirectly from a more stable and equitable society, but bears the direct financial cost.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, taxpayers, payer,
    organized, biographical, mobile, national).

% The existing means-tested welfare system would be largely superseded by a universal unconditional income. Its administrative function and associated jobs would be significantly reduced or re-purposed, leading to institutional resistance.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_bureaucracy, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a universal baseline of economic security, ensuring all citizens have a minimum standard of living and the capacity to participate in society and the labor market on more equitable terms. Solves the collective action problem of poverty and precarity.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to all citizens, regardless of employment status or other income, creating a floor of economic security.
% ABSENT_VOICES: The existing welfare bureaucracy, which would see its power and function diminished, would argue against universality in favor of targeted, conditional support. Advocates for a 'work-first' approach would also be excluded, arguing for conditional benefits tied to employment.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, the autonomy and dignity of many citizens would immediately diminish. Precarious workers would lose their exit capacity, caregivers would face renewed economic pressure, and the overall bargaining power of labor would weaken, leading to a re-entrenchment of existing power imbalances in the labor market and society.
% FOUNDING_PROBLEM: The problem of persistent poverty, economic precarity, and the erosion of worker bargaining power in an increasingly automated and globalized economy, leading to widespread insecurity and diminished human autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for basic income, social scientists studying precarity, and labor unions attest that the founding problem is very much alive, citing rising inequality, automation risks, and the gig economy's impact. This is corroborated by numerous pilot programs demonstrating positive impacts on health, education, and entrepreneurship, and by economic analyses from independent research institutions.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15 declining to 0.10) because the primary transfer is from the general tax base to all citizens, with no identifiable victims in the traditional sense; the 'cost' is a collective investment in social stability and individual freedom. Suppression is near zero (0.05 declining to 0.00) as the system is designed to remove coercive pressures, not impose them. Theater ratio is negligible (0.02 declining to 0.00) as the function is direct and transparent. Accessibility collapse is low (0.2) because it creates alternatives rather than collapsing them. Resistance is low (0.1) from beneficiaries, but higher from those whose power is challenged (e.g., employers, welfare bureaucracy).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this is a pure Rope, providing essential support. From the perspective of employers, it might be seen as a Tangled Rope, as it coordinates social stability but extracts higher labor costs. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   All citizens, especially vulnerable groups like caregivers and precarious workers, are direct beneficiaries (d near 0.0). Employers and taxpayers are payers (d near 1.0) as they bear the costs, but employers also face a rebalanced labor market, which is a benefit to society as a whole. The existing welfare bureaucracy is structurally excluded, as their function would be largely replaced.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly addresses the potential for mandatrophy by framing the 'founding problem' as ongoing precarity and power imbalances, which remain 'live.' The solution is designed to be adaptive to changing economic conditions, preventing the constraint from becoming a Piton. The low extractiveness and suppression also guard against it becoming a Snare, as the benefits are universal and non-coercive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_sustainability,
    'Is the proposed funding model for unconditional income support fiscally sustainable over the long term, especially during economic downturns or demographic shifts?',
    'Long-term macroeconomic modeling, pilot program data on behavioral responses (e.g., labor supply, entrepreneurship), and analysis of tax base elasticity.',
    'If unsustainable, the constraint might require higher taxation (increasing extractiveness for payers) or reduced benefit levels (diminishing its ''freedom floor'' function), potentially shifting its classification towards a Tangled Rope or even a Snare if benefits become insufficient while costs remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_sustainability, empirical, 'Uncertainty regarding the long-term fiscal viability of universal unconditional income support.').

omega_variable(
    labor_supply_response,
    'What is the actual long-term impact of unconditional income support on labor supply and the overall productivity of the economy?',
    'Large-scale, multi-year randomized control trials (RCTs) or natural experiments in jurisdictions implementing universal basic income, measuring labor force participation, hours worked, and entrepreneurial activity.',
    'If labor supply significantly decreases without compensatory increases in other productive activities (e.g., care work, education, entrepreneurship), the ''freedom floor'' argument might be weakened, and the ''dependency_trap_reading'' could gain empirical support, shifting the constraint''s perceived function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response, empirical, 'Uncertainty about the behavioral response of labor supply to unconditional income support.').

omega_variable(
    framing_under_determination,
    'Is the ''freedom_floor_reading'' the most defensible framing, or does the ''dependency_trap_reading'' or ''targeting_efficiency_reading'' offer a more accurate structural account?',
    'A comprehensive analysis of empirical evidence from pilot programs, combined with a philosophical and ethical evaluation of the underlying normative claims (autonomy vs. work ethic vs. fiscal prudence).',
    'If an alternative reading were adopted, the constraint''s classification would shift dramatically: the ''dependency_trap_reading'' would likely classify it as a Snare (extracting work capacity, suppressing individual initiative), while the ''targeting_efficiency_reading'' might see it as an inefficient Rope (coordinating poorly, wasting resources).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Ambiguity in the foundational framing of unconditional income support''s purpose and effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.02).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.01).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.01).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.01).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__freedom_floor_reading, theater_ratio, 50, 0.0).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__freedom_floor_reading, base_extractiveness, 50, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.04).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.03).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__freedom_floor_reading, suppression_requirement, 30, 0.02).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__freedom_floor_reading, suppression_requirement, 40, 0.01).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__freedom_floor_reading, suppression_requirement, 50, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, labor_market_wage_setting).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, social_safety_net_design).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, care_economy_valuation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
