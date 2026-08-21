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
 *   human_readable: Unconditional Income Support (Freedom Floor Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'freedom floor' reading of unconditional
 *   income support, where the primary goal is to enhance individual autonomy,
 *   dignity, and capacity to exit exploitative labor. It is framed as a pure
 *   coordination mechanism (Rope) to establish a universal baseline, with
 *   minimal inherent extraction. The beneficiaries are broad, including
 *   caregivers, precarious workers, and abuse survivors, who gain increased
 *   agency. There are no direct victims, as the system aims for universal
 *   benefit, funded through general taxation. The challenge lies in
 *   coordinating the funding level and tax base, not in overcoming inherent
 *   extraction.
 *
 * KEY AGENTS:
 *   - all_citizens: Primary beneficiary (organized/mobile) — gains autonomy and dignity
 *   - caregivers: Primary beneficiary (moderate/constrained) — gains recognition and financial stability
 *   - precarious_workers: Primary beneficiary (powerless/constrained) — gains exit capacity from exploitation
 *   - abuse_survivors: Primary beneficiary (powerless/trapped) — gains financial means to escape
 *   - artists_and_entrepreneurs: Primary beneficiary (moderate/mobile) — gains stability for creative/innovative work
 *   - employers: Payer (powerful/mobile) — faces pressure for better wages, contributes to tax base
 *   - taxpayers: Payer (organized/mobile) — contributes to funding, benefits from social stability
 *   - welfare_state_administrators: Agenda-setter (institutional/constrained) — manages the program
 *   - rival_readings_advocates: Excluded (powerful/analytical) — advocates for alternative framings
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
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '2f53686d-aa0a-4ffd-b6d8-a3e92c33b426').
narrative_ontology:cs_kernel_codification('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', formalized).
narrative_ontology:cs_authority_grounding('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', lineage).
narrative_ontology:cs_interpretation_layer_present('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426').
narrative_ontology:cs_reading_relation('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', foundational, human_dignity_requires_economic_security).
narrative_ontology:cs_axiom_status(human_dignity_requires_economic_security, holdable).
narrative_ontology:cs_axiom_grounding('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', human_dignity_requires_economic_security, deontological).
narrative_ontology:cs_axiom('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', foundational, exit_capacity_enhances_labor_market_efficiency).
narrative_ontology:cs_axiom_status(exit_capacity_enhances_labor_market_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', exit_capacity_enhances_labor_market_efficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', universal_human_flourishing).
narrative_ontology:cs_drift_state('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', contemporary_policy_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2f53686d-aa0a-4ffd-b6d8-a3e92c33b426', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_and_entrepreneurs).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, all_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a baseline income, enabling greater autonomy, dignity, and the capacity to refuse exploitative labor. Benefits from reduced precarity and improved social cohesion.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, all_citizens, beneficiary,
    organized, generational, mobile, national).

% Receives recognition and financial support for unpaid care work, allowing them to sustain their families without external market pressure or means-tested stigma.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, local).

% Gains a safety net that reduces vulnerability to exploitative labor conditions, enabling them to seek better employment or pursue education without immediate financial desperation.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, local).

% Receives the financial means to exit abusive relationships or situations, where economic dependence is often a primary barrier to escape.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, trapped, local).

% Gains the financial stability to pursue creative or innovative ventures that may not offer immediate returns, fostering cultural and economic dynamism.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_and_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Faces increased pressure to offer competitive wages and working conditions as workers gain greater exit capacity from undesirable jobs. Contributes to the tax base funding the income support.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, payer,
    powerful, biographical, mobile, national).

% Contributes to the tax base required to fund the universal income support. Benefits indirectly from a more stable and equitable society.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, taxpayers, payer,
    organized, biographical, mobile, national).

% Administers the universal income support program, ensuring efficient distribution and managing the tax collection necessary to sustain it. Benefits from simplified administration compared to means-tested programs.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_state_administrators, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a society-wide commitment to a baseline standard of living, ensuring all citizens have the financial means to meet basic needs and participate in society, thereby reducing poverty and precarity.
% TRANSFER_FUNCTION: Transfers financial resources from the general tax base to all citizens, providing a regular, unconditional income floor.
% ABSENT_VOICES: Advocates for a 'work-first' welfare model, who would argue that unconditional income disincentivizes labor, are present in public discourse but are structurally excluded from the design of this specific reading, which prioritizes autonomy over labor force participation.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, millions would immediately lose their financial floor, leading to a sharp increase in poverty, precarity, and a significant shift in labor market dynamics as workers lose their exit capacity. Social safety nets would be overwhelmed, and the economy would reorganize around a more coercive labor market.
% FOUNDING_PROBLEM: The problem of widespread poverty, economic insecurity, and the erosion of human dignity caused by precarious labor markets and inadequate social safety nets.
% FOUNDING_PROBLEM_CORROBORATION: Numerous independent economic studies, social policy analyses, and human rights organizations corroborate the ongoing problem of economic insecurity and the potential for unconditional income to address it. Public opinion polls also show significant concern about poverty and inequality.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.15) because the design aims to provide a net benefit to all participants by reducing precarity and enhancing autonomy, with the 'cost' being the necessary taxation for coordination. Suppression is minimal (0.05) as the system is designed to remove coercive pressures, not impose them. Theater ratio is very low (0.02) because the stated function (providing a freedom floor) directly aligns with its operation. Accessibility collapse is low (0.1) as it expands, rather than collapses, alternatives for individuals. Resistance is low (0.1) from beneficiaries, but higher from those who oppose the underlying philosophy (e.g., 'work-first' advocates).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this constraint is a pure Rope, enabling collective flourishing. From the perspective of those who oppose the underlying philosophy (e.g., 'dependency trap' advocates), it might be seen as a misallocation of resources or even a Snare that fosters dependence. The engine's classification will reflect the structural reality of this reading, which is designed to be non-extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   All citizens, caregivers, precarious workers, abuse survivors, and artists/entrepreneurs are direct beneficiaries, experiencing low directionality as the constraint subsidizes their autonomy. Employers and taxpayers are payers, bearing the cost of funding, but also benefiting from a more stable society, leading to a moderate directionality. Welfare state administrators are agenda-setters, managing the system, with a directionality reflecting their administrative role and the public good served.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly addresses the 'mandatrophy' concern by framing income support not as a temporary fix for unemployment, but as a permanent enabler of human flourishing and labor market flexibility. The mandate is to provide a 'freedom floor,' which is intended to be a live and evolving problem, not one that atrophies. The low extractiveness and suppression are central to preventing it from degrading into a Snare or Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_sustainability,
    'Is the proposed tax base sufficient and resilient enough to sustainably fund a universal unconditional income support at a level that genuinely provides a ''freedom floor''?',
    'Long-term macroeconomic modeling and pilot program data on tax compliance and economic growth under universal income scenarios.',
    'If funding is unsustainable, the constraint could degrade into a Snare (if benefits are cut but taxes remain) or a Piton (if the program becomes symbolic). If sustainable, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_sustainability, empirical, 'Uncertainty regarding the long-term financial viability of universal income support.').

omega_variable(
    labor_market_impact,
    'Does the ''freedom floor'' reading accurately predict the labor market''s adaptation, or would it lead to significant labor shortages in essential sectors, as argued by the ''dependency_trap_reading''?',
    'Empirical studies from large-scale universal basic income trials, observing changes in labor supply, job quality, and sector-specific employment rates.',
    'If significant, unmitigated labor shortages occur, the ''freedom floor'' reading''s claim of universal benefit would be challenged, potentially shifting its classification towards a Tangled Rope or even Snare for certain sectors/populations. If labor markets adapt positively, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact, empirical, 'Uncertainty about the actual impact of unconditional income on labor market participation and essential services.').

omega_variable(
    political_will_for_universality,
    'Can the political will for universal, unconditional income support be sustained against pressures for means-testing and targeting, as advocated by the ''targeting_efficiency_reading''?',
    'Analysis of legislative stability, public opinion trends, and the political durability of universal programs in different jurisdictions over time.',
    'If political will erodes towards targeting, the constraint would shift towards a more extractive model (Tangled Rope or Snare) for those excluded, and its ''freedom floor'' promise would be undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_will_for_universality, preference, 'Uncertainty about the long-term political feasibility of maintaining universal income support.').


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
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.02).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.02).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__freedom_floor_reading, theater_ratio, 40, 0.02).
narrative_ontology:measurement(inco_tr_t50, income_support_commitment__freedom_floor_reading, theater_ratio, 50, 0.02).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__freedom_floor_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(inco_be_t50, income_support_commitment__freedom_floor_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__freedom_floor_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__freedom_floor_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(inco_su_t50, income_support_commitment__freedom_floor_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_commitment' kernel, focusing on autonomy and dignity. It is structurally distinct from the 'dependency_trap_reading' and 'targeting_efficiency_reading' due to different beneficiary structures and underlying normative claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
