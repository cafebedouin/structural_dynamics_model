% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the freedom-floor reading of the
 *   unconditional-income-support kernel: an unconditional transfer functions
 *   as a coordination mechanism that removes the coercive edge from
 *   labor-market participation and from dependency relationships more
 *   broadly. Under this reading, precarious workers, unpaid caregivers,
 *   artists, abuse survivors, and informal-sector workers gain a genuine exit
 *   option they currently lack, without an identifiable victim class bearing
 *   concentrated costs — the funding burden is modeled as diffuse and
 *   Pareto-improving rather than extractive. This is a deliberately narrow
 *   reading: the dependency_trap_reading and universality_paradox_reading are
 *   separate constraints, generated separately, describing the same
 *   underlying policy mechanism through incompatible structural claims
 *   (identifiable victims and incentive distortion in one case; politically
 *   ambiguous convergent fiscal outcomes in the other). Do not average these
 *   readings together — each has its own stable epsilon.
 *
 * KEY AGENTS:
 *   - precarious_workers: primary beneficiary (powerless/constrained) — gains reservation-wage leverage
 *   - unpaid_caregivers: primary beneficiary (powerless/trapped) — gains independent income stream
 *   - domestic_abuse_survivors: primary beneficiary (powerless/trapped) — gains exit capacity from dependency
 *   - taxpaying_public: diffuse payer (organized/constrained) — funds via general taxation, no concentrated victim
 *   - policy_evaluators: analytical observer — assesses labor supply and autonomy claims against pilot data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.18).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.08).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, 'c96ebd34-cfa4-453a-8b68-daa1d83c582e').
narrative_ontology:cs_kernel_codification('c96ebd34-cfa4-453a-8b68-daa1d83c582e', distributed).
narrative_ontology:cs_authority_grounding('c96ebd34-cfa4-453a-8b68-daa1d83c582e', distributed).
narrative_ontology:cs_reading_relation('c96ebd34-cfa4-453a-8b68-daa1d83c582e', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('c96ebd34-cfa4-453a-8b68-daa1d83c582e', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('c96ebd34-cfa4-453a-8b68-daa1d83c582e', foundational, material_security_is_precondition_for_voluntary_participation).
narrative_ontology:cs_axiom_status(material_security_is_precondition_for_voluntary_participation, holdable).
narrative_ontology:cs_axiom_grounding('c96ebd34-cfa4-453a-8b68-daa1d83c582e', material_security_is_precondition_for_voluntary_participation, empirically_contingent).
narrative_ontology:cs_axiom('c96ebd34-cfa4-453a-8b68-daa1d83c582e', foundational, unconditional_transfer_produces_no_identifiable_victim_class).
narrative_ontology:cs_axiom_status(unconditional_transfer_produces_no_identifiable_victim_class, holdable).
narrative_ontology:cs_axiom_grounding('c96ebd34-cfa4-453a-8b68-daa1d83c582e', unconditional_transfer_produces_no_identifiable_victim_class, empirically_contingent).
narrative_ontology:cs_reference_frame('c96ebd34-cfa4-453a-8b68-daa1d83c582e', conditional_means_tested_welfare_baseline).
narrative_ontology:cs_drift_state('c96ebd34-cfa4-453a-8b68-daa1d83c582e', post_pilot_evidence_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c96ebd34-cfa4-453a-8b68-daa1d83c582e', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists_and_creators).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, informal_sector_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpaying_public).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, employers_of_low_wage_labor).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, labor_market_coercion_reduction_thesis).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, welfare_stigma_elimination_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently must accept whatever wages and conditions are on offer because refusal means no income at all. An unconditional floor gives them a real reservation wage below which they can decline exploitative or unsafe work without facing destitution. They receive the transfer regardless of employment status.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, constrained, national).

% Perform childcare, eldercare, and household labor with no wage and no independent income, often financially dependent on a partner or family member. The floor provides an individualized income stream that does not depend on market employment or on staying in a dependent relationship for material survival.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, generational, trapped, national).

% Produce work with uncertain and irregular market returns; currently must either commercialize output prematurely or abandon the practice to take stable employment. The floor decouples subsistence from immediate market success, letting creative or exploratory labor proceed on its own timeline.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists_and_creators, beneficiary,
    powerless, biographical, mobile, national).

% Financial dependency on an abusive partner is frequently the binding constraint preventing exit from the relationship. An unconditional, individually-paid floor provides an independent income stream that does not require employment, a bank account controlled by the abuser, or bureaucratic proof of hardship to access.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors, beneficiary,
    powerless, immediate, trapped, national).

% Work outside formal employment records (day labor, subsistence farming, informal trade) and are typically excluded from conditional benefit systems that require pay stubs or employer verification. Because the floor is unconditional, it reaches them without documentation barriers.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, informal_sector_workers, beneficiary,
    powerless, biographical, constrained, national).

% Fund the transfer through general taxation. Under this reading the funding is framed as a Pareto-improving social investment rather than a redistributive extraction: reduced downstream costs of poverty, crisis intervention, and coercive dependency are claimed to offset the fiscal outlay, and the tax burden is distributed broadly rather than concentrated on an identifiable victim group.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpaying_public, payer,
    organized, generational, constrained, national).

% May face modestly higher wage floors or reduced applicant desperation once workers have an exit option, and may pay somewhat more in aggregate taxation. This reading claims the labor-supply effect is minimal per pilot evidence, so the cost to this seat is treated as small rather than a defining extraction relationship.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, employers_of_low_wage_labor, observer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, employers_of_low_wage_labor, payer).

% Study labor supply responses, autonomy outcomes, and fiscal effects across pilots (Alaska Permanent Fund, Kenya GiveDirectly, Finland, Stockton) to evaluate whether the coordination story this reading tells matches observed behavior.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, policy_evaluators, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(unconditional_income_support__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of coerced labor-market participation: without an income floor, workers must accept any offered terms because refusal means destitution, which suppresses wages, tolerates unsafe conditions, and traps people in dependent relationships. An unconditional floor coordinates a baseline of material security that lets participation in labor markets, caregiving, or creative work become genuinely voluntary rather than compelled.
% TRANSFER_FUNCTION: Moves general tax revenue from the broad taxpaying public to individuals unconditionally, regardless of employment status, need-testing, or behavior. Under this reading the movement is not extraction from an identifiable victim group but a pooled social-insurance transfer that every contributor could in principle also receive.
% ABSENT_VOICES: Fiscal conservatives concerned about long-run program cost growth and targeted-welfare administrators whose institutional role would shrink are not seated as victims in this reading because the reading claims no victims exist — but their objections properly belong to the sibling readings (dependency_trap_reading, universality_paradox_reading), not to this constraint.
% DISAPPEARANCE_RATIONALE: If the floor disappeared, precarious workers would lose their reservation-wage leverage and be pushed back toward accepting any available terms; caregivers and abuse survivors would lose independent income and some would become financially trapped again; artists would face renewed pressure to abandon uncommercial work. The autonomy gains this reading claims are structurally dependent on the floor's continued existence.
% FOUNDING_PROBLEM: Labor markets and welfare bureaucracies impose coercive conditionality: workers must accept degrading terms to survive, and means-tested aid is stigmatizing, administratively costly, and creates cliff-edge disincentives. The founding problem is the absence of a no-questions-asked income baseline beneath which no one can fall.
% FOUNDING_PROBLEM_CORROBORATION: Independent pilot evaluations (Alaska Permanent Fund longitudinal studies, GiveDirectly Kenya randomized trials, Stockton SEED evaluation by independent university researchers) attest that labor-market coercion and welfare-stigma effects are measurable and that recipients report increased autonomy — these evaluators are outside the direct beneficiary population and outside advocacy organizations promoting the policy.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.18) reflecting this reading's empirical claim that labor-supply effects are minimal (per Alaska Permanent Fund and Kenya GiveDirectly data) and that funding is broadly diffused rather than concentrated on an identifiable payer class. Suppression is low (0.08) because participation requires no coercive enforcement — the floor is received passively and no one is compelled to change behavior to obtain it. Theater ratio is low (0.10) because the transfer mechanism is functionally direct: cash moves, no elaborate compliance apparatus intervenes. Resistance is moderate (0.35) reflecting genuine political contestation over funding mechanisms even though this reading claims no identifiable victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared explicitly and are the structural center of this reading: agents currently constrained by bad labor-market or relational options (low d, benefiting from the constraint's operation). No victims are declared under this reading — the taxpaying_public is authored as a payer role but not a base_properties victim, since the reading's core claim is that the transfer is a Pareto improvement funded diffusely rather than an extraction from an identifiable group. This is the load-bearing structural choice that differentiates this reading from dependency_trap_reading, which would declare victims (net contributors, displaced targeted-aid recipients) explicitly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coercive labor-market conditionality and welfare stigma) is authored as live and corroborated by independent pilot evaluation, which forecloses a mandatrophy reading under this specific framing — the mechanism has not outlived its function under this reading's own evidentiary standard. Whether the mechanism DOES outlive its function, or converges toward the dependency-trap or universality-paradox structure over time, is precisely what the sibling readings and the omega variables below track; this story does not adjudicate that contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_response_magnitude,
    'Do unconditional transfers meaningfully reduce labor-force participation or work intensity at scale, or do pilot findings of minimal effect (Alaska, Kenya, Stockton) generalize to permanent, universal, higher-value implementations?',
    'Long-run natural experiments at national scale with permanent (not time-limited) transfers of sufficient size to test whether the minimal-effect finding holds outside pilot conditions; comparison of Alaska''s multi-decade dividend against short-duration pilots elsewhere.',
    'If labor supply effects turn out to be substantial at scale, this reading''s claim of moderate epsilon and ''no victims'' becomes harder to sustain, and the dependency_trap_reading''s structural claims gain empirical support relative to this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_magnitude, empirical, 'Whether minimal labor-supply effects generalize beyond pilot scale and duration.').

omega_variable(
    diffuse_versus_concentrated_funding_incidence,
    'Is the fiscal burden of funding an unconditional floor genuinely diffuse across the taxpaying public, or does it concentrate on identifiable groups (e.g., middle-income wage earners, or via inflation on fixed-income populations) once financing mechanisms are specified?',
    'Distributional incidence analysis of specific proposed funding mechanisms (VAT, wealth tax, carbon dividend, deficit financing) to determine whether any single group bears disproportionate net cost.',
    'If funding incidence concentrates on an identifiable group, this reading''s ''no victims'' claim would need revision — the constraint would begin to resemble the tangled_rope or dependency_trap structure rather than a clean Pareto-improving rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_versus_concentrated_funding_incidence, empirical, 'Whether the funding side of this reading genuinely lacks a concentrated payer class.').

omega_variable(
    reading_convergence_over_time,
    'Does an unconditional income support program, once implemented under this freedom-floor framing, structurally drift toward the dependency_trap_reading or universality_paradox_reading as political coalitions and administrative practice evolve?',
    'Longitudinal tracking of implemented programs (if any reach permanent national scale) for drift in claimed coordination function, emergence of conditionality creep, or emergence of identifiable victim groups.',
    'If drift toward a different reading is observed, it would validate treating these as genuinely distinct constraints (per the ε-invariance principle) rather than as three interpretations of one stable mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_convergence_over_time, conceptual, 'Whether this reading is stable over time or transitions structurally into a sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__freedom_floor_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement_basis(unco_tr_t4, observed).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__freedom_floor_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(unco_tr_t8, observed).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__freedom_floor_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(unco_tr_t12, projected).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__freedom_floor_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(unco_tr_t16, projected).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(unco_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__freedom_floor_reading, base_extractiveness, 4, 0.16).
narrative_ontology:measurement_basis(unco_be_t4, observed).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__freedom_floor_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement_basis(unco_be_t8, observed).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__freedom_floor_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement_basis(unco_be_t12, projected).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__freedom_floor_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(unco_be_t16, projected).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(unco_be_t20, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unconditional_income_support kernel. freedom_floor_reading claims moderate epsilon, no victims, rope classification (genuine coordination enabling voluntary participation). dependency_trap_reading claims identifiable victims (net contributors, displaced targeted-aid recipients) and incentive distortion. universality_paradox_reading claims the cross-ideological appeal itself is the structural feature, masking convergent-but-incompatible implementation paths. Each story carries its own epsilon and its own beneficiary/victim declarations; they are not to be merged or averaged. Link direction: this story does not claim upstream/downstream priority over its siblings — all three are coequal readings of the same contested kernel, hence bidirectional affects_constraints links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
