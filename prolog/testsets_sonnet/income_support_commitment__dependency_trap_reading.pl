% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Work-Disincentive / Dependency Trap
 *   domain: political_economy/social_policy/welfare_state
 *
 * SUMMARY:
 *   This story instantiates the dependency-trap reading of the income-support
 *   commitment kernel: unconditional transfers, viewed through this lens,
 *   function as a tangled rope — a real coordination function (income floor
 *   against poverty and volatility) fused to an asymmetric extraction from
 *   working taxpayers toward non-participating recipients, compounded by an
 *   internal cost to recipients themselves as skills atrophy the longer
 *   labor-market exit persists. The extraction rises modestly over the
 *   measured interval as labor-supply elasticity effects and skill-decay
 *   evidence accumulate in the dependency-school literature this reading
 *   draws on.
 *
 * KEY AGENTS:
 *   - working_taxpayers: bear the fiscal transfer cost while continuing to work
 *   - labor_market_exiters: beneficiary class that substitutes transfer income for wages
 *   - skill_atrophying_recipients: dual-positioned — short-term beneficiary, long-term payer through employability erosion
 *   - welfare_administering_state: agenda-setter that could add conditionality but bears political cost of doing so
 *   - employers_facing_labor_shortages: excluded party affected by reduced applicant pools
 *   - labor_economists_dependency_school: analytical observers producing the empirical case for this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.52).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.44).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Work-Disincentive / Dependency Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/welfare_state").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '62607a6a-d7a6-4279-bc21-bc9ff0d873b0').
narrative_ontology:cs_kernel_codification('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', distributed).
narrative_ontology:cs_authority_grounding('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', distributed).
narrative_ontology:cs_reading_relation('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', foundational, work_participation_is_load_bearing_for_dignity_and_capacity).
narrative_ontology:cs_axiom_status(work_participation_is_load_bearing_for_dignity_and_capacity, holdable).
narrative_ontology:cs_axiom_grounding('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', work_participation_is_load_bearing_for_dignity_and_capacity, empirically_contingent).
narrative_ontology:cs_axiom('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', secondary, unconditional_transfers_causally_suppress_labor_supply).
narrative_ontology:cs_axiom_status(unconditional_transfers_causally_suppress_labor_supply, holdable).
narrative_ontology:cs_axiom_grounding('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', unconditional_transfers_causally_suppress_labor_supply, empirically_contingent).
narrative_ontology:cs_reference_frame('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', conditional_reciprocity_welfare_norm).
narrative_ontology:cs_drift_state('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', contemporary_ubi_pilot_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('62607a6a-d7a6-4279-bc21-bc9ff0d873b0', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, labor_market_exiters).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, skill_atrophying_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, skill_atrophying_recipients).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, work_incentive_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund the transfer program through payroll and income taxation while continuing to work full hours. They bear the fiscal cost of subsidizing non-participants and, in this reading, absorb the opportunity cost of a shrinking productive tax base as more recipients exit work. Exit from the funding obligation is not available short of emigration or informal-sector withdrawal, both costly.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    moderate, biographical, constrained, national).

% Receive the unconditional transfer without a work requirement and, in this reading, use it to substitute for wage labor rather than to bridge into it. Their exit option from the labor market itself is real and low-friction precisely because the transfer removes the income floor that would otherwise compel job search.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_market_exiters, beneficiary,
    powerless, biographical, mobile, national).

% Receive short-term income security but, on this reading, experience long-run erosion of employability, professional networks, and work-readiness the longer they remain outside the labor market. They benefit in the immediate term and pay in the biographical and generational term through diminished re-entry capacity — a dual position within the same seat.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, skill_atrophying_recipients, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, skill_atrophying_recipients, beneficiary).

% Designs, funds, and enforces the unconditional transfer's eligibility rules and disbursement mechanics. It can adjust the design (add work requirements, tapering, time limits) but faces political and administrative costs in doing so, and in this reading is accused of underweighting the labor-supply-response evidence that would justify tightening the design.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, welfare_administering_state, agenda_setter,
    institutional, generational, analytical, national).

% Report difficulty filling entry-level and low-wage positions in sectors where the transfer competes with wages. They are not formal parties to the transfer's design process but would argue, if consulted, that the unconditional structure raises their effective labor costs and reduces available applicant pools.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, employers_facing_labor_shortages, excluded,
    organized, biographical, constrained, national).

% Study labor-supply elasticity, reservation-wage effects, and re-entry trajectories among transfer recipients. They produce the empirical case this reading relies on — that unconditional transfers measurably reduce labor force participation and are associated with skill decay over multi-year windows.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_economists_dependency_school, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, labor_market_exiters).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a guaranteed income floor so that no household falls below subsistence regardless of employment status, removing the need for means-testing bureaucracy at the point of disbursement.
% TRANSFER_FUNCTION: Moves tax revenue collected from working households to non-working or partially-working households, unconditioned on job search, training participation, or any labor-market re-engagement obligation.
% ABSENT_VOICES: Employers facing entry-level labor shortages and the tax base of future workers who will inherit a smaller productive base are not seated in the design conversation; the design forum centers recipients and administrators, not the funders' labor-market counterparties.
% DISAPPEARANCE_RATIONALE: If the unconditional structure were removed overnight and replaced with a work-conditioned transfer, some recipients would re-enter job search sooner, the tax burden calculus for working households would shift, and administering agencies would need new conditionality infrastructure — but the underlying income floor commitment (the kernel) would likely persist in a different reading's form.
% FOUNDING_PROBLEM: Poverty and income volatility left large populations without subsistence security during unemployment, illness, or structural economic transitions; means-tested programs were slow, stigmatizing, and left gaps.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists in the dependency school attest that the original poverty-alleviation problem is substantially addressed but a new problem — labor-supply suppression and skill atrophy — has emerged as a side effect of the unconditional design; this attestation comes from outside the recipient and administering-state seats, though it is itself a contested empirical claim disputed by economists in the freedom-floor tradition.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52 by interval end — genuine coordination (poverty floor) is present, so this is not pure extraction, but the asymmetry between the funding class and the exiting class is real and structurally maintained by continued unconditional eligibility rules. Suppression is moderate (0.44): the constraint does not coercively trap taxpayers, but it does require active enforcement (tax collection, eligibility administration) and its persistence depends on political majorities not tightening conditionality. Theater ratio is low-moderate (0.28) — most administrative activity is substantively disbursement and eligibility verification, not performance, though the observer/advocacy dimension around 'dignity' framing carries some performative weight in this reading's telling.
 *
 * DIRECTIONALITY LOGIC:
 *   Labor_market_exiters sit near the beneficiary end of directionality: they receive the transfer, incur no repayment obligation, and can choose to remain out of the labor force. Working_taxpayers sit toward the target end: they fund the transfer with no direct return and cannot easily exit the tax obligation. Skill_atrophying_recipients occupy an unusual dual position — beneficiary in the immediate term (they receive income), but functionally shifted toward victim in the generational time horizon as their re-entry capacity degrades; this is why they carry both roles on one seat rather than being split into two stakeholders.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (poverty floor) has not disappeared — the founding problem is contested rather than dead, since poverty and volatility risk persist. What this reading identifies as mandatrophy-adjacent is not the disappearance of the founding problem but a mismatch between the original design (temporary bridge) and its unconditional persistence (permanent substitute for wages) — the tangled_rope classification captures that the coordination function and the extractive drift coexist rather than one having fully displaced the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity_magnitude,
    'How large is the actual labor-supply response to unconditional transfers — do recipients meaningfully reduce work hours or exit entirely, or do transfer levels sit too low to change behavior at the margin?',
    'Randomized or quasi-experimental transfer trials (e.g. negative income tax experiments, basic income pilots) measuring hours worked and labor-force participation pre/post, with sufficient follow-up window to detect skill-atrophy effects.',
    'A large, robust elasticity effect would support this reading''s tangled_rope classification and its extractiveness trajectory; a near-zero effect would undermine the beneficiary/victim asymmetry this reading depends on and would favor the freedom_floor reading''s account of the same data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_magnitude, empirical, 'Whether unconditional transfers actually reduce labor supply at meaningful magnitude.').

omega_variable(
    reading_selection_is_contested_not_resolved,
    'Is the dependency-trap framing the structurally correct reading of this kernel, or is it one contested interpretation among three live readings (dependency_trap, freedom_floor, targeting_efficiency) with no framework-external adjudicator?',
    'No single resolution mechanism exists at the level of this story — the three readings are held by different political and academic coalitions and the disagreement is partly empirical (labor supply effects), partly normative (value of non-market time), and partly institutional-design (universal vs targeted). This omega documents that this story deliberately does not adjudicate between readings, per Rule 1 of the committer frame.',
    'If the empirical elasticity omega above resolves toward near-zero labor-supply response, this reading''s structural claim weakens substantially relative to freedom_floor_reading, even though both readings would continue to coexist as normative positions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_is_contested_not_resolved, conceptual, 'This story instantiates one reading of a three-reading kernel; the kernel-level dispute is not resolved here.').

omega_variable(
    skill_atrophy_causal_attribution,
    'When recipients who exit the labor market show reduced re-employment rates and lower wages upon re-entry, is this caused by skill atrophy from non-work, or by pre-existing characteristics that made both transfer-reliance and weak labor-market attachment likely (selection effect)?',
    'Panel data with instrumental variation in transfer eligibility, comparing skill and wage trajectories of marginal recipients versus marginal non-recipients with similar baseline characteristics.',
    'If atrophy is causal, the victim classification of skill_atrophying_recipients is structurally sound; if it is selection, the same population''s inclusion as victims is a measurement artifact and the extractiveness score should be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skill_atrophy_causal_attribution, empirical, 'Whether skill atrophy among recipients is caused by the transfer or by pre-existing selection into non-work.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__dependency_trap_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__dependency_trap_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__dependency_trap_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__dependency_trap_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__dependency_trap_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(inco_be_t4, income_support_commitment__dependency_trap_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__dependency_trap_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__dependency_trap_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__dependency_trap_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__dependency_trap_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(inco_su_t4, income_support_commitment__dependency_trap_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(inco_su_t8, income_support_commitment__dependency_trap_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(inco_su_t12, income_support_commitment__dependency_trap_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(inco_su_t16, income_support_commitment__dependency_trap_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__dependency_trap_reading, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_commitment kernel. dependency_trap_reading and freedom_floor_reading share identical structural facts (the same unconditional transfer mechanism) but assign opposite valence to labor-market exit — this is the clearest case in the corpus of two constraints with different ε values arising from the same underlying policy instrument read through different normative and empirical lenses. targeting_efficiency_reading is structurally distinct (a universal-vs-targeted design axis rather than a valence dispute) but shares the same funding-transfer mechanics and is linked because policy debates typically invoke all three readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, powerless, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
