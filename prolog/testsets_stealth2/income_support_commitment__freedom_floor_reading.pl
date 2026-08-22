% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Floor — Autonomy and Exit-Capacity Reading
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   A national unconditional income floor: every resident household receives
 *   a regular payment regardless of earnings, assets, or household
 *   composition, financed by progressive taxation administered through the
 *   existing tax authority. This story instantiates the freedom_floor_reading
 *   of the income_support_commitment kernel — the arrangement as an enabler
 *   of autonomy, dignity, and labor-market exit capacity. Per the
 *   epsilon-referent rule, epsilon is authored for the standing arrangement
 *   (the floor and its funding schedule) assessed by this reading's own
 *   lights: universality means every seat holds the entitlement, net
 *   contributors carry the option value of future receipt, and no means test
 *   generates stigma, surveillance, or exclusion victims. The sibling
 *   readings (dependency_trap_reading, targeting_efficiency_reading) are
 *   separate constraint stories with their own epsilon values and
 *   beneficiary/victim surfaces; they are linked as a constraint family
 *   through network.affects_constraints and are not described or averaged
 *   inside this file. Claim and metrics are authored independently: the
 *   claimed type is what this reading takes to be structurally true, and the
 *   metrics describe the arrangement's observed operation from this seat.
 *
 * KEY AGENTS:
 *   - - caregivers: primary beneficiary (moderate/constrained) — holds income of their own while caring
 *   - - precarious_workers: primary beneficiary (moderate/constrained) — refusal capacity funded by the base
 *   - - abuse_survivors: primary beneficiary (powerless/trapped) — the income half of exit
 *   - - artists_entrepreneurs: primary beneficiary (moderate/mobile) — failure insurance and working capital
 *   - - net_taxpayers: funding seat, dual-positioned (organized/constrained) — pays the bill, holds the option value
 *   - - high_wealth_individuals: funding seat (powerful/arbitrage) — steepest rates, mobile assets
 *   - - means_test_administrators: displaced-function seat (organized/identity_locked) — careers built on removed assessments
 *   - - low_wage_sector_employers: dual-positioned counterparty (institutional/mobile) — higher wage offers, stabilized demand
 *   - - national_legislature_treasury: agenda setter (institutional/constrained) — sets amount, rules, and tax schedule
 *   - - welfare_state_researchers: analytical observer — produces the evidence all seats cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.22).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.18).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Floor — Autonomy and Exit-Capacity Reading").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(income_support_commitment__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '644062f5-c944-4904-8519-12f435af7bb6').
narrative_ontology:cs_kernel_codification('644062f5-c944-4904-8519-12f435af7bb6', formalized).
narrative_ontology:cs_authority_grounding('644062f5-c944-4904-8519-12f435af7bb6', distributed).
narrative_ontology:cs_reading_relation('644062f5-c944-4904-8519-12f435af7bb6', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('644062f5-c944-4904-8519-12f435af7bb6', income_support_commitment__targeting_efficiency_reading, forecloses).
narrative_ontology:cs_axiom('644062f5-c944-4904-8519-12f435af7bb6', foundational, unconditional_floor_constitutive_of_dignity).
narrative_ontology:cs_axiom_status(unconditional_floor_constitutive_of_dignity, holdable).
narrative_ontology:cs_axiom_grounding('644062f5-c944-4904-8519-12f435af7bb6', unconditional_floor_constitutive_of_dignity, deontological).
narrative_ontology:cs_axiom('644062f5-c944-4904-8519-12f435af7bb6', foundational, exit_capacity_requires_unconditionality).
narrative_ontology:cs_axiom_status(exit_capacity_requires_unconditionality, holdable).
narrative_ontology:cs_axiom_grounding('644062f5-c944-4904-8519-12f435af7bb6', exit_capacity_requires_unconditionality, instrumental).
narrative_ontology:cs_reference_frame('644062f5-c944-4904-8519-12f435af7bb6', universal_autonomy_guarantee).
narrative_ontology:cs_drift_state('644062f5-c944-4904-8519-12f435af7bb6', contemporary_pilot_evidence_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('644062f5-c944-4904-8519-12f435af7bb6', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, net_taxpayers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, low_wage_sector_employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, net_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, high_wealth_individuals).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, means_test_administrators).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, low_wage_sector_employers).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, autonomy_floor_thesis).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, monopsony_exit_capacity_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the floor's amount, the residency rules, and the tax schedule that funds it, and operates the payment rail through the tax authority. Collects the funding and passes it through as universal payments, retaining only administration. Its room to adjust is bounded by electoral cycles and debt-market confidence; abandoning the commitment would mean dismantling a program its own machinery runs.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, national_legislature_treasury, agenda_setter,
    institutional, generational, constrained, national).

% Provide unpaid care for children, elders, or disabled relatives and receive the floor as personal income unrelated to caregiving hours. Previously their consumption depended on a partner's earnings or on allowances requiring proof of need; they now hold money of their own. Their constrained position is caregiving itself — the floor removes the need to abandon caregiving in order to eat.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Cycle through temporary contracts, gig platforms, and seasonal work. The payment arrives regardless of hours worked, so declining a dangerous or underpaid assignment no longer means missing rent. They still pay consumption and payroll taxes on earned income, so their net position varies by earnings year.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).

% Live with partners or relatives who control access to money. An unconditional payment in their own name is capital a controlling partner cannot condition, monitor through caseworker channels, or revoke as punishment. Their practical exit also depends on housing and shelter capacity outside this arrangement; the floor supplies the income half of leaving.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, trapped, national).

% Work in markets with long unpaid ramp-ups and high failure variance. The floor functions as working capital and failure insurance: studio time, prototypes, and first ventures are financed from the guaranteed base, and a failed venture returns them to the floor rather than to destitution.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Pay more in funding taxes than they receive back in a typical year while holding the identical unconditional entitlement as everyone else. Their stake bundles the tax bill, the option value of the floor in any future year, and reduced crisis-driven social spending. At ordinary incomes, exiting the tax system is not available to them.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, net_taxpayers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, net_taxpayers, beneficiary).

% Bear the steepest marginal rates under the progressive funding schedule. Their asset mobility lets them shift residence, incorporation, and realization timing across jurisdictions, so their effective contribution depends on enforcement reach they can partially outrun.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, high_wealth_individuals, payer,
    powerful, generational, arbitrage, global).

% Staff the eligibility-assessment offices, fraud units, and casework hierarchies of the prior targeted system. Universality removes the assessments their careers were built on; redeployment into payment operations exists but at lower status and headcount. Their professional identity centers on needs-assessment expertise, which makes the transition feel like demotion rather than rotation.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, means_test_administrators, payer,
    organized, biographical, identity_locked, national).

% Hire from a labor pool whose outside option the floor raised. They face somewhat higher wage offers to fill unattractive shifts and enjoy lower turnover among retained staff, while consumer demand in their markets is propped up by the floor's spending. Relocating production abroad remains open to many of them.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, low_wage_sector_employers, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__freedom_floor_reading, low_wage_sector_employers, payer).

% Study take-up, labor supply, health, and exit outcomes across pilots and rollouts. They publish the evidence every seat cites and hold no material stake in the payment flow.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_state_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides every resident an unconditional income floor, solving problems no individual can solve alone: insuring against labor-market precarity and life-course interruption, financing the capacity to refuse coercive work or relationships, and replacing fragmented means-tested administration with a single predictable payment rail.
% TRANSFER_FUNCTION: Moves purchasing power from net contributors — progressively weighted, with the steepest rates on high-wealth holders — to all resident households equally; indirectly moves bargaining leverage from employers to workers by changing the outside option, with no direct employer transfer.
% ABSENT_VOICES: Residents excluded by residency and citizenship criteria — undocumented residents and newly arrived migrants — would object that the floor's protection stops at the boundary; future generations would object if funding were debt-shifted rather than taxed currently. Neither seat is inside the funding coalition's conversation.
% DISAPPEARANCE_RATIONALE: Overnight removal breaks the household budgets of every recipient seat, erases refusal capacity (bad assignments re-taken, abuse exits closed for lack of exit capital), halts caregiver-retained and early-stage venture activity the floor finances, and re-expands means-testing bureaucracy — the labor market and family structures reorganize around restored income dependence.
% FOUNDING_PROBLEM: Post-industrial abundance coexisting with destitution and coerced choice: wages set under employer-dominated conditions, caregiving and creative work unremunerated, abusive relationships sustained by financial control, and means-tested relief that stigmatizes, surveils, and systematically misses eligible takers.
% FOUNDING_PROBLEM_CORROBORATION: National statistical offices' persistent-poverty and insecure-work series, labor-economic research documenting employer wage-setting power, and domestic-violence service data identifying financial control as the leading reported barrier to leaving — all sources outside the recipient advocacy set. No corroborating source attests the founding problem is solved.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low (0.22) because the transfer is the product, not overhead: the funding cost falls on contributors who simultaneously hold the universal entitlement, and the reading counts no seat as victimized by the arrangement. Suppression is low (0.18) and reflects only the compulsory character of taxation — the arrangement suppresses no alternative and in fact manufactures exit options, which is the opposite of the suppression signature. Theater is low (0.12): the overwhelming share of activity is the payment itself, with a thin reporting layer accumulating as programs mature. Accessibility collapse is low-moderate (0.30) because alternatives — means-tested programs, charitable provision, family transfers — persist visibly alongside the floor rather than collapsing once it is understood. Resistance is moderate (0.45): contributor coalitions and work-ethic traditions mount real fiscal-political friction, which is descriptive of the arrangement's reception and does not by itself imply victimhood. The measurement series run on one shared time grid (t=0..30, step 5) with both tracked metrics authored at every point; the mild upward drift in both series reflects fiscal scaling and reporting-layer accumulation, not proxy-goal takeover. Receipt surface: gain_flow is authored as 'diffuse' affirmatively — the transfer distributes across the entire recipient population and the treasury is a conduit, not a capturer; no named seat concentrates the gains. Fixing (removing) the floor is prohibitive: the fiscal saving is small against the reorganization cost of restored income dependence across every beneficiary seat.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute very differently from one another despite sharing a role. Net taxpayers sit near symmetric: they fund the floor and hold its option value, so their effective extraction stays modest. High-wealth individuals sit nearer the full-target end on paper but their arbitrage-grade exit damps what the arrangement can actually take from them. Means-test administrators bear a real cost — the displacement of their function — amplified by identity lock: their professional self-concept is needs-assessment expertise, so the same budget line that liberates recipients reads as expropriation from their seat. Beneficiary seats compute subsidy-side throughout, with abuse survivors showing the largest benefit-to-power ratio. The engine derives these divergent classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map directly onto the four recipient seats, driving their d toward the beneficiary end: caregivers and precarious workers are constrained beneficiaries (subsidy they cannot arbitrage away), abuse survivors are trapped beneficiaries (maximum subsidy effect per unit of power), artists and entrepreneurs are mobile beneficiaries (the floor converts their exit option into venture capacity). Net taxpayers derive near-symmetric d from their dual payer/beneficiary position. High-wealth individuals derive high d from the payer role, damped by arbitrage exit. Means-test administrators derive high d from bearing the arrangement's displacement cost, modulated by identity_locked exit. Employers derive mildly-above-symmetric d: genuine demand-stabilization benefit against curtailed wage-setting latitude. No directionality overrides are authored — the derivation chain from declared positions and exit options captures every seat without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — destitution and coerced choice amid abundance — is live and chronically regenerated by labor-market and family structures, so the arrangement carries no sunset clause and no mandatrophy declaration: its mandate has not outlived its function. The classification discipline cuts both ways here. Against the snare misread: a compulsory transfer with an enforcement apparatus looks superficially extractive, but the coordination function (insurance, exit finance, administrative simplification) is the point of the exercise, and naming the beneficiary seats blocks the pure-extraction verdict. Against the rope complacency: the fiscal_trajectory_extraction_threshold omega tracks the real drift risk — if demographic pressure forces funding rates onto a narrowing contributor base, the same structure honestly recomputes as tangled_rope (coordination carrying asymmetric extraction), and this story's metrics would need re-authoring rather than its claim defended. The piton failure mode is distant while take-up and delivery remain functional (theater_ratio 0.12), but the displaced-administrator seat is where theatrical maintenance would first appear if the program were ever wound down without abolishing its reporting shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the unconditional floor''s dominant structural effect autonomy-enablement (this reading''s premise) or dependence-cultivation (the dependency_trap_reading''s premise)? This story instantiates only the freedom_floor_reading of the income_support_commitment kernel; the sibling reading is a separate constraint with its own epsilon and victim structure.',
    'Mature rollout natural experiments measuring labor-supply response, skill formation trajectories, and documented exit events (job refusal, relationship exit, venture formation) over multi-year horizons.',
    'If the sibling reading proves structurally correct, epsilon rises sharply, a fiscally-coerced contributor victim set appears, and the computed classification drifts from rope toward snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Whether this reading or its dependency-trap sibling describes the arrangement''s real effect structure.').

omega_variable(
    universality_constitutive_question,
    'Is universality constitutive of this constraint, or a replaceable funding parameter? The targeting_efficiency_reading holds that support should concentrate on demonstrated need — a structurally different constraint with a different beneficiary/victim surface.',
    'Comparative analysis of universal versus means-tested systems on stigma incidence, take-up completeness, and exit-capacity outcomes, holding funding constant.',
    'If targeting supersedes universality, this story decomposes into a distinct constraint with higher epsilon and a new victim set (near-miss households excluded by tests); the foreclosure edge declared toward the targeting sibling activates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_constitutive_question, conceptual, 'Whether universality is the constraint''s defining structure or an incidental design choice.').

omega_variable(
    fiscal_trajectory_extraction_threshold,
    'At what funding-burden distribution does the taxation side stop functioning as coordination cost and begin registering as extraction on contributor seats?',
    'Actuarial modeling of required contribution rates under demographic aging against feasible base-broadening, tracked against contributor-seat burden distributions.',
    'Crossing the threshold pushes taxpayer-seat effective extraction upward and could shift the computed classification toward tangled_rope even within this reading''s own assessment — genuine coordination carrying asymmetric funding extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_trajectory_extraction_threshold, empirical, 'Where the funding side crosses from coordination cost to extraction.').

omega_variable(
    employer_offset_dynamics,
    'Does the floor''s constraint on employer wage-setting translate into durable worker gains, or do employers offset it through automation, offshoring, and scheduling control?',
    'Staggered-rollout wage, vacancy, and hours studies isolating employer-side responses from worker-side refusal behavior.',
    'Full offsetting converts exit capacity into unemployment exposure for the worker seats, weakening the autonomy mechanism this reading is built on and raising effective extraction experienced at the bottom of the labor market.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_offset_dynamics, empirical, 'Whether the exit-capacity mechanism survives employer-side adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__freedom_floor_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__freedom_floor_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(inco_tr_t25, income_support_commitment__freedom_floor_reading, theater_ratio, 25, 0.11).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__freedom_floor_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__freedom_floor_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__freedom_floor_reading, base_extractiveness, 15, 0.19).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(inco_be_t25, income_support_commitment__freedom_floor_reading, base_extractiveness, 25, 0.21).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__freedom_floor_reading, base_extractiveness, 30, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' covers three structurally distinct claims about one standing arrangement. This story (freedom_floor_reading) authors epsilon for the arrangement as an autonomy-and-exit-capacity enabler — low extraction, no victim set, universality constitutive. income_support_commitment__dependency_trap_reading authors epsilon for the same arrangement as a dependence-cultivating work disincentive — high extraction with a fiscally-coerced contributor victim set. income_support_commitment__targeting_efficiency_reading authors epsilon for the needs-concentrated alternative arrangement — different beneficiary/victim surface entirely (near-miss excluded households appear as victims of the tests). The upstream/downstream structure runs from pilot evidence: this reading's rollout results are cited as evidence by both siblings' debates, so this story links to both. Each file documents the decomposition; no single file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
