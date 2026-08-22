% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap (Welfare-Trap Reading)
 *   domain: political economy/social policy/labor economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the income_support_conditionality
 *   kernel: the dependency-trap reading, under which unconditional income
 *   support operates as a welfare trap. On this reading the standing
 *   arrangement — a subsistence guarantee decoupled from work — taxes the
 *   productive to finance transfers whose marginal terms reward non-work,
 *   binding recipients into long-duration dependency and skill atrophy while
 *   a bureaucratic-political complex grows around the caseload. Per the
 *   epsilon-referent rule for kernel readings, extractiveness is authored for
 *   the standing unconditional-support arrangement as THIS reading assesses
 *   it — not for the conditional alternative this reading would put in its
 *   place. The sibling readings (freedom_floor_reading, wage_subsidy_reading)
 *   are separate constraints in separate files, linked via
 *   network.affects_constraints; the contest between readings is carried in
 *   omega variables, not inside this constraint's classification. The claimed
 *   type (snare) and the metrics are authored independently: the claim states
 *   what this reading holds to be structurally true, the metrics state what
 *   it holds to be descriptively true of the arrangement's operation.
 *
 * KEY AGENTS:
 *   - working_age_transfer_recipients: primary target (moderate/trapped) — receives the transfer and, per this reading, bears the dependency and skill-atrophy cost
 *   - taxpayers: primary funder and target (moderate/constrained) — finances the transfers through compulsory taxation with no per-program exit
 *   - public_assistance_bureaucracies: administrator-beneficiary (institutional/identity_locked) — runs intake, compliance, and disbursement; budgets and careers scale with caseload
 *   - patronage_politicians: agenda-setter-beneficiary (powerful/mobile) — design and defend the arrangement; harvest durable electoral support from dependent constituencies
 *   - future_taxpayers: excluded voice (powerless/trapped) — inherits the fiscal commitment with no seat in current budgeting
 *   - independent_policy_evaluators: analytical observer (moderate/analytical) — runs pilots and longitudinal studies; sees the full structure, depends on the administrators for data access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.7).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.62).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap (Welfare-Trap Reading)").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political economy/social policy/labor economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '5371c92e-35ad-479b-a348-39036b786695').
narrative_ontology:cs_kernel_codification('5371c92e-35ad-479b-a348-39036b786695', distributed).
narrative_ontology:cs_authority_grounding('5371c92e-35ad-479b-a348-39036b786695', expertise).
narrative_ontology:cs_interpretation_layer_present('5371c92e-35ad-479b-a348-39036b786695').
narrative_ontology:cs_reading_relation('5371c92e-35ad-479b-a348-39036b786695', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('5371c92e-35ad-479b-a348-39036b786695', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('5371c92e-35ad-479b-a348-39036b786695', foundational, contribution_obligation_grounds_entitlement).
narrative_ontology:cs_axiom_status(contribution_obligation_grounds_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('5371c92e-35ad-479b-a348-39036b786695', contribution_obligation_grounds_entitlement, deontological).
narrative_ontology:cs_axiom('5371c92e-35ad-479b-a348-39036b786695', foundational, unconditional_transfers_induce_skill_atrophy).
narrative_ontology:cs_axiom_status(unconditional_transfers_induce_skill_atrophy, holdable).
narrative_ontology:cs_axiom_grounding('5371c92e-35ad-479b-a348-39036b786695', unconditional_transfers_induce_skill_atrophy, empirically_contingent).
narrative_ontology:cs_reference_frame('5371c92e-35ad-479b-a348-39036b786695', reciprocal_obligation_baseline).
narrative_ontology:cs_drift_state('5371c92e-35ad-479b-a348-39036b786695', contemporary_post_pilot_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('5371c92e-35ad-479b-a348-39036b786695', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, public_assistance_bureaucracies).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, patronage_politicians).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, working_age_transfer_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, working_age_transfer_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a subsistence-level payment independent of employment status. The cash flow is real and covers rent and food, but re-entering steady work means losing the benefit at a high effective marginal rate, explaining resume gaps to employers, and rebuilding routines and credentials after years out of the labor market. Each year outside work raises the cost of the next job search. Leaving the arrangement entirely means subsistence risk; staying means the gap widens.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, working_age_transfer_recipients, beneficiary,
    moderate, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, working_age_transfer_recipients, payer).

% Finance the transfers through compulsory taxation on earnings and consumption. They cannot decline individual programs, and relocating to a lower-tax jurisdiction means leaving jobs, family networks, and citizenship. Their influence runs through occasional elections and taxpayer advocacy, exercised against a budget line that grows automatically with caseloads and demographic aging.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Run intake, eligibility determination, compliance monitoring, and disbursement. Staffing, budgets, and career ladders scale with caseload and program count. Several professional generations have spent entire careers inside these agencies; the agencies' procedures, vocabularies, and self-justification are built around administering the caseload, and their senior staff cannot describe the agency apart from that function.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, public_assistance_bureaucracies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, public_assistance_bureaucracies, beneficiary).

% Design, expand, and defend the transfer architecture in legislation. Expansion builds grateful constituencies and clientel relationships that persist across election cycles; retrenchment mobilizes the same constituencies against the proposer. Individual politicians can and do pivot to other portfolios, but the party apparatus as a whole treats the recipient bloc as a durable electoral asset.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, patronage_politicians, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, patronage_politicians, beneficiary).

% Will service the accumulated fiscal commitments and absorb any long-run labor-supply contraction the arrangement induces. They are not yet present in budgeting processes, cannot vote on the programs that bind them, and have no procedural vehicle by which to register objection before the obligations attach.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, future_taxpayers, excluded,
    powerless, civilizational, trapped, national).

% Academic economists and evaluation institutes run pilot programs, administrative-data studies, and cross-country comparisons of transfer designs. They collect no transfer and pay no program tax; their stake is reputational and intellectual, and their access to administrative data depends on cooperation from the very bureaucracies they evaluate.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, independent_policy_evaluators, observer,
    moderate, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, working_age_transfer_recipients).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools income risk across the population: guarantees a subsistence floor independent of employment status, eliminates means-testing stigma and screening overhead, and stabilizes household consumption and aggregate demand during unemployment spells.
% TRANSFER_FUNCTION: Moves purchasing power from the broad taxpayer base to working-age residents irrespective of work status; on this reading it simultaneously moves labor supply out of the market by paying for non-work at the margin, and moves discretion over recipients' time allocation from recipients to the benefit schedule's incentive structure.
% ABSENT_VOICES: Future taxpayers inherit the fiscal commitment with no seat in current budgeting. Net-contributor workers who receive nothing back enter the conversation only as revenue sources. Former recipients who exited and rebuilt careers — whose trajectories complicate the permanence of the trap — are rarely called in either expansion or reform hearings, because their testimony inconveniences both the defending and the attacking coalitions.
% DISAPPEARANCE_RATIONALE: Millions of households lose their subsistence floor overnight; food and housing markets in high-caseload regions convulse; low-wage labor supply surges faster than vacancies open; the administrative apparatus and its political clientele lose their organizing purpose; charitable and municipal structures cannot absorb the scale on any short timeline. The political coalitions built around the caseload would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Destitution among those unable to find paid work — unemployment, illness, disability, old age before pensions matured — administered under poor-law regimes that were discretionary, humiliating, and sometimes lethal; later, the macroeconomic problem of demand collapse in downturns.
% FOUNDING_PROBLEM_CORROBORATION: National statistical agencies' long-run poverty and labor-force-participation series — compiled outside the benefiting parties — corroborate that the material deprivation the arrangement targeted has fallen sharply while long-duration benefit receipt has grown in step. Historians of the poor law corroborate the founding destitution problem from archival record. No source outside the beneficiary set attests that the arrangement currently solves more deprivation than it manufactures; that evidentiary asymmetry is itself signal, and the parties dispute which side of the ledger dominates.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.70 at interval end) because the arrangement compels finance from the entire tax base while, on this reading, degrading the earning capacity of the population it supports — a two-sided claim on non-consenting parties. Suppression (0.62) reflects the arrangement's holding machinery: compulsory taxation with constrained jurisdictional exit on the funding side, and benefit cliffs, effective marginal tax rates, and resume penalties on the recipient side. Theater (0.45) is moderate and rising: alleviation rhetoric and program activity increasingly detach from measured exit outcomes as caseload maintenance becomes the operational goal. Accessibility_collapse (0.55) is mid-range — alternative designs (work-conditioned credits, means-tested aid, wage subsidies) remain politically imaginable but each faces organized defense of the incumbent architecture. Resistance (0.60) is sustained: recurring reform movements, taxpayer mobilization, and pilot-driven criticism. The temporal series run on one shared grid (T=0..50, decade steps mapped to the post-war transfer-state maturation era) and show a ratchet rather than a cycle: programs added in downturns are rarely removed in expansions, so extractiveness, enforcement machinery, and theatrical share all climb monotonically. Suppression_requirement is tracked because the story specifically traces enforcement-capacity growth — withholding systems, cross-matched eligibility databases, compliance staffing — not merely shifting extraction. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope, engine-side.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the bureaucratic and political seats the arrangement is a functioning, democratically mandated institution they staff and defend; from the taxpayer seat it is compelled finance with no per-program consent; from the recipient seat it splits into a real gross benefit (income without work conditions) and a real net cost (cliff effects, atrophy, narrowing options) — the reading's claim is that the net term dominates. Recipients and taxpayers sit at the same nominal power level (citizens and voters) yet experience opposite directionalities, differentiated by exit options and time horizon: the recipient's immediate horizon makes the certain payment dominate the deferred atrophy cost, which is precisely the mechanism the trap thesis identifies. Coalition potential between the two victim classes is structurally blocked: the taxpayer's interest in shrinking the transfer and the recipient's interest in a livable floor diverge at the first policy proposal, and the recipient class is fragmented by the immediacy of its needs — the arrangement does not need to suppress the coalition because its incentive structure dissolves it.
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients are declared victims despite receiving cash: on this reading the payment is the bait and the exit cost is the hook, so the net structural relationship is extraction — their trapped exit pushes derived d toward the full-target end, with the gross receipt noted in their situation text rather than in the structural declaration. Taxpayers are victims with constrained exit (jurisdictional lock, limited arbitrage), placing them high on the target side but below the trapped recipients. Public assistance bureaucracies are beneficiaries whose identity_locked exit fuses the organization with its function — their derived d sits near the beneficiary end, damping or inverting effective extraction for that seat. Patronage politicians are beneficiaries with mobile exit — the lowest d in the story, since they can abandon the portfolio while the asset persists. Spatial scope is national throughout the domestic seats, which engine-side amplifies verification difficulty and hence effective extraction on the target seats. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already differentiate every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this arrangement as a snare keeps its genuine coordination function (risk pooling, demand stabilization) visible in the account while flagging the asymmetric extraction riding on it — preventing both the rope mislabel (the freedom-floor framing, which reads the arrangement as pure liberation) and the piton mislabel (reading its persistence as mere inertial neglect). The persistence here is defended, not neglected: budgets grow with caseloads and the electoral asset is actively maintained, which is why the snare rather than the piton is the honest claim. The R5 interview returns founding_problem_status=contested against disappearance_verdict=world_rearranges — no dead-mandate/zombie mismatch fires, because the founding problem (destitution) plausibly persists even as the parties dispute whether the arrangement now manufactures the condition it treats. mandatrophy_resolved is therefore not declared: the mandate is disputed, not outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_within_kernel,
    'Is the dependency-trap reading the correct instantiation of the income_support_conditionality kernel, or do the freedom-floor or wage-subsidy readings better characterize the same arrangement?',
    'Decade-scale longitudinal evidence on recipient labor supply, skill accumulation, and exit rates, combined with explicit normative adjudication of the reciprocity premise; the sibling stories carry the alternative instantiations and the corpus compares them as a family.',
    'If the freedom-floor reading prevails, the victim set dissolves and the arrangement computes toward rope; if the wage-subsidy reading prevails, the victim set shifts to non-recipient workers and the extraction seat changes identity entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_within_kernel, conceptual, 'Which reading of the income-support kernel this constraint instantiates.').

omega_variable(
    long_run_atrophy_evidence_gap,
    'Does unconditional income support cause durable skill atrophy and dependency lock-in over horizons longer than the 1-3 year pilot window, or do the short-run labor-supply nulls from pilots extend indefinitely?',
    'Longitudinal cohort tracking of long-duration recipients across a major policy change; natural experiments from regional benefit discontinuities followed ten or more years.',
    'Confirmed atrophy hardens the snare classification and validates the victim declarations; durable nulls collapse this reading''s core mechanism and push the story toward the freedom-floor sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(long_run_atrophy_evidence_gap, empirical, 'Whether the trap mechanism operates at decade scale or is an artifact of short observation windows.').

omega_variable(
    demand_side_confound,
    'Is persistent recipient status caused by the transfer itself, or by underlying labor-market conditions — weak local demand, childcare costs, discrimination, credential gates — that the transfer merely cushions?',
    'Cross-region comparison of identical transfer schedules against differing local labor-market conditions; dose-response designs exploiting benefit-schedule variation.',
    'If structural labor-market barriers dominate, extraction is misattributed to the transfer; the operative snare sits in the labor market, and this story''s victim declarations need re-seating onto the barrier''s actual beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_side_confound, empirical, 'Whether observed dependency is caused by the transfer or confounded by labor-market structure.').

omega_variable(
    rent_capture_vs_neutral_administration,
    'Do administrative bodies and program-patronage politicians actually capture rents from the arrangement, or do they administer it neutrally at cost?',
    'Budget-growth versus caseload-growth regression; comparative administrative overhead across jurisdictions; career-path analysis of senior program administrators.',
    'Absent a concentrated capturer, persistence reads as inertia rather than defended extraction, drifting the classification toward piton; confirmed capture secures the snare reading and the beneficiary declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_capture_vs_neutral_administration, empirical, 'Whether the beneficiary seats capture rents or administer neutrally.').

omega_variable(
    structural_vs_internalized_dependency,
    'Is the trap''s holding power structural (benefit cliffs, effective marginal tax rates, resume penalties) or internalized (habituation, eroded worker self-concept, learned helplessness that persists after the schedule changes)?',
    'Post-exit trajectory study: if recipients who lose eligibility for exogenous reasons return to stable work at rates indistinguishable from matched non-recipients, the barrier was structural; if they relapse disproportionately, an internalized component carries.',
    'Internalized dependency raises effective suppression above the structural measure and survives formal schedule reform — reclassification pressure toward a deeper snare; a purely structural trap yields to schedule redesign alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_dependency, empirical, 'Structural versus internalized mechanism of the dependency trap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__dependency_trap_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(inco_tr_t30, observed).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(inco_tr_t40, observed).
narrative_ontology:measurement(inco_tr_t50, income_support_conditionality__dependency_trap_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement_basis(inco_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__dependency_trap_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(inco_be_t30, observed).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(inco_be_t40, observed).
narrative_ontology:measurement(inco_be_t50, income_support_conditionality__dependency_trap_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement_basis(inco_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__dependency_trap_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement_basis(inco_su_t30, observed).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__dependency_trap_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(inco_su_t40, observed).
narrative_ontology:measurement(inco_su_t50, income_support_conditionality__dependency_trap_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(inco_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' covers three structurally distinct claims about the same policy instrument, decomposed per the epsilon-invariance principle. This story authors the dependency-trap claim (high extraction, recipients and taxpayers as victims, snare). The freedom_floor_reading authors the decommodification claim (negligible extraction from its seat, recipients as beneficiaries, rope-flavored). The wage_subsidy_reading authors the employer-subsidy claim (extraction redirected onto non-recipient workers, tangled-rope-flavored). The upstream empirical question — what unconditionality does to labor supply at long horizons — feeds all three; each downstream reading cites overlapping pilot evidence for structurally different conclusions. Family members are linked via affects_constraints so contamination and evidence-sharing propagate across the triad.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
