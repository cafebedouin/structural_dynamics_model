% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling — Coordination Scaffold Reading
 *   domain: constitutional/political economy/fiscal governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   statutory_debt_ceiling kernel: the coordination_scaffold_reading, under
 *   which the debt ceiling is a procedural coordination mechanism letting
 *   Congress delegate the mechanics of continuous federal borrowing to
 *   Treasury while retaining a single aggregate decision point. On this
 *   reading the arrangement solves the collective-action problem that made
 *   per-issuance approval unworkable after 1917, adjustments are ordinarily
 *   routine, no group is systematically targeted, and Treasury operates with
 *   substantial autonomy inside the cap. The reading treats the ceiling as
 *   transitional support — a way-station between micromanagement and a
 *   cleaner delegation such as automatic adjustment keyed to enacted budgets
 *   — not as a steady-state ideal and not as an extraction engine. Episodic
 *   brinkmanship (1995, 2011, 2013, 2023) enters the record as exogenous
 *   attack on the mechanism, not its designed function; the metric series
 *   preserve those spikes rather than smoothing them away. Sibling readings
 *   are separate constraint files linked through network.affects_constraints;
 *   nothing about them is averaged into this file. KEY AGENTS (by structural
 *   relationship): - congressional_fiscal_leadership: Agenda-setting
 *   principal (institutional/mobile) — sets and adjusts the aggregate limit;
 *   gains relief from per-issuance micromanagement -
 *   us_treasury_debt_management: Operating beneficiary
 *   (institutional/constrained) — runs issuance and refinancing inside the
 *   cap - treasury_securities_holders: Beneficiary with arbitrage exit
 *   (organized/global) — collects predictability, can leave instantly -
 *   federally_funded_program_agencies: Dual-positioned beneficiary/payer
 *   (organized/constrained) — gains seamless disbursement, absorbs episodic
 *   cash uncertainty - automatic_adjustment_reform_advocates: Excluded voice
 *   (moderate/constrained) — proposes replacement mechanisms with no
 *   procedural path - fiscal_governance_analysts: Analytical observer
 *   (analytical/analytical) — CRS/GAO/academic record-keepers
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.24).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling — Coordination Scaffold Reading").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional/political economy/fiscal governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, 'bf3b3006-0372-4d74-b55c-feb606565bab').
narrative_ontology:cs_kernel_codification('bf3b3006-0372-4d74-b55c-feb606565bab', fixed_text).
narrative_ontology:cs_authority_grounding('bf3b3006-0372-4d74-b55c-feb606565bab', lineage).
narrative_ontology:cs_interpretation_layer_present('bf3b3006-0372-4d74-b55c-feb606565bab').
narrative_ontology:cs_reading_relation('bf3b3006-0372-4d74-b55c-feb606565bab', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf3b3006-0372-4d74-b55c-feb606565bab', statutory_debt_ceiling__constitutional_nullity_reading, forecloses).
narrative_ontology:cs_axiom('bf3b3006-0372-4d74-b55c-feb606565bab', foundational, borrowing_mechanics_delegable_by_statute).
narrative_ontology:cs_axiom_status(borrowing_mechanics_delegable_by_statute, holdable).
narrative_ontology:cs_axiom_grounding('bf3b3006-0372-4d74-b55c-feb606565bab', borrowing_mechanics_delegable_by_statute, conventional).
narrative_ontology:cs_axiom('bf3b3006-0372-4d74-b55c-feb606565bab', secondary, aggregate_checkpoint_preserves_prerogative).
narrative_ontology:cs_axiom_status(aggregate_checkpoint_preserves_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('bf3b3006-0372-4d74-b55c-feb606565bab', aggregate_checkpoint_preserves_prerogative, conventional).
narrative_ontology:cs_reference_frame('bf3b3006-0372-4d74-b55c-feb606565bab', routine_delegation_with_aggregate_oversight).
narrative_ontology:cs_drift_state('bf3b3006-0372-4d74-b55c-feb606565bab', contemporary_brinkmanship_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bf3b3006-0372-4d74-b55c-feb606565bab', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congressional_fiscal_leadership).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, us_treasury_debt_management).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_securities_holders).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, federally_funded_program_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__coordination_scaffold_reading, federally_funded_program_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the aggregate limit by statute and adjusts it through ordinary legislation, typically through bipartisan leadership agreement framed as protecting the full faith and credit. Gains relief from per-issuance micromanagement — one periodic decision replaces hundreds of floor votes — and retains a visible checkpoint at which to demonstrate fiscal vigilance. Can restructure or replace the mechanism whenever it coheres internally, which makes its exit from the arrangement effectively free.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congressional_fiscal_leadership, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, congressional_fiscal_leadership, beneficiary).

% Runs auctions, bill and bond issuance, and continuous refinancing inside the statutory cap, deploying extraordinary measures near the limit while awaiting adjustment. Operational autonomy within the aggregate is the arrangement's working product; Treasury cannot exceed the cap regardless of cash-flow arithmetic and cannot decline the mandate, so its position is bounded rather than voluntary.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, us_treasury_debt_management, beneficiary,
    institutional, biographical, constrained, national).

% Hold the benchmark risk-free asset and rely on uninterrupted coupon and principal payments plus a continuous issuance calendar. They collect the arrangement's predictability and can exit instantly by selling into the deepest sovereign bond market in the world, giving them disciplining visibility with no obligation to remain.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_securities_holders, beneficiary,
    organized, biographical, arbitrage, global).

% Deliver appropriated programs financed partly by borrowed funds and depend on Treasury disbursements continuing on schedule. They gain from seamless refinancing but absorb episodic cash-management uncertainty in weeks when the limit binds and payment prioritization is publicly debated — a cost they bear without holding any seat in setting the limit.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, federally_funded_program_agencies, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, federally_funded_program_agencies, payer).

% Policy scholars, former officials, and some legislators who propose replacing the standing limit with automatic adjustments keyed to enacted budgets or a Gephardt-rule-style default. They testify, publish, and draft model statutes but lack a procedural path to a floor vote; they live under the arrangement they seek to replace and cannot opt out of its consequences.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, automatic_adjustment_reform_advocates, excluded,
    moderate, generational, constrained, national).

% Congressional Research Service, Government Accountability Office, and academic fiscal historians who document the mechanism's operation, trace its lineage from the 1917 Act, and score its performance across adjustment episodes. They collect no rents and bear no costs; their seat is the record-keeping vantage from which the arrangement's routine and episodic phases can be distinguished.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, fiscal_governance_analysts, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of continuously refinancing a permanent rolling public debt: instead of Congress approving each issuance's terms individually, a single aggregate limit delegates issuance mechanics to Treasury while preserving one periodic congressional decision point.
% TRANSFER_FUNCTION: Moves decision rights and administrative workload rather than resources: Congress cedes per-issue approval labor to Treasury's operational autonomy within the cap, retaining only the aggregate-limit decision; no systematic wealth transfer between social groups is part of the mechanism's design or normal operation.
% ABSENT_VOICES: Automatic-adjustment reform advocates are heard in testimony but excluded from the operative bargain, with no procedural path to a floor vote. Future cohorts bearing debt-service costs also have no seat — though under this reading the ceiling neither creates nor accelerates the debt they inherit.
% DISAPPEARANCE_RATIONALE: If the ceiling vanished overnight, Treasury would keep auctioning under residual general authority, but Congress would face an immediate institutional vacuum: either per-issuance approval returns and the refinancing calendar seizes, or a replacement delegation (automatic adjustment, commission-triggered raises) must be constructed under deadline pressure. The fiscal apparatus would rearrange around whatever delegation instrument emerged.
% FOUNDING_PROBLEM: Before 1917, Congress authorized each bond issue separately; as federal borrowing became continuous with the Liberty Loans and the modern administrative state, per-issuance approval became administratively impossible and threatened refinancing failures.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal historians and the Congressional Research Service — seats outside the daily beneficiary positions — corroborate that per-issuance approval was abandoned as unworkable and that some delegation framework remains necessary. Policy institutes across the ideological spectrum (Bipartisan Policy Center, Peterson Foundation) attest the underlying need while disputing the ceiling's specific form: corroboration of the problem, contestation of the instrument.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is authored independently of the metrics: the reading's structural thesis is a transitional coordination scaffold, and the metrics are authored as the descriptive record of the standing arrangement under this reading's lights. Extractiveness ends at 0.24 — low but not zero, because the series refuses to smooth away the 1995/2011/2023 friction spikes; under this reading those spikes are exogenous weaponization of the boundary, and preserving them keeps the reading falsifiable. Suppression is 0.12 and static: the ceiling is a self-executing statutory limit with no coercive machinery, so no suppression_requirement series is authored — the scalar carries the flat enforcement picture. Accessibility collapse is 0.40: per-issuance approval is dead as an alternative, but automatic-adjustment replacements remain fully live proposals, so alternatives are only partly collapsed. Resistance is 0.30 — episodic holdouts and brinkmanship votes against a background of routine bipartisan passage. Theater ends at 0.22: raise votes are substantively necessary under this reading, with a ritual residue around milestone votes. All series share one eleven-point grid (1917-2025) so the engine samples every metric at every examined time point. The oscillation is an attack-and-recovery cycle imposed on the mechanism from outside, not intermittent reinforcement by the mechanism itself; under the rival snare reading the same oscillation would be read as the extraction mechanism, which is precisely the structural question the kernel contest turns on. Receipt surface: gain_flow is authored 'diffuse' as an affirmative finding — every named seat was checked and none captures the arrangement's product, which is distributed operational convenience (Treasury's autonomy, Congress's agenda relief, holders' predictability). fixing_cost is 'prohibitive': replacement attempts (the Gephardt rule's lapse, successive automatic-adjustment proposals) have repeatedly stalled against status-quo inertia, and the benefit of replacement is modest on this reading, so cost dominates. That diffuse-plus-prohibitive cell is piton-flavored; the divergence is addressed in mandatrophy_analysis.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the agenda-setter seat the arrangement is a routine instrument its holder controls and can discard at will. From the constrained operator seat (Treasury) it is a hard envelope that binds regardless of cash-flow arithmetic. From the arbitrage seat (holders) it is nearly invisible — pure predictability with instant exit. From the excluded seat (reform advocates) it is an entrenched suboptimal equilibrium kept in place by procedural gates they cannot reach. Same-level differentiation appears inside Congress itself: leadership holds the agenda-setter seat while reform-minded members occupy the excluded seat at identical nominal institutional power, differentiated purely by procedural position. The engine computes these per-seat classifications from the structural data; this file does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: congressional_fiscal_leadership (agenda-setter plus beneficiary, mobile exit) derives near the beneficiary pole; us_treasury_debt_management (pure beneficiary, constrained exit) sits close behind; treasury_securities_holders (beneficiary, arbitrage exit) derives nearest the full-beneficiary end, since arbitrage-grade exit is the strongest damping condition. federally_funded_program_agencies carries a genuine dual position — primary beneficiary of seamless disbursement, secondary bearer of episodic cash-management cost — and should derive intermediate rather than pole values. No victims are declared, which is definitional to this reading: a reading that found systematic victims would be authoring the snare sibling, not this constraint. No directionality overrides are used; the declarations plus exit atoms produce the intended spread without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — per-issuance approval of a now-continuous borrowing program — is live, and the facilitative function is intact, which separates this scaffold from a piton despite occupying the receipt-surface cell (diffuse gains, prohibitive fixing cost) that pitons typically inhabit: the piton test requires an atrophied function maintained theatrically, and this reading's theater ratio (0.22) and live founding problem deny both conditions. The scaffold characterization carries one honest defect: no codified sunset clause exists, so the transition justification rests on sustained reform momentum rather than an expiry date; that gap is routed to the sunset_without_codification omega rather than papered over with a false has_sunset_clause flag. Mandatrophy prevention cuts both ways here: reading the episodic spikes as intrinsic drift would misclassify a functioning delegation as degrading toward extraction, while reading the ceiling as timeless steady-state coordination would miss that its own tradition treats it as transitional. The scaffold claim holds the middle: functional now, justified by the transition it enables, awaiting the replacement its own advocates describe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the statutory_debt_ceiling kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative reading of the three family files'' structural data: the snare sibling declares victims and high epsilon for the same referent; the nullity sibling declares no binding constraint. The disagreement localizes to a single structural element — whether the periodic-adjustment requirement is neutral procedure or a coercive lever.',
    'If the snare reading''s structural data proves correct for the standing arrangement, this file''s epsilon is understated roughly threefold and the computed type migrates toward tangled_rope; if the nullity reading prevails, this constraint ceases to bind at all and its classification becomes moot.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the debt-ceiling kernel; siblings alter victim structure and bindingness.').

omega_variable(
    design_vs_use_attribution,
    'Are the brinkmanship episodes (1995, 2011, 2013, 2023) intrinsic to the ceiling''s structure, or exogenous capture of a benign mechanism?',
    'Natural experiments: state and foreign jurisdictions without aggregate ceilings, and the 1979-1995 Gephardt-rule window when adjustment was automated — if friction tracks the automation rule''s presence rather than the ceiling''s existence, the mechanism is benign and its use hostile.',
    'If intrinsic, epsilon rises and the correct classification shifts toward the snare sibling; if exogenous, this reading stands and the spikes belong to the attackers'' account rather than the mechanism''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_vs_use_attribution, empirical, 'Whether weaponization episodes are properties of the constraint or of its users.').

omega_variable(
    sunset_without_codification,
    'Can the scaffold characterization stand without a codified sunset clause, resting only on the transition justification and sustained reform momentum?',
    'Legislative trajectory of automatic-adjustment proposals (Gephardt-rule revival bills, fiscal-commission recommendations): sustained momentum confirms the transitional reading; stabilization of the ceiling as an accepted permanent fixture with live function converges the classification toward rope.',
    'If the ceiling stabilizes as permanent, the scaffold claim loses its justification-is-the-transition premise and the story should be re-authored as a rope carrying the same metrics; if reform arrives, the scaffold reading is vindicated retrospectively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_without_codification, conceptual, 'Gap between the scaffold type''s sunset expectation and the statute''s lack of any expiry provision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t1917, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t1917, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t1939, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1939, 0.08).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t1939, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t1953, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1953, 0.12).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t1953, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t1971, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1971, 0.15).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t1971, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t1979, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1979, 0.06).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t1979, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t1995, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t1995, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t2005, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t2005, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t2011, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2011, 0.48).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t2011, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t2017, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t2017, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t2023, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2023, 0.44).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t2023, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_tr_t2025, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(sdc_coordination_scaffold_be_t1917, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1917, 0.15).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t1917, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t1939, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1939, 0.14).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t1939, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t1953, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1953, 0.16).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t1953, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t1971, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1971, 0.17).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t1971, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t1979, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1979, 0.12).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t1979, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t1995, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1995, 0.34).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t1995, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t2005, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2005, 0.2).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t2005, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t2011, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2011, 0.45).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t2011, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t2017, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2017, 0.24).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t2017, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t2023, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2023, 0.42).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t2023, observed).
narrative_ontology:measurement(sdc_coordination_scaffold_be_t2025, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2025, 0.24).
narrative_ontology:measurement_basis(sdc_coordination_scaffold_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__coordination_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the debt ceiling' decomposes into three structurally distinct claims about one fixed-text kernel (statutory_debt_ceiling). This file is the coordination_scaffold_reading (low ε, no victims, transitional justification). The extraction_snare_reading authors high ε with declared victims for the same referent; the constitutional_nullity_reading authors no binding constraint at all. Each member links the others via affects_constraints; ε values differ because the readings differ, not because one constraint is measured inconsistently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
