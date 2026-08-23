% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Legitimate Market-Clearing Mechanism (Market-Efficiency Reading)
 *   domain: economic/labor/social_policy
 *
 * SUMMARY:
 *   Platform-mediated flexible employment (ride-hail, delivery, task work) is
 *   the standing arrangement under contest. This file instantiates ONE
 *   reading of the flexible_employment_legitimacy kernel: the
 *   market_efficiency_reading, under which the arrangement is a legitimate
 *   clearing mechanism — observed task prices are equilibrium prices, wage
 *   convergence signals blue-collar scarcity, dispatch algorithms are neutral
 *   matching technology, and worker autonomy is maximized. Per the
 *   epsilon-invariance principle, the sibling readings
 *   (precarity_extraction_reading, developmental_state_reading) are separate
 *   constraint stories with their own epsilon, victim structure, and
 *   classification; they are linked, not averaged, here. The epsilon referent
 *   is fixed: the existing flexible-employment arrangement as it stands,
 *   assessed by this reading's own lights — never the formalized arrangement
 *   a sibling would endorse. KEY AGENTS (by structural relationship): -
 *   platform_operators: Agenda-setter and primary collector
 *   (institutional/arbitrage) — sets take rates, dispatch, and deactivation
 *   policy; collects commission on every matched task -
 *   schedule_autonomy_workers: Net-beneficiary supplier segment
 *   (powerless/mobile) — casual earners who price in their own flexibility
 *   premium - full_time_gig_dependents: Dual-positioned supplier segment
 *   (powerless/constrained) — platform income is primary; bears volatility
 *   and deactivation exposure - demand_side_businesses: Beneficiary-payer
 *   (organized/mobile) — buys elastic capacity without fixed payroll -
 *   on_demand_consumers: Beneficiary-payer (moderate/mobile) — receives lower
 *   prices and round-the-clock availability - displaced_incumbent_drivers:
 *   Cost-bearer (moderate/trapped) — medallion-debt holders on the losing
 *   side of market entry - gig_worker_organizers: Excluded voice
 *   (organized/identity_locked) — outside the frame's venues; contests the
 *   voluntariness premise - labor_regulators: Analytical observer
 *   (institutional/analytical) — adjudicates classification and pay-floor
 *   questions
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.26).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.3).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Legitimate Market-Clearing Mechanism (Market-Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "economic/labor/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, '5e190ca9-53ea-43c1-8106-28ad7263cb2b').
narrative_ontology:cs_kernel_codification('5e190ca9-53ea-43c1-8106-28ad7263cb2b', distributed).
narrative_ontology:cs_authority_grounding('5e190ca9-53ea-43c1-8106-28ad7263cb2b', distributed).
narrative_ontology:cs_reading_relation('5e190ca9-53ea-43c1-8106-28ad7263cb2b', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e190ca9-53ea-43c1-8106-28ad7263cb2b', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('5e190ca9-53ea-43c1-8106-28ad7263cb2b', foundational, observed_task_prices_are_equilibrium_prices).
narrative_ontology:cs_axiom_status(observed_task_prices_are_equilibrium_prices, holdable).
narrative_ontology:cs_axiom_grounding('5e190ca9-53ea-43c1-8106-28ad7263cb2b', observed_task_prices_are_equilibrium_prices, empirically_contingent).
narrative_ontology:cs_axiom('5e190ca9-53ea-43c1-8106-28ad7263cb2b', foundational, revealed_flexibility_preferences_are_welfare_authoritative).
narrative_ontology:cs_axiom_status(revealed_flexibility_preferences_are_welfare_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('5e190ca9-53ea-43c1-8106-28ad7263cb2b', revealed_flexibility_preferences_are_welfare_authoritative, empirically_contingent).
narrative_ontology:cs_reference_frame('5e190ca9-53ea-43c1-8106-28ad7263cb2b', competitive_market_clearing_equilibrium).
narrative_ontology:cs_drift_state('5e190ca9-53ea-43c1-8106-28ad7263cb2b', contemporary_monopsony_critique_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5e190ca9-53ea-43c1-8106-28ad7263cb2b', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, demand_side_businesses).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, on_demand_consumers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, schedule_autonomy_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, full_time_gig_dependents).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, schedule_autonomy_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, full_time_gig_dependents).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, demand_side_businesses).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, on_demand_consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, displaced_incumbent_drivers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, competitive_equilibrium_price_theory).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, revealed_preference_welfare_criteria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates dispatch algorithms, sets take rates and incentive schemes, and enforces service terms including deactivation; collects a commission on each matched task; operates across many cities and verticals and can rebalance investment among them when regulation tightens in one jurisdiction.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, beneficiary).

% Supplies hours around school, caregiving, or another job; logs on when prices suit and logs off otherwise; treats the platform as one of several income sources and can pause without penalty; absorbs fuel, wear, and unpaid waiting time as priced-in costs of the flexibility they report valuing.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, schedule_autonomy_workers, beneficiary,
    powerless, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, schedule_autonomy_workers, payer).

% Drives or delivers as primary income; accepts most offered tasks to hit weekly earnings targets; carries vehicle loans, insurance gaps, and uncompensated queue time; leaving would mean rebuilding income elsewhere while loan payments continue, so participation continues even in weeks the math is thin.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, full_time_gig_dependents, beneficiary,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, full_time_gig_dependents, payer).

% Restaurants, retailers, and clinics buy per-task delivery and staffing instead of fixed payroll; they pay per completed task and absorb surge premiums at peaks; reverting to employed staff is possible but slower and less elastic than the spot channel they now plan around.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, demand_side_businesses, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, demand_side_businesses, payer).

% Order rides, meals, and errands on demand at metered prices; pay per use; retain everyday substitutes such as driving themselves, cooking, or scheduled couriers, which caps what they will pay and disciplines the price side of the exchange.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, on_demand_consumers, beneficiary,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, on_demand_consumers, payer).

% Hold medallion debt and fleet assets priced for a previously protected market; after mass platform entry, fares and utilization fell and asset values collapsed; licenses, vehicle finance, and late-career skill specificity tie them to the trade while retraining paths are slow relative to their obligations.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, displaced_incumbent_drivers, payer,
    moderate, biographical, trapped, regional).

% Organize strikes, ballot campaigns, and pay-formula litigation over deactivation appeals and earnings floors; are largely absent from platform policy forums and the consultancy panels that shape the efficiency framing; their organizations' identity and funding are bound to the contest over platform labor terms.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, gig_worker_organizers, excluded,
    organized, generational, identity_locked, national).

% Run classification proceedings, minimum-pay standard-setting, and benefit-portability pilots; weigh testimony from platforms, worker groups, and academic economists; can alter the arrangement's legal footing but not the underlying demand patterns that made spot matching valuable.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves real-time search-and-matching between heterogeneous, self-scheduled labor supply and spiky, unpredictable demand: algorithmic dispatch and surge pricing allocate idle private capacity (vehicles, skills, odd hours) to demand surges without long-term contracts, replacing slow hiring pipelines for variable workloads.
% TRANSFER_FUNCTION: Moves per-task payment from demand-side payers (consumers, businesses) to task-performing workers via platform-metered pricing; moves a commission share of every transaction to platform operators; transferred quasi-rents formerly held by incumbent licensed operators to consumers through price competition.
% ABSENT_VOICES: Gig worker organizers and precarity-focused labor economists sit outside the frame's deliberative venues (platform policy forums, consultancy-commissioned studies); displaced incumbent drivers had no seat when the arrangement was designed. Present, they would contest the premise that observed participation demonstrates acceptable outside options.
% DISAPPEARANCE_RATIONALE: On-demand ride-hail, delivery, and task logistics are built on continuous spot matching; overnight removal would force reversion to shift-based employment, scheduled dispatch, or direct hiring. Prices, coverage hours, worker composition, and business models built around elastic capacity would rearrange within months.
% FOUNDING_PROBLEM: Idle private capacity (cars, skills, odd hours) could not reach spiky urban demand; traditional employment contracts were too rigid for variable workloads; job seekers needed immediate income without navigating slow hiring pipelines.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: labor economists across camps, including critics of platform work, acknowledge the matching function is real; municipal transport data document reduced passenger wait times and expanded off-peak service; even precarity-school researchers concede the coordination value while disputing how its surplus is divided.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).
:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claim is rope: from this reading's seat, the arrangement coordinates a genuine matching problem, participants are net beneficiaries, exit is real (workers log off, consumers substitute, businesses revert to payroll), and no coercion is needed to sustain the core mechanism. The metrics describe the arrangement's actual operation as even a sympathetic market-efficiency observer must concede it: extractiveness 0.26 (take rates rose industry-wide as subsidy wars ended and concentration increased in dense markets — margins above the competitive benchmark the reading itself posits), suppression 0.30 (deactivation regimes, rating discipline, and arbitration mandates bind participation more than pure spot contracting would, though multi-homing preserves exit), theater_ratio 0.18 (the matching function is genuinely performed; the small performative share is 'be your own boss' branding drifting from scheduled-block and queue realities), accessibility_collapse 0.42 (traditional employment, off-platform self-employment, and W-2 gig variants remain visible and usable), resistance 0.48 (strike waves, classification litigation, and minimum-pay ordinances are real and recurring). Suppression_requirement is tracked temporally because enforcement capacity is the story's dynamic: platforms built deactivation automation, forced-arbitration regimes, and ballot-campaign machinery as worker resistance grew — an enforcement buildout, not a static picture. All three series share one six-point grid (2009–2026) so no metric row is sampled against another metric's end-state. Demand seasonality oscillates but is not the tracked dynamic; no cyclical pattern is modeled. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently from identical global standing. The platform seat experiences the arrangement as the coordination it built and profits from; the schedule-autonomy worker seat experiences near-pure benefit with trivial cost incidence; the full-time dependent seat sits near symmetric — income it needs against volatility and deactivation exposure it cannot shed; the displaced incumbent seat, with trapped exit and stranded assets, experiences the same 'clearing' as a loss imposed without compensation. The reading's frame predicts rope at every seat; the engine computes per-seat classifications from power, exit, and role data, and trapped-exit payer seats will register materially higher effective burden. That divergence between the reading's uniform prediction and the computed seat spread is precisely the datum this story contributes to the corpus.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: platform_operators (agenda_setter + collector) derive near the beneficiary pole; schedule_autonomy_workers near it with slight cost drag from their secondary payer role; demand_side_businesses and on_demand_consumers derive near symmetric from dual beneficiary/payer positioning; full_time_gig_dependents likewise near symmetric, weighted by their constrained exit; displaced_incumbent_drivers, a payer with trapped exit, derive near the target pole. No directionality_overrides are authored: the dual-role declarations plus exit atoms already encode the asymmetries, and a power_atom-keyed override would smear across both powerless worker segments, which genuinely differ in cost incidence. Scope amplification applies modestly at the platform's global footprint and barely at the regional incumbent seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — matching idle capacity to spiky demand — is live, corroborated from outside the benefiting parties, and no sunset logic applies: this reading asserts no transition, so mandatrophy_resolved is not declared and the status-by-verdict pair (live x world_rearranges) raises no zombie flag. The classification discipline runs in both directions across the family: this story prevents the precarity failure mode of labeling genuine coordination as pure extraction before the distributional evidence is in, while the sibling precarity_extraction_reading prevents the efficiency failure mode of laundering concentrated collection as neutral clearing. Receipt-surface facts are recorded independently of the claim: gains demonstrably accrue to the platform seat (commission on every match), and fixing is prohibitive for the actors who could attempt it — regulators face wholesale rearrangement of dependent logistics sectors against a benefit this reading assesses as small, which is the reading's own judgment recorded as a fact about cost class, not a defense of the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the market_efficiency_reading of the flexible_employment_legitimacy kernel; how would instantiating the precarity_extraction_reading or the developmental_state_reading instead change the structural classification of the same standing arrangement?',
    'Cross-reading comparison of the sibling stories'' computed types over the shared referent: the precarity file authors high epsilon with gig workers as victims; the developmental file authors sunset/transition logic. Classification deltas are read across the family, not within this file.',
    'Under the precarity reading the arrangement computes toward snare or tangled_rope with concentrated collection; under the developmental reading it computes toward scaffold with formalization pressure. This file''s rope claim is valid only within its own reading''s lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one kernel, three readings, reading-indexed epsilon over a fixed referent (Rules 1-3 routing).').

omega_variable(
    monopsony_vs_competitive_clearing,
    'Do platform labor markets approximate the competitive clearing this reading''s frame assumes (many buyers, low concentration, frictionless entry), or do they exhibit monopsony power (few platforms per locale, multi-homing frictions, opaque pay formulas)?',
    'Labor-supply elasticity estimates facing individual platforms, multi-homing incidence studies, and natural experiments from pay-formula changes and city-level minimum-pay ordinances.',
    'If monopsony power is material, the foundational axiom that observed task prices are equilibrium prices fails empirically, epsilon rises toward the precarity reading''s value, and the rope claim loses its warrant at the affected seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopsony_vs_competitive_clearing, empirical, 'Whether the frame''s competitive-clearing premise survives the monopsony evidence base.').

omega_variable(
    flexibility_preference_authenticity,
    'Is workers'' revealed preference for flexibility an authentic welfare-relevant preference, or adaptation to constrained outside options that would revise under improved alternatives?',
    'Panel studies tracking platform workers whose outside options improve exogenously (local wage floors, sectoral hiring booms): retention under improved alternatives distinguishes authentic preference from adaptive preference.',
    'If preferences adapt to the constraint set, the autonomy-maximization premise weakens, part of the measured suppression is internalized rather than structural, and the beneficiary weighting of worker seats is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_preference_authenticity, empirical, 'Authenticity of the revealed flexibility preferences the reading treats as welfare-authoritative.').

omega_variable(
    net_earnings_after_true_costs,
    'Do gross per-task earnings minus vehicle depreciation, insurance gaps, and uncompensated waiting time yield net hourly returns at or above the reservation standards the reading''s price theory presumes?',
    'GPS-telemetry time-use studies and expense-diary datasets reconciling gross payout screens against fully loaded costs and total engaged time.',
    'If net returns fall below reservation standards, the clearing price is systematically mispriced, the scarcity signal reading of wage convergence is confounded, and measured extractiveness is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_earnings_after_true_costs, empirical, 'Whether observed task prices clear at welfare-relevant net earnings levels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 2009, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2009, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2009, 0.08).
narrative_ontology:measurement_basis(flex_tr_t2009, observed).
narrative_ontology:measurement(flex_tr_t2013, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2013, 0.1).
narrative_ontology:measurement_basis(flex_tr_t2013, observed).
narrative_ontology:measurement(flex_tr_t2017, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2017, 0.13).
narrative_ontology:measurement_basis(flex_tr_t2017, observed).
narrative_ontology:measurement(flex_tr_t2020, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement_basis(flex_tr_t2020, observed).
narrative_ontology:measurement(flex_tr_t2023, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2023, 0.17).
narrative_ontology:measurement_basis(flex_tr_t2023, observed).
narrative_ontology:measurement(flex_tr_t2026, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(flex_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(flex_be_t2009, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2009, 0.12).
narrative_ontology:measurement_basis(flex_be_t2009, observed).
narrative_ontology:measurement(flex_be_t2013, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2013, 0.16).
narrative_ontology:measurement_basis(flex_be_t2013, observed).
narrative_ontology:measurement(flex_be_t2017, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2017, 0.21).
narrative_ontology:measurement_basis(flex_be_t2017, observed).
narrative_ontology:measurement(flex_be_t2020, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2020, 0.23).
narrative_ontology:measurement_basis(flex_be_t2020, observed).
narrative_ontology:measurement(flex_be_t2023, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2023, 0.25).
narrative_ontology:measurement_basis(flex_be_t2023, observed).
narrative_ontology:measurement(flex_be_t2026, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2026, 0.26).
narrative_ontology:measurement_basis(flex_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2009, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2009, 0.1).
narrative_ontology:measurement_basis(flex_su_t2009, observed).
narrative_ontology:measurement(flex_su_t2013, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2013, 0.14).
narrative_ontology:measurement_basis(flex_su_t2013, observed).
narrative_ontology:measurement(flex_su_t2017, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2017, 0.2).
narrative_ontology:measurement_basis(flex_su_t2017, observed).
narrative_ontology:measurement(flex_su_t2020, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2020, 0.24).
narrative_ontology:measurement_basis(flex_su_t2020, observed).
narrative_ontology:measurement(flex_su_t2023, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2023, 0.27).
narrative_ontology:measurement_basis(flex_su_t2023, observed).
narrative_ontology:measurement(flex_su_t2026, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2026, 0.3).
narrative_ontology:measurement_basis(flex_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the flexible_employment_legitimacy kernel decomposes into three reading-stories sharing one referent (the standing platform-flexibility arrangement) with reading-indexed epsilon per OQ-26/OQ-258. This market_efficiency file authors low epsilon and a rope claim; the precarity_extraction file authors high epsilon with gig workers declared victims; the developmental_state file authors transitional/sunset logic toward formalization. Direction of influence: this reading is upstream — its policy dominance (classification rulings citing efficiency, consultancy evidence bases) shapes the legitimacy conditions and resource availability of both siblings, which is why the edges run from this file. Each family member links the others via affects_constraints; no member averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
