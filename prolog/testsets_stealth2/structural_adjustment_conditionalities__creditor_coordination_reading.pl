% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities — Creditor Coordination Reading
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   When a sovereign state faces a financing crisis, multilateral and
 *   bilateral lenders extend credit contingent on policy performance: fiscal
 *   targets, subsidy removal, privatization, trade liberalization, and
 *   governance benchmarks, verified through periodic reviews before each
 *   tranche releases. This file instantiates the creditor-coordination
 *   reading of the structural-adjustment kernel: on this reading the
 *   arrangement exists to solve a collective-action problem no single actor
 *   can solve alone — dispersed creditors would otherwise race for the exits
 *   and force disorderly default, and debtor governments would otherwise
 *   defer the fiscal correction their own future citizens require. The
 *   epsilon referent is the standing conditionalities arrangement itself,
 *   assessed by this reading's lights: costs imposed on protected sectors and
 *   public payrolls are priced as the cost of restoring solvency, not as
 *   creditor rent, so base extractiveness is authored low. Sibling readings
 *   (debtor_extraction_reading, hybrid_selectivity_reading) are separate
 *   constraint files linked through network.affects_constraints; they share
 *   this kernel and differ on epsilon attribution and victim identification.
 *   Claim and metrics are authored independently: the reading claims a
 *   coordination arrangement, while the metrics describe operation as this
 *   reading honestly observes it — real financial leverage behind formal
 *   consent, growing performative compliance in the serial-program years, and
 *   persistent street-level and electoral resistance throughout.
 *
 * KEY AGENTS:
 *   - imf_and_multilateral_lenders: agenda-setting institutional creditor ([institutional]/[arbitrage]) — designs programs, controls tranche release, absorbs reputational risk
 *   - bilateral_creditor_governments: co-agenda-setter via Paris Club coordination ([institutional]/[arbitrage]) — tie official debt relief to program compliance
 *   - international_creditor_community: primary beneficiary seat ([institutional]/[arbitrage]) — receives coordinated repayment, bears episodic restructuring risk
 *   - future_taxpayers: intended beneficiary with no seat ([powerless]/[trapped]) — inherits the settled debt stock
 *   - program_country_populations: short-run cost bearer, long-run promised beneficiary ([moderate]/[trapped]) — pays austerity now against stabilization later
 *   - public_sector_payrolls: organized cost bearer ([organized]/[constrained]) — wage ceilings and retrenchment
 *   - protected_state_enterprises: organized cost bearer ([organized]/[constrained]) — lose subsidies, protection, and charters
 *   - political_patronage_networks: cost bearer with offshore exit ([powerful]/[mobile]) — lose directed credit and procurement rents
 *   - program_country_legislatures: excluded ratification seat ([moderate]/[constrained]) — presented with packaged bills and disbursement deadlines
 *   - independent_distributional_analysts: analytical observer ([moderate]/[analytical]) — audit who bore costs and who received flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.22).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.42).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities — Creditor Coordination Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'f2ffe44d-31f4-4107-8319-2ce3651454f1').
narrative_ontology:cs_kernel_codification('f2ffe44d-31f4-4107-8319-2ce3651454f1', formalized).
narrative_ontology:cs_authority_grounding('f2ffe44d-31f4-4107-8319-2ce3651454f1', expertise).
narrative_ontology:cs_interpretation_layer_present('f2ffe44d-31f4-4107-8319-2ce3651454f1').
narrative_ontology:cs_reading_relation('f2ffe44d-31f4-4107-8319-2ce3651454f1', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2ffe44d-31f4-4107-8319-2ce3651454f1', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('f2ffe44d-31f4-4107-8319-2ce3651454f1', foundational, conditionalities_solve_creditor_collective_action).
narrative_ontology:cs_axiom_status(conditionalities_solve_creditor_collective_action, holdable).
narrative_ontology:cs_axiom_grounding('f2ffe44d-31f4-4107-8319-2ce3651454f1', conditionalities_solve_creditor_collective_action, empirically_contingent).
narrative_ontology:cs_axiom('f2ffe44d-31f4-4107-8319-2ce3651454f1', foundational, adjustment_costs_price_solvency_not_rent).
narrative_ontology:cs_axiom_status(adjustment_costs_price_solvency_not_rent, holdable).
narrative_ontology:cs_axiom_grounding('f2ffe44d-31f4-4107-8319-2ce3651454f1', adjustment_costs_price_solvency_not_rent, empirically_contingent).
narrative_ontology:cs_reference_frame('f2ffe44d-31f4-4107-8319-2ce3651454f1', coordinated_solvency_restoration_framework).
narrative_ontology:cs_drift_state('f2ffe44d-31f4-4107-8319-2ce3651454f1', post_conditionality_review_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f2ffe44d-31f4-4107-8319-2ce3651454f1', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditor_community).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, protected_state_enterprises).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_payrolls).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, political_patronage_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, bilateral_creditor_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs crisis lending programs: negotiates letters of intent with finance ministries, sets quantitative performance criteria and structural benchmarks, conducts reviews, and releases tranches only when criteria pass. Waivers and prior actions give it day-to-day leverage over debtor policy. It recycles interest and repayments into its lending pool and carries the reputational weight of program success or failure. Its exit is structural: it can redesign instruments, shift to precautionary credit lines, or step back and let markets price sovereign risk directly.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_and_multilateral_lenders, agenda_setter,
    institutional, generational, arbitrage, global).

% Coordinate official debt relief through the Paris Club, tying rescheduling and reduction to the debtor maintaining a multilateral program. They vote on terms, their export-credit agencies follow the consensus, and they weigh strategic and commercial interests alongside repayment. They receive debt service when programs hold and book losses when relief is granted.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, bilateral_creditor_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, bilateral_creditor_governments, beneficiary).

% Commercial banks, bondholders, and asset managers holding sovereign paper. Coordinated adjustment restores the repayment capacity their claims depend on and prevents a disorderly default that would force fire-sale exits. They lend back into programs when confidence returns, price risk through spreads, and take haircuts when private-sector-involvement clauses apply. Their capital is mobile: they can decline to roll over and move to other markets.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditor_community, beneficiary,
    institutional, biographical, arbitrage, global).

% Not yet born or not yet represented when crisis settlements are struck. They inherit either a serviced, sustainable debt stock with restored market access or an unpayable overhang with defaulted relationships. No seat exists for them at the negotiating table; their interest enters only through the sustainability projections written into program documents.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers, beneficiary,
    powerless, generational, trapped, national).

% Households living inside the adjustment: fuel and food subsidies removed, utility tariffs raised, user fees introduced, public services tightened. They bear the front-loaded costs and are promised stabilization, lower inflation, and renewed growth as the back end. They cannot leave the currency, the debt, or the territory; their levers are protest, elections, and endurance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_populations, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_populations, beneficiary).

% Civil servants, teachers, health workers, and state employees under wage-bill ceilings, hiring freezes, and retrenchment exercises. Unions can strike and lobby, but the employer is the state executing program targets, and outside labor markets are thin where the state is the dominant employer.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_payrolls, payer,
    organized, biographical, constrained, national).

% Parastatals and subsidized industries losing price supports, preferential credit, import protection, and monopoly charters under privatization and liberalization conditions. Managers face restructuring or sale; workers face redundancy. Their political channels narrow as the subsidies that financed their alliances disappear.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, protected_state_enterprises, payer,
    organized, biographical, constrained, national).

% Ruling coalitions, connected contractors, and elite factions whose directed credit, exchange-rate arbitrage, procurement margins, and subsidy skims are curtailed by fiscal and governance conditions. Many negotiate in the capital while moving wealth offshore; family members and assets often hold foreign residency and accounts regardless of program outcome.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, political_patronage_networks, payer,
    powerful, immediate, mobile, national).

% Parliaments in program countries frequently learn program contents after executive-fund negotiation concludes; ratification votes arrive as packaged bills with disbursement deadlines attached. Budget committees request program documents that arrive late or in draft; amendments risk delaying tranches and are correspondingly rare.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, program_country_legislatures, excluded,
    moderate, biographical, constrained, national).

% Academic economists, NGO monitors, and independent evaluation offices outside program governance. They reconstruct who bore program costs and who received program flows, publish distributional audits, and testify to legislative bodies and executive boards. They hold no votes and control no tranches.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, independent_distributional_analysts, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditor_community).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves two coupled collective-action problems stated without evaluation: (a) among dispersed creditors — preventing a race to exit that forces disorderly default, by committing all claimants to a common adjustment-and-repayment framework; (b) within the borrowing state — binding the government to a fiscal correction it would otherwise defer, using staged disbursement as a commitment device against its own time inconsistency.
% TRANSFER_FUNCTION: Moves credit and policy control in opposite directions: disbursed funds flow from multilateral and bilateral creditors to the program country's treasury; policy concessions flow from the borrowing state to creditors and markets. Within the country, resources shift from protected sectors, subsidized consumers, and public payrolls toward debt service and the projected future taxpayer position.
% ABSENT_VOICES: Program-country legislatures are structurally absent from negotiation (executive-fund bargaining, packaged ratification); affected workers and subsidy-dependent households had marginal consultation channels before the 2000s participation reforms; creditors outside the framework (non-Paris-Club official lenders) negotiate separately or not at all. Each would object to specific terms if seated, and their absence shapes the unanimity recorded in program documents.
% DISAPPEARANCE_RATIONALE: If conditionality vanished overnight, emergency lending would continue but uncoordinated: creditors would race to exit failing sovereigns, debtor governments would face no enforcement mechanism for deferred adjustment, and the crisis-lending architecture would reorganize around ad hoc bilateral deals and market pricing — a materially different world for every named seat, which is why the verdict is rearrangement rather than indifference.
% FOUNDING_PROBLEM: The 1970s-80s sovereign debt overhang: petrodollar recycling left developing states carrying dollar-denominated debts they could not service after interest rates spiked in 1979-82, and no machinery existed either to coordinate the hundreds of exposed creditor banks or to commit debtor governments to adjustment — threatening cascading defaults through the international banking system.
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous records corroborate the founding problem from outside the benefiting parties: Federal Reserve and Treasury deliberations from 1982-84 warning of systemic exposure to Mexican and Brazilian default, commercial bank disclosures of developing-country loan concentrations exceeding capital, and debtor-government archive requests for payment respite addressed to creditor committees before any program existed. Whether the founding problem remains live in current applications is disputed between the lending institutions and their critics, and neither side's self-attestation settles it.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).
:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at interval end) because, on this reading's own accounting, the costs imposed track the solvency-restoration function: subsidy removal, wage restraint, and liberalization are the adjustment itself, and the series shows extraction peaking around 2002 when conditionality breadth was maximal, then easing after the streamlining reforms. Suppression is moderate (0.42) and is authored as a raw structural property, unscaled by power or scope: the arrangement operates through financial leverage — withheld tranches, cross-conditionality, market-access cutoffs — behind formally negotiated consent; alternatives (default, refusal, alternative lenders) remain reachable, which is why suppression sits well below coercive maxima. Theater ratio (0.28) reflects real verification work (reviews, missions, data audits) mixed with a growing share of performative compliance: targets met through one-off asset sales, accounting reclassifications, and nominal reforms timed to review calendars. Accessibility collapse is low-moderate (0.35) because alternatives visibly persist — outright default, bilateral rescue from outside the framework, regional swap arrangements — and knowing the arrangement's terms does not eliminate them. Resistance is substantial (0.6): the historical record of adjustment-era protests, general strikes, and electoral reversals is among the densest of any international arrangement. The temporal series run on one shared seven-point grid (1980-2025) so every tracked metric is authored at every examined time point; suppression_requirement is tracked because enforcement intensity is a dynamic this story specifically traces — it ratcheted up through the 1980s cross-conditionality era and decayed after the 2002 conditionality review, a falling trajectory modeling enforcement softening rather than a static picture. Boltzmann coordination type is enforcement_mechanism: the arrangement's defining machinery is dedicated enforcement infrastructure (reviews, waivers, tranche gating), and its characteristic failure is enforcement collapse into free-riding.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is crisis engineering: reviews, waivers, and tranche timing are instruments of a design. From the patronage-network seat the same reviews read as confiscation of established rents. From the payroll seat, wage-bill ceilings read as external control of livelihoods; from the treasury seat they read as the only credible anchor available once markets close. Program-country populations split internally: households paying removed subsidies experience the present tense of the arrangement, while the same households' children are the intended beneficiaries of its promised end state — a within-seat temporal divergence the engine computes from the dual role declaration. The payer and beneficiary seats should classify differently from the agenda-setter seat, and nothing in the authored claim adjudicates that divergence; it is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Future taxpayers sit nearest the full-beneficiary end: they receive the stabilized debt stock and have no exit whatsoever — unborn parties cannot arbitrage. The international creditor community sits near-beneficiary but not at zero: coordinated repayment flows to it, yet private-sector-involvement episodes (Brady haircuts, 2010s restructurings) show it bearing real restructuring risk, damping its subsidy below a pure beneficiary. Program-country populations carry a dual declaration — payer now, beneficiary later — placing them mid-to-high on the target side in the short run, amplified by trapped exit. Political patronage networks sit nearest the full-target end, but their mobile exit (offshore asset movement, foreign residency) dampens the effective burden they actually experience relative to formally trapped payers. Public payrolls and protected enterprises are constrained targets with organized voice — strikes and lobbying raise their resistance but not their exit. The agenda-setting seats derive low directional pressure from controlling the rules themselves. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place every seat, and the override surface keys on power atoms too coarsely to improve on the derivation here — an override at 'institutional' would distort both the Fund and the creditor community simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the 1980s sovereign debt overhang threatening the commercial banking system — was substantially resolved by the late 1990s through Brady-plan restructurings and restored market access. The arrangement persisted across successor crises (1997 Asian crisis, 2010s peripheral-europe programs, pandemic-era emergency lending), which keeps founding-problem status contested rather than dead: each new crisis revives a version of the original coordination problem, but serial programming shows the mechanism outliving any single crisis it was built for. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-problem zombie flag fires, because the coordination function remains demonstrably active in acute crises. The degradation vector is specific and watched: serial-programming drift, documented in the measurement series as extraction and theater peaking together around 2002, is the path along which this arrangement would decay into maintenance of itself. Mandatrophy is therefore not declared resolved — the honest state is a live function with a visible aging signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the structural_adjustment_conditionalities kernel correctly characterizes the standing arrangement — creditor coordination, debtor extraction, or selective discipline?',
    'Comparative institutional analysis: distributional audits of program episodes against matched non-program crises, enforcement-symmetry data across strategically differentiated debtors, and counterfactual modeling of disorderly-default paths.',
    'If the extraction reading is correct, base extractiveness rises sharply and the classification migrates toward enforced asymmetric transfer; if the selectivity reading is correct, enforcement asymmetry becomes the dominant structural feature and per-seat classifications diverge by geopolitical position rather than by economic role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame ambiguity: this file instantiates one of three live readings of one kernel; the choice among them is not settled inside this story.').

omega_variable(
    counterfactual_disorderly_default,
    'What would program countries'' trajectories have been absent conditionality — disorderly default and deeper collapse, or autonomous recovery at comparable cost?',
    'Natural experiments: post-default recoveries (Argentina 2002-2007, Russia 1998-2000) and countries that refused or lost program access, compared on output, inflation, and poverty paths using matching methods.',
    'This reading''s low extractiveness rests on the counterfactual being worse; if unilateral default paths match or beat program paths, the coordination justification fails and the measured financial leverage reads as pure coercion rather than enforcement of an agreement all parties needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_disorderly_default, empirical, 'The load-bearing counterfactual beneath the coordination claim.').

omega_variable(
    ownership_rhetoric_vs_substance,
    'Did the post-2002 country-ownership reforms change program substance, or only program language?',
    'Structured comparison of pre- and post-2002 letters of intent: conditionality counts, structural benchmark density, waiver rates, and prior-action composition across the reform boundary; staff interviews; continuity analysis of program design within repeat-borrower countries.',
    'If substance is unchanged, the falling suppression_requirement series measures rhetorical adaptation rather than reduced coercion, and part of the theater_ratio plateau reflects reform performance layered onto unchanged enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ownership_rhetoric_vs_substance, empirical, 'Whether the enforcement softening visible in the temporal series is real or performed.').

omega_variable(
    serial_programming_function_decay,
    'When adjustment programs become serial — countries spending decades in consecutive arrangements — does the coordination function decay into dependency management?',
    'Cohort study of long-term program countries versus one-time adjusters: fiscal outcomes, timing of regained market access, institutional quality indicators, and program-document reuse rates.',
    'If decay is real, theater_ratio resumes rising and the arrangement drifts toward inertial persistence maintained by bureaucratic momentum despite the coordination claim — the characteristic degraded-lifecycle failure mode for this family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(serial_programming_function_decay, empirical, 'Long-run drift risk for the coordination function in repeat-borrower cohorts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(stru_tr_t1988, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(stru_tr_t1995, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(stru_tr_t2002, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(stru_tr_t2017, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(stru_tr_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(stru_be_t1988, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1988, 0.18).
narrative_ontology:measurement(stru_be_t1995, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(stru_be_t2002, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2002, 0.26).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(stru_be_t2017, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2017, 0.23).
narrative_ontology:measurement(stru_be_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(stru_su_t1988, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1988, 0.62).
narrative_ontology:measurement(stru_su_t1995, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(stru_su_t2002, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2002, 0.5).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(stru_su_t2017, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2017, 0.43).
narrative_ontology:measurement(stru_su_t2025, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% 'Structural adjustment conditionalities' is a colloquial label covering at least three structurally distinct claims about one practice. This file authors the creditor-coordination claim: low epsilon, coordination-first, costs priced as adjustment. The debtor-extraction claim (high epsilon, transfer-first, identifiable victims) and the hybrid-selectivity claim (enforcement asymmetry as the defining feature) are separate files sharing the kernel; each carries its own epsilon, its own beneficiary/victim structure, and its own classification. Family members link through affects_constraints. Citation runs downstream: the sibling critiques cite this reading's mechanisms, program documents, and review records as their evidence base, which is why this reading sits upstream in the family graph.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
