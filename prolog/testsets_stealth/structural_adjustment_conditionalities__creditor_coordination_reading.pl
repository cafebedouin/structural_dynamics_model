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
 *   domain: economic/political/international development finance
 *
 * SUMMARY:
 *   Since the 1980s debt crisis, multilateral and official lending to
 *   distressed sovereigns has been conditioned on policy packages — fiscal
 *   targets, monetary restraint, trade liberalization, privatization, subsidy
 *   reform — monitored through reviews that gate disbursement. The
 *   creditor-coordination reading holds that these conditions solve a real
 *   collective-action problem: without common discipline, creditors race for
 *   exit and debtors lose the credibility anchor that lets reform governments
 *   survive implementing painful adjustments. On this reading the
 *   arrangement's costs fall mainly on protected, inefficient state sectors
 *   whose dismantling is the point of the exercise, while the gains accrue to
 *   restored market access and to the taxpayers of a stabilized future. This
 *   story authors THAT reading alone as a clean, ε-invariant constraint: the
 *   ε referent is the standing conditional-lending arrangement itself,
 *   assessed by this reading's lights — not the arrangement any reading would
 *   prefer. The claim/metric gap is deliberate: claimed_type is rope per this
 *   reading's structural thesis, while the metrics are authored independently
 *   as honest description of modest-but-real residual costs; the engine
 *   measures the divergence. KEY AGENTS (by structural relationship): -
 *   imf_program_designers: Agenda-setting coordinator
 *   (institutional/arbitrage) — designs conditions, gates disbursements -
 *   international_bondholders: Primary beneficiary (institutional/arbitrage)
 *   — collects service flows, prices risk off program status -
 *   future_taxpayers_of_debtor_states: Claimed residual beneficiary
 *   (powerless/trapped) — inherits the stabilized-or-failed debt stock -
 *   debtor_finance_ministries: Implementing payer with secondary beneficiary
 *   position (organized/constrained) - overstaffed_parastatals: Sectoral
 *   payer (organized/constrained) — dismantled under structural conditions -
 *   import_protected_industries: Sectoral payer (moderate/constrained) —
 *   exposed by liberalization conditions - parastatal_workers:
 *   Household-level payer (organized/constrained) — bears retrenchment and
 *   wage caps - alternative_creditors: Excluded outsider
 *   (institutional/mobile) — competes outside the framework -
 *   program_evaluation_offices: Analytical observer
 *   (institutional/analytical) — audits outcomes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.38).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities — Creditor Coordination Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "economic/political/international development finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e').
narrative_ontology:cs_kernel_codification('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', formalized).
narrative_ontology:cs_authority_grounding('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', expertise).
narrative_ontology:cs_interpretation_layer_present('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e').
narrative_ontology:cs_reading_relation('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', foundational, coordinated_creditor_action_prevents_race_to_exit).
narrative_ontology:cs_axiom_status(coordinated_creditor_action_prevents_race_to_exit, holdable).
narrative_ontology:cs_axiom_grounding('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', coordinated_creditor_action_prevents_race_to_exit, empirically_contingent).
narrative_ontology:cs_axiom('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', foundational, conditional_discipline_restores_market_access).
narrative_ontology:cs_axiom_status(conditional_discipline_restores_market_access, holdable).
narrative_ontology:cs_axiom_grounding('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', conditional_discipline_restores_market_access, instrumental).
narrative_ontology:cs_reference_frame('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', creditor_coordination_equilibrium).
narrative_ontology:cs_drift_state('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', post_washington_consensus_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ea63eb49-dce4-4fdc-b2bf-4a3a1cc20a9e', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_of_debtor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_bondholders).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, overstaffed_parastatals).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, import_protected_industries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, parastatal_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_finance_ministries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_finance_ministries).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, fiscal_sustainability_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, market_confidence_hypothesis).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_moral_hazard_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff and executive board of the International Monetary Fund design lending programs: they set quantitative performance criteria, structural benchmarks, and review schedules, and they release tranches only when criteria are met. They justify conditions as restoring debt sustainability and reopening market access. They can redesign instruments, waive tests, or shift among facilities; their institution is repaid with interest and recycles repayments into subsequent lending.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_program_designers, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold sovereign bonds of emerging and developing states. Program approval and compliance signals lower perceived default risk and stabilize secondary-market prices, letting them price risk and sell positions at will. They lend at spreads calibrated to program status and bear losses only when programs fail catastrophically.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_bondholders, beneficiary,
    institutional, biographical, arbitrage, global).

% Will inherit whatever debt stock today's negotiations leave behind. If stabilization succeeds they service a sustainable obligation out of a growing economy; if it fails they face default, inflation, or prolonged austerity later. They cannot vote in current negotiations and cannot exit the jurisdiction's obligations.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_of_debtor_states, beneficiary,
    powerless, generational, trapped, national).

% Sign letters of intent, publish policy memoranda, and implement fiscal targets: wage bill ceilings, subsidy reductions, tax measures. They secure the financing that keeps the state solvent and gain technical assistance, but absorb domestic blame when prices rise and payrolls shrink. Walking away means losing the funding line and facing markets alone; default remains a last-resort option with severe consequences.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_finance_ministries, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_finance_ministries, beneficiary).

% State-owned utilities, marketing boards, and industrial holdings targeted for commercialization, privatization, or liquidation under program conditions. They lose subsidized credit, protected markets, and tolerated staffing levels; managers restructure or wind down operations, and their workforces absorb retrenchment.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, overstaffed_parastatals, payer,
    organized, biographical, constrained, national).

% Manufacturers operating behind tariff walls, import licensing regimes, and multiple exchange-rate systems. Liberalization conditions expose them to import competition and raise input costs where currencies depreciate; some modernize and export, many contract or close.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, import_protected_industries, payer,
    moderate, biographical, constrained, national).

% Civil servants and employees of state enterprises facing hiring freezes, nominal wage caps, and severance-based retrenchment. Public employment has been the stable rung of the urban labor market; displaced workers move into informal work with thinner protections. Unionized minorities strike or protest; most endure.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, parastatal_workers, payer,
    organized, biographical, constrained, local).

% Bilateral export-credit agencies and policy banks outside the traditional creditor committee. They lend against collateral or strategic assets without joining program conditionality, gaining market share precisely when program financing is delayed. Their growth pressures the coordinating framework from outside; they are not parties to its reviews.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, alternative_creditors, excluded,
    institutional, biographical, mobile, global).

% Independent evaluation units and academic researchers who audit program outcomes: growth records, poverty incidence during adjustment, condition-compliance rates. They publish findings without setting conditions or disbursing funds, supplying the evidentiary record the other seats argue with.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, program_evaluation_offices, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, international_bondholders).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the sovereign-debt collective-action problem: without common conditions, each creditor races to be repaid first and each new loan free-rides on others' discipline. Conditionality synchronizes creditor behavior, restores a credible fiscal path, and gives all lenders a shared signal that the debtor's policy trajectory is sustainable, preserving market access at tolerable risk premia.
% TRANSFER_FUNCTION: Moves policy control over fiscal, monetary, and structural decisions from debtor polities to creditor-designed program benchmarks, and moves loan principal (repayable with service) from official and multilateral creditors to distressed treasuries; downstream it moves employment security out of protected public sectors into the open labor market.
% ABSENT_VOICES: Populations subject to adjustment rarely sit in program negotiation: parliaments often learn conditions after signature, affected workers and subsidy recipients hold no seat, and debtor civil society gained consultative channels only in the post-1999 ownership era. Alternative creditors remain wholly outside the coordinating framework.
% DISAPPEARANCE_RATIONALE: If conditionality vanished overnight, official creditors would race for exit, private markets would reprice sovereign risk punitively, debtor governments would lose the external anchor that lets reform cabinets sustain painful policies against domestic pressure, and fiscal stabilizations would fail serially — the debt-crisis management architecture built after 1982 would have to be reinvented under emergency conditions.
% FOUNDING_PROBLEM: The sovereign debt overhang of the late 1970s and early 1980s: recycled petrodollar lending left developing states with unsustainable external obligations, and the 1982 Mexican default threat threatened cascading commercial bank failures. Something had to coordinate dispersed creditors and restore payment capacity without collapsing the international financial system.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: debtor finance ministries repeatedly request programs voluntarily, G20 debt-service suspension initiatives presuppose a coordinating body, and the economic-history literature on inter-creditor races (1930s default waves, the 1982 cascade) attests the collective-action problem independently. Independent evaluation offices confirm recurring balance-of-payments crises requiring coordinated response.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.28 (end-state): this reading treats the arrangement as a paid-for coordination service — financing supplied in exchange for policy correction — with residual extraction concentrated in the seniority of official claims and debt-service priority over social spending during programs. Suppression 0.38 is a raw structural property, unscaled by power or scope: tranche withholding and review gating coerce compliance, but exit routes (disorderly default, restructuring, alternative creditors) remain open, so the coercive perimeter is partial. Theater 0.33 reflects ownership-era proceduralism — consultations and documents that legitimize rather than alter the creditor-set core. Accessibility collapse 0.45: alternatives persist and have grown (bond markets, non-traditional bilateral lenders), so understanding the arrangement does not close exits. Resistance 0.58: adjustment met sustained opposition — strikes, riots, electoral turnover of reform governments — which this reading interprets as the predictable price of removing entrenched rents but which is descriptively real. The measurement series runs on one shared six-point grid (1980–2020) across all three tracked metrics; the falling suppression_requirement series traces genuine enforcement decay (cross-conditionality hardening gave way to streamlined, ownership-framed programs), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is technical craft: identify unsustainable trajectories, attach conditions, restore access. From the payer seats the same reviews arrive as externally authored budgets — wage caps and subsidy cuts written abroad and defended locally by finance ministers who signed under the duress of insolvency. Bondholder seats see an insurance mechanism that makes long-horizon lending to fragile sovereigns possible at all. Future taxpayers, this reading's decisive beneficiaries, experience nothing yet — their seat is a projection onto a generation that cannot object. The engine computes these divergent per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive international_bondholders toward the beneficiary pole, reinforced by arbitrage-grade exit. future_taxpayers_of_debtor_states are declared beneficiaries but carry trapped exit — the derivation will pull their d upward from the pure-beneficiary pole, and this reading accepts that modulation since their gain is contingent on program success. debtor_finance_ministries sit mid-scale: they pay politically and fiscally yet receive the financing line and technical support. The payer sectors and parastatal_workers derive high d from victim declarations plus constrained exit. alternative_creditors are excluded rather than coordinated: the framework forecloses their participation, placing them above the midpoint despite carrying no victim declaration. program_evaluation_offices are analytical and feed no directional arithmetic. No directionality overrides are authored: the derivation chain from role, exit, and declarations captures every seat's relationship without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating dispersed creditors around an insolvent sovereign without collapsing the system — recurs with each debt cycle (1980s Latin America, 1997 Asia, 2010s Greece, 2020s Zambia and Sri Lanka), so the mandate has not outlived its function; founding_problem_status is live with corroboration from outside the benefiting parties. Mandatrophy risk sits elsewhere: as private creditor committees and collective action clauses mature, the coercive layer of conditionality could atrophy into ritual while the coordination need persists — the flat late-interval extractiveness alongside slowly rising theater is the early signature worth watching. Nothing here resolves mandatrophy; the flag is deliberately unset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (creditor_coordination_reading) of the kernel structural_adjustment_conditionalities. What changes structurally if a sibling reading is adopted instead?',
    'Adopting the debtor_extraction_reading re-authors the same referent with high epsilon (~0.8), broadens victims from protected sectors to debtor populations generally, and flips the computed type toward snare. Adopting the hybrid_selectivity_reading keeps moderate epsilon but makes waiver-pattern asymmetry a core structural feature. The disagreement is located in the transfer function — symmetric exchange versus asymmetric drain — and in whether beneficiaries are diffuse-and-future or concentrated-and-present.',
    'The identical arrangement classifies from rope (this file) through tangled_rope to snare depending on which reading''s structural declarations the corpus loads; cross-reading comparison is the designed measurement, not a defect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three live readings, disagreement located in transfer symmetry and beneficiary concentration.').

omega_variable(
    waiver_selectivity_hinge,
    'Are observed waivers and non-enforcement of conditions for geopolitically strategic debtors isolated noise within coordination, or a systematic selectivity that contradicts this reading''s uniform-discipline premise?',
    'Code historical program records for condition-waiver rates regressed on geopolitical alignment measures (UN voting coincidence, alliance membership, strategic lending); systematic alignment effects would falsify the uniform-discipline premise.',
    'Systematic selectivity undermines the coordination axiom this reading stands on and shifts evidentiary weight to the hybrid reading''s structure; isolated cases leave the premise intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waiver_selectivity_hinge, empirical, 'Whether waiver patterns are noise or the hinge on which the sibling readings turn.').

omega_variable(
    counterfactual_welfare_comparison,
    'Would countries entering programs have fared better under the feasible counterfactual — self-directed adjustment, disorderly default, or reliance on alternative creditors?',
    'Matched comparisons of program and non-program countries on comparable fundamentals, exploiting variation in program timing and eligibility; the existing literature is mixed and identification-contested.',
    'If counterfactuals dominate, the coordination story functions as cover and the beneficiary declaration for future taxpayers fails; if programs dominate, the rope claim holds with the measured residual costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_welfare_comparison, empirical, 'The counterfactual on which this reading''s net-benefit claim rests.').

omega_variable(
    voluntary_coordination_sufficiency,
    'Does creditor coordination actually require enforceable conditions on debtors, or would voluntary creditor committees and contractual devices such as collective action clauses coordinate lenders adequately without debtor-side coercion?',
    'Compare restructuring outcomes under ad hoc creditor councils and CAC-enabled bond restructurings against program-era outcomes on recovery rates and time-to-resolution.',
    'If voluntary devices suffice, the coercive component is removable overhead and the arrangement''s suppression is excess rather than structural; if not, conditionality carries genuine coordination load.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_coordination_sufficiency, empirical, 'Whether the enforcement layer is load-bearing coordination or removable overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement_basis(stru_tr_t1980, observed).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement_basis(stru_tr_t1990, observed).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement_basis(stru_tr_t2000, observed).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement_basis(stru_tr_t2010, observed).
narrative_ontology:measurement(stru_tr_t2015, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement_basis(stru_tr_t2015, observed).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.33).
narrative_ontology:measurement_basis(stru_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.36).
narrative_ontology:measurement_basis(stru_be_t1980, observed).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.34).
narrative_ontology:measurement_basis(stru_be_t1990, observed).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement_basis(stru_be_t2000, observed).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement_basis(stru_be_t2010, observed).
narrative_ontology:measurement(stru_be_t2015, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement_basis(stru_be_t2015, observed).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement_basis(stru_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(stru_su_t1980, observed).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement_basis(stru_su_t1990, observed).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement_basis(stru_su_t2000, observed).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement_basis(stru_su_t2010, observed).
narrative_ontology:measurement(stru_su_t2015, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2015, 0.39).
narrative_ontology:measurement_basis(stru_su_t2015, observed).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement_basis(stru_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, hipc_debt_relief_initiative).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'structural adjustment conditionalities' per the ε-invariance principle: the label conflates at least three structurally distinct claims — necessary coordination (this file, low-moderate ε, rope claim), extractive drain (sibling file, high ε, snare-flavored claim), and selective discipline keyed to geopolitical alignment (sibling file, waiver asymmetry as core structure). Each carries its own ε, victim set, and stakeholders; these edges link the family so contamination analysis and cross-reading comparison propagate. By mainstream empirical confidence this coordination reading is upstream; the extraction reading cites the same historical record as counterevidence, hence the mutual family linkage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
