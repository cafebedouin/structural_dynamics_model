% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support — Freedom-Floor Reading
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   Unconditional income support — a periodic payment to every resident with
 *   no means test, no work requirement, and no behavioral conditions — is
 *   advanced by this reading as an autonomy-enabling floor: it makes
 *   labor-market participation genuinely voluntary, ends the surveillance and
 *   stigma of conditional welfare, and insures households against market
 *   shocks. The reading's beneficiary structure is deliberately asymmetric in
 *   one direction: precarious workers, unpaid caregivers, artists, and abuse
 *   victims gain options they currently lack, and the reading claims no
 *   victims, treating contribution as membership in a universal insurance
 *   pool rather than as loss. EPSILON REFERENT: following the kernel-reading
 *   rule, epsilon is authored for the standing arrangement under contest —
 *   the unconditional programs and pilots actually operating (permanent
 *   dividends, long-term cash-transfer trials, national experiments) — as
 *   this reading assesses them, never for the fully adequate floor the
 *   reading advocates. CLAIM/METRIC INDEPENDENCE: the claimed type (rope)
 *   states what this reading believes is structurally true; the metrics state
 *   what is descriptively true of the arrangement's actual operation,
 *   including its drift toward pilot theater and slow fiscal-layer
 *   accumulation. The engine computes per-seat classifications from the
 *   structural data; divergence between claim and computed type is signal,
 *   not error. FAMILY NOTE: the colloquial label 'unconditional income
 *   support' decomposes into three readings of one kernel — this file
 *   instantiates freedom_floor_reading; dependency_trap_reading and
 *   universality_paradox_reading are separate constraints linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - national_legislatures: Agenda setter (institutional/arbitrage) — sets the payment level, funding formula, and survival of the program; can restructure or raid it each budget cycle
 *   - - precarious_workers: Primary beneficiary (moderate/constrained) — gig, seasonal, and short-contract workers whose volatility the floor insures against
 *   - - unpaid_caregivers: Primary beneficiary (moderate/constrained) — household care work becomes economically survivable without a wage attached
 *   - - artists_and_creative_workers: Primary beneficiary (moderate/constrained) — thin-market creative work sustained through lean periods
 *   - - abuse_victims: Primary beneficiary (powerless/trapped) — money in their own name is an exit resource no abuser can condition
 *   - - taxpayers: Contributing members (organized/constrained) — fund the pool through progressive taxation while insured by it; dual-positioned
 *   - - employers: Incidental gainer and marginal contributor (organized/mobile) — face workers who can decline bad offers; draw on steadier demand
 *   - - welfare_bureaucracies: Excluded voice (organized/constrained) — staff the conditional systems universality displaces; rarely seated in pilot design
 *   - - labor_economists: Analytical observer (analytical/analytical) — design and evaluate the pilots; referee the evidentiary dispute between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.27).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.27).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support — Freedom-Floor Reading").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "economic/political/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '22e431f8-9af1-4c16-a7b1-3fef170d5c12').
narrative_ontology:cs_kernel_codification('22e431f8-9af1-4c16-a7b1-3fef170d5c12', formalized).
narrative_ontology:cs_authority_grounding('22e431f8-9af1-4c16-a7b1-3fef170d5c12', expertise).
narrative_ontology:cs_interpretation_layer_present('22e431f8-9af1-4c16-a7b1-3fef170d5c12').
narrative_ontology:cs_reading_relation('22e431f8-9af1-4c16-a7b1-3fef170d5c12', unconditional_income_support__dependency_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('22e431f8-9af1-4c16-a7b1-3fef170d5c12', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('22e431f8-9af1-4c16-a7b1-3fef170d5c12', foundational, unconditional_support_preserves_voluntary_choice).
narrative_ontology:cs_axiom_status(unconditional_support_preserves_voluntary_choice, holdable).
narrative_ontology:cs_axiom_grounding('22e431f8-9af1-4c16-a7b1-3fef170d5c12', unconditional_support_preserves_voluntary_choice, empirically_contingent).
narrative_ontology:cs_axiom('22e431f8-9af1-4c16-a7b1-3fef170d5c12', foundational, conditionality_itself_is_the_harm).
narrative_ontology:cs_axiom_status(conditionality_itself_is_the_harm, holdable).
narrative_ontology:cs_axiom_grounding('22e431f8-9af1-4c16-a7b1-3fef170d5c12', conditionality_itself_is_the_harm, deontological).
narrative_ontology:cs_reference_frame('22e431f8-9af1-4c16-a7b1-3fef170d5c12', voluntary_market_participation_baseline).
narrative_ontology:cs_drift_state('22e431f8-9af1-4c16-a7b1-3fef170d5c12', contemporary_partial_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('22e431f8-9af1-4c16-a7b1-3fef170d5c12', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists_and_creative_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpayers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, employers).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, ubi_labor_supply_null_result).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, universal_risk_pooling_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the payment level, the funding formula, and eligibility breadth by statute, and administers the program through the treasury. Can restructure, dilute, or raid it in any budget cycle; in the flagship jurisdiction the legislature has repeatedly rebalanced the dividend formula against revenue swings, and elsewhere legislatures authorize pilots and decide whether to scale them. Its own revenues are bound up in the funding stream, giving it a standing interest in the formula's shape.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, national_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Work in gig, seasonal, and short-contract jobs with volatile hours and thin savings. The payment arrives regardless of employment status, which lets them decline unsafe or underpaid shifts and wait for better matches instead of taking the first offer. Full exit from paid work remains unattractive — the payment alone rarely covers complete living costs — so the floor changes their bargaining position rather than removing them from the market.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).

% Perform household care — children, elders, disabled family members — that conditional systems classify as unemployment and police accordingly. An unconditional payment recognizes the household's income need without requiring them to document job search or accept unrelated work, making care work economically survivable. Their household finances remain tied to the regional tax base and local costs; relocating away from extended family usually means losing the care network that makes the arrangement workable.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Earn sporadically from creative work with long unpaid development periods. The floor carries them through lean stretches so the work survives instead of being abandoned for full-time unrelated employment. Markets for the work stay thin regardless of the payment; what changes is whether a bad sales year ends the career.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists_and_creative_workers, beneficiary,
    moderate, biographical, constrained, national).

% Financial dependence on an abusive partner is frequently what makes leaving impossible: no independent income, no credit, every resource conditioned on staying. Money arriving in their own name, unconditionally and without a household means-test, is an exit resource no abuser can intercept or condition. Until they leave, household finances remain enmeshed and the payment is the first asset that is fully theirs; shelter capacity and safety planning determine whether the resource converts into an actual exit.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims, beneficiary,
    powerless, biographical, trapped, national).

% Fund the program through the tax system, progressively weighted so net contribution rises with income. The same households are covered by the floor in any year illness, disaster, or job loss hits, and they live amid fewer desperation-driven spillovers — property crime, emergency-room overflow, distressed selling. Net lifetime position varies widely by income: lower-income households draw more than they contribute, higher-income households the reverse, and the median household contributes roughly what it can expect to draw. Relocating to escape the tax base means abandoning the jurisdiction whose services and stability they also consume.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, taxpayers, payer).

% Face workers who can now decline bad offers, which nudges wages and conditions upward at the bottom of the labor market — a cost they bear at the margin. In exchange they draw on steadier consumer demand, lower turnover among workers who take jobs willingly, and a workforce where accepting a position signals genuine availability rather than desperation. Sectors with chronic vacancies report harder recruiting; sectors competing on conditions report better retention. Firms with mobile operations can relocate across jurisdictions, though most serve local markets.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, employers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, employers, payer).

% Staff the conditional systems — eligibility offices, work-requirement administration, fraud investigation, case management — that universality displaces. Their budgets, caseloads, and professional identities are built around assessing need and verifying compliance; a program that needs none of that renders their function redundant. They are rarely seated in pilot-design conversations, which are run by treasuries and advocacy coalitions, and their institutional knowledge about where conditional systems fail enters the debate mainly through hostile testimony.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, welfare_bureaucracies, excluded,
    organized, biographical, constrained, national).

% Design and evaluate the pilots and permanent programs, publish labor-supply, wellbeing, and price-effect estimates, and referee the evidentiary dispute between the readings of this arrangement. Their instruments — what counts as a labor-supply effect, what counts as autonomy, what comparison window suffices — define what the policy conversation treats as known. They collect no transfers and bear no tax burden from the programs they study.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of universal shock protection and an autonomy floor that no household can self-provide: by pooling contributions across the entire population, the arrangement makes refusal of coercive bargains affordable for anyone, spreads the cost of individual disasters across everyone, and does so without the assessment apparatus that conditional schemes require.
% TRANSFER_FUNCTION: Moves purchasing power from the broad tax base — progressively weighted, so net flows run from higher earners to lower earners and from secure households to shock-exposed ones — to every resident as an equal unconditional payment; within the beneficiary class, flows are equal by design rather than proportional to need.
% ABSENT_VOICES: Welfare bureaucracies and case-management professionals whose function universality displaces would object and are not in the room; fiscal conservatives who reject the insurance framing and forecast debt service onto future generations are heard only adversarially; recipients themselves enter mainly as pilot subjects rather than as designers. Pilot-design tables are staffed by treasuries and advocacy coalitions, which is where the missing seats would sit.
% DISAPPEARANCE_RATIONALE: Regional economies anchored on the dividend (retail cycles timed to disbursement in the flagship jurisdiction), the household budgets of caregivers and precariously employed recipients, and the pilot-evaluation industry would all rearrange immediately; recipient consumption would contract within weeks, abuse victims would lose the one unconditional exit resource, and the conditional-welfare apparatus would have to be rebuilt from scratch to catch those who fall — at far higher administrative cost per person covered.
% FOUNDING_PROBLEM: Industrial-era welfare was conditional, stigmatizing, and porous: it policed the poor through work requirements, eligibility investigations, and home visits, while caregivers, intermittently employed workers, and shock-hit households fell through its gaps; and it offered no protection against market-wide shocks that made work itself unavailable. The founding problem was to build support that reaches everyone without surveillance or stigma and decouples survival from acceptance of any particular bargain.
% FOUNDING_PROBLEM_CORROBORATION: National statistical offices' poverty and economic-insecurity series attest the underlying problem persists, independent of any advocacy; historical documentation of pre-welfare-state mutual aid collapsing during the Great Depression attests the original gap; employer federations' recruitment-friction surveys and domestic-violence services' financial-dependence casework attest the specific mechanisms from outside the beneficiary set. No corroboration rests solely on recipients' or advocates' testimony.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.27, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored moderate-low (0.27 at interval end) because the reading's own lights find the classic disincentive channel empirically mild — Alaska's dividend and long-run cash-transfer trials show minimal labor-supply contraction — leaving residual extraction in the deadweight cost of taxation, administrative overhead, and episodic diversions (dividend raids, formula dilution). Suppression is low (0.15) and intentionally so: the arrangement's defining feature is the absence of a compliance apparatus — no eligibility policing, no work verification, no home visits; what suppressive force remains is compulsory taxation and political lock-in, not behavioral control. Suppression is a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled engine-side by directionality and scope. Theater_ratio (0.28, rising from 0.10) tracks a real drift: the announce-pilot-evaluate-shelve cycle became a political genre whose announcement value sometimes exceeds its implementation content, even while permanent transfers continue functioning. Accessibility_collapse (0.45) reflects partial crowding-out: conditional programs, tax credits, and charity persist alongside universality. Resistance (0.55) is real and persistent — failed referenda, repeated legislative defeats, work-ethic constituencies — which is what a defended construct, not a natural law, meets. The measurement series run on ONE shared time grid (t=0..42 in seven-year steps, anchored to 1982-2024, the era of the first permanent unconditional dividend through the mature pilot era) so every tracked metric is authored at every examined point; suppression_requirement series are deliberately omitted because the enforcement picture is static — the scalar captures it. Receipt surface: gain_flow='diffuse' is an affirmative finding, not a default — every named seat was checked, and no seat captures the flows; transfers follow the statutory formula to the beneficiary class as the program's function, with no concentrated appropriator (universality is precisely the design feature that prevents capture). fixing_cost='prohibitive': unwinding an operating universal floor costs more than any fix achieves — attempted dividend raids triggered recall campaigns and constitutional-amendment politics in the flagship jurisdiction — so adjustment happens at the margins (level, formula) rather than by removal.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the agenda-setter seat the arrangement is an insurance instrument the legislature tunes annually; from the recipient seats it is the difference between refusing a dangerous shift and taking it, between leaving and staying. From the excluded seat — the conditional-welfare bureaucracies — the same arrangement appears as the scheduled obsolescence of their function, a outcome they were never invited to negotiate. From the observer seat it is a natural experiment whose instruments define what counts as evidence. The engine computes these divergent per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The four declared beneficiary groups sit near the full-beneficiary end: the payment subsidizes them without reciprocal extraction, and their constrained or trapped exit positions amplify the subsidy's effect (an exit resource matters most to those with none). Taxpayers are the structural hinge: dual-positioned (contributor and insured member), they derive near-symmetric directionality — they fund the pool and are covered by it, with net lifetime position varying by income (see the net_contributor_status omega). Employers sit mildly beneficiary-side: they pay marginally higher wages at the bottom of the market but draw demand stability. No seat approaches the full-target end — that absence is the structural signature this reading asserts, and it is what the dependency_trap reading denies. Scope is national, so the engine's scope amplification of effective extraction applies modestly; with low base extraction and no full-target seat, amplified values stay low.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — conditional welfare policed the poor while leaving caregivers, unstable workers, and shock-exposed households uncovered — is still live, so no mandatrophy is declared and no sunset clause applies: this is not a transitional arrangement claiming to be permanent infrastructure. The classification discipline cuts both ways. Against the dependency_trap reading, authoring the structural data (real beneficiaries, no victims, minimal enforcement) prevents a functioning coordination mechanism from being mislabeled as pure extraction riding on an incentive story. Against this reading's own advocacy, the temporal series keeps the claim honest: theater_ratio is the sentinel — if pilot-signaling continues replacing implementation, the ratio crosses toward proxy-goal territory and the arrangement drifts from rope toward piton-shaped performance of a program; the slow extractiveness accumulation (0.16 to 0.27) is watched for the fiscal-layer ratchet (raids, formula dilution, administrative growth) that historically converts redistributive mechanisms into something less clean.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the freedom_floor reading of the unconditional_income_support kernel; what would the dependency_trap or universality_paradox readings change structurally?',
    'Compile the sibling stories and diff victim sets, epsilon, and computed types across the three readings of the shared kernel.',
    'Under the dependency_trap reading the recipient seats become targets of incentive distortion and epsilon rises sharply; under the universality_paradox reading the agenda-setter seat becomes the contested center and assessment turns on coalition mechanics rather than transfer effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    net_contributor_status,
    'Are higher-earning net contributors genuinely members of the insured pool (lifetime expected transfers plus externality and option value exceeding contributions), or concealed payers with no compensating position?',
    'Distributional analysis of lifetime expected transfers versus contributions across income deciles, including valuation of shock protection and desperation-externality reductions.',
    'If large cohorts are net losers with no offsetting benefit, the no-victims claim fails, effective extraction rises for those seats, and the type shifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_contributor_status, empirical, 'Whether the Pareto-improvement claim survives lifetime accounting for net contributors.').

omega_variable(
    adequacy_dose_threshold,
    'At what payment level does the floor actually deliver autonomy — remove compulsion from market participation? Are implemented levels above or below that threshold?',
    'Dose-response comparison across pilots and permanent programs at different payment levels, tracking refusal-of-bad-offers behavior and bargaining outcomes rather than headline labor-supply aggregates.',
    'If current levels are sub-threshold, the mild measured effects reflect an inert dose rather than a verified floor; the reading''s empirical support weakens and epsilon and type reassessment follows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_dose_threshold, empirical, 'Whether implemented payment levels are sufficient to produce the autonomy the reading promises.').

omega_variable(
    transfer_recapture_incidence,
    'How much of the gross transfer survives to recipients after price and rent adjustments — do landlords and local monopolists recapture part of the dividend?',
    'Local price and rent series around dividend disbursement events and program introductions, compared against matched control regions.',
    'Substantial recapture would mean declared beneficiaries receive less than the statutory transfer, hidden third-party gainers exist outside the named seats, and effective extraction for recipient seats rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_recapture_incidence, empirical, 'Whether second-round price effects leak the transfer away from the declared beneficiary seats.').

omega_variable(
    coalition_durability,
    'Will the cross-ideological coalition that sustains universality hold as fiscal conditions tighten, or does universality unravel into conditionality?',
    'Track legislative episodes: dividend raids, formula changes, means-testing creep, and pilot cancellations across jurisdictions and business cycles.',
    'Collapse of universality converts the arrangement into a conditional program — the referent of this reading changes, and reclassification of the successor arrangement follows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_durability, preference, 'Political durability of the unconditional character that defines this reading''s object.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t7, unconditional_income_support__freedom_floor_reading, theater_ratio, 7, 0.12).
narrative_ontology:measurement_basis(unco_tr_t7, observed).
narrative_ontology:measurement(unco_tr_t14, unconditional_income_support__freedom_floor_reading, theater_ratio, 14, 0.15).
narrative_ontology:measurement_basis(unco_tr_t14, observed).
narrative_ontology:measurement(unco_tr_t21, unconditional_income_support__freedom_floor_reading, theater_ratio, 21, 0.17).
narrative_ontology:measurement_basis(unco_tr_t21, observed).
narrative_ontology:measurement(unco_tr_t28, unconditional_income_support__freedom_floor_reading, theater_ratio, 28, 0.2).
narrative_ontology:measurement_basis(unco_tr_t28, observed).
narrative_ontology:measurement(unco_tr_t35, unconditional_income_support__freedom_floor_reading, theater_ratio, 35, 0.25).
narrative_ontology:measurement_basis(unco_tr_t35, observed).
narrative_ontology:measurement(unco_tr_t42, unconditional_income_support__freedom_floor_reading, theater_ratio, 42, 0.28).
narrative_ontology:measurement_basis(unco_tr_t42, observed).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t7, unconditional_income_support__freedom_floor_reading, base_extractiveness, 7, 0.17).
narrative_ontology:measurement_basis(unco_be_t7, observed).
narrative_ontology:measurement(unco_be_t14, unconditional_income_support__freedom_floor_reading, base_extractiveness, 14, 0.19).
narrative_ontology:measurement_basis(unco_be_t14, observed).
narrative_ontology:measurement(unco_be_t21, unconditional_income_support__freedom_floor_reading, base_extractiveness, 21, 0.2).
narrative_ontology:measurement_basis(unco_be_t21, observed).
narrative_ontology:measurement(unco_be_t28, unconditional_income_support__freedom_floor_reading, base_extractiveness, 28, 0.22).
narrative_ontology:measurement_basis(unco_be_t28, observed).
narrative_ontology:measurement(unco_be_t35, unconditional_income_support__freedom_floor_reading, base_extractiveness, 35, 0.24).
narrative_ontology:measurement_basis(unco_be_t35, observed).
narrative_ontology:measurement(unco_be_t42, unconditional_income_support__freedom_floor_reading, base_extractiveness, 42, 0.27).
narrative_ontology:measurement_basis(unco_be_t42, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' decomposes into three structurally distinct claims sharing one kernel. freedom_floor_reading (this file): autonomy-enabling floor, no victims claimed, moderate-low epsilon, rope. dependency_trap_reading: incentive-distorting subsidy, recipients as harmed party, high epsilon. universality_paradox_reading: coalition mechanics and fiscal convergence, agenda-setter-centered. The upstream/downstream structure runs from this reading outward: its cross-ideological appeal is the raw material the paradox reading theorizes, and its empirical claims are what the dependency reading directly contests. Each member carries its own epsilon, beneficiaries, and type; links here preserve the family for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
