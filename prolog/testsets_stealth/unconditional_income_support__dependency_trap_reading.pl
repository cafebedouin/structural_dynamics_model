% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support — Dependency Trap Reading
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   The standing arrangement under contest is the consolidating architecture
 *   of unconditional income support: a flat, universal cash payment delivered
 *   through a single channel, advanced through pilot programs, advocacy
 *   infrastructure, and legislative designs whose fiscal-offset schedules
 *   retire means-tested programs. This story instantiates the
 *   dependency_trap_reading of that kernel and authors epsilon FOR THAT
 *   ARRANGEMENT AS THIS READING ASSESSES IT — high, because the reading holds
 *   that the payment rewards labor-market exit at the margin (pooled
 *   large-pilot estimates near -3.2% employment), displaces targeted aid
 *   whose calibrated depth exceeds the flat amount for the poorest
 *   households, and routes a large net share of gross transfer to households
 *   with no income need, at roughly $1.4 trillion annual net cost after
 *   offsets. The referent is the universal-transfer arrangement itself, never
 *   the freedom-floor alternative this reading rejects; sibling readings are
 *   separate constraints linked in network.affects_constraints. Claim and
 *   metrics are independent authored facts: the type is claimed as snare from
 *   this reading's structural assessment, and the metrics describe the
 *   arrangement's operation as the reading's evidentiary tradition measures
 *   it; the engine computes per-seat classifications from the structural data
 *   and owns any divergence. KEY AGENTS (by structural relationship): -
 *   legislative_fiscal_authority: Agenda setter (institutional/mobile) —
 *   drafts transfer level and offset schedule, enforces program replacement -
 *   middle_upper_income_recipients: Primary beneficiary (powerful/arbitrage)
 *   — retains net transfers without income need - ubi_advocacy_networks:
 *   Secondary beneficiary (organized/identity_locked) — collects political
 *   capital from universality - working_poor: Primary target
 *   (moderate/constrained) — loses targeted-support depth exceeding the flat
 *   payment - net_taxpayers: Primary target (moderate/constrained) — bears
 *   the $1.4T annual net cost - caseworkers_and_local_aid_providers: Excluded
 *   voice (moderate/constrained) — sees unmet need daily, holds no design
 *   seat - fiscal_policy_analysts: Analytical observer
 *   (analytical/analytical) — sees full incidence and employment structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.78).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.62).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support — Dependency Trap Reading").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, 'd5cb8505-ed49-4633-a850-617c8c2b9d3a').
narrative_ontology:cs_kernel_codification('d5cb8505-ed49-4633-a850-617c8c2b9d3a', formalized).
narrative_ontology:cs_authority_grounding('d5cb8505-ed49-4633-a850-617c8c2b9d3a', expertise).
narrative_ontology:cs_interpretation_layer_present('d5cb8505-ed49-4633-a850-617c8c2b9d3a').
narrative_ontology:cs_reading_relation('d5cb8505-ed49-4633-a850-617c8c2b9d3a', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5cb8505-ed49-4633-a850-617c8c2b9d3a', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('d5cb8505-ed49-4633-a850-617c8c2b9d3a', foundational, unconditional_transfers_distort_labor_supply).
narrative_ontology:cs_axiom_status(unconditional_transfers_distort_labor_supply, holdable).
narrative_ontology:cs_axiom_grounding('d5cb8505-ed49-4633-a850-617c8c2b9d3a', unconditional_transfers_distort_labor_supply, empirically_contingent).
narrative_ontology:cs_axiom('d5cb8505-ed49-4633-a850-617c8c2b9d3a', foundational, universality_redistributes_upward_to_non_needy).
narrative_ontology:cs_axiom_status(universality_redistributes_upward_to_non_needy, holdable).
narrative_ontology:cs_axiom_grounding('d5cb8505-ed49-4633-a850-617c8c2b9d3a', universality_redistributes_upward_to_non_needy, empirically_contingent).
narrative_ontology:cs_axiom('d5cb8505-ed49-4633-a850-617c8c2b9d3a', secondary, targeted_aid_dominates_for_poverty_depth).
narrative_ontology:cs_axiom_status(targeted_aid_dominates_for_poverty_depth, holdable).
narrative_ontology:cs_axiom_grounding('d5cb8505-ed49-4633-a850-617c8c2b9d3a', targeted_aid_dominates_for_poverty_depth, instrumental).
narrative_ontology:cs_reference_frame('d5cb8505-ed49-4633-a850-617c8c2b9d3a', need_calibrated_targeted_baseline).
narrative_ontology:cs_drift_state('d5cb8505-ed49-4633-a850-617c8c2b9d3a', post_pilot_evidence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d5cb8505-ed49-4633-a850-617c8c2b9d3a', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_income_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_networks).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, net_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the annual payment level and the offset schedule that retires means-tested programs; enforces replacement through appropriations riders and eligibility statute amendments. Collects campaign support and coalition goodwill from the universality bloc while answering to taxpayer constituencies. Exit is procedural — repeal or redesign is available with a governing majority but electorally expensive once households budget around the payment.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, legislative_fiscal_authority, agenda_setter,
    institutional, biographical, mobile, national).

% Receive the same flat payment as the poorest households while progressive taxation claws back part of it; on net, much of the cohort retains a positive transfer despite having no income need. Their votes and donations anchor the cross-ideological coalition behind universality. Exit is trivial: they lose little if the program ends, and income planning shields part of the clawback.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_income_recipients, beneficiary,
    powerful, biographical, arbitrage, national).

% Think tanks, movement organizations, and philanthropic funders whose staffing, budgets, and public identities are built around advancing the universal payment. Universality itself delivers their legitimacy, media attention, and fundraising base. Stepping back would mean dismantling the organization's reason for being, so internal dissent from the universality framing is rare.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_networks, beneficiary,
    organized, generational, identity_locked, national).

% Currently stack targeted supports — housing assistance, nutrition benefits, earned-income top-ups, utility relief — whose combined value exceeds the flat payment. Under replacement they lose depth they cannot rebuild by working more, because rent, childcare, and health costs scale with need rather than with hours. Adding hours or moving helps at the margin, but every jurisdiction inside the program runs the same flat-payment arithmetic.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    moderate, immediate, constrained, national).

% Finance roughly $1.4 trillion in annual net cost after offsets through federal taxation. Organized taxpayer and deficit-reduction groups contest the outlay publicly, but individual households cannot leave the tax base without giving up residence or income sources; high earners shift part of the burden through mobility and planning, leaving the residual on less-mobile filers.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, net_taxpayers, payer,
    moderate, biographical, constrained, national).

% Run the means-tested programs slated for retirement — eligibility processing, housing placement, benefits counseling. They see daily which households need more than any flat payment and would testify that replacement strips calibrated support, but they hold no seat in the coalitions drafting the offset schedules.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, caseworkers_and_local_aid_providers, excluded,
    moderate, biographical, constrained, regional).

% Score proposals and run pilot evaluations — congressional scorekeepers, academic panels, think-tank meta-analyses. They see the full structure: gross cost, incidence by income decile, employment effects. Their findings feed both this reading's critique and its rivals', and their methods are contested by every camp.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, fiscal_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_upper_income_recipients).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the delivery problem of getting cash to residents: one uniform payment channel replaces the eligibility machinery of multiple means-tested programs, eliminating take-up gaps, application stigma, and per-program administrative cost. Stated without evaluation: whatever else it does, it coordinates income-floor delivery through a single universal pipe.
% TRANSFER_FUNCTION: Moves roughly $1.4 trillion annually (net of tax offsets) from the federal tax base to every resident household in equal flat payments; relative to the targeted system it displaces, it shifts marginal dollars away from the deepest-need households toward the broad middle, and delivers legitimacy, media attention, and fundraising capacity to the advocacy organizations that champion it.
% ABSENT_VOICES: Caseworkers and local aid providers who see daily which households need more than a flat payment would object to replacement design but hold no seat in the universality coalitions; deep-poverty households whose combined targeted supports exceed the flat payment are represented only statistically, not as negotiating parties; future cohorts who inherit the crowded-out safety-net capacity are absent entirely.
% DISAPPEARANCE_RATIONALE: Recipient households budget around the monthly payment; agencies and advocacy organizations are staffed and funded around it; the universality coalition's cross-ideological structure depends on continued receipt. Overnight removal would force re-budgeting by tens of millions of households, collapse the advocacy infrastructure built on the program, and reopen the fight over which targeted programs return.
% FOUNDING_PROBLEM: Means-tested welfare's failures: stigma that suppresses take-up, eligibility cliffs that punish work, bureaucratic complexity, and gaps that leave households below any floor — the arrangement was built to guarantee income unconditionally and administratively simply.
% FOUNDING_PROBLEM_CORROBORATION: Independent poverty measurement corroborates that the founding problem remains live: Census Supplemental Poverty Measure series, academic panel studies of income volatility, and take-up research from outside the benefiting parties all document persistent income insecurity and means-testing failures. No party outside the beneficiary set attests that the universal flat payment specifically solves it — pilot evaluations report mixed results, and the employment-effect meta-analysis this reading cites is itself disputed by the sibling readings' proponents.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at interval end) because the reading's referent arrangement combines three channels: behavioral (pooled large-pilot employment losses withdraw output from the tax base that funds the payment), distributive (flat payments to non-needy households survive clawback in material net amounts), and displacement (offset schedules retire targeted programs whose combined value for deep-need households exceeds the flat payment). Suppression (0.62) is authored as a raw structural property — unscaled by power or scope — reflecting the enforcement work the arrangement requires: retiring entrenched entitlements over recipient and provider opposition, and holding the universality coalition together against reform pressure from both fiscal conservatives and targeted-aid defenders. Theater_ratio (0.35) tracks pilot theater: small demonstrations (hundreds to thousands of recipients, fixed durations) rhetorically scaled to justify national permanence, a share of advocacy activity that has grown as pilots returned mixed results. Accessibility_collapse (0.50): the targeted-aid alternative persists wherever programs survive, but each offset schedule collapses it further, so alternatives are half-closed and closing. Resistance (0.60): organized taxpayer and deficit opposition, anti-poverty organizations defending program depth, and provider associations. The measurement series share one time grid (points 0, 4, 8, 12, 16, 20, 24) so every tracked metric is authored at every examined point; trajectories are monotone accumulation, not oscillation. Coalition note: the two victim seats could in principle combine — taxpayer cost and poverty-depth loss point at the same fix — but their preferred remedies diverge (smaller government vs. deeper targeting), and the universality framing exploits exactly that divergence, which is why the coalition never forms.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical metrics. Middle- and upper-income recipients sit near the beneficiary pole: the payment subsidizes them and their exit is arbitrage-grade, so effective extraction inverts toward subsidy. UBI advocacy networks are beneficiaries with identity-locked exit — ideologically fused with the arrangement, they experience no extraction and cannot imagine exit, which raises persistence independent of performance. The working poor and net taxpayers sit near the target pole: both bear costs (depth lost, taxes paid) with constrained exit, so effective extraction amplifies toward the full-target end, and national scope further amplifies it by making verification of who truly needs the floor harder. The agenda setter sits mid-range with electoral feedback running in both directions. Same-level divergence: working poor and net taxpayers hold the same nominal power atom yet occupy different structural relationships — the differentiation is exit-relevant specificity (the poor lose a determinate bundle; taxpayers bear a diffuse bill), which the engine reads from the declared roles and exits rather than from power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: middle_upper_income_recipients derive d near 0.0 (the payment is a net subsidy after clawback for much of the cohort) and ubi_advocacy_networks derive d near 0.1 (they collect legitimacy and funding rather than cash, but collect reliably). Victim declarations: working_poor derive d near 0.9 (they finance nothing directly but absorb the largest per-household loss when targeted depth is retired) and net_taxpayers derive d near 0.85 (they bear the $1.4T residual). The agenda setter derives a mid-range d: it administers the arrangement and collects coalition support from it while remaining answerable to taxed constituencies. No directionality overrides are authored: the derivation chain from roles, power, and exit options reproduces these positions without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem (income insecurity amid means-testing failures) is corroborated as live by measurement from outside the beneficiary set, so the arrangement has not outlived its mandate — it is contested at scale-up, not decayed. The snare claim functions as the guard against mislabeling: it refuses the rope reading (pure coordination of an income floor) by insisting the same structure that delivers the floor also displaces deeper aid and subsidizes the non-needy, and it refuses the piton reading by identifying concentrated receipts (non-needy recipients) and an active enforcement schedule (offset legislation) rather than inertial drift. The omegas keep the classification falsifiable: a stacking adoption path would thin the victim set toward tangled_rope; null pooled employment effects would cut the behavioral leg; a broken advocacy identity-lock would lower fixing_cost from prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the dependency_trap_reading of kernel unconditional_income_support; what structural facts would the sibling readings (freedom_floor_reading, universality_paradox_reading) change?',
    'Cross-reading comparison of authored victim/beneficiary sets, epsilon, and disappearance verdicts across the three stories linked in network.affects_constraints.',
    'freedom_floor_reading would vacate the victim set (no household loses; all gain a floor) and drive epsilon toward coordination-cost levels; universality_paradox_reading relocates the harm from class incidence to fiscal convergence, changing which seats count as targets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    employment_effect_magnitude,
    'Does unconditional income support materially reduce labor supply at national scale, as the -3.2% pooled estimate from large-pilot meta-analysis asserts?',
    'Long-run follow-ups on completed pilots (Finland 2017-2018, Stockton SEED, Alaska Permanent Fund dividend series) and any national-scale natural experiment; heterogeneity analysis separating liquidity effects from substitution effects.',
    'If pooled employment effects are near zero, the rewards-idleness premise weakens, epsilon falls, and the snare claim loses its behavioral leg; if effects are strongly negative at scale, the reading''s account firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_effect_magnitude, empirical, 'Contested magnitude of labor-supply response to unconditional cash.').

omega_variable(
    replacement_vs_stacking_design,
    'Do actual adoption paths replace targeted programs with the flat payment, or stack the payment atop them?',
    'Legislative text and fiscal-offset schedules of enacted and serious proposals; official scoring of which programs are sunset in each offset path.',
    'Under stacking, the working poor are net gainers, the victim set thins toward taxpayers alone, and the arrangement trends toward tangled_rope; under replacement, the declared victim structure stands and the snare reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replacement_vs_stacking_design, empirical, 'Whether the flat payment displaces or supplements means-tested aid — the crux of the working-poor victim claim.').

omega_variable(
    net_incidence_after_clawback,
    'After tax clawback, what share of gross transfer is retained by middle- and upper-income households, and is the upward-redistribution claim robust to incidence-modeling choice?',
    'Distributional incidence analysis across static and behavioral scoring models (decile tables under competing offset designs).',
    'If clawback recovers most gross payments to non-needy households, the middle_upper_income_recipient seat''s directionality rises toward symmetric and the upward-redistribution axiom weakens; if retention is large, the seat sits near full beneficiary and the snare reading firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_incidence_after_clawback, empirical, 'Net incidence of the universal payment across income deciles.').

omega_variable(
    advocacy_identity_lock_duration,
    'How durable is the advocacy networks'' identity lock — does it survive a sequence of disappointing or null pilot results?',
    'Observe funding trajectories, staffing, and public statements of major advocacy organizations following null-result publications; track whether reframing (rather than exit) dominates.',
    'If the lock breaks, the universality coalition fragments, reversal becomes legislatively cheaper, and fixing_cost drops from prohibitive; if it holds, persistence is insulated from evidence and the prohibitive cost class stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(advocacy_identity_lock_duration, empirical, 'Durability of ideological fusion binding advocacy organizations to the universality framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(unco_tr_t4, unconditional_income_support__dependency_trap_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__dependency_trap_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__dependency_trap_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__dependency_trap_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__dependency_trap_reading, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unco_be_t4, unconditional_income_support__dependency_trap_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__dependency_trap_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__dependency_trap_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__dependency_trap_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__dependency_trap_reading, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(unco_su_t4, unconditional_income_support__dependency_trap_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__dependency_trap_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__dependency_trap_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__dependency_trap_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(unco_su_t24, unconditional_income_support__dependency_trap_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'universal basic income' decomposes into three structurally distinct constraints sharing one kernel: this dependency-trap reading (high epsilon, victims = working poor + net taxpayers, type snare), freedom_floor_reading (epsilon near coordination cost, no victim set), and universality_paradox_reading (harm located in implementation-path convergence). Each file authors its own epsilon, beneficiaries, and victims per the epsilon-invariance principle; the family is linked through affects_constraints. The freedom-floor reading is upstream in public argument — its normative case is cited as evidence for adoption — while this reading contests the same adoption path downstream of the pilot-evidence era.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
