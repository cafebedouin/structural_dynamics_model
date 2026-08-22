% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Unconditional Income Support as Welfare Trap (Dependency-Trap Reading)
 *   domain: political economy / social policy / labor economics
 *
 * SUMMARY:
 *   This story instantiates the dependency_trap_reading of the kernel
 *   income_support_conditionality: the claim that unconditional income
 *   support undermines work incentives, producing long-term dependency and
 *   skill atrophy, with the arrangement's income-security framing serving as
 *   cover for a structure whose persistence depends on active administration
 *   and whose costs fall on identifiable parties. The standing arrangement
 *   under contest is the broad architecture of unconditional support as
 *   actually operated: universal or near-universal transfer instruments,
 *   unconditional floors with earnings tapers, and the pilot programs
 *   (Finland 2017-2018, Stockton SEED, GiveDirectly) that carry the policy
 *   debate. Assumption stated explicitly: epsilon and suppression are
 *   authored for that broad architecture including taper-mediated floors, not
 *   for a hypothetical pure no-taper transfer; the decomposition risk is
 *   carried in omega referent_conflation_strict_ubi_vs_means_tested_floor.
 *   KEY AGENTS (by structural relationship): -
 *   transfer_program_administrators: Agenda-setting beneficiary
 *   (institutional / identity_locked) — writes schedules and tapers, collects
 *   budgets and discretion - clientelist_political_coalitions: Secondary
 *   beneficiary (organized / immediate horizon) — collects electoral support
 *   from program defense - ubi_recipient_households: Primary target
 *   (powerless / trapped) — bears attachment and skill costs - net_taxpayers:
 *   Primary target (moderate / constrained) — bears the fiscal cost -
 *   future_taxpayer_cohorts: Excluded target (powerless / generational) —
 *   bears deferred costs with no seat - policy_evaluation_researchers:
 *   Analytical observer — sees the full structure across pilots and
 *   jurisdictions Family note (epsilon divergence, same referent):
 *   freedom_floor_reading would author low epsilon over this same arrangement
 *   (transfers read as purchasing the freedom to refuse work; the taxpayer
 *   contribution priced as a legitimate premium), and wage_subsidy_reading
 *   would author moderate epsilon with extraction relocated to competing
 *   labor-market entrants whose wages the transfer suppresses. This story
 *   authors high epsilon with a dual victim set. The divergence is
 *   reading-indexed valuation over one fixed referent, not measurement
 *   disagreement about a shared quantity.
 *
 * KEY AGENTS:
 *   - transfer_program_administrators: agenda-setting beneficiary (institutional/identity_locked) — administers schedules, tapers, and activation rules; budgets and missions scale with the programs
 *   - clientelist_political_coalitions: secondary beneficiary (organized/immediate) — converts program defense into electoral support
 *   - ubi_recipient_households: primary target (powerless/trapped) — net return to work compressed by tapers and costs; skills erode over long spells
 *   - net_taxpayers: primary target (moderate/constrained) — fund the transfers; influence limited to elections
 *   - future_taxpayer_cohorts: excluded target (powerless/generational) — inherit the fiscal legacy with no seat
 *   - policy_evaluation_researchers: analytical observer (analytical/global) — produce the causal evidence all readings selective cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.72).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.62).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Welfare Trap (Dependency-Trap Reading)").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political economy / social policy / labor economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '811ca3ae-2920-4000-a078-25f71ed7f325').
narrative_ontology:cs_kernel_codification('811ca3ae-2920-4000-a078-25f71ed7f325', formalized).
narrative_ontology:cs_authority_grounding('811ca3ae-2920-4000-a078-25f71ed7f325', distributed).
narrative_ontology:cs_reading_relation('811ca3ae-2920-4000-a078-25f71ed7f325', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('811ca3ae-2920-4000-a078-25f71ed7f325', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('811ca3ae-2920-4000-a078-25f71ed7f325', foundational, unconditional_transfers_erode_work_incentives).
narrative_ontology:cs_axiom_status(unconditional_transfers_erode_work_incentives, holdable).
narrative_ontology:cs_axiom_grounding('811ca3ae-2920-4000-a078-25f71ed7f325', unconditional_transfers_erode_work_incentives, empirically_contingent).
narrative_ontology:cs_axiom('811ca3ae-2920-4000-a078-25f71ed7f325', foundational, contributory_reciprocity_is_binding).
narrative_ontology:cs_axiom_status(contributory_reciprocity_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('811ca3ae-2920-4000-a078-25f71ed7f325', contributory_reciprocity_is_binding, conventional).
narrative_ontology:cs_axiom('811ca3ae-2920-4000-a078-25f71ed7f325', secondary, work_requirements_restore_incentive_structure).
narrative_ontology:cs_axiom_status(work_requirements_restore_incentive_structure, holdable).
narrative_ontology:cs_axiom_grounding('811ca3ae-2920-4000-a078-25f71ed7f325', work_requirements_restore_incentive_structure, instrumental).
narrative_ontology:cs_reference_frame('811ca3ae-2920-4000-a078-25f71ed7f325', work_conditioned_reciprocity).
narrative_ontology:cs_drift_state('811ca3ae-2920-4000-a078-25f71ed7f325', post_pandemic_unconditional_expansion, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('811ca3ae-2920-4000-a078-25f71ed7f325', '2026-06-11T09:30:00Z').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, transfer_program_administrators).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, clientelist_political_coalitions).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipient_households).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, net_taxpayers).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, future_taxpayer_cohorts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs benefit schedules, taper rates, and activation requirements, and administers eligibility and payment. Agency budgets, headcount, and regulatory discretion scale with caseload and program count. Careers and institutional missions are bound to the programs' continuation; winding the arrangement down would mean dismantling the organizations themselves. Collects salaries, budgets, and discretionary authority from the arrangement's operation.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, transfer_program_administrators, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, transfer_program_administrators, beneficiary).

% Assembles electoral majorities partly around defense and expansion of transfer programs, campaigning on benefit levels and against cuts. Receives votes, donations, and volunteer energy from constituencies served by the programs. Pivoting away would forfeit a reliable bloc; pivoting toward austerity invites primary challenges from within.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, clientelist_political_coalitions, beneficiary,
    organized, immediate, constrained, national).

% Receive periodic unconditional payments that anchor household budgets. Taking work triggers benefit reductions, childcare costs, and transport expenses that can absorb most of the gross wage gain, so the net return to a first job or a raise is small. Long spells outside employment erode routines, references, and skills. Leaving the system means a period with no income floor at all.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipient_households, payer,
    powerless, biographical, trapped, national).

% Fund the transfers through general taxation and receive no direct payment. Bear the fiscal cost and, on this reading's account, the drag from reduced labor supply. Influence over program design is limited to infrequent elections and blunt ballot measures; exit would require emigration.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, net_taxpayers, payer,
    moderate, biographical, constrained, national).

% Will service debt accumulated to finance current transfers and inherit a labor market whose participation norms were shaped by decades of support design. Hold no vote, seat, or organization in current design decisions; their interests enter only as projections made by others.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, future_taxpayer_cohorts, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, future_taxpayer_cohorts, excluded).

% Design and analyze pilots (negative income tax experiments, Finland's 2017-2018 trial, Stockton SEED, GiveDirectly villages) and publish estimates of employment, wellbeing, and fiscal effects. Professional standing rides on the debate staying live; findings feed every side. Hold no material stake in program size.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, policy_evaluation_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, transfer_program_administrators).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an income floor that does not vary with work status or eligibility screening: insures against job loss, disability, and old-age destitution without means-testing machinery, and stabilizes aggregate demand in downturns through automatic transfers.
% TRANSFER_FUNCTION: Moves purchasing power from net taxpayers to households at or below the margin of labor-market attachment; simultaneously moves labor-market attachment, skill accumulation, and future payroll-tax contributions out of recipient households (this reading's central claim), and case-management authority and budgetary discretion into the administrative apparatus.
% ABSENT_VOICES: Future taxpayer cohorts bear the fiscal legacy and the long-run participation effects but hold no seat; their interests appear only as projections by others. Former recipients who reattached to employment and now oppose the arrangement's design are likewise absent from design conversations, which are staffed by administrators, advocates, and incumbent coalitions.
% DISAPPEARANCE_RATIONALE: Household budgets anchored to the payments would collapse immediately; program bureaucracies and their vendor ecosystems would shed staff; political coalitions built on program defense would fracture; local economies scaled around transfer spending (healthcare providers, landlords in voucher markets) would contract; and labor supply, the wage floor, and the payroll-tax base would all shift as participation incentives changed.
% FOUNDING_PROBLEM: Mass income insecurity when wage labor fails: depression-era unemployment without savings, widespread old-age destitution before pensions, and the absence of any consumption floor for households between jobs.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and pre-transfer poverty statistics (elderly poverty rates before old-age pensions, 1930s unemployment surveys) attest the founding problem was real and severe. OECD and academic labor-supply analyses attest that the insurable core (job loss, disability, old age) remains live while the arrangement has expanded well past it; this corroboration comes from outside the benefiting parties. The recipient constituency itself attests only relief, not the expanded mandate; no member of the beneficiary set corroborates the current scope.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.72) because this reading prices two simultaneous outflows: fiscal resources moved from taxpayers without a corresponding productively-linked return, and labor-market attachment plus skill accumulation drained from recipient households over long support spells. Suppression (0.62) is authored as predominantly structural — earnings tapers, benefit cliffs, housing-allocation rules that punish the first steps back into work — with a minority internalized component (eroded self-efficacy and routine loss that persist after rules change); the split is estimated at roughly 70% structural to 30% internalized and is carried as an omega rather than resolved. Theater (0.30) reflects activation programming whose visible activity (courses, CV workshops, job-search monitoring) is real but partially substitutive for the incentive repair the reading demands. Accessibility_collapse (0.55) is mid-range: work remains nominally available but its net payoff collapses once tapers and costs are understood, while taxpayer-side alternatives (repeal) are blunted by clientelist defense. Resistance (0.60) is substantial and recurring: taxpayer revolts, work-requirement legislation, pilot evaluations cited against expansion, and claimant organizing on the other side.
 *   
 *   Temporal picture: the series run on one shared grid (nine points, 2017-2027, every tracked metric authored at every point). The 2020-2021 jump is the pandemic expansion (stimulus checks, supplemented unemployment payments, the unconditional child allowance); the 2022-2024 retreat tracks inflation-era retrenchment and work-requirement revival. This is modeled as an asymmetric crisis ratchet rather than a symmetric cycle: each emergency leg leaves the arrangement's baseline slightly larger (extraction settles at 0.72 versus the 0.66 pre-crisis level) while the enforcement apparatus re-hardens above its prior level (suppression_requirement ends projected at 0.62 versus 0.58 at start) — the intermittent-reinforcement shape in which each crisis leg strengthens both the transfer flow and the subsequent disciplinary response. The suppression_requirement series is authored because enforcement-capacity change is genuinely the traced dynamic here: eligibility machinery was suspended in 2020, then rebuilt and tightened past its starting point by 2024.
 *   
 *   Identity-lock note: the administrator seat carries identity_locked exit because the agencies have become their function — professional identities, vendor contracts, and career ladders are constituted by program administration, so the seat cannot price exit rationally even where legal authority to wind down exists.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seat should compute differently from the same statutes and schedules. From the administrator seat the arrangement is functioning coordination it personally operates: payments arrive, caseloads are managed, the floor holds. From the trapped recipient seat the same taper schedule operates as a wall around the exit. From the taxpayer seat it is a bill with attenuated representation. From the researcher seat it is unsettled empirical terrain. The engine computes these per-seat classifications from the authored structural data; the divergence across seats is the datum this corpus exists to take, and the authored snare claim is deliberately not reconciled to any seat's likely computed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: administrators derive low directionality from their beneficiary declaration (partly offset by their identity-locked entanglement), and political coalitions similarly low. Targets derive high directionality: recipient households combine the victim declaration with a trapped exit atom, placing them nearest the full-target end; net taxpayers combine the victim declaration with constrained exit; future cohorts, added to the victim set as the reading's generational fiscal-burden claim, derive high directionality from payer-plus-trapped structure. No directionality overrides are authored: the derivation chain from role plus exit reproduces the qualitative ordering, and overrides are keyed by power atom alone, so an override for the two powerless seats would collide (recipient households and future cohorts warrant similar-but-distinct values that the structural derivation already separates). Suppression is authored as a raw structural property and is not scaled; extractiveness alone is scaled by the engine through directionality and the national spatial scope, which modestly amplifies effective extraction for the target seats. Gain-flow note: the receipts land on the administrator seat (budgets, headcount, discretion), with coalitions accruing derivative electoral rents; recipients receive the gross transfers, but under this reading the transfer is the extraction vector itself, not retained gain — hence gain_flow names the administrative seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare claim guards against the mislabeling this reading most fears: a pure-coordination reading (rope) that would book the arrangement's insurance function and ignore the identifiable parties bearing its costs. The reverse error is equally live — a pure-extraction verdict that ignores the genuine insurable core (job loss, disability, old age) the arrangement still delivers for a subset of cases. The genealogy interview carries that nuance structurally: the founding problem (mass income insecurity when wage labor fails) is contested rather than dead, so the mismatch consumer reads status=contested against verdict=world_rearranges and correctly declines to fire the zombie flag — the arrangement still performs part of its founding function while overrunning it. The mandatrophy boundary is concrete: if the insurable core were fully displaced (private disability and pension coverage universal, unemployment insurance absorbing job loss) and only the taper-bound remainder persisted, the founding problem would flip to dead, the world_rearranges verdict would rest entirely on clientelist and administrative dependence, and the flag should fire. The theater_ratio trajectory (peak 0.33 at peak expansion, settling near 0.30) tracks the growth of performative activation activity alongside the core transfer flow, consistent with partial but not dominant theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (dependency_trap_reading) of the kernel income_support_conditionality; would instantiating a sibling reading against the same referent change the computed classification?',
    'Generate the sibling stories (freedom_floor_reading, wage_subsidy_reading) against the identical standing arrangement and compare per-seat computed types; the cross-reading delta is the indexical signal.',
    'freedom_floor_reading would empty the recipient victim set (recipients become beneficiaries; coerced workers become the victim seat) and likely compute a coordination-dominant profile; wage_subsidy_reading would relocate victims to competing labor-market entrants and beneficiaries to employers, computing a hybrid profile. Only this reading produces the dual-victim high-extraction profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel membership and expected cross-reading classification delta.').

omega_variable(
    labor_supply_elasticity_dispute,
    'What do the best causal estimates (1970s negative income tax experiments, Finland 2017-2018 trial, Stockton SEED, GiveDirectly village studies) actually establish about employment effects of unconditional support, and do effects grow with spell duration?',
    'Pooled re-analysis of the negative-income-tax archives plus long-horizon follow-ups of completed pilots; duration-dependence tested by comparing short-window treatment arms against multi-year arms.',
    'Near-zero short-run effects with no duration gradient would strip this reading''s causal engine and push the computed profile toward the freedom_floor shape; sizable negative effects growing with spell length would confirm the self-reinforcement premise and harden the extraction computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_dispute, empirical, 'Whether the incentive-erosion premise survives causal scrutiny.').

omega_variable(
    exit_cost_structural_vs_internalized,
    'Is the high cost of leaving the support system structural (earnings tapers, housing-allocation rules, childcare cliffs) or internalized (identity fusion with non-employment, eroded self-efficacy that persists after rules change)?',
    'Post-exit trajectory studies following households after rule changes that remove the structural barriers: if labor-market attachment does not recover once tapers are lifted, a substantial internalized component is established.',
    'An internalized component means effective suppression exceeds the structural measure and travels with the agent after exit; conditionality-only fixes would underperform, and the recipient seat sits closer to identity-lock than to a purely priced exit barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_structural_vs_internalized, empirical, 'Structural versus internalized share of the exit barrier.').

omega_variable(
    referent_conflation_strict_ubi_vs_means_tested_floor,
    'Does this reading''s epsilon measure one arrangement or two: strict unconditional transfers with no earnings taper (where the exit-barrier mechanism is weak) versus means-tested unconditional floors with high implicit marginal tax rates (where it is strong)?',
    'Decompose per the epsilon-invariance rule: author separate stories for a strict universal transfer and for a means-tested unconditional floor, each with its own epsilon, victim structure, and classification; compare against this story.',
    'If the referent is narrowed to strict unconditional transfers, epsilon falls materially (no taper, low exit cost) and the extraction-heavy computation likely fails; the trap profile is carried almost entirely by the taper-mediated variant. This story''s values assume the broad architecture including taper-mediated floors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(referent_conflation_strict_ubi_vs_means_tested_floor, conceptual, 'Referent-scope ambiguity inside the reading''s own label.').

omega_variable(
    recipient_coalition_potential,
    'Could recipient households convert diffuse individual powerlessness into coalition power (claimants'' unions, voting blocs) sufficient to alter the arrangement''s design, and does the arrangement''s administration actively impede that conversion?',
    'Comparative study of claimant-organizing episodes (UK claimants'' unions, US Poor People''s Campaign) and of administrative rules affecting organizing capacity, such as reporting requirements and scheduling conflicts between activation duties and meetings.',
    'Viable coalition formation would raise the payer seat''s power atom and lower effective extraction; demonstrated impedance of organizing would raise the suppression metric and strengthen the extraction-heavy profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recipient_coalition_potential, empirical, 'Coalition pathway available to the powerless payer seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 2017, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dep_trap_reading_tr_t2017, income_support_conditionality__dependency_trap_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2017, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t2018, income_support_conditionality__dependency_trap_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2018, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t2019, income_support_conditionality__dependency_trap_reading, theater_ratio, 2019, 0.27).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2019, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t2020, income_support_conditionality__dependency_trap_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2020, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t2021, income_support_conditionality__dependency_trap_reading, theater_ratio, 2021, 0.33).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2021, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t2022, income_support_conditionality__dependency_trap_reading, theater_ratio, 2022, 0.31).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2022, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t2023, income_support_conditionality__dependency_trap_reading, theater_ratio, 2023, 0.3).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2023, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t2024, income_support_conditionality__dependency_trap_reading, theater_ratio, 2024, 0.29).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2024, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t2027, income_support_conditionality__dependency_trap_reading, theater_ratio, 2027, 0.3).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t2027, projected).

% Extraction over time
narrative_ontology:measurement(dep_trap_reading_be_t2017, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2017, 0.66).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2017, observed).
narrative_ontology:measurement(dep_trap_reading_be_t2018, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2018, observed).
narrative_ontology:measurement(dep_trap_reading_be_t2019, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2019, 0.68).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2019, observed).
narrative_ontology:measurement(dep_trap_reading_be_t2020, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2020, observed).
narrative_ontology:measurement(dep_trap_reading_be_t2021, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2021, 0.76).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2021, observed).
narrative_ontology:measurement(dep_trap_reading_be_t2022, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2022, 0.73).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2022, observed).
narrative_ontology:measurement(dep_trap_reading_be_t2023, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2023, 0.72).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2023, observed).
narrative_ontology:measurement(dep_trap_reading_be_t2024, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2024, 0.71).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2024, observed).
narrative_ontology:measurement(dep_trap_reading_be_t2027, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2027, 0.72).
narrative_ontology:measurement_basis(dep_trap_reading_be_t2027, projected).

% Suppression requirement over time
narrative_ontology:measurement(dep_trap_reading_su_t2017, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2017, 0.58).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2017, observed).
narrative_ontology:measurement(dep_trap_reading_su_t2018, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2018, observed).
narrative_ontology:measurement(dep_trap_reading_su_t2019, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2019, 0.56).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2019, observed).
narrative_ontology:measurement(dep_trap_reading_su_t2020, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2020, observed).
narrative_ontology:measurement(dep_trap_reading_su_t2021, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2021, 0.46).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2021, observed).
narrative_ontology:measurement(dep_trap_reading_su_t2022, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2022, observed).
narrative_ontology:measurement(dep_trap_reading_su_t2023, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2023, 0.58).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2023, observed).
narrative_ontology:measurement(dep_trap_reading_su_t2024, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2024, observed).
narrative_ontology:measurement(dep_trap_reading_su_t2027, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2027, 0.62).
narrative_ontology:measurement_basis(dep_trap_reading_su_t2027, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel income_support_conditionality. The colloquial label 'unconditional income support' covers three structurally distinct claims with different epsilon values, victim sets, and classifications: this story (dependency_trap_reading, high epsilon, dual victim set, extraction-dominant claim), freedom_floor_reading (low epsilon, recipients as beneficiaries, decommodification claim), and wage_subsidy_reading (moderate epsilon, extraction concentrated on competing workers, incidence claim). All three readings share one referent, the standing unconditional-support arrangement, and author reading-indexed epsilon over it. The empirical pilot literature is upstream of all three; each reading cites the subset of evidence its premises select. Every family member links the others here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
