% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Unconditional Income Floor as Autonomy and Exit-Capacity Enabler
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the freedom_floor_reading of the
 *   income_support_commitment kernel: unconditional income support treated as
 *   an enabler of exit capacity and dignity rather than as a work
 *   disincentive (the dependency_trap_reading) or as a mistargeted transfer
 *   better replaced by means-tested efficiency (the
 *   targeting_efficiency_reading). Under this reading's own lights, the
 *   standing arrangement being evaluated is the current
 *   conditional/means-tested welfare landscape that this reading contests,
 *   assessed for its coordination failure: people without independent income
 *   cannot credibly exit bad employment or household arrangements. The
 *   reading's ε is authored low because universality removes the stigma and
 *   gatekeeping machinery that generates most measured extraction in
 *   conditional systems — there is no victim class harmed by the floor's
 *   operation, only payers bearing a funding cost proportionate to ability to
 *   pay.
 *
 * KEY AGENTS:
 *   - unpaid_caregivers: beneficiary, gains independent bargaining position
 *   - precarious_workers: beneficiary, gains reservation-wage leverage
 *   - domestic_abuse_survivors: beneficiary, gains credible exit fund
 *   - artists_and_solo_entrepreneurs: beneficiary, gains creative/entrepreneurial runway
 *   - low_wage_employers: payer, loses some wage-setting latitude
 *   - general_taxpayers: payer, funds through progressive tax schedule
 *   - welfare_administering_agency: agenda_setter, designs and disburses
 *   - means_tested_welfare_bureaucracy: excluded, institutional interest not represented
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.08).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Floor as Autonomy and Exit-Capacity Enabler").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5').
narrative_ontology:cs_kernel_codification('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', distributed).
narrative_ontology:cs_authority_grounding('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', distributed).
narrative_ontology:cs_reading_relation('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', foundational, exit_capacity_constitutes_real_freedom).
narrative_ontology:cs_axiom_status(exit_capacity_constitutes_real_freedom, holdable).
narrative_ontology:cs_axiom_grounding('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', exit_capacity_constitutes_real_freedom, deontological).
narrative_ontology:cs_axiom('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', secondary, unconditional_provision_eliminates_stigma_extraction).
narrative_ontology:cs_axiom_status(unconditional_provision_eliminates_stigma_extraction, holdable).
narrative_ontology:cs_axiom_grounding('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', unconditional_provision_eliminates_stigma_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', conditional_means_tested_welfare_baseline).
narrative_ontology:cs_drift_state('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', contemporary_ubi_pilot_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('7755e9f3-3602-4c5f-8ac9-f39de8fcfbb5', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, domestic_abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_and_solo_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, low_wage_employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, universal_provision_eliminates_means_test_stigma).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, exit_capacity_rebalances_wage_bargaining).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform unwaged household and family labor that markets do not price. An unconditional floor gives them income independent of a spouse's or employer's goodwill, converting a previously unpaid, unexitable role into one with a real fallback and a modest degree of bargaining leverage inside the household.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, constrained, national).

% Hold insecure, low-wage, or gig-classified jobs where refusing bad terms means going without income entirely. The floor lets them decline exploitative shifts, abusive employers, or unsafe conditions without facing destitution, shifting some negotiating power at the margin from employer to worker.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Often remain in abusive households because leaving means losing shared income with no independent means of subsistence. An unconditional, individually-paid floor is money the abuser cannot administratively withhold or condition, materially improving the credibility of an exit threat and, for some, enabling actual departure.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, domestic_abuse_survivors, beneficiary,
    powerless, immediate, trapped, national).

% Pursue creative or entrepreneurial work with irregular or absent income during startup and development phases. A guaranteed floor functions as unconditional seed runway, permitting risk-taking and non-market-validated work that would otherwise require either family wealth or abandonment of the project.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_and_solo_entrepreneurs, beneficiary,
    moderate, biographical, constrained, national).

% Currently benefit from a labor supply with limited ability to refuse low wages or poor conditions. A credible income floor raises the effective reservation wage and forces some employers to raise pay, improve conditions, or automate roles they previously staffed cheaply. They experience the floor as a cost imposed on their wage-setting latitude, not as extraction from a victim class — it is a shift in bargaining position, not a transfer taken from them by force.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, low_wage_employers, payer,
    organized, biographical, constrained, national).

% Fund the floor through the general tax base. Most are themselves eligible recipients under universality, so the transfer is substantially circular for the median taxpayer; the net fiscal burden falls more heavily on higher earners through the tax schedule that funds it, not on a targeted victim group.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, general_taxpayers, payer,
    moderate, generational, constrained, national).

% Designs and disburses the unconditional payment, sets the funding mechanism and benefit level, and answers to legislatures over the tax base required. Administrative burden per recipient is far lower than means-tested alternatives because there is no eligibility verification to police.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_administering_agency, agenda_setter,
    institutional, generational, analytical, national).

% Staff and institutions built around eligibility verification, fraud investigation, and conditionality enforcement for the prior means-tested system would see their function shrink or disappear under universal provision. Their institutional interest in retaining conditionality is not represented in this reading's coordination account, though it shapes political resistance to the floor.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, means_tested_welfare_bureaucracy, excluded,
    organized, biographical, trapped, national).

% Study labor-supply response, fiscal sustainability, and distributional effects of universal versus targeted transfers, informing the ongoing kernel contest between this reading and its siblings.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective problem that market wages and household bargaining leave some people with no credible exit option — no fallback income independent of a specific employer or partner — which suppresses their real freedom to refuse bad terms even when refusal would be individually and socially rational.
% TRANSFER_FUNCTION: Moves tax revenue, weighted toward higher earners through the funding schedule, into an unconditional per-person payment; because provision is universal, most payers are also recipients, so the net transfer is smaller and more diffuse than a targeted program's headline figure suggests.
% ABSENT_VOICES: The means-tested welfare bureaucracy whose institutional function contracts under universality is not a party to the coordination account and would object on institutional-survival grounds; low-wage employers who benefit from workers' current lack of exit capacity would object on cost grounds and are represented here as payers, not excluded, since their objection is captured in the stakeholder set.
% DISAPPEARANCE_RATIONALE: If the floor disappeared, caregivers would lose independent income and household bargaining power, precarious workers would face a harder floor under wage negotiation, abuse survivors would lose a credible exit fund, and artists/entrepreneurs would lose runway for non-market-validated work — labor markets would revert to a lower effective reservation wage and employers would regain unilateral wage-setting latitude at the bottom of the market.
% FOUNDING_PROBLEM: Market wages and unpaid household labor leave categories of people (caregivers, the precariously employed, those trapped in abusive dependency, and those pursuing unremunerated creative or entrepreneurial work) with no independent subsistence floor, making their formal legal freedoms to exit bad arrangements practically unusable.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying wage-setting power and domestic violence researchers studying economic barriers to leaving abusive relationships corroborate that lack of independent income is a persistent, measurable barrier to exit, independently of advocacy for any particular income-support design; this corroboration comes from empirical researchers outside the constituency of direct beneficiaries.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18) and falling slightly over the interval because universality removes the administrative gatekeeping (means-testing, sanctions, conditionality reviews) that generates measurable extraction and stigma in targeted alternatives — as the floor matures politically and administratively, per-recipient overhead and associated theater (compliance monitoring, eligibility churn) both decline. Suppression is low (0.08): nothing about the floor's operation depends on coercing participants or foreclosing exits — quite the opposite, the floor's function IS expanding exit options. Resistance (0.35) is moderate and comes from payer-side actors (employers whose wage-setting latitude narrows, and the means-tested bureaucracy whose function is displaced), not from beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (caregivers, precarious workers, survivors, artists/entrepreneurs) sit near the full-beneficiary end: the floor subsidizes their exit capacity directly and unconditionally. Low-wage employers and general taxpayers are payers, but structurally distinct from a 'victim' class — this reading declares no victims because the transfer is a funding-and-bargaining reallocation, not an extraction from an identifiable harmed party; employers pay a cost of adjusted wage-setting power, not damages for wrongdoing. Taxpayers are substantially the same population as beneficiaries under universality, which the derivation should reflect as largely self-funding for the median household.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of independent subsistence floor undermining exit capacity) remains live by this reading's own lights — labor-market precarity and domestic-abuse economic entrapment are not solved problems. This blocks the mandatrophy read that would apply if the coordination function had been achieved and the constraint persisted only administratively; instead this reading treats the current absence-of-floor as the still-unsolved coordination failure the constraint (once implemented) would resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_response_magnitude,
    'How large is the actual labor-supply reduction (if any) from an unconditional floor, and does it represent voluntary exit from exploitative arrangements (consistent with this reading) or genuine withdrawal from productive labor (consistent with the dependency_trap_reading)?',
    'Pilot and natural-experiment data (basic income trials, negative income tax experiments, unconditional cash transfer studies) measuring hours worked, job quality transitions, and self-reported reasons for withdrawal.',
    'If withdrawal is concentrated among people leaving abusive or exploitative arrangements or pursuing caregiving/entrepreneurship, this reading is empirically supported. If withdrawal is concentrated among people simply working less for the same conditions, the dependency_trap_reading gains support and the same intervention would need re-scoring at higher effective extractiveness from a productivity-loss framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_magnitude, empirical, 'Whether observed labor-supply effects support the freedom-enabling account or the disincentive account.').

omega_variable(
    universality_vs_targeting_efficiency_tradeoff,
    'Does universal provision''s stigma-elimination and administrative-simplicity benefit outweigh the fiscal inefficiency of paying benefits to non-needy recipients, relative to well-targeted means-tested alternatives?',
    'Comparative fiscal and welfare-outcome analysis across universal and targeted transfer designs, accounting for administrative cost, take-up rates, and stigma-driven non-take-up in targeted systems.',
    'If targeting can be achieved without meaningful stigma or take-up loss, the targeting_efficiency_reading''s core premise is strengthened at this reading''s expense; if targeting inherently reproduces stigma and gatekeeping extraction, this reading''s low-ε claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_vs_targeting_efficiency_tradeoff, conceptual, 'Whether universality''s coordination gains outweigh targeting''s fiscal efficiency, and what that implies for comparative ε across readings.').

omega_variable(
    employer_cost_as_extraction_or_rebalancing,
    'Is the wage-setting cost borne by low-wage employers properly classified as extraction from them, or as removal of a pre-existing extraction they held over workers?',
    'Analysis of pre-floor wage-setting power asymmetry: if employers'' prior wage-setting latitude itself depended on workers'' lack of exit options, the floor is better read as correcting a prior extraction rather than creating a new one.',
    'If the correction framing holds, employer ''costs'' should not be treated as symmetric harm alongside genuine victim classes, supporting this reading''s declaration of zero victims. If employers can show the pre-floor wage level was itself a fair market equilibrium, the payer classification approaches something closer to a victim, which would push this story toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employer_cost_as_extraction_or_rebalancing, conceptual, 'Whether employer cost from constrained wage-setting is extraction or is the removal of employers'' own prior structural extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__freedom_floor_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__freedom_floor_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__freedom_floor_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__freedom_floor_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(inco_be_t4, income_support_commitment__freedom_floor_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__freedom_floor_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__freedom_floor_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__freedom_floor_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_commitment kernel (ε-invariance decomposition per DP-001): freedom_floor_reading (this story, rope, low ε), dependency_trap_reading (work-disincentive framing, likely tangled_rope or snare from its own account of induced dependency), and targeting_efficiency_reading (contests universality itself on fiscal-efficiency grounds, likely rope or tangled_rope depending on administrative burden framing). Each reading authors its own ε over the same underlying policy debate but from a distinct structural account of what the standing arrangement does; they are linked here rather than merged because merging would violate ε-invariance (the label 'universal basic income debate' covers three structurally distinct claims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
