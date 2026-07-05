% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Unconditional Income Floor as Autonomy and Exit-Capacity Guarantee
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the freedom_floor_reading of the
 *   income_support_commitment kernel: an unconditional income guarantee
 *   treated as an enabler of autonomy, exit capacity from bad employment
 *   relationships and abusive households, and dignity-preserving
 *   universality. Under this reading the floor's coordination problem is
 *   genuinely a coordination problem (setting the funding level and tax
 *   base), not a cover for extraction, and it has no victims — universality
 *   is precisely what removes the means-test stigma and administrative
 *   gatekeeping that the sibling readings treat as central. The employer and
 *   general-taxpayer seats bear real costs, but those costs are the ordinary
 *   cost of coordination, not extraction from a victim class.
 *
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
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Floor as Autonomy and Exit-Capacity Guarantee").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '593b6520-7c05-476d-a457-1cdd373a64c2').
narrative_ontology:cs_kernel_codification('593b6520-7c05-476d-a457-1cdd373a64c2', distributed).
narrative_ontology:cs_authority_grounding('593b6520-7c05-476d-a457-1cdd373a64c2', distributed).
narrative_ontology:cs_reading_relation('593b6520-7c05-476d-a457-1cdd373a64c2', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('593b6520-7c05-476d-a457-1cdd373a64c2', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('593b6520-7c05-476d-a457-1cdd373a64c2', foundational, unconditional_provision_preserves_dignity).
narrative_ontology:cs_axiom_status(unconditional_provision_preserves_dignity, holdable).
narrative_ontology:cs_axiom_grounding('593b6520-7c05-476d-a457-1cdd373a64c2', unconditional_provision_preserves_dignity, deontological).
narrative_ontology:cs_axiom('593b6520-7c05-476d-a457-1cdd373a64c2', foundational, exit_capacity_disciplines_employer_power).
narrative_ontology:cs_axiom_status(exit_capacity_disciplines_employer_power, holdable).
narrative_ontology:cs_axiom_grounding('593b6520-7c05-476d-a457-1cdd373a64c2', exit_capacity_disciplines_employer_power, empirically_contingent).
narrative_ontology:cs_reference_frame('593b6520-7c05-476d-a457-1cdd373a64c2', means_tested_conditionality_baseline).
narrative_ontology:cs_drift_state('593b6520-7c05-476d-a457-1cdd373a64c2', post_pilot_ubi_trials_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('593b6520-7c05-476d-a457-1cdd373a64c2', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_gig_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, domestic_abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_and_entrepreneurs).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, low_wage_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, low_wage_employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, unconditional_provision_preserves_dignity).
narrative_ontology:constraint_vindicates(income_support_commitment__freedom_floor_reading, exit_capacity_disciplines_employer_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform unremunerated household and care labor with no independent income stream and no exit from dependency on a partner or family member. An unconditional floor gives them their own income for the first time, converting exit from the household from a hypothetical to a live option regardless of whether they take it.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, constrained, national).

% Work unstable, algorithmically managed jobs with no floor beneath a bad week. The income guarantee gives them a reservation wage below which they will not accept work, letting them refuse the worst gigs without risking destitution.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_gig_workers, beneficiary,
    moderate, biographical, constrained, national).

% Are financially dependent on an abusive partner and cite lack of independent income as the primary barrier to leaving. An unconditional floor, paid to the individual rather than the household, functions as an exit fund that does not require proving abuse to a caseworker or meeting a means test administered by the same system the abuser controls.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, domestic_abuse_survivors, beneficiary,
    powerless, immediate, trapped, national).

% Want to pursue speculative, low-initial-return work (art, invention, small business) but cannot absorb the income gap during the startup period. The floor functions as patient capital for human effort that markets do not yet price, letting them attempt ventures that would otherwise require independent wealth.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_and_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Currently set wages and working conditions partly on the assumption that workers have no real alternative to accepting them. With a floor in place, they must offer wages and conditions competitive with simply not working that job, which raises labor costs at the bottom of the wage distribution. They fund this indirectly through the tax base that supports the floor and directly through upward wage pressure.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, low_wage_employers, payer,
    organized, biographical, constrained, national).

% Fund the floor through the general tax base. Because the benefit is universal rather than targeted, higher-income taxpayers pay in more than they draw out net, but face no means test, no stigma-driven underclaim, and no bureaucratic determination of their own eligibility.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, general_taxpayers, payer,
    organized, generational, constrained, national).

% Sets the payment level, funding mechanism, and disbursement schedule. Because eligibility is unconditional, the agency's caseload administration burden shrinks relative to means-tested systems it may have previously run; it retains agenda-setting power over the level and indexation of the floor, which is the live coordination problem this reading treats as genuine (not extraction).
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_administering_agency, agenda_setter,
    institutional, generational, analytical, national).

% Staff and institutions built around eligibility verification, fraud detection, and conditionality enforcement have a professional and budgetary stake in means-tested design. They are not consulted as beneficiaries of universality and would object that unconditional provision eliminates their function; their objection is institutional self-preservation rather than a claim about recipient welfare, but it is a real absent voice in this reading's framing.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, means_tested_welfare_bureaucracy, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools tax revenue to guarantee every individual a floor income independent of employment, household status, or means-tested proof of need, solving the collective problem that markets and households do not reliably protect people below a survivable income line.
% TRANSFER_FUNCTION: Moves general tax revenue (disproportionately from higher earners and employers who benefit from a captive low-wage labor supply) to every individual as an unconditional payment, with the largest behavioral effect on those with the least prior independent income or exit capacity.
% ABSENT_VOICES: The means-tested welfare bureaucracy, whose institutional function is displaced by universality, is not part of the coordination this reading describes; some employers who rely on low-exit-capacity labor would object to the wage pressure but are represented here as payers, not excluded.
% DISAPPEARANCE_RATIONALE: If the unconditional floor disappeared, caregivers would lose their only independent income, gig workers would lose their reservation wage floor, abuse survivors would lose their most direct exit fund, and low-wage employers would regain full leverage over wage-setting at the bottom of the market — arrangements throughout households and labor markets are built on its presence.
% FOUNDING_PROBLEM: Markets do not price unpaid care labor, means-tested welfare systems impose stigma and administrative barriers that suppress uptake among those who need support most, and financial dependency traps people (especially in abusive households) who have no independent exit route.
% FOUNDING_PROBLEM_CORROBORATION: Domestic violence advocacy organizations independently corroborate that lack of independent income is the most commonly cited barrier to leaving an abusive relationship, prior to and outside any income-support policy debate. Labor economists studying reservation wages and monopsony power in low-wage labor markets independently corroborate that income floors shift bargaining power, a finding that predates and is external to advocacy for universal payments.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.18) and rising only marginally over the interval because the floor's primary effect, in this reading, is redistribution toward independent income and bargaining leverage rather than rent capture by any administering party. Suppression is very low (0.08) because unconditional provision requires no compliance monitoring of recipients. Theater ratio is low and flat (0.12) because there is minimal proxy-goal substitution; the visible activity (disbursement, indexation) tracks the real function (income floor) closely. Accessibility collapse is low (0.15) because the floor does not foreclose alternative income strategies — recipients can still work, start ventures, or not, without penalty.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (caregivers, gig workers, survivors, artists) sit near the full-beneficiary end of directionality because the payment flows to them net of what they contribute, and their prior exit options were narrowly constrained or trapped. Payers (employers, taxpayers) sit nearer symmetric-to-target because they fund the transfer, but this reading declares no victims: the employer's loss of below-market wage-setting leverage is the intended discipline of the mechanism, not an extraction from an innocent party — it is the coordination function operating as designed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy in the other direction from typical welfare-state critiques: rather than a founding problem going dead while the arrangement persists as capture, the founding problem (unpriced care labor, dependency traps, means-test stigma) is authored as live and independently corroborated by sources outside the beneficiary set (domestic violence advocates, labor economists). There is no zombie-mandate signal here because status=live pairs with verdict=world_rearranges, which is the non-mismatch case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_response_magnitude,
    'Does an unconditional floor meaningfully reduce labor supply at the margin (supporting the dependency_trap_reading) or does it primarily raise the reservation wage without reducing overall participation (supporting this reading''s exit-capacity framing)?',
    'Randomized or natural-experiment basic income trials with labor-supply tracking over multi-year horizons, distinguishing short-run adjustment from steady-state participation.',
    'If labor supply falls substantially and durably, the dependency_trap_reading''s core empirical premise strengthens and this reading''s low-ε claim becomes harder to sustain against evidence of atrophied skills or reduced attachment; if supply effects are modest and concentrated in caregiving/exit contexts, this reading''s framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_magnitude, empirical, 'Whether observed labor-supply effects favor the freedom-floor or dependency-trap reading.').

omega_variable(
    universal_vs_targeted_efficiency_tradeoff,
    'Is the administrative and stigma cost this reading attributes to means-testing large enough to justify the fiscal cost of paying the floor to people who do not need it, relative to the targeting_efficiency_reading''s claim that need-concentration is the more just and sustainable design?',
    'Comparative cost-benefit analysis of universal vs. means-tested programs accounting for administrative overhead, uptake rates among eligible non-claimants, and stigma-driven underclaim, versus the fiscal cost of paying non-needy recipients.',
    'If administrative and stigma costs of targeting are shown to exceed the fiscal cost of universality, this reading''s efficiency claim strengthens relative to targeting_efficiency_reading; if targeting can be made low-stigma and high-uptake, the case for universality over targeting weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_targeted_efficiency_tradeoff, empirical, 'Whether universal provision is actually more efficient than well-designed targeting.').

omega_variable(
    genuine_kernel_vs_political_framing,
    'Is ''income_support_commitment'' a single underlying policy kernel with genuinely competing structural readings, or is the appearance of a shared kernel itself a political-rhetorical convenience that obscures that these are three different policies (universal basic income, conditional welfare, targeted transfers) dressed as readings of one thing?',
    'Trace whether historical policy proposals under each reading converge on the same benefit level, funding mechanism, and eligibility rule, or diverge into materially different policy instruments.',
    'If the three readings converge on materially different policy instruments rather than different interpretations of the same instrument, the kernel framing itself may be an artifact of debate framing rather than a genuine shared commitment — this would not change this story''s ε but would affect how the sibling network should be modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_kernel_vs_political_framing, conceptual, 'Whether the kernel is a genuine shared commitment or a rhetorical merger of distinct policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__freedom_floor_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__freedom_floor_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__freedom_floor_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__freedom_floor_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(inco_be_t4, income_support_commitment__freedom_floor_reading, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__freedom_floor_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__freedom_floor_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__freedom_floor_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_commitment__freedom_floor_reading, targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_commitment kernel. freedom_floor_reading (this file) authors low ε, no victims, rope classification, emphasizing exit capacity and dignity. dependency_trap_reading authors the same policy instrument as producing skill atrophy and dependence — likely higher ε with the recipient population reframed as a victim class of the dependency mechanism itself. targeting_efficiency_reading authors universality as the extractive/inefficient element, with non-needy universal recipients as a beneficiary class whose inclusion imposes cost on a more efficient targeted alternative. All three share the underlying policy object but diverge in beneficiary/victim assignment and in claimed type; they are linked here rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
