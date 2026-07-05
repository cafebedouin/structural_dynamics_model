% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Adaptation-Priority Reading of Climate Harm Prevention
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   Adaptation-priority policy frameworks (seen in national climate
 *   adaptation plans, resilience-bond financing, and 'loss and damage'
 *   negotiating postures from wealthy emitters) treat mitigation targets as
 *   aspirational rather than binding and direct the bulk of committed
 *   near-term fiscal capacity toward protecting existing populations and
 *   capital stock from climate harms already in the pipeline. The reading's
 *   coordination function is real — no government can protect people from
 *   harms that mitigation, even if pursued maximally, could not reverse in
 *   time — but the same infeasibility claim that grounds this coordination
 *   also serves fossil fuel incumbents and adaptation contractors whose
 *   interests are structurally aligned with mitigation continuing to be
 *   deferred.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.61).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.42).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.61).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-Priority Reading of Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '90c58766-3139-4cfc-b82d-6cfad3780487').
narrative_ontology:cs_kernel_codification('90c58766-3139-4cfc-b82d-6cfad3780487', distributed).
narrative_ontology:cs_authority_grounding('90c58766-3139-4cfc-b82d-6cfad3780487', distributed).
narrative_ontology:cs_reading_relation('90c58766-3139-4cfc-b82d-6cfad3780487', climate_harm_prevention__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('90c58766-3139-4cfc-b82d-6cfad3780487', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('90c58766-3139-4cfc-b82d-6cfad3780487', foundational, political_feasibility_bounds_legitimate_obligation).
narrative_ontology:cs_axiom_status(political_feasibility_bounds_legitimate_obligation, holdable).
narrative_ontology:cs_axiom_grounding('90c58766-3139-4cfc-b82d-6cfad3780487', political_feasibility_bounds_legitimate_obligation, instrumental).
narrative_ontology:cs_axiom('90c58766-3139-4cfc-b82d-6cfad3780487', foundational, present_identifiable_harm_takes_priority_over_diffuse_future_harm).
narrative_ontology:cs_axiom_status(present_identifiable_harm_takes_priority_over_diffuse_future_harm, holdable).
narrative_ontology:cs_axiom_grounding('90c58766-3139-4cfc-b82d-6cfad3780487', present_identifiable_harm_takes_priority_over_diffuse_future_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('90c58766-3139-4cfc-b82d-6cfad3780487', post_paris_agreement_voluntary_commitment_framework).
narrative_ontology:cs_drift_state('90c58766-3139-4cfc-b82d-6cfad3780487', post_2023_global_stocktake, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('90c58766-3139-4cfc-b82d-6cfad3780487', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_coastal_and_urban_populations_in_wealthy_states).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_infrastructure_contractors).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, national_treasuries_deferring_transition_costs).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_states).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, subsistence_agricultural_communities).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, political_feasibility_constraint_doctrine).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, discount_rate_legitimacy_of_near_term_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set budget allocations between adaptation and mitigation, citing legislative gridlock and short electoral cycles to justify front-loading resilience infrastructure (sea walls, drought-resistant agriculture, heat-response systems) over emissions reduction. They administer the funding formulas and can, in principle, shift the allocation; they bear none of the residual warming cost themselves.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, national_climate_ministries, agenda_setter,
    institutional, biographical, arbitrage, national).

% Receive flood defenses, cooling centers, and insurance backstops funded by the adaptation-priority allocation. Their near-term exposure to climate harm is substantially reduced; they have political voice and vote on the budgets that fund their own protection.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_coastal_and_urban_populations_in_wealthy_states, beneficiary,
    organized, biographical, constrained, national).

% Continue extraction and combustion operations largely unconstrained because the political consensus defers aggressive mitigation. They lobby to reinforce the 'mitigation is infeasible' framing that grounds the adaptation-priority reading, and capture a portion of adaptation-contract spending directly.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Win the sea-wall, levee, and resilience-infrastructure contracts that the adaptation-priority allocation funds. Their revenue stream depends on adaptation remaining the dominant policy frame rather than mitigation succeeding and shrinking the need for defensive infrastructure.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_infrastructure_contractors, beneficiary,
    organized, biographical, mobile, national).

% Inherit a higher equilibrium warming trajectory locked in by deferred mitigation. They bear the compounding residual climate damage — sea level rise, ecosystem collapse, extreme weather intensification — that today's adaptation spending does not address, and have no seat in the budget decisions being made now.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Lack the fiscal capacity to build the resilience infrastructure that wealthy states fund for themselves. They face the same or worse warming trajectory without the adaptation cushion, and their emissions were never the primary driver of the warming they now must absorb.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_states, payer,
    powerless, generational, trapped, national).

% Face existential territorial loss from sea level rise that no feasible adaptation budget can fully offset. They have repeatedly argued for mitigation priority at international fora but lack the negotiating leverage of major emitters and are structurally outvoted in the political-feasibility calculus that grounds this reading.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, small_island_states, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, small_island_states, excluded).

% Depend on stable rainfall and temperature patterns for survival agriculture. They receive minimal adaptation transfer relative to their exposure, since adaptation spending concentrates where political and economic returns are highest, not where vulnerability is greatest.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, subsistence_agricultural_communities, payer,
    powerless, biographical, trapped, regional).

% Document the warming trajectory implied by deferred mitigation and publish the residual-damage projections that the adaptation-priority framing must contend with. They have no enforcement power but supply the empirical record against which the 'infeasibility' claim is tested.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_scientists_and_ipcc_working_groups, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, present_coastal_and_urban_populations_in_wealthy_states).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates limited near-term fiscal and political capital on defending existing populations and infrastructure against climate harms that are already locked in or imminent, rather than dispersing that capital across a mitigation program judged unlikely to succeed at the pace required.
% TRANSFER_FUNCTION: Moves adaptation funding and protective infrastructure to present populations in states with fiscal capacity, while shifting the cost of the resulting higher warming trajectory onto future generations and onto present populations in low-capacity states and regions who receive little adaptation transfer.
% ABSENT_VOICES: Future generations have no seat by construction. Small island states and low-adaptation-capacity states are present in international fora but are structurally outvoted by the emitters whose 'political infeasibility' claim sets the frame; their objection — that mitigation deferral is a choice, not a constraint — is heard but not binding.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing were abandoned in favor of mitigation-priority, near-term budget allocations would shift sharply toward emissions infrastructure, fossil fuel incumbents would face accelerated stranded-asset exposure, adaptation contractors would see reduced near-term contract flow, and the warming trajectory bequeathed to future generations and low-capacity states would shift favorably — a substantial rearrangement of both present budgets and future outcomes.
% FOUNDING_PROBLEM: Given real constraints on political will, international coordination capacity, and near-term economic disruption tolerance, protect populations already facing climate harm rather than gambling protection on a mitigation program that may not be delivered in time.
% FOUNDING_PROBLEM_CORROBORATION: National climate ministries and adaptation contractors attest the infeasibility premise is live, citing repeated failure of binding international mitigation targets. Climate scientists and IPCC working groups, along with small island state delegations — both outside the beneficiary set — attest that the political infeasibility of mitigation is itself partly constructed by the same fossil fuel incumbents who benefit from the adaptation-priority frame, making the founding problem's status genuinely contested rather than a neutral empirical finding.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that the reading transfers real resources to present, largely wealthy populations while shifting the compounding cost of a higher warming trajectory onto future generations and low-capacity states who have no seat in the allocation decision — this is asymmetric but not total extraction, since a genuine coordination function (protecting people from harms mitigation cannot now reverse) is also present. Suppression (0.42) is moderate: there is no direct coercive mechanism preventing advocacy for mitigation, but the 'infeasibility' framing is actively defended by incumbents with lobbying capacity, and international fora structurally discount the voice of low-capacity states. Theater ratio (0.38) captures that some adaptation spending is performative — announced resilience initiatives that under-deliver relative to their press cycle — rising over the measured interval as political pressure to be seen addressing climate harm outpaces delivered protection.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (national climate ministries), this reading is coordination under real constraint — the responsible response to a mitigation program judged politically undeliverable. From the seat of low-adaptation-capacity states and future generations, the identical structure is an extraction mechanism: a political-feasibility claim that happens to track incumbent interests, used to justify deferring costs onto parties with no voice in the deferral decision. The engine computes both seats from the same structural data; the divergence is exactly what the framework is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Present wealthy-state populations and adaptation contractors sit near the beneficiary end: they receive concrete protective infrastructure funded now, with mobile or arbitrage-grade exit from the policy regime's downside. Fossil fuel incumbents benefit doubly — from continued extraction and from capturing adaptation-contract spending. Future generations and low-adaptation-capacity states sit at the target end: trapped exit (they cannot renegotiate a warming trajectory set by decisions made before they had standing), civilizational or generational time horizon, and no seat in the resource allocation that produces their exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protect people from harms already locked in, given real political constraints on rapid mitigation) may remain partly live — some harms genuinely cannot be reversed by mitigation alone regardless of political will. But the founding_problem_status is authored as contested rather than dead or fully live, because outside corroborators (IPCC scientists, small island state delegations) argue the infeasibility premise is itself partly manufactured by the beneficiaries of its persistence. This is the mandatrophy signal: a mandate that began as honest triage under constraint risks calcifying into a self-reinforcing justification for continued deferral, especially where adaptation-contract revenue and fossil fuel continuation both depend on the frame holding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infeasibility_claim_endogeneity,
    'Is the political and economic infeasibility of rapid mitigation an exogenous constraint this reading correctly responds to, or is the infeasibility itself partly produced by the same incumbents who benefit from the adaptation-priority allocation?',
    'Comparative political economy analysis: track whether lobbying expenditure and messaging campaigns by fossil fuel incumbents and adaptation contractors measurably shift legislative mitigation votes, and whether infeasibility claims track objective technical/economic constraints or track incumbent political investment.',
    'If infeasibility is substantially endogenous (manufactured), the coordination story weakens and the reading looks more like a tangled rope tilting toward snare; if infeasibility is substantially exogenous (a genuine binding constraint), the reading''s coordination function is more robust and its tangled-rope classification is more defensible as a real trade-off under constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infeasibility_claim_endogeneity, empirical, 'Whether mitigation infeasibility is a genuine external constraint or a constructed one that benefits identifiable parties.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does committing fiscal and political capital to adaptation-priority now structurally foreclose the mitigation-priority reading later, by consuming the capital and political capacity mitigation would have required, or do the two remain compatible parallel tracks?',
    'Budget-trajectory analysis: examine whether states pursuing adaptation-priority allocation show declining mitigation investment over the same period, versus states that pursue both tracks concurrently.',
    'If adaptation-priority measurably crowds out mitigation investment, the ''coexists_with'' relation to mitigation_priority understates the structural pressure this reading exerts; if the tracks are genuinely funded in parallel, coexistence is the accurate characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, empirical, 'Whether adaptation-priority spending crowds out mitigation spending in practice.').

omega_variable(
    discount_rate_legitimacy,
    'Is prioritizing present, identifiable, politically enfranchised populations over future, unidentifiable, unenfranchised populations a legitimate application of standard policy discounting, or an ethically arbitrary transfer that happens to track present power?',
    'This is fundamentally a normative question about intergenerational discount rates; it can be partially informed by philosophical literature on pure time preference in intergenerational ethics but is not fully resolvable empirically.',
    'If discounting future welfare is ethically legitimate, the reading''s cost-shifting to future generations is a defensible policy choice rather than extraction; if illegitimate, the same cost-shifting is better characterized as extraction dressed as prudence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discount_rate_legitimacy, preference, 'Whether standard discounting of future welfare against present welfare is ethically legitimate grounds for this reading''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t6, climate_harm_prevention__adaptation_priority, theater_ratio, 6, 0.26).
narrative_ontology:measurement(clim_tr_t12, climate_harm_prevention__adaptation_priority, theater_ratio, 12, 0.3).
narrative_ontology:measurement(clim_tr_t18, climate_harm_prevention__adaptation_priority, theater_ratio, 18, 0.33).
narrative_ontology:measurement(clim_tr_t24, climate_harm_prevention__adaptation_priority, theater_ratio, 24, 0.36).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t6, climate_harm_prevention__adaptation_priority, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(clim_be_t12, climate_harm_prevention__adaptation_priority, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(clim_be_t18, climate_harm_prevention__adaptation_priority, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(clim_be_t24, climate_harm_prevention__adaptation_priority, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_harm_prevention__adaptation_priority, suppression_requirement, 6, 0.33).
narrative_ontology:measurement(clim_su_t12, climate_harm_prevention__adaptation_priority, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(clim_su_t18, climate_harm_prevention__adaptation_priority, suppression_requirement, 18, 0.38).
narrative_ontology:measurement(clim_su_t24, climate_harm_prevention__adaptation_priority, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_harm_prevention kernel. adaptation_priority (this file) front-loads resilience expenditure for present populations and accepts a higher warming trajectory; mitigation_priority prioritizes emissions reduction within a growth framework to prevent future harm; degrowth_reading holds that mitigation within growth is physically/politically impossible and requires planned Global North contraction. Each reading has a distinct ε, beneficiary/victim structure, and stakeholder set, reflecting genuinely different structural claims rather than one constraint viewed from three angles. adaptation_priority structurally influences mitigation_priority (fiscal and political capital consumed by adaptation-priority spending is capital unavailable for mitigation investment) without foreclosing it (both tracks can be pursued in parallel with sufficient resources) — hence 'influences' rather than 'forecloses'. degrowth_reading's core premise (mitigation within growth is impossible) directly contradicts this reading's implicit premise (mitigation is merely politically infeasible now, not physically impossible under growth), producing tension short of full foreclosure since both can be held as live positions by different political factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
