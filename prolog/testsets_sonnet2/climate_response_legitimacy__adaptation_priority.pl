% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Reading of Climate Response Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint captures the 'adaptation-priority' reading of the
 *   contested climate response legitimacy kernel: the position that a
 *   legitimate climate response accepts the current warming trajectory as
 *   substantially locked in and directs resources toward protecting
 *   vulnerable populations through resilience infrastructure rather than
 *   centering binding emissions reduction or structural economic
 *   transformation. This reading has a genuine coordination function —
 *   resilience investment saves lives now, on a horizon shorter than
 *   mitigation payoffs. But it also structurally benefits wealthy,
 *   carbon-intensive economies by relieving them of the deeper transfer
 *   obligations that binding mitigation or loss-and-damage compensation would
 *   require, while low-income frontline regions and small island states enter
 *   the victim set immediately via the ~$350B annual adaptation finance gap.
 *   Costs to future generations are deferred but compounded: every year of
 *   accepted-rather-than-averted warming raises the total adaptation burden
 *   inherited later.
 *
 * KEY AGENTS:
 *   - wealthy_carbon_incumbent_economies: agenda-setter and primary beneficiary — sets finance architecture, preserves development model
 *   - fossil_fuel_dependent_industries: beneficiary — faces no binding mitigation mandate under this framing
 *   - adaptation_finance_and_engineering_contractors: beneficiary — revenue depends on adaptation deficit persisting
 *   - low_income_climate_frontline_regions: primary payer — bears physical impact, underfunded protection
 *   - small_island_states: payer — existential territorial loss the adaptation frame cannot address
 *   - future_generations_post_2050: payer — inherits compounded warming and adaptation debt
 *   - climate_vulnerable_national_governments: excluded — preferred remedy structurally unactionable
 *   - climate_science_and_policy_analysts: observer — quantifies the gap, no redirection authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.52).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Reading of Climate Response Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '23dedf81-2072-4f5f-a147-07b2b31e90d4').
narrative_ontology:cs_kernel_codification('23dedf81-2072-4f5f-a147-07b2b31e90d4', distributed).
narrative_ontology:cs_authority_grounding('23dedf81-2072-4f5f-a147-07b2b31e90d4', distributed).
narrative_ontology:cs_reading_relation('23dedf81-2072-4f5f-a147-07b2b31e90d4', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('23dedf81-2072-4f5f-a147-07b2b31e90d4', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('23dedf81-2072-4f5f-a147-07b2b31e90d4', foundational, warming_trajectory_is_substantially_locked_in).
narrative_ontology:cs_axiom_status(warming_trajectory_is_substantially_locked_in, holdable).
narrative_ontology:cs_axiom_grounding('23dedf81-2072-4f5f-a147-07b2b31e90d4', warming_trajectory_is_substantially_locked_in, empirically_contingent).
narrative_ontology:cs_axiom('23dedf81-2072-4f5f-a147-07b2b31e90d4', foundational, protecting_the_already_vulnerable_takes_priority_over_averting_future_harm).
narrative_ontology:cs_axiom_status(protecting_the_already_vulnerable_takes_priority_over_averting_future_harm, holdable).
narrative_ontology:cs_axiom_grounding('23dedf81-2072-4f5f-a147-07b2b31e90d4', protecting_the_already_vulnerable_takes_priority_over_averting_future_harm, deontological).
narrative_ontology:cs_reference_frame('23dedf81-2072-4f5f-a147-07b2b31e90d4', post_paris_agreement_finance_architecture).
narrative_ontology:cs_drift_state('23dedf81-2072-4f5f-a147-07b2b31e90d4', contemporary_adaptation_gap_reporting_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23dedf81-2072-4f5f-a147-07b2b31e90d4', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_carbon_incumbent_economies).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_dependent_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_finance_and_engineering_contractors).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_climate_frontline_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations_post_2050).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, warming_trajectory_is_now_fixed).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, resilience_investment_is_the_responsible_response).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of international climate finance and diplomatic framing, channeling resources toward adaptation and resilience projects rather than binding emissions cuts or growth-model change. Preserves its existing energy and growth model while funding a fraction of the adaptation gap it helped create, framing this as pragmatic and humane given 'already locked in' warming.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_carbon_incumbent_economies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, wealthy_carbon_incumbent_economies, beneficiary).

% Continues extraction and combustion activity largely undisturbed because the adaptation frame treats further warming as accepted background condition rather than a preventable outcome requiring their curtailment. Faces no binding mitigation mandate under this reading; lobbies to keep the policy conversation centered on resilience spending rather than phase-out timelines.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_fuel_dependent_industries, beneficiary,
    powerful, biographical, arbitrage, global).

% Wins contracts to build seawalls, early-warning systems, drought-resistant agriculture programs, and resettlement infrastructure funded by adaptation finance flows. Their revenue depends on the adaptation deficit persisting and being addressed project-by-project rather than the underlying warming being halted.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_finance_and_engineering_contractors, beneficiary,
    organized, biographical, mobile, global).

% Faces the physical brunt of accepted warming — floods, droughts, heat, crop failure — while receiving only a fraction (roughly a third by most estimates) of the adaptation finance the gap requires. Cannot migrate en masse, cannot self-finance resilience infrastructure at scale, and has no seat that sets the global emissions trajectory it must adapt to. Exit from the arrangement means exit from the international finance system itself, which is not a real option.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_climate_frontline_regions, payer,
    powerless, biographical, trapped, regional).

% Faces existential territorial loss from a warming trajectory the adaptation-priority reading treats as accepted and to be managed rather than reversed. Diplomatically vocal and morally central in UNFCCC forums but structurally powerless to alter the emissions decisions made in wealthy incumbent economies; adaptation cannot address the specific harm of land disappearing.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, small_island_states, payer,
    powerless, civilizational, trapped, national).

% Inherits a higher-warming baseline than would exist under an aggressive mitigation trajectory, plus whatever adaptation infrastructure survives and whatever adaptation debt remains unfunded. Has no representation in present decisions; the intergenerational transfer compounds because deferred mitigation raises the total physical damage this generation must adapt to further.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations_post_2050, payer,
    powerless, civilizational, trapped, global).

% Would prefer binding mitigation commitments or loss-and-damage compensation over discretionary adaptation grants, but lack the negotiating leverage inside the finance architecture that wealthy donors and multilateral development banks control. Their preferred framing — that they are owed compensation for a trajectory they did not cause — is heard in forums but rarely shapes the finance instruments actually disbursed.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_vulnerable_national_governments, excluded,
    moderate, generational, constrained, national).

% Model the adaptation gap, track finance flows against pledged targets, and document how the adaptation-priority framing interacts with mitigation ambition. Can quantify the $350B annual adaptation finance gap and the compounding cost of deferred mitigation, but has no authority to redirect finance flows or emissions policy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_science_and_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, wealthy_carbon_incumbent_economies).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates real, urgently needed protective investment — seawalls, early-warning systems, drought-resilient agriculture, heat management — for populations already experiencing climate impacts, using finite global climate finance where it can save lives now rather than only pursuing longer-horizon emissions reduction.
% TRANSFER_FUNCTION: Moves a portion of global climate finance toward resilience infrastructure in vulnerable regions, while allowing wealthy, carbon-intensive economies to avoid the deeper transfer that binding emissions cuts or reparative loss-and-damage payments would require. Net effect: frontline populations receive partial protection funding while bearing the compounding physical and financial cost of a warming trajectory they did not set.
% ABSENT_VOICES: Small island states and frontline low-income governments raise loss-and-damage and mitigation-ambition demands in UNFCCC forums, but the instruments that actually disburse finance are designed and gatekept by donor economies and multilateral development banks; their preferred remedy (binding cuts, compensation) is spoken but structurally unactionable within this reading's institutions.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing disappeared and were replaced by binding mitigation-first commitments, wealthy incumbent economies would face immediate pressure to curtail fossil infrastructure and growth-as-usual, adaptation contractors would lose their primary revenue rationale, and frontline regions would face a different (potentially lower) total warming trajectory but with less near-term protective infrastructure funded under this specific frame — the finance architecture, diplomatic language, and investment priorities would all reorganize.
% FOUNDING_PROBLEM: Some warming was already locked in by historical emissions by the time this framing consolidated (post-Paris era), so a real problem existed: populations facing imminent, un-mitigable-in-time impacts needed protective infrastructure regardless of what mitigation path was chosen.
% FOUNDING_PROBLEM_CORROBORATION: IPCC working group reports and independent finance-tracking bodies (e.g. Climate Policy Initiative, UNEP Adaptation Gap Report) corroborate that some locked-in warming genuinely requires adaptation investment — that much is live. But those same independent sources also document that the adaptation-priority framing is used to justify continued fossil-fuel expansion and to substitute for mitigation ambition rather than complement it, a use the framing's original justification does not support; this critique comes from climate finance analysts and small-island governments outside the donor-economy beneficiary set.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the adaptation-priority framing, however well-intentioned in its coordination function, systematically relieves the highest-emitting economies of the deeper transfer mitigation or reparative finance would require, while frontline regions absorb both the physical damage and a persistent two-thirds shortfall in the finance meant to protect them. Suppression (0.52) is moderate — this is not primarily a coercive arrangement but one sustained by the diplomatic and institutional architecture of climate finance (who designs instruments, who disburses them) rather than force, so it registers lower than a classic snare but higher than pure voluntary coordination. Theater ratio (0.38) and its rising trajectory reflect a growing gap between pledged adaptation finance and disbursed, effective finance — a documented and worsening pattern in UNFCCC finance tracking.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (wealthy incumbent economies), this reading computes as coordination: pragmatic, humane triage given physical reality. From the payer seats (frontline regions, small island states, future generations), the same structure computes as extraction with a coordination veneer — protection is real but partial, funded at a fraction of need, while the parties best positioned to prevent further harm continue the activity causing it. The engine is expected to register this divergence rather than resolve it in either seat's favor.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy incumbent economies and fossil-fuel industries sit near the full-beneficiary end: they retain arbitrage-grade exit (can shift capital and policy framing at will) and structurally avoid the costs a mitigation-first or degrowth reading would impose on them. Frontline regions and small island states sit near the full-target end: trapped exit options, no seat in setting the emissions trajectory they must adapt to, and a persistent, documented finance shortfall. Future generations are declared payers with trapped exit and civilizational time horizon — they cannot exit a trajectory decided before their existence, and the compounding physics of deferred mitigation means their adaptation burden is larger than it would be under alternative readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — some warming is locked in and requires protective investment regardless of mitigation choices — remains genuinely live; adaptation investment is not obsolete theater. What has drifted is scope: the framing is increasingly used not as a complement to aggressive mitigation but as a substitute for it, extending the mandate from 'protect against unavoidable impacts' to 'accept as background and manage the impacts of avoidable ones.' This is not full mandatrophy (the founding problem is not dead) but a live contest over whether the mandate has been quietly widened beyond its original justification — hence founding_problem_status is authored as contested rather than dead or live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_as_complement_or_substitute,
    'Is prioritizing adaptation finance a legitimate complement to mitigation ambition given already-locked-in warming, or is it functioning as a substitute that relieves high emitters of deeper transfer obligations?',
    'Track whether adaptation finance commitments from wealthy economies correlate with stalled or declining mitigation ambition (NDC strengthening rates, fossil fuel subsidy trends) in the same donor countries over the same period; a strong negative correlation between adaptation pledges and mitigation ambition supports the substitute reading.',
    'If substitute, the coordination function is substantially cover for continued extraction and the constraint moves further toward the extractive end structurally, not merely rhetorically. If genuine complement, the tangled_rope classification''s coordination component is stronger than the extraction component suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_as_complement_or_substitute, empirical, 'Whether adaptation-priority framing complements or substitutes for mitigation ambition among high emitters.').

omega_variable(
    kernel_reading_selection_legitimacy,
    'Which reading of climate response legitimacy — adaptation-priority, mitigation-priority, or degrowth-transformation — should structure global finance architecture, and who has standing to decide?',
    'This is not resolvable by data alone; it depends on contested normative commitments about historical responsibility, intergenerational obligation, and the moral weight of near-term versus long-term harm. Route to political/philosophical resolution (UNFCCC negotiation outcomes, loss-and-damage fund governance) rather than empirical measurement.',
    'The reading selected determines which populations enter the victim/beneficiary sets and at what scope — this is the committer-axis structure documented per the kernel/reading framework; a shift to mitigation_priority or degrowth_transformation would produce structurally different constraints with different epsilon values, not a re-measurement of this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_legitimacy, preference, 'Which kernel reading should govern climate finance architecture — an irreducibly normative/political choice, not an empirical one.').

omega_variable(
    locked_in_warming_estimate_uncertainty,
    'How much warming is genuinely already locked in (irreducible regardless of near-term mitigation action) versus how much is being treated as locked in to justify deferring mitigation?',
    'Compare climate model ensembles under aggressive near-term mitigation scenarios versus current-policy scenarios; the delta between them represents warming that is NOT locked in and remains avoidable through mitigation choices this reading treats as settled.',
    'A large avoidable delta would indicate the adaptation-priority framing overstates the inevitability of the warming trajectory it accepts, strengthening the case that this reading''s foundational premise is itself contestable rather than a neutral factual starting point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(locked_in_warming_estimate_uncertainty, empirical, 'How much of the ''accepted'' warming trajectory is genuinely locked in versus still avoidable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__adaptation_priority, theater_ratio, 5, 0.27).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__adaptation_priority, theater_ratio, 10, 0.31).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.34).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.36).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__adaptation_priority, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__adaptation_priority, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__adaptation_priority, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__adaptation_priority, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__adaptation_priority, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__adaptation_priority, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__adaptation_priority, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__adaptation_priority, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__adaptation_priority, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, loss_and_damage_finance_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'legitimate climate response' claim per the epsilon-invariance principle: adaptation_priority (this story, tangled_rope — genuine protective coordination function plus asymmetric extraction favoring high emitters), mitigation_priority (separate story — growth-preserving decarbonization), and degrowth_transformation (separate story — structural economic transformation in wealthy nations). Each reading has a distinct beneficiary/victim structure and a distinct epsilon; they are not three measurements of one constraint but three structurally different constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
