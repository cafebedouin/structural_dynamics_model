% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response: Resilience Investment Under Accepted Warming
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint models the adaptation-priority reading of the climate
 *   response kernel: the position that meaningful near-term temperature rise
 *   is now unavoidable given cumulative emissions, and that climate policy
 *   resources should therefore prioritize building resilience infrastructure
 *   and adaptive capacity for vulnerable populations rather than centering
 *   emissions-reduction targets. The reading has a genuine coordination
 *   function — pooling capital and engineering capacity for seawalls,
 *   drought-resistant agriculture, and early warning systems addresses real,
 *   physically locked-in exposure. But the same 'inevitability' premise that
 *   justifies this framing also relieves high-emissions incumbents of
 *   pressure to change behavior, and the resulting finance flows are
 *   structured such that wealthy, well-resourced actors capture
 *   disproportionate protection while the $350B annual North-South financing
 *   gap leaves the most exposed populations under-protected. This is a
 *   distinct constraint from the mitigation_priority and
 *   degrowth_transformation readings of the same kernel — it has its own
 *   beneficiary/victim structure, its own extraction profile, and its own
 *   contested founding-problem status, and should not be averaged with the
 *   sibling readings.
 *
 * KEY AGENTS:
 *   - donor_nation_governments: agenda-setter, controls disbursement and conditionality of adaptation finance
 *   - adaptation_infrastructure_contractors: beneficiary, captures procurement flows regardless of emissions trajectory
 *   - wealthy_coastal_property_owners: beneficiary, receives first-tier protection funded by broader adaptation budgets
 *   - low_lying_developing_nations: primary target, bears both physical exposure and the unmet financing gap
 *   - climate_displaced_populations: primary target, bears realized cost of accepted warming
 *   - future_generations: primary target, inherits locked-in warming with no voice in the financing bargain
 *   - mitigation_advocacy_coalitions: excluded voice, disputes the inevitability framing itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.61).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.47).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.61).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response: Resilience Investment Under Accepted Warming").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, 'cea724a6-6675-4ca0-8bcd-ba74ad944d9a').
narrative_ontology:cs_kernel_codification('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', distributed).
narrative_ontology:cs_authority_grounding('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', distributed).
narrative_ontology:cs_reading_relation('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', climate_response_action__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', climate_response_action__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', foundational, near_term_warming_is_substantially_locked_in).
narrative_ontology:cs_axiom_status(near_term_warming_is_substantially_locked_in, holdable).
narrative_ontology:cs_axiom_grounding('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', near_term_warming_is_substantially_locked_in, empirically_contingent).
narrative_ontology:cs_axiom('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', foundational, protection_of_currently_vulnerable_populations_takes_precedence_over_marginal_future_emissions_avoidance).
narrative_ontology:cs_axiom_status(protection_of_currently_vulnerable_populations_takes_precedence_over_marginal_future_emissions_avoidance, holdable).
narrative_ontology:cs_axiom_grounding('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', protection_of_currently_vulnerable_populations_takes_precedence_over_marginal_future_emissions_avoidance, deontological).
narrative_ontology:cs_reference_frame('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', post_paris_agreement_carbon_budget_consensus).
narrative_ontology:cs_drift_state('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', post_2023_global_stocktake, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cea724a6-6675-4ca0-8bcd-ba74ad944d9a', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, wealthy_coastal_property_owners).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, adaptation_infrastructure_contractors).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, high_emissions_incumbent_industries).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, donor_nation_governments).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, low_lying_developing_nations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, climate_displaced_populations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, smallholder_agricultural_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of international climate finance, choosing to fund resilience and adaptation infrastructure at home and selectively abroad rather than commit to binding emissions cuts that would constrain their own economies. They frame adaptation financing as pragmatic realism about temperature trajectories already locked in, while retaining full discretion over how much is disbursed, to whom, and under what conditions.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, donor_nation_governments, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, donor_nation_governments, beneficiary).

% Engineering firms, insurers, and infrastructure developers who win contracts to build seawalls, drought-resistant agriculture systems, and climate-resilient housing. They benefit directly from the framing that adaptation spending is the primary climate response, since it generates sustained, large-scale procurement independent of whether emissions actually fall.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, adaptation_infrastructure_contractors, beneficiary,
    organized, biographical, mobile, global).

% Own high-value real estate in vulnerable but well-resourced coastal zones. They receive first-tier protection through seawalls, insurance backstops, and relocation subsidies funded by adaptation budgets, effectively externalizing the cost of their location choice onto public and international finance.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, wealthy_coastal_property_owners, beneficiary,
    powerful, biographical, mobile, national).

% Fossil fuel and heavy industry interests find the adaptation-priority framing convenient: by accepting temperature rise as inevitable and shifting the policy center of gravity to resilience infrastructure, pressure for near-term emissions constraints on their operations is diffused. They face no binding requirement to change core business models.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, high_emissions_incumbent_industries, beneficiary,
    institutional, biographical, arbitrage, global).

% Face existential exposure to sea-level rise, cyclones, and drought but lack the fiscal capacity to fund the $540B in annual resilience investment the adaptation-priority approach requires. They depend on the $350B North-South financing gap being closed by donor pledges that routinely under-deliver, leaving them to absorb both the physical damage and the debt incurred trying to protect against it.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, low_lying_developing_nations, payer,
    powerless, generational, trapped, national).

% People already forced from land and livelihood by warming-driven disasters. Adaptation infrastructure, where built, arrives too late or too underfunded to prevent displacement, and resettlement systems are not designed to receive them at scale. They bear the realized cost of the temperature rise the reading accepts as a starting premise.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_displaced_populations, payer,
    powerless, biographical, trapped, regional).

% Inherit a warmer baseline locked in by a strategy that treats further temperature rise as acceptable in exchange for near-term resilience spending. They have no voice in current financing negotiations and will face compounding physical and financial costs as adaptation limits are exceeded in a hotter world.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Rural communities dependent on rain-fed agriculture in regions where adaptation infrastructure (irrigation, drought-resistant seed systems, early warning networks) is chronically underfunded relative to urban and coastal protection. Their exit options are limited to distress migration or continued exposure to yield collapse.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, smallholder_agricultural_communities, payer,
    powerless, generational, constrained, regional).

% Scientists, activists, and negotiating blocs who argue that accepting temperature rise as inevitable forecloses emissions-reduction pathways still technically available, and that adaptation-priority framing is being used to relieve political pressure on high emitters. Their objections are voiced in UN forums but hold no binding authority over how adaptation finance is structured.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, mitigation_advocacy_coalitions, excluded,
    organized, generational, constrained, global).

% Independent researchers and multilateral bank economists who track financing flows, disbursement gaps, and protection disparities across the North-South divide, producing the evidence base for whether adaptation-priority financing is closing or widening the resilience gap.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_finance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__adaptation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_action__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools capital and engineering capacity to build resilience infrastructure — seawalls, drought-resistant agriculture, early warning systems, managed retreat programs — for populations facing climate impacts that are, at current emissions trajectories, already substantially locked in regardless of near-term mitigation choices.
% TRANSFER_FUNCTION: Moves capital nominally toward vulnerable populations via adaptation finance, but the $350B annual North-South financing gap means the bulk of realized protection accrues to wealthy, well-resourced, mostly Northern property and infrastructure interests, while the fiscal and mortality burden of unmet adaptation needs falls on low-capacity states, displaced populations, and future generations who did not choose the accepted-warming premise.
% ABSENT_VOICES: Mitigation advocacy coalitions and degrowth movements argue the 'inevitability' framing is not a neutral technical judgment but a political choice that relieves incumbent emitters of near-term obligation; low-lying developing nations' negotiators are present in UN forums but structurally outmatched in financing negotiations by donor governments who control disbursement timing and conditionality.
% DISAPPEARANCE_RATIONALE: Donor governments and adaptation contractors would say the world rearranges catastrophically — infrastructure projects, insurance backstops, and displacement response systems currently in motion would stall. Mitigation advocates and developing-nation negotiators would say the underlying vulnerability and the political relief this framing provides to high emitters would remain the same or worsen, since accepted warming and its damages don't reverse just because the adaptation-priority framing disappears; what changes is whether political pressure returns to emissions reduction.
% FOUNDING_PROBLEM: Even under aggressive emissions cuts, a substantial amount of additional warming and associated physical damage was already locked into the climate system by cumulative historical emissions; the adaptation-priority reading holds that failing to build protective capacity now, while treating further warming as avoidable, leaves vulnerable populations exposed to harm that mitigation alone cannot prevent in time.
% FOUNDING_PROBLEM_CORROBORATION: Climate finance analysts and IPCC adaptation working groups (outside the donor-government and contractor beneficiary set) corroborate that some locked-in warming is real and adaptation needs are genuine and underfunded. However, the same independent analysts also document that the 'inevitability' framing is applied more expansively than the physical science requires, and that financing shortfalls concentrate protection on high-capacity actors rather than the most vulnerable — meaning the founding problem is real but the reading's application of it is contested even by sources outside its beneficiaries.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, contested).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61 at interval end) reflects that the reading's coordination function is real but its financing structure systematically underdelivers to those most exposed while channeling resources to well-capitalized actors — a structural asymmetry, not incidental underfunding. Suppression (0.47) is moderate: developing nations are not coerced into accepting the adaptation-priority framing outright, but their weak bargaining position in financing negotiations and dependence on donor disbursement timing functions as a structural constraint on genuine alternatives. Theater ratio (0.38) captures that a meaningful share of adaptation pledges are announced but not disbursed at pledged levels — a rising trend as commitments outpace delivery over the interval. Accessibility collapse (0.42) is moderate-low: mitigation and degrowth alternatives remain live and contested rather than foreclosed, which is why this is authored as tangled_rope rather than snare — genuine coordination coexists with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Donor governments and adaptation contractors sit near the beneficiary end: they set terms, retain discretion, and capture procurement value regardless of whether protection is delivered equitably. Wealthy coastal property owners and high-emissions incumbents benefit through cost externalization and reduced mitigation pressure respectively. Low-lying developing nations, displaced populations, smallholder communities, and future generations sit near the full-target end: trapped exit options (geography, poverty, non-existence-yet-in-the-case-of-future-generations), high dependence on financing flows they don't control, and direct exposure to the physical costs the reading's inevitability premise accepts.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare depends on the coordination function being genuine and not merely cover: locked-in warming is a real physical constraint, and adaptation infrastructure does prevent some deaths and displacement that would otherwise occur. Reclassifying this as pure snare would mislabel the real protective function adaptation finance serves for at least some vulnerable populations who do receive funded resilience infrastructure. But labeling it rope would ignore the asymmetric extraction: the same inevitability framing that justifies the coordination also serves incumbent emitters by diffusing mitigation pressure, and the financing gap systematically favors high-capacity actors. The tangled_rope classification requires holding both facts simultaneously rather than collapsing to either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_premise_political_or_physical,
    'Is the ''temperature rise is inevitable, prioritize adaptation'' premise a physically forced conclusion from cumulative emissions, or a politically convenient framing that forecloses mitigation pathways still technically available?',
    'Compare the reading''s accepted-warming baseline against IPCC remaining-carbon-budget assessments at the time the reading was adopted; track whether mitigation pathways described as ''foreclosed'' by adaptation-priority advocates were still rated technically feasible by independent climate science bodies.',
    'If the premise substantially overstates physical lock-in relative to the contemporaneous scientific consensus, the coordination function is partly a cover story for relieving incumbent emitters of pressure, pushing the classification toward snare. If the premise accurately reflects locked-in warming, the coordination function is more clearly genuine, supporting tangled_rope or even a more rope-leaning read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_premise_political_or_physical, empirical, 'Whether the inevitability premise is physically grounded or politically convenient.').

omega_variable(
    financing_gap_persistence_mechanism,
    'Is the persistent $350B North-South adaptation financing gap a structural feature that donor governments benefit from maintaining (via reduced disbursement obligations), or a genuine capacity/coordination failure without an identifiable beneficiary?',
    'Track whether donor-government fiscal positions, negotiating behavior, and disbursement patterns show a consistent pattern of pledging above what is subsequently delivered, and whether the gap correlates with reduced pressure for binding mitigation commitments from the same donor states.',
    'If the gap is a stable, beneficial-to-donors structural feature, this strengthens the tangled_rope reading and could push the constraint toward snare over time. If it reflects genuine coordination failure without a capturing beneficiary, the constraint drifts toward a piton or a poorly-resourced but sincere rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financing_gap_persistence_mechanism, empirical, 'Whether the financing gap is structurally beneficial to donors or a sincere coordination failure.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Where exactly does the adaptation_priority reading''s core commitment diverge from the mitigation_priority reading''s core commitment — is it a genuine either/or allocation choice under fixed resources, or are the readings compatible complements being falsely presented as competing priorities?',
    'Examine specific international financing instruments (e.g. Green Climate Fund allocation rules) to determine whether adaptation and mitigation financing are structurally in competition for the same capital pool, or whether the framing of adaptation-vs-mitigation as opposed priorities is itself a rhetorical move that obscures a resource question.',
    'If the readings are genuinely in structural competition for capital, the tangled_rope classification of this reading is sharpened by real opportunity cost to mitigation. If the readings are compatible and falsely presented as opposed, part of the extraction measured here is attributable to the false dichotomy itself rather than to adaptation financing''s own structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether adaptation and mitigation priorities are structurally competing or falsely dichotomized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t4, climate_response_action__adaptation_priority, theater_ratio, 4, 0.27).
narrative_ontology:measurement(clim_tr_t8, climate_response_action__adaptation_priority, theater_ratio, 8, 0.31).
narrative_ontology:measurement(clim_tr_t12, climate_response_action__adaptation_priority, theater_ratio, 12, 0.33).
narrative_ontology:measurement(clim_tr_t16, climate_response_action__adaptation_priority, theater_ratio, 16, 0.36).
narrative_ontology:measurement(clim_tr_t20, climate_response_action__adaptation_priority, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t4, climate_response_action__adaptation_priority, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(clim_be_t8, climate_response_action__adaptation_priority, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(clim_be_t12, climate_response_action__adaptation_priority, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(clim_be_t16, climate_response_action__adaptation_priority, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(clim_be_t20, climate_response_action__adaptation_priority, base_extractiveness, 20, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(clim_su_t4, climate_response_action__adaptation_priority, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(clim_su_t8, climate_response_action__adaptation_priority, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(clim_su_t12, climate_response_action__adaptation_priority, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(clim_su_t16, climate_response_action__adaptation_priority, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(clim_su_t20, climate_response_action__adaptation_priority, suppression_requirement, 20, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_response_action kernel, each authored as a separate, ε-invariant constraint per the decomposition principle. adaptation_priority (this file) treats warming as substantially locked-in and prioritizes resilience investment, generating a North-South financing gap and protection disparities. mitigation_priority treats emissions reduction below 2°C as achievable via technology and carbon markets while preserving growth. degrowth_transformation treats structural economic transformation away from growth as necessary. The three readings are linked via network edges because political and financial commitment to one reading structurally reduces resources and legitimacy available to the others — a dollar or a unit of political capital spent affirming adaptation-priority framing is, in the current finance architecture, frequently a dollar or unit of capital not spent on mitigation or transformation pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
