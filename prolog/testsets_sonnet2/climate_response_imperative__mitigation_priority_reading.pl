% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Mitigation-Priority Reading of the Climate Response Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story authors the mitigation-priority reading of the climate
 *   response imperative kernel: the claim that climate response is primarily
 *   emissions reduction via technological innovation and carbon markets, with
 *   adaptation treated as a residual to be addressed once mitigation pathways
 *   are underway. Since the Kyoto Protocol era, international climate
 *   governance has structurally favored measurable, tradable,
 *   technology-linked mitigation instruments (carbon markets, clean-tech
 *   subsidies, NDC pledges scored in tons abated) over adaptation and
 *   loss-and-damage financing, which is harder to quantify, less bankable,
 *   and slower to mobilize. The reading treats this asymmetry as
 *   descriptively real and increasingly extractive over time as reliance on
 *   unproven carbon dioxide removal (CDR) grows without commensurate
 *   adaptation investment. This is ONE of three readings of the same kernel;
 *   the adaptation_priority_reading and degrowth_reading are separate
 *   constraint stories with their own ε and stakeholder structures, not
 *   alternative framings folded into this one.
 *
 * KEY AGENTS:
 *   - global_north_innovation_sectors: institutional beneficiary capturing subsidy and IP rents from mitigation-technology framing
 *   - carbon_market_intermediaries: organized beneficiary whose business model requires mitigation-centrism to persist
 *   - incumbent_energy_majors_transitioning_to_ccs: institutional beneficiary extending fossil-linked operations under a CDR-compliant banner
 *   - future_generations: powerless, trapped payer bearing residual warming and unbuilt adaptation capacity
 *   - small_island_and_low_lying_states: powerless, trapped payer facing existential risk under-financed by the dominant frame
 *   - smallholder_farmers_in_climate_exposed_regions and urban_poor_in_heat_and_flood_corridors: powerless payers bearing local adaptation shortfalls
 *   - degrowth_and_adaptation_advocates: excluded voice present in UNFCCC process but structurally out-resourced
 *   - climate_science_and_ipcc_working_groups: analytical observer documenting the emissions and adaptation-finance gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.52).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Mitigation-Priority Reading of the Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '89733e3d-0b71-4a1a-837b-e5cc77aa4d7d').
narrative_ontology:cs_kernel_codification('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', distributed).
narrative_ontology:cs_authority_grounding('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', distributed).
narrative_ontology:cs_reading_relation('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', foundational, technological_substitutability_of_emissions_pathways).
narrative_ontology:cs_axiom_status(technological_substitutability_of_emissions_pathways, holdable).
narrative_ontology:cs_axiom_grounding('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', technological_substitutability_of_emissions_pathways, empirically_contingent).
narrative_ontology:cs_axiom('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', foundational, market_price_signals_as_primary_allocation_mechanism).
narrative_ontology:cs_axiom_status(market_price_signals_as_primary_allocation_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', market_price_signals_as_primary_allocation_mechanism, instrumental).
narrative_ontology:cs_reference_frame('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', kyoto_market_mechanism_framework).
narrative_ontology:cs_drift_state('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', post_paris_ndc_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89733e3d-0b71-4a1a-837b-e5cc77aa4d7d', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, incumbent_energy_majors_transitioning_to_ccs).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, multilateral_climate_finance_institutions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, small_island_and_low_lying_states).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, smallholder_farmers_in_climate_exposed_regions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, urban_poor_in_heat_and_flood_corridors).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, innovation_led_decarbonization_feasibility_doctrine).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, market_mechanism_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clean-tech firms, battery and CDR startups, and carbon-market platforms headquartered mostly in wealthy states capture subsidies, patent rents, and export markets created by a mitigation-first framing. They co-author the IPCC-adjacent policy consensus that privileges technological pathways, and they can relocate capital and intellectual property across jurisdictions if any single regime tightens.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, agenda_setter).

% Verification firms, offset brokers, and exchange operators earn fees on every ton traded or offset issued. Their business model requires mitigation to remain the dominant policy frame; a pivot toward direct adaptation transfers or degrowth would shrink their transaction volume substantially.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Fossil-linked energy companies pivot into carbon capture, hydrogen, and offset generation, using the mitigation-innovation frame to extend their operating licenses and continue extraction while claiming a compliant transition pathway. They lobby to keep negative-emissions technology central to national climate plans.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, incumbent_energy_majors_transitioning_to_ccs, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, incumbent_energy_majors_transitioning_to_ccs, agenda_setter).

% Development banks and climate funds channel the bulk of concessional finance into mitigation projects with clear technical metrics and bankable returns, because these are easier to structure as loans than adaptation investments in subsistence agriculture or informal settlements. Their institutional mandates and staff expertise are built around mitigation project pipelines.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, multilateral_climate_finance_institutions, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, multilateral_climate_finance_institutions, observer).

% Inherit whatever residual warming, locked-in infrastructure, and unbuilt adaptation capacity the mitigation-priority approach leaves behind. If negative-emissions technologies underperform current projections, this generation bears the compounding physical and fiscal cost with no voice in today's allocation decisions and no capacity to renegotiate the terms.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Face existential sea-level and storm risk now, while global finance and diplomatic attention are weighted toward mitigation technology deployment in large emitting economies. Adaptation and loss-and-damage funding arrives late, underscaled, and often structured as debt rather than grant, because the dominant frame treats adaptation as a residual rather than a coequal priority.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, small_island_and_low_lying_states, payer,
    powerless, generational, trapped, regional).

% Experience shifting rainfall, drought, and crop failure directly, but agricultural adaptation programs are chronically underfunded relative to mitigation and energy-transition financing. Migration or occupational exit is possible for some but at high personal cost and loss of land tenure and community ties.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, smallholder_farmers_in_climate_exposed_regions, payer,
    powerless, biographical, trapped, regional).

% Live in informal settlements or under-resilient housing stock exposed to intensifying heatwaves and flooding. Municipal budgets are drawn toward nationally visible mitigation and green-infrastructure projects with donor appeal, leaving local drainage, cooling, and early-warning systems chronically under-resourced.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, urban_poor_in_heat_and_flood_corridors, payer,
    powerless, biographical, constrained, local).

% Researchers, Global South delegations, and civil-society coalitions who argue that mitigation-technology framing structurally under-resources adaptation and ignores consumption-side transformation. They participate in UNFCCC processes but their proposals are consistently marginalized in favor of technology-and-market-centered national pledges, and they lack the institutional funding base that innovation-sector lobbies command.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, degrowth_and_adaptation_advocates, excluded,
    moderate, generational, constrained, global).

% Synthesize physical and social science across all response pathways. Their assessments document the emissions gap and the growing reliance on unproven carbon dioxide removal at scale, providing the evidentiary basis for cross-reading contestation without themselves adjudicating which policy framing should be adopted.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_science_and_ipcc_working_groups, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuinely collective problem — global emissions reduction requires cross-border technology diffusion, price signals, and standardized measurement so that abatement effort in one jurisdiction is comparable and creditable elsewhere. Carbon markets and innovation subsidies solve a real free-rider problem in decarbonization investment.
% TRANSFER_FUNCTION: Moves near-term political attention, public R&D subsidy, and concessional finance toward mitigation technology and market infrastructure located substantially in the Global North, while deferring adaptation and loss-and-damage costs onto vulnerable regions now and onto future generations who will face residual warming if negative-emissions technologies underperform.
% ABSENT_VOICES: Small island states, smallholder communities, and future generations are formally represented in UNFCCC proceedings but structurally outvoted by the coalition of innovation-sector interests, carbon-market intermediaries, and finance institutions whose funding models and expertise are built around mitigation project pipelines; degrowth and adaptation-first advocates participate but without comparable lobbying capacity.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framing disappeared overnight, innovation-sector subsidy flows and carbon-market transaction volumes would collapse immediately (world_rearranges for beneficiaries), but whether physical adaptation needs in exposed regions would be better or worse served under an alternative framing is genuinely disputed among the reading's own proponents and its critics — hence contested rather than a clean verdict either way.
% FOUNDING_PROBLEM: Atmospheric greenhouse gas concentrations rising faster than natural sinks can absorb, requiring a coordinated global reduction in emissions rates before cumulative warming crosses catastrophic thresholds; the mitigation-priority reading was built to solve the technical and economic core of that problem via decarbonization pathways and price mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group III and independent emissions-gap analyses (UNEP Emissions Gap Report) corroborate that the mitigation problem itself remains live and urgent — the disagreement is not about whether mitigation is needed but whether prioritizing it over adaptation, and relying on unproven CDR to close the gap, is a defensible allocation of scarce climate finance and attention. Vulnerable-state delegations and independent climate-justice researchers, outside the beneficiary set, corroborate that the adaptation shortfall this reading produces is real and measurable, not merely a rhetorical grievance.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, contested).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at 2024) reflects a genuine and rising asymmetry: mitigation-technology and carbon-market financing has grown steadily while adaptation and loss-and-damage funding remains a small fraction of pledged and disbursed climate finance, and the gap widens as reliance on unproven large-scale CDR increases the risk that the strategy leaves a larger residual burden than currently modeled. Suppression (0.52) is moderate rather than severe — no single actor coercively forecloses adaptation-first proposals, but the accumulated weight of institutional funding models, IPCC scenario architecture built around mitigation pathways, and donor preference for bankable projects functions as a structural suppression of the alternative framings. Theater ratio (0.44) is elevated because a substantial share of pledged mitigation finance and voluntary carbon offsets has been documented (by the observer seats' own science) as producing far less real abatement than claimed, functioning partly as legitimacy performance for continued high-emissions activity by incumbent actors. Accessibility collapse (0.50) and resistance (0.61) are moderate: adaptation-first and degrowth alternatives remain live, articulated positions in international negotiations — they have not been eliminated, but they face real, organized resistance from the mitigation-technology coalition when they compete for finance and attention.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North innovation sectors, carbon-market intermediaries, transitioning energy majors, and mitigation-oriented multilateral finance institutions are declared beneficiaries: the mitigation-priority frame directs subsidy, project finance, and market infrastructure disproportionately toward them, and their institutional mandates and revenue models depend on the frame's continuation — low d, benefiting from the arrangement. Future generations, small island states, smallholder farmers, and urban poor in exposed corridors are declared victims: they bear deferred or displaced costs (residual warming, underfunded resilience, immediate physical exposure) without commensurate voice in the resource allocation that produces those costs — high d, targets of the arrangement. Trapped exit options for all victim groups reflect that none can meaningfully exit the climate system or the international finance architecture that allocates response resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rising atmospheric concentrations requiring coordinated emissions reduction — remains genuinely live, corroborated by IPCC and UNEP assessments outside the beneficiary set; this is not a case of an empty mandate persisting by inertia. What is contested is not whether mitigation is needed but whether prioritizing it at adaptation's expense, and betting heavily on unproven CDR to close the remaining gap, is a defensible allocation given the asymmetric distribution of who bears the cost of underperformance. Classifying this as tangled_rope rather than snare or mountain preserves the genuine coordination function (cross-border technology diffusion and price signals do solve a real collective-action problem) while registering the asymmetric extraction (deferred adaptation costs falling on those least responsible and least resourced) that the mitigation-first frame produces through the same institutional structures that deliver the coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cdr_scalability_uncertainty,
    'Will carbon dioxide removal technologies scale to the levels current mitigation pathways assume, or will the mitigation-priority strategy leave a substantially larger residual warming burden than modeled?',
    'Track deployed CDR capacity against IPCC pathway assumptions over the next two decades; compare actual gigatons removed to modeled requirements at each five-year assessment cycle.',
    'If CDR substantially underdelivers, the extractiveness of this reading is understated — the deferred cost transferred to future generations and vulnerable regions would be larger than currently measured, strengthening the case for reclassification toward snare-adjacent severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scalability_uncertainty, empirical, 'Whether relied-upon CDR technology will scale as mitigation pathways assume.').

omega_variable(
    coordination_vs_capture_boundary,
    'Is the mitigation-technology coalition''s dominance of climate finance a necessary feature of solving the genuine cross-border coordination problem, or is it separable capture by incumbent and innovation-sector interests riding on that coordination need?',
    'Compare finance allocation patterns in jurisdictions or negotiation rounds where adaptation-first advocates gained comparable institutional resourcing to those where the mitigation coalition dominated; assess whether coordination outcomes (measured emissions reductions) differ.',
    'If separable, current mitigation dominance in finance allocation is largely capture rather than functional necessity, supporting a higher-severity reading; if inseparable, part of the measured extraction reflects genuine technical requirements of the coordination problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_capture_boundary, conceptual, 'Whether mitigation-coalition dominance is functionally necessary or separable capture.').

omega_variable(
    kernel_framing_choice_ambiguity,
    'Is the choice to read the climate response imperative as mitigation-priority (rather than adaptation-priority or degrowth) itself a neutral empirical judgment about tractability, or a framing that reflects which interests currently hold institutional power in climate governance?',
    'Examine the correlation between institutional actors'' funding sources and their advocacy for one reading over another; assess whether the mitigation-priority reading was adopted before or after Global North innovation-sector lobbying intensified in UNFCCC processes.',
    'If framing choice tracks institutional power rather than neutral tractability assessment, this reading''s claimed_type divergence from a computed extractive type is evidence of exactly the false-neutral-framing dynamic the corpus exists to detect; if framing tracks genuine technical judgment, the coordination function is more robust than the extraction critique suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_ambiguity, conceptual, 'Whether the mitigation-priority framing choice reflects institutional power or neutral tractability judgment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1997, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1997, 0.25).
narrative_ontology:measurement(clim_tr_t2005, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(clim_tr_t2012, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(clim_tr_t2015, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(clim_tr_t2019, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2019, 0.41).
narrative_ontology:measurement(clim_tr_t2024, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t1997, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement(clim_be_t2005, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(clim_be_t2012, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2012, 0.57).
narrative_ontology:measurement(clim_be_t2015, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(clim_be_t2019, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2019, 0.64).
narrative_ontology:measurement(clim_be_t2024, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1997, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(clim_su_t2005, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(clim_su_t2012, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2012, 0.4).
narrative_ontology:measurement(clim_su_t2015, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(clim_su_t2019, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2019, 0.48).
narrative_ontology:measurement(clim_su_t2024, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__mitigation_priority_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'climate response imperative' per the epsilon-invariance principle. The mitigation_priority_reading authors high reliance on unproven CDR and a beneficiary set concentrated in Global North innovation and finance institutions, with future generations and vulnerable regions as victims via deferred adaptation costs. The adaptation_priority_reading authors a different epsilon centered on immediate-harm victim sets in exposed regions with mitigation treated as aspirational. The degrowth_reading authors a structural-transformation claim with Global North consumption patterns as the primary target of extraction critique. All three share the same underlying physical crisis (the kernel) but are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different classification outcomes — they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
