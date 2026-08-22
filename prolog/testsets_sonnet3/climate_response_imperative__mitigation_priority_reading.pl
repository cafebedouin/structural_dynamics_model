% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Mitigation-Priority Reading of the Climate Response Imperative
 *   domain: climate policy / political economy / intergenerational justice
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'climate response
 *   imperative' kernel: the mitigation-priority reading, under which the
 *   correct global climate response is emissions reduction via technological
 *   innovation and market mechanisms (carbon pricing, cleantech subsidy, CCS,
 *   direct air capture), with adaptation and resilience-building treated as a
 *   residual claim on whatever finance remains once mitigation is funded.
 *   Under this reading's own lights, the standing arrangement channels the
 *   bulk of global climate finance, diplomatic attention, and
 *   technology-transfer effort toward emissions-reduction technology and
 *   market instruments, while adaptation for populations already exposed to
 *   physical climate damage is funded late, inconsistently, and as a lesser
 *   priority. This is not a story about which reading is correct — the
 *   sibling readings (adaptation-priority, degrowth) are separate constraints
 *   with their own ε, beneficiaries, and victims, linked via
 *   network.affects_constraints. This story's ε is authored solely for the
 *   mitigation-priority arrangement as this reading characterizes it.
 *
 * KEY AGENTS:
 *   - global_north_cleantech_sector: Primary beneficiary (organized/arbitrage) — captures subsidy and patent rents from the mitigation-first framing
 *   - carbon_market_intermediaries: Beneficiary and co-agenda-setter (organized/arbitrage) — profits from and lobbies for the market-mechanism framing
 *   - future_generations: Primary victim (powerless/trapped/civilizational) — inherits the deferred adaptation gap
 *   - small_island_and_low_lying_states: Primary present-tense victim (powerless/trapped) — bears physical exposure now while finance concentrates on mitigation tech elsewhere
 *   - multilateral_climate_finance_institutions: Agenda-setter (institutional) — sets the eligibility criteria that structurally favor measurable mitigation projects over harder-to-quantify adaptation projects
 *   - degrowth_and_adaptation_advocates: Excluded voice (moderate/constrained) — present in discourse but marginal in the finance-allocation rooms
 *   - climate_policy_analysts: Analytical observer — tracks the mitigation-adaptation finance gap without bearing its costs
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
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Mitigation-Priority Reading of the Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate policy / political economy / intergenerational justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '2baacac4-bcb4-463a-ab3f-0c675f3343b6').
narrative_ontology:cs_kernel_codification('2baacac4-bcb4-463a-ab3f-0c675f3343b6', distributed).
narrative_ontology:cs_authority_grounding('2baacac4-bcb4-463a-ab3f-0c675f3343b6', distributed).
narrative_ontology:cs_reading_relation('2baacac4-bcb4-463a-ab3f-0c675f3343b6', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('2baacac4-bcb4-463a-ab3f-0c675f3343b6', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('2baacac4-bcb4-463a-ab3f-0c675f3343b6', foundational, technological_substitution_can_decouple_growth_from_emissions).
narrative_ontology:cs_axiom_status(technological_substitution_can_decouple_growth_from_emissions, holdable).
narrative_ontology:cs_axiom_grounding('2baacac4-bcb4-463a-ab3f-0c675f3343b6', technological_substitution_can_decouple_growth_from_emissions, empirically_contingent).
narrative_ontology:cs_axiom('2baacac4-bcb4-463a-ab3f-0c675f3343b6', foundational, market_price_mechanisms_are_the_efficient_allocator_of_abatement_effort).
narrative_ontology:cs_axiom_status(market_price_mechanisms_are_the_efficient_allocator_of_abatement_effort, holdable).
narrative_ontology:cs_axiom_grounding('2baacac4-bcb4-463a-ab3f-0c675f3343b6', market_price_mechanisms_are_the_efficient_allocator_of_abatement_effort, instrumental).
narrative_ontology:cs_reference_frame('2baacac4-bcb4-463a-ab3f-0c675f3343b6', kyoto_common_but_differentiated_responsibility_framework).
narrative_ontology:cs_drift_state('2baacac4-bcb4-463a-ab3f-0c675f3343b6', post_paris_agreement_ndc_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2baacac4-bcb4-463a-ab3f-0c675f3343b6', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_cleantech_sector).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, incumbent_energy_majors_transitioning_to_ccs).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, multilateral_climate_finance_institutions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, small_island_and_low_lying_states).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, subsistence_agricultural_communities).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, adaptation_dependent_coastal_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Captures the bulk of public R&D subsidy, carbon-credit revenue, and patent rents flowing from a mitigation-centered framing. Can relocate capital and IP across jurisdictions as policy incentives shift, and profits whether or not the underlying emissions trajectory actually bends fast enough to avert damages elsewhere.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_cleantech_sector, beneficiary,
    organized, biographical, arbitrage, global).

% Design, verify, and trade offset and allowance instruments; their revenue depends on the mitigation-market framing remaining dominant over adaptation-financing or degrowth alternatives, and they lobby heavily to keep it so.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, agenda_setter).

% Reframe continued fossil operation as compatible with net-zero via carbon capture and offsets, extending the operating life of existing assets under the mitigation-priority banner while deferring the harder question of whether removal will scale in time.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, incumbent_energy_majors_transitioning_to_ccs, beneficiary,
    institutional, generational, arbitrage, global).

% Set eligibility criteria and disbursement priorities for climate funds, systematically weighting mitigation projects (measurable emissions-per-dollar) over adaptation projects (harder to quantify, more site-specific), shaping which reading of the imperative gets funded.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, multilateral_climate_finance_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Inherit whatever gap remains between promised mitigation trajectories and delivered decarbonization, plus the adaptation infrastructure that was deferred as residual because mitigation was funded first. Cannot participate in current resource-allocation decisions and cannot exit the physical consequences.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Face existential sea-level and storm exposure now, while global finance and diplomatic attention concentrate on emissions-reduction technology deployed mostly in large emitting economies; adaptation and relocation funding arrives late, underfunded, and structured as charity rather than obligation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, small_island_and_low_lying_states, payer,
    powerless, generational, trapped, regional).

% Experience yield collapse and water stress in the near term; the mitigation-priority framing directs global capital toward emissions technology in industrial economies rather than toward the irrigation, seed, and insurance systems that would let these communities adapt now.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, subsistence_agricultural_communities, payer,
    powerless, biographical, trapped, regional).

% Need seawalls, managed retreat, and early-warning systems now; compete for a residual budget line after mitigation programs are funded, since the dominant framing treats adaptation spending as an admission of mitigation failure rather than a coequal necessity.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, adaptation_dependent_coastal_populations, payer,
    powerless, biographical, constrained, regional).

% Argue that consumption reduction and resilience-building deserve parity with or priority over technological mitigation; are marginalized in COP negotiation tracks, IPCC scenario weighting, and finance-facility design because their proposals do not fit the innovation-and-markets vocabulary that dominant negotiators and funders speak.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, degrowth_and_adaptation_advocates, excluded,
    moderate, generational, constrained, global).

% Track the widening gap between pledged emissions trajectories and delivered reductions, and the persistent underfunding of the Loss and Damage and adaptation finance tracks relative to mitigation finance, without directly bearing either cost.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuinely global collective-action problem — dispersed emitters must jointly reduce a shared atmospheric stock — around a legible, market-tradable unit (tons of CO2e) that lets heterogeneous economies price and trade abatement effort instead of negotiating bespoke bilateral reductions.
% TRANSFER_FUNCTION: Moves present climate-adaptation capacity and future physical safety from populations already exposed to warming (small island states, subsistence farmers, coastal communities, and unborn generations) toward capital, R&D subsidy, and policy attention concentrated in Global North innovation and carbon-finance sectors, on the promise that sufficiently fast technological mitigation will make large-scale adaptation spending unnecessary.
% ABSENT_VOICES: Adaptation-priority advocates and degrowth economists are present in academic literature and some UNFCCC side negotiations but structurally marginal in the finance-facility design rooms and national NDC-setting processes that actually allocate money; small island and least-developed-country blocs raise Loss and Damage demands repeatedly but hold little leverage against larger emitting economies that control both emissions volume and mitigation-technology supply chains.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framing lost its grip on climate finance and diplomacy overnight, adaptation and Loss and Damage funding would have to compete on equal footing rather than as a residual line item, carbon-market intermediary revenue models would need rebuilding, and cleantech capital allocation currently justified by net-zero pathways would face redirection toward resilience infrastructure — a substantial reallocation of resources and institutional mandates.
% FOUNDING_PROBLEM: Atmospheric greenhouse gas concentrations are a genuine collective-action problem: no single actor's abatement matters much alone, emissions mix globally regardless of source, and without some coordinated reduction mechanism the problem worsens for everyone, so an early framing task was to build cross-border mechanisms (carbon markets, technology-transfer funds, innovation subsidies) that could scale reduction effort without requiring a single global authority.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II assessments and Loss and Damage negotiators from vulnerable states attest that the underlying atmospheric problem remains live but that the mitigation-first allocation of finance and attention has left adaptation needs chronically underserved relative to their urgency; this corroboration comes from parties outside the beneficiary set (vulnerable-state negotiators, independent IPCC scientists) rather than from the cleantech or carbon-finance sectors that benefit from the current allocation.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68 at interval end) reflects a persistent and widening gap between mitigation finance and delivered adaptation capacity: this reading directs a large majority of both public and private climate capital toward emissions-reduction technology and markets, while populations already suffering physical damage receive comparatively little and late. Suppression (0.52) is moderate rather than extreme — the mitigation-priority reading is maintained through agenda-setting power in finance institutions and negotiation forums (who gets to define 'countable' climate action) rather than through direct coercion; alternative framings are marginalized, not outlawed. Theater ratio (0.44) is substantial and rising because a growing share of mitigation activity — net-zero pledges resting on unproven or unscaled carbon dioxide removal, offset markets with weak additionality — performs decarbonization more than it delivers it. Accessibility collapse (0.5) is moderate: alternative framings (adaptation-priority, degrowth) remain articulable and are actively argued in academic and some diplomatic venues, so alternatives have not collapsed as completely as in a genuine mountain. Resistance (0.58) is substantial and growing, driven by Loss and Damage advocacy from vulnerable states and by degrowth critique gaining traction in some policy circles.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North cleantech firms, carbon-market intermediaries, and transitioning energy majors sit near the full-beneficiary end: they collect subsidy, patent rents, and offset revenue directly from the mitigation-priority allocation of attention and capital, and they retain arbitrage-grade exit (capital and IP mobility across jurisdictions) regardless of whether the underlying emissions trajectory actually holds. Multilateral finance institutions are agenda-setters whose eligibility criteria constitute the mechanism by which mitigation crowds out adaptation funding — they do not personally collect rents but they administer the allocation. Future generations, small island states, subsistence farmers, and coastal populations sit near the full-target end: they bear a deferred or immediate cost (adaptation infrastructure not built, physical exposure not reduced) generated by a resource-allocation choice made by parties who will not bear its consequences, and their exit options are trapped or constrained by geography, poverty, or non-existence-yet (future generations cannot exit a decision made before they exist).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a genuine global collective-action problem requiring coordinated emissions reduction — remains live (atmospheric CO2 concentration is a real physical stock problem with no unilateral solution). What has drifted is the allocation choice nested inside the response: mitigation's founding coordination logic has been extended, via agenda-setting power in finance institutions, into a near-exclusive claim on climate finance that leaves the coequal adaptation need chronically residual. This is not mandatrophy in the classic sense of an arrangement whose founding problem has died — the atmospheric problem is very much alive — but a founding problem whose SOLUTION has been captured by one contested reading of what 'solving' it means, at the expense of populations for whom mitigation alone (even if successful) does not address already-locked-in physical exposure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cdr_technology_maturity_ambiguity,
    'Will carbon dioxide removal and carbon capture technologies scale to the volumes assumed in mitigation-priority net-zero pathways within the timeframes those pathways assume, or is this reading''s core premise resting on unproven technology?',
    'Track deployed CDR/CCS capacity against IPCC pathway assumptions over the next decade; compare pledged versus actually-operating tonnage.',
    'If CDR fails to scale as assumed, the mitigation-priority reading''s deferral of adaptation spending will have left vulnerable populations without either delivered mitigation or delivered adaptation — sharply increasing the reading''s effective extraction from the victim set. If CDR scales as pledged, some of the deferred-adaptation cost may be avoided.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_technology_maturity_ambiguity, empirical, 'Whether unproven CDR reliance is a sound technical bet or a extraction-enabling deferral mechanism.').

omega_variable(
    mitigation_adaptation_tradeoff_or_complementarity,
    'Is the mitigation/adaptation finance split a genuine scarcity tradeoff (limited global climate finance forces prioritization) or a constructed political choice (finance could scale to fund both, but institutional agenda-setting has entrenched mitigation as primary)?',
    'Compare total available global climate finance capacity (including reallocable fossil subsidy and defense spending) against actual mitigation/adaptation allocation ratios; assess whether adaptation underfunding tracks genuine capital scarcity or institutional criteria design.',
    'If genuine scarcity, this reading''s structure is closer to an unavoidable coordination tradeoff (weaker case for tangled_rope, stronger case for scaffold-with-contested-sunset). If constructed political choice, the extraction is more clearly asymmetric and the tangled_rope classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_adaptation_tradeoff_or_complementarity, conceptual, 'Whether the mitigation-first allocation reflects real scarcity or institutional capture of allocation criteria.').

omega_variable(
    reading_boundary_and_kernel_identity,
    'Is ''the climate response imperative'' genuinely one contested kernel with three readings sharing a referent, or do the readings differ enough in their underlying empirical premises (about tipping points, technology feasibility, growth-emissions decoupling) that they are better modeled as three independent constraints with no shared kernel at all?',
    'Assess whether the three readings share enough common ground (shared physical problem: atmospheric GHG stock) despite differing solution premises; if the empirical premises diverge too far (e.g., degrowth''s claim that decoupling is impossible directly contradicts mitigation-priority''s core premise), the kernel model may itself be a simplification.',
    'If the readings are not genuinely one kernel, the forecloses/coexists_with/influences relations declared here would need re-examination — a forecloses relation might in fact be correct where coexists_with is declared, or vice versa.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_and_kernel_identity, conceptual, 'Whether the shared-kernel framing itself holds up under scrutiny of the readings'' actual empirical premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__mitigation_priority_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__mitigation_priority_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__mitigation_priority_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__mitigation_priority_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__mitigation_priority_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__mitigation_priority_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__mitigation_priority_reading, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__mitigation_priority_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the climate response imperative' per the ε-invariance principle. mitigation_priority_reading (this story) authors ε=0.68 with Global North innovation/finance sectors as beneficiaries and future generations/vulnerable regions as victims. adaptation_priority_reading authors a different ε and victim/beneficiary structure centered on resilience-financing allocation. degrowth_reading authors a third ε and structure centered on Global North consumption/redistribution. All three share the same underlying physical referent (atmospheric GHG accumulation) but diverge sharply in claimed solution premise, beneficiary set, and victim set — exactly the signal that these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
