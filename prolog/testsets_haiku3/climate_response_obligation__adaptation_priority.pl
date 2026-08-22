% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Climate Adaptation Priority over Mitigation (2-3°C Warming Acceptance)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading frames climate response as a choice
 *   between expensive mitigation (rapid decarbonization with near-term
 *   economic transition costs) and inevitable warming requiring resilience
 *   investment. It accepts 2-3°C as the outcome of current policy
 *   trajectories and allocates resources to adaptive infrastructure, crop
 *   breeding, relocation services, and disaster management. The frame
 *   protects incumbent economic systems and current-generation wealth from
 *   transition costs while transferring climate impacts and adaptation
 *   burdens to future generations and climate-vulnerable populations. This
 *   constraint is ONE reading of the contested kernel
 *   'climate_response_obligation'; the sibling readings (mitigation_priority,
 *   degrowth_reading) present structurally different beneficiary/victim sets
 *   and different treatments of inevitability.
 *
 * KEY AGENTS:
 *   - high_income_current_generation: Avoids transition costs; benefits from climate acceptance frame
 *   - fossil_fuel_capital: Extended operational runway; delayed transition means continued profit
 *   - energy_intensive_industries: Avoid costly decarbonization of production processes
 *   - future_generations: Locked into 2-3°C warming; powerless to renegotiate; no seat at table
 *   - global_south_populations: Climate-vulnerable; contribute least to emissions; bear greatest physical hazard
 *   - climate_vulnerable_communities: Local impacts (land loss, livelihood collapse); structurally excluded
 *   - adaptation_industry: Profit from accepting warming; solutions-economy beneficiary
 *   - mitigation_advocates: Structurally excluded; dispute inevitability and cost assumptions
 *   - regulatory_institutions: Enforce adaptation-priority through budgetary allocation and carbon pricing that is too weak
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.72).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Climate Adaptation Priority over Mitigation (2-3°C Warming Acceptance)").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, 'ee33876b-afaa-439b-a0fe-658d182a4d9d').
narrative_ontology:cs_kernel_codification('ee33876b-afaa-439b-a0fe-658d182a4d9d', fixed_text).
narrative_ontology:cs_authority_grounding('ee33876b-afaa-439b-a0fe-658d182a4d9d', extraction).
narrative_ontology:cs_interpretation_layer_present('ee33876b-afaa-439b-a0fe-658d182a4d9d').
narrative_ontology:cs_reading_relation('ee33876b-afaa-439b-a0fe-658d182a4d9d', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('ee33876b-afaa-439b-a0fe-658d182a4d9d', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('ee33876b-afaa-439b-a0fe-658d182a4d9d', foundational, warming_magnitude_economically_determined).
narrative_ontology:cs_axiom_status(warming_magnitude_economically_determined, holdable).
narrative_ontology:cs_axiom_grounding('ee33876b-afaa-439b-a0fe-658d182a4d9d', warming_magnitude_economically_determined, empirically_contingent).
narrative_ontology:cs_axiom('ee33876b-afaa-439b-a0fe-658d182a4d9d', foundational, current_generation_response_priority).
narrative_ontology:cs_axiom_status(current_generation_response_priority, holdable).
narrative_ontology:cs_axiom_grounding('ee33876b-afaa-439b-a0fe-658d182a4d9d', current_generation_response_priority, instrumental).
narrative_ontology:cs_reference_frame('ee33876b-afaa-439b-a0fe-658d182a4d9d', economic_feasibility_framework).
narrative_ontology:cs_drift_state('ee33876b-afaa-439b-a0fe-658d182a4d9d', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ee33876b-afaa-439b-a0fe-658d182a4d9d', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, high_income_current_generation).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_fuel_capital).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, energy_intensive_industries).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_populations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, climate_vulnerable_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wealthy nations and their current populations avoid the transition costs of rapid decarbonization (infrastructure replacement, employment disruption, energy price shocks) by accepting higher warming as 'inevitable' and planning only for adaptation. They retain economic advantages, consumption patterns, and financial security during their lifetime. Adaptation investment flows disproportionately to their own regions where wealth enables protective infrastructure.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, high_income_current_generation, beneficiary,
    institutional, biographical, arbitrage, global).

% Oil, gas, and coal industries gain extended operational runway by framing deep mitigation as economically impossible. Delayed transition means continued extraction, capital realization, and political influence. The adaptation frame does not disrupt incumbent energy systems; it treats warming as externality to be managed rather than cause to be eliminated.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_fuel_capital, beneficiary,
    powerful, biographical, mobile, global).

% Steel, cement, chemicals, aviation, and manufacturing industries avoid costly transformation of production processes. They argue adaptation is cheaper than retooling industrial capacity for zero-carbon pathways. They maintain profit margins and operational continuity.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, energy_intensive_industries, beneficiary,
    institutional, biographical, constrained, global).

% Will inherit a climate 2-3°C warmer than pre-industrial baseline with compound harms: extreme weather, ecosystem collapse, crop failures, water scarcity, migration crises. They have no seat at the table where this constraint is set. They cannot exit the constraint or renegotiate it. They bear the adaptation costs that wealthy regions do not prevent.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Bangladesh, sub-Saharan Africa, Pacific island nations, and other climate-vulnerable regions experience warming disproportionately: monsoon intensification, sea-level rise, drought, heat extremes. They have contributed least to cumulative emissions but face the greatest physical hazard. Adaptation funding promised by wealthy nations is chronically under-delivered. Their exit option is limited migration, often met with border restrictions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_populations, payer,
    organized, biographical, constrained, regional).

% Indigenous communities, small-island states, subsistence farmers, and urban poor in climate frontlines carry disproportionate impacts: land loss, livelihood collapse, health crisis. They have minimal capacity to adapt individually and are often excluded from national climate policy forums. The adaptation frame that accepts 2-3°C warming treats them as a cost to be managed rather than injustice to be prevented.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_vulnerable_communities, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, climate_vulnerable_communities, excluded).

% Emerging sector of consultants, engineers, and technology firms selling climate resilience solutions: seawalls, drought-resistant crops, climate-controlled infrastructure, disaster insurance, relocation services. They profit from accepting warming as inevitable and mobilizing adaptation capital. Mitigation would reduce their addressable market.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, adaptation_industry, beneficiary,
    powerful, biographical, mobile, global).

% Climate scientists, environmental organizations, and justice advocates argue that 2-3°C is incompatible with stable civilization, that mitigation costs are lower than adaptation costs, and that accepting warming is intergenerational theft. They are excluded from decision-making forums where energy and economic policy is set. Their presence would fundamentally contest the adaptation-priority framing.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, mitigation_advocates, excluded,
    organized, civilizational, constrained, global).

% National governments and international climate bodies (IPCC, UNFCCC) adjudicate climate response. The adaptation-priority reading is enforced through budgetary allocation to adaptation funds, carbon pricing that is too weak to drive decarbonization, and regulatory frameworks that treat warming as manageable risk rather than civilizational threat. They maintain this frame through institutional inertia and pressure from economic interests.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, regulatory_institutions, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_fuel_capital).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes a climate response that protects near-term economic stability and avoids transition disruption for wealthy incumbent systems. Provides a decision rule for allocating scarce climate finance: toward adaptive infrastructure in capable regions rather than mitigation everywhere.
% TRANSFER_FUNCTION: Transfers the burden of unmitigated climate impacts (2-3°C warming harms) from the current generation of wealthy nations and fossil industries to future generations and climate-vulnerable populations. Also transfers adaptation costs from those who caused warming to those who must live with it.
% ABSENT_VOICES: Future generations cannot attend policy forums. Global South representatives are structurally under-powered in climate negotiations. Indigenous communities and small-island states have minimal influence on energy and infrastructure decisions. Climate scientists' warnings about non-linear tipping points are filtered through economic feasibility frames that exclude them from the decisive forum.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority frame disappeared and mitigation became the binding goal, capital allocation would shift dramatically: fossil subsidies would be redirected to renewable infrastructure, energy-intensive industries would face pressure to decarbonize on accelerated timelines, and stranded-asset risk would materialize for coal and oil reserves. Energy prices and industrial economics would reorganize. Wealthy nations could not defer transition costs.
% FOUNDING_PROBLEM: Early climate science created profound uncertainty about warming magnitude and timing; economic models suggested mitigation costs were unaffordably high; incumbent energy systems were locked in by capital stock and political influence. The adaptation-priority frame emerged as a way to manage this uncertainty and high-cost problem without disrupting economic arrangements.
% FOUNDING_PROBLEM_CORROBORATION: The founding-problem status is 'dead' based on corroboration from outside the benefiting parties: (1) Renewable energy cost fell 85% 2010-2020, faster than 2015 projections; current cost-of-electricity studies show renewables cheaper than fossil in most markets (IRENA, Lazard). (2) Climate impact estimates have revised upward: IPCC AR6 (2021) shows that adaptation costs for 2-3°C warming would exceed mitigation costs for limiting to 1.5-2°C. (3) Insurance markets have repriced climate risk sharply upward: uninsurable risks emerging, stranded-asset recognitions driving capital reallocation. (4) Institutional investors and central banks (Bank for International Settlements, major pension funds) now treat the 'inevitability' of 2-3°C as contingent on policy choice, not physical lock-in. The founding problem's original justification (unaffordable mitigation, inevitable warming) has been substantially undermined by empirical developments. The constraint persists through institutional inertia and beneficiary power, not through continued validity of the original rationale.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the asymmetric burden transfer: wealthy regions and industries collect the benefit of avoided transition costs while future generations and the Global South carry the cost of unmitigated warming impacts. Suppression (0.72) is high because the constraint requires actively excluding mitigation advocates' argument that warming is preventable at lower cost; mitigation-cost claims and tipping-point science must be suppressed from high-level policy forums where adaptation-priority is treated as the rational baseline. Theater ratio (0.42) is moderate-high: adaptation investment is real and necessary (theaters are built, seawalls constructed, crop research funded), but a growing share of the constraint's maintenance activity involves defending the 'inevitability' claim against increasingly challenging evidence that mitigation costs have fallen faster than warming-damage estimates have risen. Accessibility collapse (0.38) is relatively low because alternative response pathways remain intellectually and technically available; the constraint's persistence depends more on institutional power and economic interest than on the collapse of real alternatives. Resistance (0.58) is substantial: scientists, environmental advocates, and climate-vulnerable governments continue to contest the adaptation-priority frame despite institutional disadvantages. The measurement series shows extractiveness and suppression rising over 2015-2050 as climate impacts compound and the constraint must work harder to suppress growing evidence that prevention would have been cheaper.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (high-income current generation, fossil capital, energy industries) perceive adaptation-priority as rational optimization under constraint — the best available policy given that mitigation is supposedly unaffordable. Regulatory institutions see it as balancing near-term economic stability with long-term resilience investment. The payer seats (future generations, Global South, vulnerable communities) experience the same constraint as imposed extraction: the constraint forecloses their capacity to inherit a stable climate and transfers costs to them that wealthier actors could have borne. Future generations cannot attend policy forums and have no voice; the Global South is under-represented in climate governance; climate scientists are excluded from energy-policy decision-making. The engine will compute different effective extraction (χ) for each seat from the structural data: beneficiary seats get low d (near 0.0), target seats get high d (near 1.0), and excluded/powerless seats show the paradox of high-impact constraints on actors with no power to negotiate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (high_income_current_generation, fossil_fuel_capital, energy_intensive_industries) have institutional power and structured exit routes (arbitrage-grade mobility for capital; arbitrage for industries via adaptation-industry capture). They derive d values near 0.0 (full beneficiary). Victims (future_generations, global_south_populations, climate_vulnerable_communities) are powerless or organized-but-constrained, with exit options that are identity-locked or trapped. Future generations literally have zero choice: they inherit the constraint post-completion. Global South and vulnerable communities face trapped or severely constrained exit (migration is restricted; relocation requires capital). Their d values approach 1.0 (full target). Mitigation advocates are organized but excluded from the table where binding decisions are made — they are structurally over-d-valued (high power atom: organized) yet politically under-represented (low agenda-setting access). A directionality override could correct this: override their power_atom downward from 'organized' toward 'moderate' to reflect the political asymmetry, or leave it high but acknowledge the override captures institutional dominance, not seat-level power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early climate uncertainty, high estimated mitigation costs, capital-stock lock-in) was real in 2015. By 2025, the problem conditions have substantially changed: technology costs have fallen faster than predicted; climate impacts are accelerating ahead of model projections; stranded-asset risk is materializing; renewable infrastructure is cost-competitive on levelized basis in most markets. The founding problem's status should be 'dead' or 'contested,' yet the adaptation-priority constraint persists — it is maintained by institutional inertia and by interests that benefit from continued fossil fuel operations. This is a mandatrophy signal: a constraint whose original rationale has eroded but whose beneficiaries have sufficient institutional power to keep it operating by suppressing alternative framings and locking in adaptation-infrastructure investment paths that become hard to reverse. The theater ratio rising from 0.22 to 0.42 over 2015-2050 reflects this: more energy spent defending the inevitability claim and less energy actually solving the coordination problem (which is now increasingly 'how do we transition infrastructure' rather than 'is transition possible').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_claim_contestation,
    'Is 2-3°C warming genuinely inevitable given current policy trajectories, or is ''inevitability'' a constructed frame that forecloses consideration of alternative pathways?',
    'Counterfactual analysis: if major economies had committed to Paris Agreement targets in 2015, what warming trajectory would result? Compare against claimed-inevitable trajectory. Cost-benefit analysis of 1.5°C vs. 2-3°C decarbonization scenarios using updated renewable energy cost curves.',
    'If alternative pathways to <2°C remain feasible at similar or lower cost than adaptation to 2-3°C, the ''inevitability'' framing is revealed as a political choice masquerading as physics. This would reclassify the constraint from tangled_rope (genuine coordination problem) toward snare (cover story for extraction). If 2-3°C is genuinely inevitable given locked-in commitments, the adaptation-priority reading becomes more defensible as tragic optimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_claim_contestation, empirical, 'Whether warming trajectory is locked-in physical necessity or contingent policy choice.').

omega_variable(
    adaptation_cost_underestimation,
    'Are adaptation costs accurately priced in the adaptation-priority frame, or are tipping-point risks, ecosystem collapse, and migration crises systematically underestimated?',
    'Systematic comparison of adaptation-cost models (used to justify adaptation priority) against peer-reviewed climate impact assessment. Track realized adaptation costs in climate-exposed regions (Pakistan floods 2022, Horn of Africa drought 2022-2023) against predicted costs. Monitor for uninsurable risks and cascade failures.',
    'If adaptation costs for 2-3°C warming exceed the mitigation costs that would have prevented it, the entire economic case for adaptation priority collapses. The constraint would be revealed as extraction dressed in false economic reasoning. If adaptation costs prove manageable but are concentrated in poor regions unable to pay, the constraint remains extractive but the distributional mechanism becomes visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_cost_underestimation, empirical, 'Whether adaptation-cost projections capture the full scope of harms and cascade failures.').

omega_variable(
    intergenerational_moral_authority,
    'What moral authority does the current generation have to accept permanent climate change on behalf of future generations? Is this constraint''s binding authority grounded in consent, power, or a framing that denies future generations standing?',
    'Philosophical/normative analysis: does the current generation''s lack of capacity to reverse climate change transfer into moral authority to impose it? Examine the constraint''s legitimacy claims (efficiency, inevitability, best-of-bad-options) and test whether they would survive if future generations held veto power. Look for institutional mechanisms that would elevate future-generation interests into decision-making.',
    'If future-generation interests are structurally excluded from the authority structure (as this analysis suggests they are), the constraint is sustained by power asymmetry rather than legitimate allocation — moving it from tangled_rope toward snare. If mechanisms for future-generation representation were built in, the legitimacy picture changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_moral_authority, preference, 'Whether the constraint''s binding authority rests on consent or structural power asymmetry.').

omega_variable(
    mitigation_cost_trajectory,
    'Are the cost projections that make adaptation-priority seem economical based on stable or outdated technology costs?',
    'Track renewable energy, battery storage, heat pump, and carbon capture costs over time. Compare 2015 cost projections (when adaptation-priority frame solidified) against 2025 and projected 2035 costs. Reassess mitigation-cost models with updated inputs.',
    'Renewable energy and storage costs have fallen 50-80% faster than projected in 2015. If this trajectory continues, the economic case for accepting warming instead of decarbonizing erodes. The constraint''s factual foundation shifts from ''mitigation is too expensive'' to ''we chose not to invest in rapidly-cheapening alternatives,'' moving the frame from defensive economics to revealed preference for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_cost_trajectory, empirical, 'Whether the economic models justifying adaptation-priority reflect current or obsolete technology costs.').

omega_variable(
    reading_kernel_contestation,
    'This constraint is ONE reading of the contested kernel ''climate_response_obligation''. Which reading — adaptation_priority, mitigation_priority, or degrowth_reading — is the legitimacy-grounding reading that the others contest?',
    'Examine institutional dominance: which reading is the default position of major economic and political actors? Which reading must make the affirmative case, and which reading is treated as the status quo? Which reading is harder to challenge without bearing the burden of proof?',
    'This reading (adaptation_priority) currently occupies the institutionally-dominant position — it is the implicit default in energy policy, carbon pricing, and adaptation-finance allocation. Mitigation-priority and degrowth-priority must make affirmative cases against it. This structural dominance affects how the constraint''s extraction is perceived: as inevitable management of a shared problem (adapting to warming) rather than as a choice to impose warming on others. If institutional balance shifted, the classification would reflect a different distribution of power and legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Whether this reading is institutionally dominant or contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 2015, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__adaptation_priority, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__adaptation_priority, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(clim_tr_t2025, climate_response_obligation__adaptation_priority, theater_ratio, 2025, 0.35).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__adaptation_priority, theater_ratio, 2030, 0.4).
narrative_ontology:measurement(clim_tr_t2040, climate_response_obligation__adaptation_priority, theater_ratio, 2040, 0.43).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__adaptation_priority, theater_ratio, 2050, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__adaptation_priority, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__adaptation_priority, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(clim_be_t2025, climate_response_obligation__adaptation_priority, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__adaptation_priority, base_extractiveness, 2030, 0.63).
narrative_ontology:measurement(clim_be_t2040, climate_response_obligation__adaptation_priority, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__adaptation_priority, base_extractiveness, 2050, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__adaptation_priority, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__adaptation_priority, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(clim_su_t2025, climate_response_obligation__adaptation_priority, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__adaptation_priority, suppression_requirement, 2030, 0.71).
narrative_ontology:measurement(clim_su_t2040, climate_response_obligation__adaptation_priority, suppression_requirement, 2040, 0.73).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__adaptation_priority, suppression_requirement, 2050, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_capital_protection).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, intergenerational_equity_obligation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, adaptation_finance_allocation).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family under the kernel 'climate_response_obligation'. The family contains three readings: adaptation_priority (this story), mitigation_priority (separate file), and degrowth_reading (separate file). Each reading instantiates different structural relationships and different victim/beneficiary sets from the same kernel. They are linked by network.affects_constraints to show family membership and causal dependencies. The family decomposition is necessary because ε differs substantially across readings: adaptation_priority frames the standing arrangement (accepting 2-3°C warming) as the constraint with substantial extraction; mitigation_priority frames rapid decarbonization as coordination with lower extraction; degrowth_reading frames material-throughput reduction as coordination with different boundary conditions. The three readings cannot be folded into one story without violating ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
