% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response Legitimacy Frame
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of the climate response legitimacy kernel
 *   is the globally dominant institutional framing: it holds that legitimate
 *   climate action prioritizes emissions reduction through technological
 *   innovation and carbon pricing, preserving economic growth through
 *   absolute decoupling. This reading is instantiated in the Paris Agreement,
 *   endorsed by the IPCC, operationalized through UN climate finance
 *   mechanisms, and defended by wealthy-nation governments, fossil fuel
 *   operators, and financial institutions. The constraint acts as an
 *   institutional filter: it legitimates certain climate policies (renewable
 *   investment, carbon markets, energy efficiency) while delegitimating
 *   others (demand reduction, economic restructuring, growth limits). It
 *   extracts from future generations (who inherit the risk if decoupling
 *   fails) and from current climate-vulnerable populations (whose adaptation
 *   is under-funded while capital flows to mitigation technology). Sibling
 *   readings—adaptation-priority and degrowth-transformation—exist in the
 *   institutional margins, excluded from mainstream climate governance but
 *   growing in influence as evidence of decoupling failure and impacts
 *   accumulation increases.
 *
 * KEY AGENTS:
 *   - Climate Policy Consensus Builders (institutional): set the mitigation-priority framing as legitimate at UNFCCC, IPCC, World Bank level
 *   - Incumbent Fossil Fuel Operators (institutional): benefit from delay embedded in technology-dependent strategy
 *   - High-Consumption Developed Economies (institutional): preserve growth trajectory while offloading transition costs and carbon-intensive production
 *   - Financial Sector Carbon Markets (institutional): extract rents from carbon pricing and green finance architecture
 *   - Climate Technology Developers (powerful): collect innovation rents and subsidies flowing from mitigation strategy
 *   - Future Generations (powerless): inherit civilizational risk if decoupling fails; trapped victims with no voice in strategy choice
 *   - Global South Nations (moderate): experience constraint as coercive; development pathways constrained by clean-tech requirement and carbon finance conditionality
 *   - Climate-Vulnerable Populations (powerless): under-funded adaptation while mitigation-tech capital flows elsewhere
 *   - Current Fossil-Sector Workers (powerless): bear concentrated transition costs; job loss without equivalent alternative income
 *   - Adaptation-Priority Advocates (moderate, excluded): structurally shut out of mainstream climate governance; would reframe toward present protection
 *   - Degrowth Advocates (powerless, identity-locked, excluded): radical reframing toward demand-side reduction; systematically delegitimated in policy discourse
 *   - Climate Scientists (analytical): provide empirical assessments of decoupling feasibility and deployment rates; measurement role only
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.52).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Response Legitimacy Frame").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, 'dc5c8742-7df6-4f3d-a733-5210f46b0bac').
narrative_ontology:cs_kernel_codification('dc5c8742-7df6-4f3d-a733-5210f46b0bac', fixed_text).
narrative_ontology:cs_authority_grounding('dc5c8742-7df6-4f3d-a733-5210f46b0bac', lineage).
narrative_ontology:cs_interpretation_layer_present('dc5c8742-7df6-4f3d-a733-5210f46b0bac').
narrative_ontology:cs_reading_relation('dc5c8742-7df6-4f3d-a733-5210f46b0bac', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('dc5c8742-7df6-4f3d-a733-5210f46b0bac', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('dc5c8742-7df6-4f3d-a733-5210f46b0bac', foundational, technological_decoupling_possible).
narrative_ontology:cs_axiom_status(technological_decoupling_possible, holdable).
narrative_ontology:cs_axiom_grounding('dc5c8742-7df6-4f3d-a733-5210f46b0bac', technological_decoupling_possible, empirically_contingent).
narrative_ontology:cs_axiom('dc5c8742-7df6-4f3d-a733-5210f46b0bac', foundational, growth_preservation_compatible_with_climate_safety).
narrative_ontology:cs_axiom_status(growth_preservation_compatible_with_climate_safety, holdable).
narrative_ontology:cs_axiom_grounding('dc5c8742-7df6-4f3d-a733-5210f46b0bac', growth_preservation_compatible_with_climate_safety, instrumental).
narrative_ontology:cs_axiom('dc5c8742-7df6-4f3d-a733-5210f46b0bac', secondary, carbon_pricing_sufficient_mechanism).
narrative_ontology:cs_axiom_status(carbon_pricing_sufficient_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('dc5c8742-7df6-4f3d-a733-5210f46b0bac', carbon_pricing_sufficient_mechanism, instrumental).
narrative_ontology:cs_reference_frame('dc5c8742-7df6-4f3d-a733-5210f46b0bac', paris_agreement_consensus).
narrative_ontology:cs_drift_state('dc5c8742-7df6-4f3d-a733-5210f46b0bac', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dc5c8742-7df6-4f3d-a733-5210f46b0bac', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_fossil_fuel_operators).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, high_consumption_developed_economies).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, financial_sector_carbon_markets).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, current_low_income_workers_fossil_sectors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as tangled rope because it performs two structural functions simultaneously: (1) genuine coordination—it solves a real collective-action problem (how to build global agreement on emissions reduction that permits simultaneous participation by countries with different development levels and capital positions) and (2) asymmetric extraction—it legitimates continued growth and extraction by wealthy nations while deferring the cost of climate safety to future generations and current vulnerable populations. Extraction rises sharply from 1990 (0.15) to 2025 (0.64) as the mitigation-priority consensus hardens and becomes the filter for legitimate climate policy, blocking alternatives. Theater rises in parallel (0.08 → 0.37) because the constraint increasingly operates as a legitimacy claim (we are taking climate action) divorced from emissions outcomes (absolute decoupling has not occurred at required scale; global emissions continued rising despite mitigation framing). Suppression (0.18 → 0.51) tracks the institutional effort required to maintain the single-reading dominance: excluding adaptation and degrowth framings from mainstream funding and policy, marginalizing advocates, and defending the narrative against empirical challenges. The measurements use one shared time grid: all three metrics are authored at every time point (1990, 2005, 2015, 2025, 2035, 2050) so temporal analysis has coherent data. The projection from 2025 onward reflects the constraint's expected trajectory under continued institutional commitment, though with slight declining extraction (0.71 → 0.68) as empirical pressure from decoupling failure and impacts accumulation mounts.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is severe. From the consensus-builder and beneficiary seats (climate policy institutions, developed governments, fossil operators, financial sector, tech developers), the constraint is experienced as genuine coordination: it solves the coordination problem of global emissions agreement, permits capital flows to solution technologies, and preserves the conditions (growth, economic stability) under which climate action is politically feasible. From the victim seats (future generations, vulnerable populations, Global South, fossil workers), the constraint is experienced as forced extraction: the coordination function is real but the extraction is unjust, the 'solutions' are speculative, and the temporal discount rate (present consumption weighted infinitely higher than future safety) is unacknowledged and indefensible. The engine computes this divergence from the structural data: beneficiaries get low d (extraction is scaled downward for them), victims get high d (extraction is scaled upward for them). From the beneficiary seats, the theater_ratio rising to 0.41 is acceptable slippage—'we are building toward true decoupling, interim performance gaps are expected.' From the victim seats, the same rising theater indicates false promise—'the constraint is maintaining legitimacy of continued extraction by claiming solutions that don't materialize.' The perspectival gap is irreducible; different seats have opposite empirical conclusions from identical metric series.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set includes fossil operators (who collect delay and policy cover), developed economies (who preserve growth), carbon-market financiers (who capture rents from new asset classes), and tech developers (who collect subsidies and market growth). For all four beneficiary seats, directionality is low (d ≈ 0.15–0.35): the constraint legitimates their continued activity, provides capital flows, and the costs they bear (transition investment, some stranded assets in fossil sectors) are manageable within their power levels. The victim set includes future generations (trapped, powerless, facing civilizational risk they cannot exit), vulnerable populations (powerless, immediate impact, constrained exit), Global South nations (moderate power, constrained exit through carbon finance conditionality), and current fossil workers (powerless, constrained exit through job loss). For all victim seats, directionality is high (d ≈ 0.65–0.95): the constraint extracts from them without providing compensation or alternative pathways; exit is trapped (generations), constrained (vulnerable populations, Global South, workers), or identity-locked (degrowth advocates). The wedge between beneficiary d and victim d is the asymmetry the constraint extracts from: same institutional framing, opposite structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint demonstrates mandatrophy—the founding problem (how to achieve global emissions agreement that permits growth) is increasingly dead relative to the constraint's persistent operation. Evidence: (1) absolute decoupling at required scale has not occurred (global emissions continued rising through the Paris Agreement period despite mitigation-priority framing); (2) technological deployment (renewables, batteries, carbon capture) lags the speed required by warming pathways; (3) the institutional response to mandatrophy is to maintain the framing anyway, intensifying theater as the gap between claim and outcome widens. Mandatrophy is coded in three field clusters: founding_problem_status = 'contested' (beneficiaries claim the founding problem is live, external analysts claim it is dead), theater_ratio rising while base_extractiveness rises (the constraint's legitimacy function is increasingly decoupled from its impact function), and the six omega variables documenting irreducible uncertainties about whether decoupling is feasible and whether the constraint is imposing catastrophic risk on future generations. The constraint's persistence despite mandatrophy is explained by: (1) path-lock—capital and policy institutional infrastructure is built around the mitigation-priority reading and cannot easily reorient, (2) beneficiary interest—those collecting extraction revenue have incentive to maintain the framing even as it fails, (3) internalized framing among policymakers—many climate consensus builders genuinely believe the narrative despite empirical challenges. Mandatrophy does not lead to rapid constraint dissolution; it leads to intensified performance (rising theater) to maintain legitimacy against mounting evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_empirical,
    'Can absolute emissions decoupling at the required scale (50-80% reduction by 2050 for 1.5°C pathway) be achieved through technological innovation and carbon pricing without demand reduction in high-consumption economies?',
    'Empirical observation: tracking actual decoupling rates, renewable deployment speed, carbon capture costs and scalability, energy return ratios, and cumulative emissions under deployed mitigation policies. The IPCC Global Stocktake and periodic emissions gap assessments measure this directly.',
    'If decoupling proves infeasible at required scale, the founding problem is dead, the constraint''s entire legitimacy architecture collapses, and future generations inherit the civilizational risk the constraint was designed to manage. The constraint would be reclassified as a snare (false promise of safety) rather than tangled rope (genuine coordination with asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoupling_feasibility_empirical, empirical, 'Whether technological decoupling can deliver required emissions reductions without demand-side policy.').

omega_variable(
    carbon_pricing_implementation_gap,
    'Does carbon pricing at levels sufficient to drive emissions reduction actually emerge, or does the mechanism remain too weak to shift industrial investment patterns (price-to-action gap)?',
    'Market observation: tracking global carbon price levels (observed: 2–30 USD/ton; required: 50–200+ USD/ton for decoupling), actual emissions reduction from price signals, and the gap between modeled and implemented pricing in major economies.',
    'A persistent implementation gap would show the constraint functions partially as theater—carbon pricing is authorized and deployed but with insufficient force to drive actual mitigation. This raises theater_ratio and suggests the constraint extracts legitimacy credit for climate action that doesn''t materialize.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_pricing_implementation_gap, empirical, 'Whether carbon pricing actually drives emissions reduction or remains performative.').

omega_variable(
    technological_lock_in_risk,
    'Does the constraint''s technology dependency create path-lock that forecloses adaptation strategies if early technological bets (renewables, batteries, carbon capture) fail to encounter hard resource limits (rare earth dependence, water use, land use conflict)?',
    'Technological feasibility analysis: deployment rates of renewable energy, energy storage scaling curves, carbon capture cost and thermodynamic efficiency, competing uses for scarce materials. If deployment curves flatten below required rates, the lock-in has materialized.',
    'If lock-in occurs, the constraint has imposed civilizational bet-all risk on future generations who cannot undo the choice. The constraint transitions from tangled rope (coordinating current action with shared sacrifice) to snare (current generation imposes unilateral risk on future without alternatives). Future generations become locked-in victims rather than merely bearing transition costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lock_in_risk, empirical, 'Whether technology-dependent strategy creates irreversible path-lock that forecloses adaptation if early bets fail.').

omega_variable(
    kernel_reading_contest,
    'Which reading of the climate response legitimacy kernel is structurally defensible: mitigation-priority (this reading), adaptation-priority, or degrowth-transformation?',
    'This is the kernel contest itself. Structural resolution comes through: (a) empirical falsification (decoupling fails, technology doesn''t scale, adaptation needs overwhelm available resources), (b) path-dependent institutional lock-in (one reading becomes self-fulfilling because capital and policy flow to it, foreclosing others), (c) normative political shift (a different reading accumulates sufficient political power to override the current consensus). No single test resolves this; the three readings will coexist under different jurisdictions and institutional framings indefinitely.',
    'If the adaptation-priority or degrowth readings gain institutional authority, the constraint''s entire classification shifts: beneficiaries lose priority status, victims gain voice, and the arrangement is reframed as partial extraction rather than coordination. The contest is not resolvable empirically; it is a permanent feature of climate governance under uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of climate response legitimacy will dominate future governance: mitigation-priority, adaptation-priority, or degrowth-transformation.').

omega_variable(
    intergenerational_discount_rate_framing,
    'Is the constraint''s implicit discount rate (present growth weighted heavily relative to future safety) ethically and epistemically defensible, or does it embed an unjustifiable burden-shifting on parties who cannot consent?',
    'Normative analysis: the constraint rests on a specific intergenerational discount rate (implicit in ''we will solve this with future technology, so today''s growth remains justified''). This is not a technical parameter—it is a normative choice about whose interests count. Different framings (adaptation-priority, degrowth) embed different discount rates. Normative resolution requires explicit political choice, not empirical discovery.',
    'If the implicit discount rate is rejected as unethical (future interests weighted zero or negative relative to present consumption), the entire legitimacy of the constraint evaporates. The constraint becomes recognized as intergenerational extraction—current generation taking resources and safety margin at the cost of future generations'' options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discount_rate_framing, preference, 'Whether the implicit intergenerational discount rate of the mitigation-priority reading is ethically justifiable.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the resistance to alternative framings (adaptation-first, degrowth) suppressed through structural exclusion (institutional gatekeeping, funding allocation), internalized framing (advocates believe the narrative is correct), or both?',
    'Institutional analysis: tracking funding flows to research (more to mitigation technology than adaptation or degrowth modeling), publication patterns (mitigation papers dominate peer-review venues), policy process access (which advocates get seats at negotiation tables), and post-exit trajectories of advocates who break from the consensus (do they face professional consequences or are they accepted as legitimate dissenters).',
    'If suppression is primarily structural, it could be reversed through institutional reform (open funding, decentralize policy setting). If suppression is internalized (advocates of alternatives have fused their professional identity to consensus, cannot imagine other framings as serious), exit and reclassification would be slower and more costly. High internalization suggests the constraint''s persistence depends on identity-lock, not just enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether resistance to alternative climate response readings is structurally suppressed or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_response_legitimacy__mitigation_priority, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(clim_tr_t2005, climate_response_legitimacy__mitigation_priority, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(clim_tr_t2015, climate_response_legitimacy__mitigation_priority, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(clim_tr_t2025, climate_response_legitimacy__mitigation_priority, theater_ratio, 2025, 0.37).
narrative_ontology:measurement(clim_tr_t2035, climate_response_legitimacy__mitigation_priority, theater_ratio, 2035, 0.42).
narrative_ontology:measurement(clim_tr_t2050, climate_response_legitimacy__mitigation_priority, theater_ratio, 2050, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_response_legitimacy__mitigation_priority, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(clim_be_t2005, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(clim_be_t2015, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(clim_be_t2025, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2025, 0.64).
narrative_ontology:measurement(clim_be_t2035, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2035, 0.71).
narrative_ontology:measurement(clim_be_t2050, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_response_legitimacy__mitigation_priority, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(clim_su_t2005, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2005, 0.32).
narrative_ontology:measurement(clim_su_t2015, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(clim_su_t2025, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2025, 0.51).
narrative_ontology:measurement(clim_su_t2035, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2035, 0.54).
narrative_ontology:measurement(clim_su_t2050, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2050, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested climate response legitimacy kernel. The mitigation-priority, adaptation-priority, and degrowth-transformation readings are three distinct constraints sharing the same kernel (climate policy legitimacy). Each reading instantiates a different ε, different beneficiary/victim structure, and different classification. The readings coexist under different institutional jurisdictions and political coalitions. Network edges link all three readings bidirectionally: each influences the others by providing alternative framings and establishing competing legitimacy claims. The upstream constraint (mitigation-priority, this one) influences downstream constraints because it is the currently dominant institutional reading; sibling readings must position themselves relative to its dominance. Decomposition rationale: the three readings have incommensurable ε values and victim sets because they make different foundational claims about what 'legitimate climate response' means. Attempting to fold all three into one constraint would violate ε-invariance and force false metrics compromises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
