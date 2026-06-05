% ============================================================================
% CONSTRAINT STORY: drinking_water_contamination_legacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_drinking_water_contamination_legacy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: drinking_water_contamination_legacy
 *   human_readable: Drinking Water Contamination Legacy
 *   domain: environmental_health/public_infrastructure
 *
 * SUMMARY:
 *   The drinking water contamination legacy — primarily lead in aging pipe
 *   infrastructure — creates a structural extraction mechanism targeting
 *   powerless populations. Unlike acute industrial contamination events, the
 *   legacy operates through deferred replacement costs, regulatory
 *   forbearance, and geographic concentration of poverty. Residents in
 *   contaminated areas cannot exit due to economic mobility barriers;
 *   utilities and municipalities benefit from deferring infrastructure
 *   replacement; the environmental regulation apparatus operates as theater
 *   (compliance testing without enforcement of actual health protection). The
 *   constraint exhibits all hallmarks of a snare: high extraction (health
 *   costs, reduced lifespan, cognitive damage), high suppression (economic
 *   immobility, regulatory capture), and asymmetric burden placement on
 *   powerless agents. The extractiveness has increased from 0.42 to 0.68 over
 *   the 45-year measurement interval as scientific understanding of lead
 *   toxicity has deepened while replacement efforts remain stalled — the
 *   extraction magnitude is now undeniable even as infrastructure debt
 *   accumulates. Theater ratio has risen from 0.35 to 0.58 as the regulation
 *   apparatus shifts from 'problem not yet recognized' to 'problem recognized
 *   and performatively addressed through compliance testing that does not
 *   drive actual replacement.'
 *
 * KEY AGENTS:
 *   - Low-Income Residents: Primary victim (powerless/trapped) — economically immobile, cannot relocate, bear full health extraction burden
 *   - Marginalized Communities: Primary victim class (powerless/trapped, generational) — cumulative extraction across generations, geographic entrapment perpetuates poverty
 *   - Future Generations: Secondary victim (powerless/identity_locked) — pre-determined extraction through inherited geographic/economic status and cognitive development damage
 *   - Water Utility Companies: Beneficiary/extractor (institutional/arbitrage) — defer replacement costs to maximize near-term shareholder value; constrained by long-term liability
 *   - Municipal Governments: Beneficiary/co-extractor (institutional/constrained) — defer replacement to manage impossible budget constraints; trapped by political economy of municipal finance
 *   - EPA/Regulatory Apparatus: Theater performer (institutional/arbitrage) — maintains compliance appearance without enforcement; captured by industry and budget constraints
 *   - Environmental Justice Advocates: Organized victims (organized/constrained) — coordinate resistance but face resource limitations and regulatory capture
 *   - Wealthy Residents: Non-victims (powerful/mobile) — can purchase alternative water supply; no meaningful extraction experienced
 *   - Analytical Observer: Risk of false mountain (analytical/analytical) — tempted to naturalize as inevitable aging infrastructure problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(drinking_water_contamination_legacy, 0.68).
domain_priors:suppression_score(drinking_water_contamination_legacy, 0.72).
domain_priors:theater_ratio(drinking_water_contamination_legacy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(drinking_water_contamination_legacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(drinking_water_contamination_legacy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(drinking_water_contamination_legacy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(drinking_water_contamination_legacy, snare).
narrative_ontology:human_readable(drinking_water_contamination_legacy, "Drinking Water Contamination Legacy").
narrative_ontology:topic_domain(drinking_water_contamination_legacy, "environmental_health/public_infrastructure").

domain_priors:requires_active_enforcement(drinking_water_contamination_legacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(drinking_water_contamination_legacy, low_income_residents).
narrative_ontology:constraint_victim(drinking_water_contamination_legacy, marginalized_communities).
narrative_ontology:constraint_victim(drinking_water_contamination_legacy, future_generations).
narrative_ontology:constraint_victim(drinking_water_contamination_legacy, public_health_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RESIDENTS (SNARE) — Residents cannot exit due to economic constraints (cannot afford to relocate, renting without mobility). Bears full extraction burden: health costs, shortened lifespan, cognitive development damage in children, chronic disease. No alternatives to contaminated water without extreme cost. Maximum experienced extraction.
constraint_indexing:constraint_classification(drinking_water_contamination_legacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (SNARE, GENERATIONAL) — Generational entrapment: contamination clusters in communities with historical disinvestment. Residents face cumulative extraction across generations — contamination exposure in childhood predicts worse economic mobility, perpetuating the geographic trap. Exit options for future generations are pre-determined by present contamination and resulting socioeconomic outcomes.
constraint_indexing:constraint_classification(drinking_water_contamination_legacy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ENVIRONMENTAL JUSTICE ADVOCATES (TANGLED ROPE) — Organized groups coordinate risk information sharing and legal action, achieving some collective power. But constrained by resource limits, litigation timelines, and regulatory capture. Some benefit from visibility and funding, but primary function is mobilization for powerless groups. Moderate extraction with genuine coordination role.
constraint_indexing:constraint_classification(drinking_water_contamination_legacy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WATER UTILITY COMPANIES (TANGLED ROPE) — Benefit from deferred infrastructure replacement costs and regulatory forbearance (arbitrage: can delay upgrades at low near-term cost). Also coordinate essential service delivery. But faces extraction too: long-term liability, regulatory pressure, infrastructure debt snowball. Hybrid position — some extraction outflow to residents, some benefit inflow to shareholders.
constraint_indexing:constraint_classification(drinking_water_contamination_legacy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL REGULATION APPARATUS (PITON) — The Safe Drinking Water Act and EPA oversight remain performative theater: compliance measured by technical parameters (lead levels in samples), not health outcomes. Enforcement concentrated on large systems with capacity to comply; small/rural systems evade enforcement. The regulatory apparatus maintains surveillance appearance while functional verification of actual water safety remains minimal. Theater ratio reflects performative compliance testing.
constraint_indexing:constraint_classification(drinking_water_contamination_legacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MUNICIPAL GOVERNMENTS (PITON) — Aware that infrastructure replacement is necessary but politically impossible (costs ≈ entire municipal budgets). Maintain theatrical urgency without action. Budget cycles and election timelines prevent long-term infrastructure investment. The constraint persists through institutional inertia — replacement would require financing mechanisms that exceed municipal capacity.
constraint_indexing:constraint_classification(drinking_water_contamination_legacy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: WEALTHY RESIDENTS (ROPE) — Can purchase alternative water supply (bottled water, filters, relocation). No meaningful extraction experienced — the constraint affects them minimally. Coordination function: public systems still serve them even if unused. Low effective extraction because exit is accessible and low-cost.
constraint_indexing:constraint_classification(drinking_water_contamination_legacy, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — Risk naturalizing as inevitable: aging infrastructure decays, replacement is expensive, political cycles prevent long-term investment. This perspective treats the contamination legacy as an immutable constraint of aging infrastructure. However, comparative analysis (Netherlands, Germany) shows that sustained political commitment and progressive taxation can solve this at scale. The mountain classification is a false summit — naturalization of a contingent political choice.
constraint_indexing:constraint_classification(drinking_water_contamination_legacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(drinking_water_contamination_legacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(drinking_water_contamination_legacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(drinking_water_contamination_legacy, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(drinking_water_contamination_legacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(drinking_water_contamination_legacy, TR),
    TR >= 0.70.

:- end_tests(drinking_water_contamination_legacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint extracts health costs, reduced cognitive development, shortened lifespan, and chronic disease burden from powerless populations. As scientific evidence accumulates (lead causality now established at ppb levels below EPA action threshold), the extraction becomes more undeniable. The trend from 0.42 to 0.68 reflects not worsening contamination but rather improved measurement and causality evidence — the extraction was always present, now measured more accurately. Suppression (0.72): High. Residents trapped by economic immobility (cannot relocate, cannot afford bottled water indefinitely, cannot afford medical care for lead exposure effects). Regulatory capture limits enforcement. Utilities use informational asymmetry to obscure replacement necessity. Widespread false belief that 'lead is no longer a problem' (theater effect) reduces political pressure. Theater ratio (0.58): Moderate-high and increasing. EPA compliance testing focuses on technical compliance (sampling protocol) rather than actual health outcomes. Lead detection shifted from universal testing to passive response — only systems detected exceeding threshold face remediation pressure. Regulatory language ('action level') obscures that 'action level' is not a health standard but a treatment threshold, creating theatrical distinction from actual public health thresholds. Increasing theater reflects the system's shift toward performative rather than functional response as costs become undeniable.
 *
 * PERSPECTIVAL GAP:
 *   Low-income residents perceive snare (trapped, maximum extraction). Wealthy residents perceive rope (mobile, minimal extraction). Environmental advocates perceive tangled_rope (organized but constrained, mixed coordination and extraction). Municipalities perceive piton (know the problem, maintain theater). EPA perceives piton (performative compliance). Analytical observer risks mountain (naturalizing contingent political failure). The engine classifies this as snare at the canonical analytical level (analytical power, global scope, analytical exit) — snare classification is preserved and justified. The piton and rope perspectives are perceptually valid from within those agents' frames but do not contradict snare as the structural type when evaluated from the canonical analytical position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural relationship to extraction flow. Low-income residents are full victims (trapped exit + victim status + powerless power → d ≈ 0.95 → f(d) ≈ 1.42 → high χ). Water utilities are partial beneficiaries (arbitrage exit + beneficiary through deferred costs + institutional power → d ≈ 0.15 → f(d) ≈ -0.01 → low χ, or even negative). Environmental justice advocates are constrained organized agents (organized power + constrained exit + partial victim status → d ≈ 0.45 → f(d) ≈ 0.50 → moderate χ). The piton perspectives (EPA, municipalities) have arbitrage exits but face institutional constraint — they derive d from their enforcer role (partial beneficiary, partial victim) → d ≈ 0.35 → f(d) ≈ 0.30 → low-moderate χ. Scope modifier: most perspectives use local or regional scope (σ ≈ 0.8-0.9, dampening χ slightly), but analytical observer uses universal scope (σ = 1.0), meaning their false mountain is evaluated with standard scaling. No directionality overrides needed — structural derivation produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via specification of mechanism. The constraint is definitively snare (not misclassified coordination): (1) Legitimate coordination could reduce lead contamination through information-sharing about tap safety and replacement priority. But this coordination is precisely what is suppressed — residents lack information about actual contamination in their water. (2) Beneficiaries exist (utilities deferring costs, municipalities avoiding budget crisis), but their benefit is not from coordination value; it is from extraction. If the primary function were coordination (establishing safe water standards and monitoring), the constraint would be rope or tangled_rope. But the primary function is extraction (capturing the surplus value of deferred replacement). The mandatrophy is resolved by identifying what function maintains the constraint: it is not 'coordinating water safety' but 'deferring replacement costs.' This is extraction, not coordination. The constraint could be replaced by genuine coordination (progressive taxation for universal replacement, transparent testing, regulatory enforcement) without loss of function. The snare classification is stable and justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lead_exposure_causality_chain,
    'How much of the observed health damage in contaminated communities is directly attributable to water contamination vs. confounded by poverty, food insecurity, pollution, and healthcare access disparities?',
    'Longitudinal cohort studies controlling for socioeconomic factors; instrumental variable analysis using geographic water quality variation; comparison of health outcomes between adjacent communities with different water systems',
    'If primarily water-driven: snare classification is overdetermined by the data. If primarily confounded: extraction may be lower than measured, though multi-factor cumulative harm remains. If mixed: extraction persists even after controlling for poverty (indicates water-specific mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lead_exposure_causality_chain, empirical, 'Causality attribution for health damage in contaminated communities').

omega_variable(
    infrastructure_replacement_financial_feasibility,
    'At what tax rate and over what timeline can municipalities finance full lead service line replacement without regressive burden on poor residents?',
    'Comparative analysis of water infrastructure financing models (public banking, progressive taxation, federal bonds); modeling of household burden by income decile for various financing structures',
    'If feasible with progressive taxation (< 5% of median household income): the constraint is a political choice (snare from policy), not technical inevitability. If requiring > 15% burden: genuine infrastructure poverty trap exists (snare from economics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_replacement_financial_feasibility, empirical, 'Financial feasibility of equitable infrastructure replacement').

omega_variable(
    regulatory_capture_depth,
    'To what degree do water utilities influence EPA and state environmental regulations on safe lead levels, compliance testing protocols, and enforcement timelines?',
    'Regulatory history analysis: timeline of lead level changes and scientific basis; revolving-door tracking (EPA staff to utilities); lobbying expenditure correlation with regulatory decisions; comparative analysis of countries with strict lead standards and no utility veto',
    'If capture is deep (utilities block tighter standards): snare classification is reinforced by extractive regulatory dynamics. If minimal: snare is driven by technical complexity alone (lower confidence in extraction claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of regulatory capture by water utilities').

omega_variable(
    intergenerational_mobility_link,
    'Do children exposed to lead-contaminated water as a cohort show reduced educational attainment and earnings relative to unexposed cohorts, net of initial family income?',
    'Longitudinal administrative data linking childhood water quality to school performance, graduation, college attendance, and adult earnings; causal inference using geographic discontinuities in water system quality',
    'If strong intergenerational link: the constraint mechanically perpetuates poverty (snare across generations, not just current exposure). If weak link: health damage is localized to exposure cohort (snare remains but not transgenerational).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_mobility_link, empirical, 'Intergenerational mobility consequences of lead exposure').

omega_variable(
    theater_ratio_measurement_validity,
    'Is the observed theater_ratio (0.58) accurate, or does EPA compliance testing underestimate actual contamination through sampling bias (testing from flushed taps vs. first-draw water)?',
    'Independent residential water quality audits in compliance-passing systems; comparison of EPA-reported lead levels vs. independently measured levels in same homes; analysis of sampling protocol effects',
    'If independent testing shows higher contamination: theater_ratio should increase (actual compliance theater higher than measured), snare classification strengthens. If EPA data accurate: theater_ratio is correctly calibrated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_measurement_validity, empirical, 'Validity of regulatory compliance testing in measuring true water quality').

omega_variable(
    exit_option_availability_for_trapped_agents,
    'What percentage of low-income residents in contaminated areas have actual capacity to relocate (employment mobility, credit access, family networks) vs. appearing trapped due to structural barriers?',
    'Survey data on housing market access, employment geography, rental discrimination, family support systems; comparison with exit rates in neighborhoods with improved water quality',
    'If exit is truly impossible: trapped exit_options are correct (d approaches 1.0). If significant hidden mobility: some agents could exit at high but surmountable cost (reclassify as constrained, not trapped; lowers d slightly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_availability_for_trapped_agents, empirical, 'Structural vs. perceived exit barriers for low-income residents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(drinking_water_contamination_legacy, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwcl_theater_t0, drinking_water_contamination_legacy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dwcl_theater_t15, drinking_water_contamination_legacy, theater_ratio, 15, 0.48).
narrative_ontology:measurement(dwcl_theater_t30, drinking_water_contamination_legacy, theater_ratio, 30, 0.58).
narrative_ontology:measurement(dwcl_theater_t45, drinking_water_contamination_legacy, theater_ratio, 45, 0.62).

% Extraction over time
narrative_ontology:measurement(dwcl_extractiveness_t0, drinking_water_contamination_legacy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dwcl_extractiveness_t15, drinking_water_contamination_legacy, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(dwcl_extractiveness_t30, drinking_water_contamination_legacy, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(dwcl_extractiveness_t45, drinking_water_contamination_legacy, base_extractiveness, 45, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(drinking_water_contamination_legacy, resource_allocation).
narrative_ontology:boltzmann_floor_override(drinking_water_contamination_legacy, 0.18).
narrative_ontology:affects_constraint(drinking_water_contamination_legacy, industrial_lead_emissions_legacy).
narrative_ontology:affects_constraint(drinking_water_contamination_legacy, soil_contamination_urban_agriculture).
narrative_ontology:affects_constraint(drinking_water_contamination_legacy, cognitive_development_inequality).

% DUAL FORMULATION NOTE:
% The drinking water contamination legacy is downstream of historical industrial pollution and urban spatial segregation but represents a distinct constraint with its own extractiveness and suppression metrics. The upstream constraints (industrial emissions, segregation policy) created the conditions; this constraint operates the extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(drinking_water_contamination_legacy, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
