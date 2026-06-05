% ============================================================================
% CONSTRAINT STORY: 2003_bush_clear_skies_emissions_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_2003_bush_clear_skies_emissions_mandate, []).

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
 *   constraint_id: 2003_bush_clear_skies_emissions_mandate
 *   human_readable: Clear Skies Act: 70% Emissions Reduction Mandate (2003-2018)
 *   domain: environmental_regulation/energy_policy
 *
 * SUMMARY:
 *   The Clear Skies Act of 2003 replaced prescriptive command-and-control
 *   regulation with outcome-based emissions targets: power plants must reduce
 *   sulfur dioxide, nitrogen oxides, and mercury emissions by 70% within 15
 *   years, with flexibility in compliance method (technology choice, fuel
 *   switching, market-based credits). This structural shift from regulatory
 *   prescription to performance obligation creates a mixed constraint
 *   exhibiting both genuine coordination (problem-solving through innovation
 *   flexibility) and asymmetric extraction (capital costs and stranded asset
 *   burdens concentrated on utilities). The constraint's effectiveness
 *   depends on: (1) whether enforcement makes targets binding, (2) whether
 *   technology cost trajectories support compliance, (3) whether utilities
 *   externalize emissions rather than reduce them, and (4) whether captured
 *   regulators block alternative emissions pathways. The extractiveness
 *   trajectory rises during years 1-10 as compliance capital costs
 *   accumulate, peaks at year 10 as stranded asset write-downs occur, and
 *   plateaus as matured investments and declining technology costs make
 *   compliance less expensive. Theater ratio rises as compliance
 *   documentation infrastructure elaborates (emissions monitoring, credit
 *   tracking, quarterly reporting), then plateaus as the compliance system
 *   matures.
 *
 * KEY AGENTS:
 *   - Affected Communities: Powerless/trapped (geographic binding) — nominal beneficiaries of air quality improvement but dependent on utility compliance; bear extraction if enforcement lapses
 *   - Coal Power Plant Operators: Moderate/constrained — face binding reduction targets with flexibility in method; bear capital costs and stranded asset losses; benefit from innovation opportunities
 *   - Clean Technology Providers: Powerful/arbitrage — institutional beneficiaries; markets created by mandate; exit through product diversification
 *   - Utilities Holding Generation Assets: Institutional/constrained — face binding compliance with operational flexibility; benefit from outcome-based method (efficiency discovery) but bear asymmetric capital costs; cannot exit the target
 *   - Environmental Advocacy Coalitions: Organized/constrained — support the 15-year binding commitment as transitional scaffold toward market-driven decarbonization; see sunset logic in technology cost decline
 *   - Regulatory Compliance Theater System: Institutional/arbitrage — emissions monitoring infrastructure, credit tracking, reporting protocols; persists through institutional inertia even as market forces drive decarbonization
 *   - Analytical Observer: Universal/analytical — views binding timeline as policy choice (contingent) rather than physics (immutable)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(2003_bush_clear_skies_emissions_mandate, 0.52).
domain_priors:suppression_score(2003_bush_clear_skies_emissions_mandate, 0.48).
domain_priors:theater_ratio(2003_bush_clear_skies_emissions_mandate, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(2003_bush_clear_skies_emissions_mandate, extractiveness, 0.52).
narrative_ontology:constraint_metric(2003_bush_clear_skies_emissions_mandate, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(2003_bush_clear_skies_emissions_mandate, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(2003_bush_clear_skies_emissions_mandate, tangled_rope).
narrative_ontology:human_readable(2003_bush_clear_skies_emissions_mandate, "Clear Skies Act: 70% Emissions Reduction Mandate (2003-2018)").
narrative_ontology:topic_domain(2003_bush_clear_skies_emissions_mandate, "environmental_regulation/energy_policy").

domain_priors:requires_active_enforcement(2003_bush_clear_skies_emissions_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(2003_bush_clear_skies_emissions_mandate, air_quality_beneficiaries).
narrative_ontology:constraint_beneficiary(2003_bush_clear_skies_emissions_mandate, public_health_sectors).
narrative_ontology:constraint_beneficiary(2003_bush_clear_skies_emissions_mandate, renewable_energy_producers).
narrative_ontology:constraint_victim(2003_bush_clear_skies_emissions_mandate, coal_power_operators).
narrative_ontology:constraint_victim(2003_bush_clear_skies_emissions_mandate, compliance_cost_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED COMMUNITIES (SNARE) — Powerless populations dependent on existing air quality face extraction despite nominal beneficiary status. The constraint's enforcement relies on utility compliance, not direct community agency. Communities cannot exit the geographic binding of pollution sources and have no mechanisms to ensure continued air quality post-mandate. Even as nominal beneficiaries, they are trapped by dependence on regulatory enforcement — if enforcement lapses, extraction resumes. No independent exit option.
constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COAL POWER OPERATORS (TANGLED ROPE) — Face binding emissions reduction targets with moderate flexibility in compliance method (technology choice within outcome framework). This generates mixed extraction and coordination: the target drives investment in cleaner technology (coordination function — problem-solving through innovation) while creating capital costs and stranded asset risk (asymmetric extraction). Operators can reduce compliance costs through early adoption and efficiency innovation, but cannot exit the binding target. Constrained by regulatory requirement but moderate power through operational flexibility.
constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLEAN TECHNOLOGY PROVIDERS (ROPE) — Institutional beneficiaries with arbitrage options. The mandate creates market demand for scrubbing equipment, renewable capacity, and efficiency upgrades. Technology providers experience the constraint as pure coordination: the target articulates a collective need, and competitive markets solve the technical problem. Exit option is available through product diversification. Net beneficiary — extraction runs toward this institutional actor through newly-created markets.
constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UTILITIES (TANGLED ROPE) — Institutional actors facing binding compliance requirements with constrained but real flexibility. The outcome-based mandate (vs. prescriptive command-and-control) provides genuine coordination benefit: utilities can optimize compliance by choosing among portfolio strategies (fuel switching, efficiency, renewable procurement, carbon credits). Simultaneously, utilities bear asymmetric extraction: capital costs, stranded coal asset write-downs, and rate pressure. The constraint serves coordination function (enabling efficient compliance discovery) alongside extraction (capital burden concentration on single sector). Institutional power provides some negotiation capacity but cannot exit the binding target.
constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL COALITIONS (SCAFFOLD) — Organized agents (Sierra Club, American Lung Association, CERES) see the 70% mandate as a temporary bridge from prescriptive regulation to market-driven emissions reduction. The 15-year timeline with binding targets contains implicit sunset logic: if renewable cost curves fall and market competition drives decarbonization naturally, the mandate becomes redundant. Suppression is high during the commitment window (utilities cannot exit) but designed to decline as technology costs drop and voluntary market adoption replaces regulatory requirement. Organized agents have exit-path visibility and see the constraint's enforced window as transitional.
constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY COMPLIANCE THEATER (PITON) — The outcome-based mandate replaces prescriptive command-and-control rules with performance targets and market mechanisms (cap-and-trade, renewable energy credits, efficiency banking). This creates new theaters: utilities demonstrating compliance through documentation of credit retirement, regulator-approved monitoring protocols, and quarterly emissions reporting. The theater_ratio is substantial (0.58) because documenting technology adoption and compliance achievement requires elaborate measurement infrastructure that is partly functional verification and partly performative documentation. The system persists through regulatory inertia — even as market forces drive decarbonization independently, the compliance apparatus maintains itself (reporting requirements, credit tracking, approval processes) because institutional structures have embedded.
constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scale, some emissions reduction lag is inherent to the physical infrastructure: replacing coal capacity with alternative generation takes time, capital investment, and R&D maturation. The engine flags this as a false summit — the physical constraint (renewable technology maturation rates) is real, but the policy constraint (binding timeline, outcome targets, compliance mechanisms) is contingent institutional design that could be more or less stringent, more or less flexible, or structured entirely differently. Naturalizing the binding 15-year timeline as immutable law would obscure that the timeline itself is policy choice, not physics.
constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(2003_bush_clear_skies_emissions_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(2003_bush_clear_skies_emissions_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(2003_bush_clear_skies_emissions_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(2003_bush_clear_skies_emissions_mandate, TR),
    TR >= 0.70.

:- end_tests(2003_bush_clear_skies_emissions_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes significant capital costs on utilities (scrubber installation, fuel switching infrastructure, renewable capacity procurement) with uneven cost distribution. Coal operators bear disproportionate burden while technology providers and renewable generators capture upside. The outcome-based method generates genuine coordination value (utilities can optimize compliance strategy) but this coordination benefit does not offset the extraction of capital costs from utilities into stranded asset losses and compliance investment. The extractiveness trajectory rises from 0.38 to 0.54 during years 0-10 as cumulative compliance investments create sunk costs, then plateaus as cost declines (technology maturation, economies of scale) reduce marginal extraction. Suppression (0.48): Moderate. Utilities face binding reduction targets (cannot exit) but have genuine flexibility in compliance method. The outcome-based framework is less suppressive than prescriptive command-and-control (which mandates specific technologies). However, suppression remains substantial because: (1) the 70% reduction is binding regardless of cost, (2) enforcement mechanisms make non-compliance risky, (3) utilities cannot exit the energy generation market entirely. Theater ratio (0.58): Moderate. The outcome-based mandate requires substantial documentation and monitoring infrastructure: emissions measurement, credit issuance and retirement tracking, compliance verification, and quarterly reporting. This theater is partly functional (real emissions must be measured to enforce targets) but partly performative (compliance documentation architecture elaborates beyond minimum verification requirements). Theater rises from 0.42 to 0.62 during years 0-10 as monitoring protocols elaborate and the compliance bureaucracy expands.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates substantial perspectival variance. Powerless communities see a Snare (trapped despite nominal beneficiary status, dependent on enforcement). Coal operators see a Tangled Rope (genuine problem-solving flexibility alongside asymmetric extraction). Technology providers see a Rope (pure coordination, market creation). Utilities see a Tangled Rope (mixed coordination and extraction). Environmental coalitions see a Scaffold (temporary binding constraint designed to sunset as technology costs decline). The regulatory system sees a Piton (outcome-based structure replaces prescriptive rules but compliance theater elaborates). The analytical observer risks seeing a Mountain (binding timeline as immutable legal/physical law) but the structural data reveals this as a false summit — the 15-year timeline is policy choice, not physics. The perspectival gap reflects the constraint's hybrid nature: genuine coordination (flexibility in compliance method enables innovation discovery) combined with asymmetric extraction (capital costs concentrated on single sector, benefits distributed across beneficiary groups).
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies by structural position. Powerless beneficiaries (trapped communities) experience high d despite nominal beneficiary status — they depend on enforcement for air quality and have no independent exit. Coal operators (moderate power, constrained exit) experience d ≈ 0.60 — they bear asymmetric extraction (capital costs) with limited escape options, derived from victim status + constraint on exit through fuel source dependency. Technology providers (powerful, arbitrage) experience d ≈ 0.10 — beneficiary status + market exit options produce low d, low f(d), negative effective extraction. Utilities (institutional, constrained) experience d ≈ 0.45 — mixed beneficiary (operational flexibility) and victim (capital burden) status produces mid-range d. Environmental coalitions (organized, constrained) experience d ≈ 0.35 — organized agent status reduces felt extraction; they see exit path (sunset). The analytical observer (analytical exit, civilizational time) derives d from the objective structure (binding legal obligation) rather than agent-specific factors, producing d ≈ 0.72 for the universal-scale view.
 *
 * MANDATROPHY ANALYSIS:
 *   The Clear Skies mandate resolves mandatrophy through outcome-based structure that generates measurable coordination benefit alongside asymmetric extraction. The constraint is NOT pure extraction (Snare) because: (1) the outcome-based method genuinely enables efficiency discovery that prescriptive rules would prevent, (2) utilities retain operational flexibility that constrained agents in pure extraction do not have, (3) technology providers and renewable generators genuinely benefit from the created markets. The constraint is NOT pure coordination (Rope) because: (1) capital costs are asymmetrically distributed (utilities bear disproportionate burden), (2) stranded asset losses represent value destruction for one actor class, (3) beneficiaries (communities, ecosystems) lack mechanisms to share the coordination burden. Tangled Rope classification is correct: the constraint serves both coordination (enabling efficient compliance discovery) and extraction (concentrating capital burdens on utilities while distributing benefits to other sectors). The distinction from an extractive Snare is the genuine coordination benefit; the distinction from a Rope is the asymmetric cost distribution. The mandatrophy is resolved by recognizing that large-scale environmental policy routinely exhibits this hybrid structure — outcomes that could not be achieved through pure coordination (utilities would not voluntarily invest in emissions reduction) require asymmetric extraction (binding targets + enforcement). Whether this extraction is justified depends on comparing the air quality benefit against the capital cost — an empirical question outside the constraint classification system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseline_emissions_gaming,
    'Can utilities manipulate baseline emissions calculations to inflate compliance credit retroactively or through accounting methods?',
    'Forensic audit of baseline emissions claims vs. historical operational data; comparison of utilities'' reported baselines against independent emissions monitoring; analysis of credit issuance patterns by EPA region',
    'If baseline gaming is widespread: effective extractiveness rises (utilities extract compliance value without real emissions reduction). If negligible: extractiveness floor holds and coordination function is genuine. Reclassification from Tangled Rope to Snare if gaming exceeds 20% of total credits issued.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseline_emissions_gaming, empirical, 'Whether baseline emissions can be manipulated to inflate compliance credits').

omega_variable(
    technology_cost_trajectory_failure,
    'Do clean technology cost curves actually decline at rates assumed in the 15-year compliance timeline, or do capital costs remain persistently high?',
    'Time-series analysis of scrubbing system costs, renewable capacity cost trajectories, and efficiency upgrade prices 2003-2018; comparison of actual cost declines against utility compliance cost projections from 2003',
    'If costs decline as projected: scaffold perspective confirmed, sunset logic is real, constraint''s extractiveness decreases over time. If costs stagnate: compliance becomes increasingly extractive for utilities, classification shifts Tangled Rope → Snare, mandatrophy unresolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_cost_trajectory_failure, empirical, 'Whether clean technology cost curves decline as assumed').

omega_variable(
    emissions_leakage_externalization,
    'Do utilities meet compliance targets by shifting generation to unregulated natural gas or coal sources outside the mandate''s scope, or by outsourcing to deregulated merchant generators with no equivalent constraints?',
    'Regional generation mix analysis; tracking of utility fuel procurement patterns before/after mandate; comparison of actual regional emissions against utility-reported compliance achievements',
    'If leakage is significant (>15% of apparent reduction): actual air quality benefit is lower than compliance numbers suggest. Extraction is partially externalized to unregulated sectors and to communities near merchant generation sources. Reclassifies Snare from powerless perspective as externalization mechanism rather than true mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emissions_leakage_externalization, empirical, 'Whether utilities externalize emissions reductions through unregulated generation').

omega_variable(
    regulatory_capture_post_mandate,
    'Do utilities captured by the regulatory process use the compliance framework to entrench incumbent technologies and block competing emissions reduction pathways (distributed generation, demand response, grid modernization)?',
    'Analysis of regulatory petitions filed by utilities 2003-2018; tracking of barriers to distributed energy resources and demand-side management; comparison of utilities'' technology investments vs. alternative pathways'' potential effectiveness',
    'If capture is present: mandate''s coordination function is inverted — instead of enabling efficient technology discovery, it enables utilities to prescribe the only acceptable compliance pathway (centralized scrubbing/efficiency). Reclassifies Tangled Rope → Snare from utilities'' institutional perspective (they capture the regulatory goal). Scaffold perspective fails (sunset is blocked by capture, not technology maturation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_post_mandate, empirical, 'Whether utilities capture the mandate to block alternative emissions reduction pathways').

omega_variable(
    enforcement_gap_extraction,
    'Is EPA enforcement capacity sufficient to detect and penalize non-compliance, or do utilities bear low enough risk of detection that the suppression mechanism (constrained exit) is effectively illusory?',
    'Analysis of EPA enforcement actions, penalties issued, and compliance verification audits 2003-2018; comparison of penalty magnitude to non-compliance cost savings; tracking of utilities achieving compliance targets vs. those with enforcement gaps',
    'If enforcement is weak: suppression is low (utilities have de facto exit option through non-compliance), classification shifts from Tangled Rope/Snare toward Rope/Piton. Beneficiaries (communities) experience extraction (no enforcement guarantee of air quality). If enforcement is strong: suppression holds, constraint''s binding nature is real, victims genuinely trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_gap_extraction, empirical, 'Whether EPA enforcement capacity makes the suppression mechanism real').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(2003_bush_clear_skies_emissions_mandate, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clearskies_tr_t0, 2003_bush_clear_skies_emissions_mandate, theater_ratio, 0, 0.42).
narrative_ontology:measurement(clearskies_tr_t5, 2003_bush_clear_skies_emissions_mandate, theater_ratio, 5, 0.54).
narrative_ontology:measurement(clearskies_tr_t10, 2003_bush_clear_skies_emissions_mandate, theater_ratio, 10, 0.62).
narrative_ontology:measurement(clearskies_tr_t15, 2003_bush_clear_skies_emissions_mandate, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(clearskies_be_t0, 2003_bush_clear_skies_emissions_mandate, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clearskies_be_t5, 2003_bush_clear_skies_emissions_mandate, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clearskies_be_t10, 2003_bush_clear_skies_emissions_mandate, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(clearskies_be_t15, 2003_bush_clear_skies_emissions_mandate, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(2003_bush_clear_skies_emissions_mandate, resource_allocation).
narrative_ontology:affects_constraint(2003_bush_clear_skies_emissions_mandate, coal_power_plant_capital_stranding).
narrative_ontology:affects_constraint(2003_bush_clear_skies_emissions_mandate, renewable_energy_market_formation).
narrative_ontology:affects_constraint(2003_bush_clear_skies_emissions_mandate, utility_regulatory_capture_mechanisms).

% DUAL FORMULATION NOTE:
% Clear Skies mandate is downstream of general air pollution constraints (PM2.5, ozone formation) and upstream of specific implementation mechanisms (cap-and-trade systems, mercury monitoring). Each story captures a distinct structural claim: (1) outcome-based regulation enables efficiency discovery (coordination function, ε≈0.30); (2) binding timeline creates capital concentration risk (extraction function, ε≈0.55); (3) enforcement gaps create compliance loopholes (suppression ambiguity, ε variable). The integrated story models all three coupled, producing net ε=0.52.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(2003_bush_clear_skies_emissions_mandate, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
