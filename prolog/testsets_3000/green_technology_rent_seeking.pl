% ============================================================================
% CONSTRAINT STORY: green_technology_rent_seeking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_green_technology_rent_seeking, []).

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
 *   constraint_id: green_technology_rent_seeking
 *   human_readable: Green Technology Rent Seeking and Policy Capture
 *   domain: economic_policy/environmental_regulation
 *
 * SUMMARY:
 *   Green technology rent-seeking emerges when climate policy mechanisms
 *   designed to accelerate decarbonization become vehicles for incumbent firm
 *   extraction through subsidy capture, standards setting, and regulatory
 *   moats. The constraint exhibits genuine coordination function (market
 *   failure correction, scaling green infrastructure) layered with
 *   substantial asymmetric extraction (incumbent firms capturing policy
 *   rents, excluding competing technologies, channeling transition costs to
 *   displaced workers and taxpayers). Extractiveness has risen over the past
 *   decade (0.35 → 0.58) as incumbent firms have consolidated policy
 *   influence and as fiscal costs of subsidy regimes have grown without
 *   proportional decarbonization gains. Theater ratio has also risen (0.42 →
 *   0.62) as climate policy activity has become increasingly performative:
 *   setting emissions targets without enforcement, subsidizing technologies
 *   without cost controls, establishing standards that lock in incumbent
 *   advantage while generating minimal incremental emissions reductions.
 *
 * KEY AGENTS:
 *   - Incumbent Green Technology Firms (institutional/arbitrage): Primary beneficiaries capturing policy rents through subsidies, tax credits, renewable portfolio standards, and preferential grid access. Experience constraint as enabling their market dominance.
 *   - Displaced Fossil Fuel Workers (powerless/trapped): Primary victims with no exit options due to geographic dependence, skill lock-in, and concentrated regional employment. Bear asymmetric transition costs while policy channels benefits to incumbent firms.
 *   - Competing Green Technology Developers (moderate/constrained): Face regulatory capture through incumbent lobbying, patent thickets, and standards capture. High costs to exit through relocation or sector switching.
 *   - General Taxpayers (powerless/trapped): Bear hidden extraction cost through subsidy regimes that exceed justified coordination costs. No individual agency; collective victims trapped through taxation.
 *   - Renewable Energy Transition Communities (moderate/mobile): Experience both coordination benefits (green energy generation, local investment) and extraction costs (land use restrictions, infrastructure concentration). Mixed position enables some exit.
 *   - Decentralized Renewable Coalition (organized/constrained): See rent-seeking as temporary institutional problem with sunset pathway through distributed generation technologies. Build alternative mechanisms outside incumbent capture.
 *   - Climate Policy Bureaucracy (institutional/arbitrage): Regulatory agencies designed for climate implementation have become vessels for incumbent preferences. Maintain performative appearance of enforcement without challenging rent extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(green_technology_rent_seeking, 0.58).
domain_priors:suppression_score(green_technology_rent_seeking, 0.48).
domain_priors:theater_ratio(green_technology_rent_seeking, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(green_technology_rent_seeking, extractiveness, 0.58).
narrative_ontology:constraint_metric(green_technology_rent_seeking, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(green_technology_rent_seeking, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(green_technology_rent_seeking, tangled_rope).
narrative_ontology:human_readable(green_technology_rent_seeking, "Green Technology Rent Seeking and Policy Capture").
narrative_ontology:topic_domain(green_technology_rent_seeking, "economic_policy/environmental_regulation").

domain_priors:requires_active_enforcement(green_technology_rent_seeking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(green_technology_rent_seeking, incumbent_green_technology_firms).
narrative_ontology:constraint_beneficiary(green_technology_rent_seeking, subsidy_capture_coalitions).
narrative_ontology:constraint_victim(green_technology_rent_seeking, competing_technologies).
narrative_ontology:constraint_victim(green_technology_rent_seeking, fossil_fuel_transition_workers).
narrative_ontology:constraint_victim(green_technology_rent_seeking, general_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Workers in coal, natural gas, and oil sectors face structural trapping through geographic dependence, capital depreciation, and skill lock-in. Climate policy channels transition support through incumbent green firms, limiting worker agency in sector choice. No alternative income pathways available at comparable wages in declining coal regions.
constraint_indexing:constraint_classification(green_technology_rent_seeking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Developers of more efficient or cost-effective green technologies face regulatory capture through incumbent firm lobbying. Patent thickets, grandfathered subsidies, and standards capture create high barriers to market entry. Can exit through relocation or sector switching, but at substantial cost to technology development and market opportunity.
constraint_indexing:constraint_classification(green_technology_rent_seeking, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Communities hosting renewable installations both benefit from green energy generation and coordination (grid stability, local investment) and bear extraction (land use restrictions, visual impact, infrastructure concentration). Genuine coordination function exists alongside asymmetric extraction of site-specific costs.
constraint_indexing:constraint_classification(green_technology_rent_seeking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Primary beneficiaries experiencing constraint as pure coordination mechanism. Subsidies and standards capture align perfectly with their business model. High-margin extraction of policy rents with minimal coercion experience — beneficiaries see regulation as enabling their market dominance.
constraint_indexing:constraint_classification(green_technology_rent_seeking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized groups (distributed solar advocates, community wind cooperatives, microgrid developers) perceive green technology rent-seeking as a temporary institutional problem with a sunset pathway. Distributed renewable technologies bypass incumbent capture mechanisms. As battery costs decline and grid technology matures, centralized subsidy regimes lose their monopoly extraction power. Sunset estimated at 15-20 years as distributed generation reaches grid parity.
constraint_indexing:constraint_classification(green_technology_rent_seeking, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Regulatory agencies designed to implement climate targets have become vessels for incumbent firm preferences. Theater ratio (0.62) reflects that much climate policy activity is performative: setting targets without enforcement mechanisms, subsidizing technologies without cost controls, or establishing standards that lock in incumbent advantage. The bureaucratic apparatus persists through institutional inertia despite degraded function.
constraint_indexing:constraint_classification(green_technology_rent_seeking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From the analytical perspective, green technology rent-seeking is a genuine hybrid: environmental coordination function (decarbonization requires scale-up of green technologies) coexists with substantial asymmetric extraction (incumbent firms capturing policy rents, excluding alternatives, shifting transition costs to workers and taxpayers). The constraint requires active enforcement of subsidy regimes and standard-setting — without regulatory backing, rent extraction would collapse. Both coordination and extraction elements are structurally real.
constraint_indexing:constraint_classification(green_technology_rent_seeking, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(green_technology_rent_seeking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(green_technology_rent_seeking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(green_technology_rent_seeking, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(green_technology_rent_seeking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(green_technology_rent_seeking, TR),
    TR >= 0.70.

:- end_tests(green_technology_rent_seeking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Policy rents captured by incumbent firms are substantial but not total — unsubsidized technologies still function, policy intervention is not uniformly enforced, and alternative mechanisms are emerging. The value reflects that extraction is real but incomplete. Suppression (0.48): Moderate. Barriers to competing technologies exist (regulatory capture, standards lock-in, incumbent lobbying) but are not insurmountable. Some competing technologies succeed through cost advantages; organized groups are building alternative institutions. Suppression reflects real barriers without absolute entrenchment. Theater ratio (0.62): Moderate-high. Climate policy generates substantial performative activity: emissions targets without enforcement mechanisms, subsidies without cost controls, standards that lock in incumbent advantage while producing marginal decarbonization gains. Theater has increased as policy has shifted from genuine market correction (early period) toward incumbent capture maintenance (current period). The analytical perspective classifies as Tangled Rope because both coordination (decarbonization requires technology scale-up) and extraction (incumbent rents, worker displacement costs, technology exclusion) are structurally real and require active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is diagnostic of how extraction can hide behind coordination framing. Green technology transition IS a genuine coordination challenge — decarbonization at scale requires market correction for carbon externalities and infrastructure investment. But that coordination function coexists with incumbent firm extraction through subsidy capture, technology exclusion, and cost-shifting to displaced workers. The gap between Rope (incumbent firm perspective) and Snare (displaced worker perspective) is not measurement error — it reflects that the same policy regime genuinely coordinates technology scaling AND genuinely extracts from workers and taxpayers. The analytical observer cannot choose between coordination and extraction; both are structurally real. The Tangled Rope classification reflects this irreducible hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the extraction flow and their exit options. Incumbent firms are beneficiaries with arbitrage exit options (d ≈ 0.05): they can relocate operations, switch technologies, or exit policy dependence with minimal cost. Displaced workers are victims with trapped exit (d ≈ 0.95): geographic dependence, skill lock-in, and concentrated regional employment eliminate alternatives. Competing technologists are victims with constrained exit (d ≈ 0.65): they can relocate or switch sectors but at substantial cost. The decentralized renewable coalition is organized with constrained exit (d ≈ 0.45): they have real agency in building alternative institutions, reducing experienced extraction despite remaining policy barriers. The climate bureaucracy is institutional with arbitrage exit (d ≈ 0.10): they benefit from policy apparatus and can exit through regulatory change if political pressure shifts. These d values feed into f(d) sigmoid functions that scale chi: beneficiaries with arbitrage experience negative or near-zero χ; victims with trapped exit experience maximum χ; organized groups with agency experience moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that green technology policy REQUIRES both coordination and extraction elements. The coordination function (market failure correction, infrastructure scaling) cannot be achieved through Rope mechanisms alone because incumbent firms lack sufficient incentive to deploy green technologies at the pace required by decarbonization targets. Policy enforcement (subsidies, standards, mandates) is necessary for coordination. But that same enforcement apparatus enables extraction because incumbent firms have capacity to capture it. The solution is not to eliminate enforcement (which would collapse coordination) but to redesign enforcement mechanisms to reduce capture surface — decentralization mandates, technology-neutral standards, worker transition guarantees, and sunset clauses that force institutional renewal. The Tangled Rope classification holds because both elements are irreducible, and the constraint's future trajectory depends on whether policy redesign reduces extraction while maintaining coordination function. Constraint families: this story links to 'incumbent_firm_regulatory_capture' (institutional-level extraction mechanism) and 'fossil_fuel_worker_displacement' (worker-level extraction mechanism) through network dependencies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_subsidy_boundary,
    'What portion of green technology subsidies reflects genuine coordination costs (market failure correction, infrastructure externalities) versus pure rent extraction (incumbent protection)?',
    'Cost-benefit analysis of subsidized technologies against unsubsidized alternatives; comparison of policy costs to measured emissions reduction; counterfactual analysis of technology deployment without subsidies',
    'If coordination dominates (>60%): constraint reclassifies toward Rope/Scaffold. If extraction dominates (>60%): constraint reclassifies toward Snare/Tangled Rope with higher effective ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_subsidy_boundary, empirical, 'Boundary between genuine coordination cost and pure rent extraction in subsidy regimes').

omega_variable(
    alternative_technology_exclusion_mechanism,
    'Are competing green technologies (advanced nuclear, green hydrogen, next-generation geothermal, carbon capture) excluded from policy support through deliberate capture or through genuine comparative advantage of incumbent technologies?',
    'Comparative analysis of research funding, patent support, and deployment incentives across technology categories; interviews with policy formulators and technologists; historical timeline of inclusion/exclusion decisions and their justifications',
    'If exclusion is deliberate capture: suppression rises, extractiveness rises, constraint tightens toward Snare. If exclusion reflects genuine technical/economic barriers: suppression may reflect legitimate risk aversion, and constraint loosens toward Rope/Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_technology_exclusion_mechanism, empirical, 'Whether technology exclusion is capture-driven or evidence-driven').

omega_variable(
    worker_transition_sufficiency,
    'Do green sector employment gains for displaced fossil fuel workers match or exceed the geographic and skill-adjusted losses from coal/oil sector contraction?',
    'Longitudinal wage and employment data for fossil fuel workers post-policy; geographic matching of green job creation to fossil fuel job loss; wage premium analysis (if green jobs pay less, workers bear hidden extraction cost)',
    'If sufficient: displacement is transitional cost justified by decarbonization goal. If insufficient: constraint extracts from workers as asymmetric transition burden, raising snare perspective validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_transition_sufficiency, empirical, 'Whether green transition creates sufficient jobs for displaced fossil fuel workers').

omega_variable(
    distributed_renewable_sunset_realism,
    'Are distributed renewable technologies (rooftop solar, community wind, microgrids) actually approaching economic viability sufficient to bypass incumbent subsidy regimes, or is the scaffold sunset pathway aspirational rather than structural?',
    'Cost trend analysis for distributed renewable deployment; analysis of grid regulation barriers to distributed systems; market adoption rates in regions with different policy frameworks',
    'If pathway is real: scaffold classification holds, sunset is structural. If pathway faces regulatory/technical barriers that persist: scaffold classification becomes piton (aspirational, not executable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributed_renewable_sunset_realism, empirical, 'Whether distributed renewables approach viability to bypass incumbent capture').

omega_variable(
    policy_lock_in_duration,
    'How long does green technology subsidy capture lock in incumbent firm advantage once deployed? At what point do sunk costs become legacy burden rather than competitive moat?',
    'Technology cost curves; competitive entry analysis; patent expiration timelines; infrastructure lifespan; policy change sensitivity analysis',
    'If lock-in is short (5-10 years): constraint is naturally sunset-prone. If lock-in is long (30+ years): constraint approaches mountain-like entrapment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_lock_in_duration, empirical, 'Duration of policy lock-in for incumbent green firms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(green_technology_rent_seeking, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gtrs_tr_t0, green_technology_rent_seeking, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gtrs_tr_t5, green_technology_rent_seeking, theater_ratio, 5, 0.54).
narrative_ontology:measurement(gtrs_tr_t10, green_technology_rent_seeking, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(gtrs_be_t0, green_technology_rent_seeking, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gtrs_be_t5, green_technology_rent_seeking, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(gtrs_be_t10, green_technology_rent_seeking, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(green_technology_rent_seeking, resource_allocation).
narrative_ontology:affects_constraint(green_technology_rent_seeking, incumbent_firm_regulatory_capture).
narrative_ontology:affects_constraint(green_technology_rent_seeking, fossil_fuel_worker_displacement).
narrative_ontology:affects_constraint(green_technology_rent_seeking, distributed_renewable_technology_adoption).

% DUAL FORMULATION NOTE:
% Green technology rent-seeking decomposes into three structurally distinct constraints with different ε values: (1) incumbent_firm_regulatory_capture (ε ≈ 0.42) addressing the institutional-level mechanism of policy capture; (2) fossil_fuel_worker_displacement (ε ≈ 0.68) addressing the worker-level asymmetric transition costs; (3) distributed_renewable_technology_adoption (ε ≈ 0.25) addressing the alternative decentralized pathway. This story integrates all three through the tangled rope coordination function that links them. See dual formulation notes in each linked constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(green_technology_rent_seeking, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
