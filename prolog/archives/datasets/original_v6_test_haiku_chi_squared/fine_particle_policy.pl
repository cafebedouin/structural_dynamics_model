% ============================================================================
% CONSTRAINT STORY: fine_particle_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fine_particle_policy, []).

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
 *   constraint_id: fine_particle_policy
 *   human_readable: Dutch Fine Particle Policy
 *   domain: environmental_policy/agricultural_regulation
 *
 * SUMMARY:
 *   The Dutch fine particle policy represents a political effort to reduce
 *   particulate matter emissions, with livestock farming identified as a
 *   significant contributor. The policy mandates equipment upgrades, feed
 *   modifications, and stall conversions at the farm level. This constraint
 *   exhibits the core tension of environmental regulation in agricultural
 *   societies: coordination (all farms must reduce emissions to meet EU air
 *   quality targets) layered over asymmetric extraction (compliance costs
 *   borne primarily by farmers, health benefits distributed across urban
 *   public). The policy evolved from EU directives (National Emissions
 *   Ceiling Directive) adapted to Dutch context, where intensive livestock
 *   operations concentrate in specific regions, creating acute air quality
 *   problems. The constraint's theater ratio (0.58) reflects a moderate
 *   degree of performative compliance architecture: subsidy paperwork,
 *   regulatory inspections, and equipment certifications create
 *   administrative overhead without always guaranteeing air quality outcomes.
 *   Base extractiveness increased from 0.32 to 0.52 over the measurement
 *   interval as initial compliance costs proved higher than projected and
 *   consolidation accelerated (small farms exiting, large operations
 *   absorbing capacity).
 *
 * KEY AGENTS:
 *   - Livestock Farmers (Small-to-Medium): Primary victims (powerless/trapped) — face mandatory equipment costs, operational disruptions, exit barriers due to multi-generational land/capital lock-in
 *   - Agricultural Unions (LTO Nederland, ZLTO): Secondary actors (organized/constrained) — negotiate transition terms, coordinate collective responses, benefit from subsidy distribution mechanisms
 *   - Environmental Regulators (NVWA, Ministry of Infrastructure): Institutional beneficiary (institutional/arbitrage) — gain regulatory authority and EU compliance leverage; experience policy as coordination success
 *   - Agricultural Technology Vendors: Secondary beneficiary (powerful/arbitrage) — capture compliance-driven equipment sales without bearing costs
 *   - Urban Public / Public Health: Abstract beneficiary (powerless/arbitrage) — receive air quality improvements without participating in policy design or cost-bearing
 *   - EU Agricultural/Environmental Framework: Continental constraint source (institutional/constrained) — sets binding air quality targets; Dutch policy is subordinate implementation
 *   - Traditional CAP Subsidy System: Institutional inertia actor (institutional/constrained) — continues distributing income support simultaneously with new compliance requirements, creating contradictory incentive architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fine_particle_policy, 0.52).
domain_priors:suppression_score(fine_particle_policy, 0.65).
domain_priors:theater_ratio(fine_particle_policy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fine_particle_policy, extractiveness, 0.52).
narrative_ontology:constraint_metric(fine_particle_policy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fine_particle_policy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fine_particle_policy, tangled_rope).
narrative_ontology:human_readable(fine_particle_policy, "Dutch Fine Particle Policy").
narrative_ontology:topic_domain(fine_particle_policy, "environmental_policy/agricultural_regulation").

domain_priors:requires_active_enforcement(fine_particle_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fine_particle_policy, urban_public_health).
narrative_ontology:constraint_beneficiary(fine_particle_policy, environmental_regulators).
narrative_ontology:constraint_victim(fine_particle_policy, livestock_farmers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED LIVESTOCK FARMER (SNARE) — Family farms with multi-generational livestock operations face compliance costs (equipment upgrades, stall conversions, feed additives) with minimal exit alternatives. Cannot relocate operations easily, cannot abandon livestock without losing livelihood. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73. Suppression high: regulatory pathway is mandatory, alternatives (leaving agriculture) carry catastrophic personal cost.
constraint_indexing:constraint_classification(fine_particle_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FARMING COLLECTIVE / AGRICULTURAL UNIONS (TANGLED ROPE) — Organized farmers (LTO Nederland, ZLTO) have genuine coordination role: collective negotiation of transition timelines, subsidized equipment access, research into low-emission livestock practices. Policy also extracts through compliance burdens and market consolidation (small farms exit, large operations benefit from economies of scale in compliance). d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.52. Requires enforcement: mandatory participation in environmental impact reduction schemes.
constraint_indexing:constraint_classification(fine_particle_policy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENVIRONMENTAL REGULATORS / MINISTRY OF INFRASTRUCTURE (ROPE) — Institutional actor coordinating compliance (NVWA enforcement, measurement standards, subsidy distribution). Experiences policy as coordination problem: information asymmetry (measuring actual farm emissions), collective action (all farms must comply simultaneously for air quality targets). d≈0.10, f(d)≈-0.02, σ=1.0 → χ≈-0.01. Net beneficiary through institutional authority and international (EU) compliance credit.
constraint_indexing:constraint_classification(fine_particle_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AGRICULTURAL TECHNOLOGY VENDORS / EQUIPMENT SUPPLIERS (ROPE) — Producers of low-emission livestock equipment (air scrubbers, feed additives, breeding technologies) benefit from mandated adoption without bearing compliance costs. Coordinate with farmers on technical implementation. d≈0.15, f(d)≈0.05, σ=1.0 → χ≈0.02. Positive externality: policy creates new revenue streams while framing as environmental benefit.
constraint_indexing:constraint_classification(fine_particle_policy, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EU AGRICULTURAL TRANSITION COALITION (SCAFFOLD) — EU-wide transition framework toward sustainable agriculture has explicit sunset logic: 2030 interim targets, 2050 climate neutrality goal. Dutch policy is subordinate to EU Green Deal. Coordination function: synchronizing national policies across member states. Theater moderate (0.58) because transition pathways are genuinely discussed, though implementation remains contested. d≈0.40, f(d)≈0.40, σ=1.1 → χ≈0.23. Sunset clauses built into EU timeline framework — extraction mechanisms decline as alternative food systems mature.
constraint_indexing:constraint_classification(fine_particle_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: TRADITIONAL AGRICULTURAL SUBSIDY ARCHITECTURE (PITON) — European Common Agricultural Policy (CAP) subsidy system predates fine particle regulation by decades. Fine particle policy layers new compliance requirements onto existing subsidy structures without dismantling old incentive mechanisms. Theater ratio 0.58 reflects that subsidy administration is substantially performative: direct income support continues even when environmental outcomes are marginal. This is degraded Rope — originally designed for pure agricultural coordination, now maintained partly through institutional inertia despite contradicting stated environmental goals. d≈0.30, f(d)≈0.30, σ=1.0 → χ≈0.15. Low effective extraction because the system is no longer functional for its stated purpose.
constraint_indexing:constraint_classification(fine_particle_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (TANGLED ROPE) — From a global food security and climate perspective, the policy is both coordination (transitioning animal agriculture to lower-emission models) and extraction (concentrating farm consolidation, shifting food production costs to farmers while distributing health benefits across public). ε=0.52 reflects genuine tension between coordination and asymmetric impact. d≈0.70, f(d)≈1.15, σ=1.2 → χ≈0.60. The constraint is neither natural law nor pure coordination — it is a political choice that can be reshaped by addressing distributional asymmetry.
constraint_indexing:constraint_classification(fine_particle_policy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fine_particle_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fine_particle_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fine_particle_policy, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fine_particle_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fine_particle_policy, TR),
    TR >= 0.70.

:- end_tests(fine_particle_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The policy extracts measurable benefits from farmers (equipment costs, operational constraints, consolidation pressures) while distributing benefits to diffuse beneficiaries (urban air quality, public health, EU regulatory compliance). The extractiveness is not maximal (0.66+) because: (1) government subsidies recover ~40-60% of equipment costs for participating farms, (2) large operations can achieve economies of scale in compliance, making extraction less uniform, and (3) some farmers view emission reduction as aligned with modern practice. Suppression (0.65): Moderate-high. Farmers face regulatory mandate (no voluntary opt-out), limited alternative exit routes (agricultural career path is narrow), and concentrated compliance costs. However, suppression is not absolute: government subsidy programs, extended compliance timelines, and differentiated requirements by farm size create partial relief valves. Theater ratio (0.58): Moderate. Compliance architecture includes administrative elements (subsidy paperwork, inspection protocols) that are partially performative, but actual equipment purchases and operational changes generate real emission reductions. The theater increased over the interval as subsidy bureaucracy expanded without corresponding air quality improvements (suggesting administrative growth outpaced functional benefit). Claimed type (tangled_rope): Policy provides genuine coordination function (synchronizing farm compliance with EU targets, enabling collective transition) but with asymmetric extraction (farmers bear costs, urban public receives benefits without participating in design).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival divergence. Trapped farmers see pure extraction (Snare): mandatory compliance with no meaningful exit alternative, subsidies that underfund actual costs, consolidation pressures that punish operational diversity. Farming unions see mixed coordination-extraction (Tangled Rope): policy enables collective negotiation of transition terms, coordinates subsidy access, but also accelerates consolidation that eliminates small-farm competition. Regulators see pure coordination (Rope): policy solves collective action problem (individual farms have no incentive to reduce emissions absent mandate), establishes measurable compliance standards, leverages EU funding. Technology vendors see pure gain (Rope, beneficiary side): mandatory equipment purchases without bearing compliance costs. Urban public sees abstract benefit (Rope, beneficiary side): air quality improvement without cost participation. EU framework sees subordinate implementation (Scaffold): Dutch policy is temporary step toward 2050 climate neutrality, with built-in obsolescence as alternative food systems mature. CAP subsidy architecture sees degraded ritual (Piton): subsidy distribution persists despite environmental contradictions, maintained through institutional inertia. The analytical observer sees political choice (Tangled Rope): the constraint is not natural or inevitable but represents a particular solution to an environmental problem that could be restructured to address distributional asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Trapped farmers: Victim + trapped exit → d≈0.92, f(d)≈1.40. Maximum extraction. Cannot exit agriculture, cannot avoid regulation, bear full compliance burden. Farming collective: Victim (cost-bearers) + constrained exit → d≈0.65, f(d)≈1.00. Significant extraction but mitigated by collective bargaining power and subsidy access. Environmental regulators: Beneficiary (regulatory authority) + arbitrage (can redefine compliance terms) → d≈0.10, f(d)≈-0.02. Net beneficiary. Agricultural technology vendors: Beneficiary (equipment sales) + arbitrage (market-driven participation) → d≈0.15, f(d)≈0.05. Net beneficiary with low friction. Urban public: Beneficiary (air quality) + arbitrage (can relocate if dissatisfied) → d≈0.20, f(d)≈0.08. Net beneficiary, though diffuse and unorganized. EU framework: Institutional beneficiary + constrained (must implement directive) → d≈0.35, f(d)≈0.35. Institutional extraction mechanism with sunset logic (2050 transition end-state). CAP subsidy system: Institutional actor + constrained → d≈0.30, f(d)≈0.30. Minimal extraction because system is degraded; primarily maintains inertial institutional structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing between legitimate coordination (all farms must reduce emissions to meet collective air quality targets) and asymmetric extraction (costs concentrated on farmers, benefits distributed to urban public). The policy COULD be reframed to address the asymmetry without abandoning the coordination function: (1) full cost subsidy for small farms (preserving farm viability while maintaining emission reductions), (2) progressive compliance timelines allowing gradual transition, (3) public carbon pricing that distributes compliance costs across beneficiaries (meat consumers pay price premium reflecting environmental cost), or (4) agricultural transition support funding alternative farm models (regenerative practices, crop rotation) rather than only emission-reduction equipment. The current design is tangled_rope, not snare, because the coordination function is real and necessary (air quality cannot improve without farm-level emission reductions). However, the distribution of burden-bearing is politically contingent, not structurally inevitable. Mandatrophy is resolved by acknowledging that both readings are correct from different structural positions: farmers experience extraction (snare view); regulators experience coordination (rope view); the full system exhibits hybrid coordination-extraction (tangled_rope). The policy's sustainability depends on addressing the perspectival gap — if extraction burden becomes unbearable, farmers will exit through farm abandonment or regulatory non-compliance, collapsing the coordination function and realizing the snare prophecy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emission_attribution_boundary,
    'How much of particulate pollution attributed to livestock farming reflects true causal contribution versus confounding with transport, industrial, and urban heating sources?',
    'Atmospheric modeling isolating agricultural contribution; isotopic tracing of PM2.5; comparison of air quality in agricultural-only regions versus mixed-source regions with similar farm density',
    'If agricultural contribution < 15%: policy targets wrong constraint (Rope reclassifies to Piton due to low extraction). If > 30%: validates snare classification for farmers bearing disproportionate burden for shared problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emission_attribution_boundary, empirical, 'Proportion of fine particles actually attributable to livestock versus other sources').

omega_variable(
    farmer_exit_feasibility,
    'What proportion of Dutch livestock farmers can viably transition to non-livestock agriculture or non-agricultural livelihoods without subsidy support?',
    'Economic viability analysis of crop conversion, land valuation, labor retraining pathways; survey of farmer exit intentions; comparison to historical agricultural exits in other EU countries',
    'If feasible for >50%: exit_options upgrades from trapped to constrained for farming collective (snare reclassifies to tangled_rope). If <20%: suppression increases, snare classification intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(farmer_exit_feasibility, empirical, 'Economic feasibility of farmer exit from livestock production').

omega_variable(
    technology_implementation_sufficiency,
    'Do mandated emission reduction technologies (air scrubbers, feed additives, stall conversions) achieve stated PM2.5 reduction targets across diverse farm sizes and livestock types?',
    'Real-world emission monitoring post-implementation; correlation between technology adoption rates and measured air quality; assessment of technology performance variance by farm scale',
    'If insufficient: policy reclassifies to Snare (extraction without coordination benefit). If sufficient: validates tangled_rope (coordination function real, but with asymmetric burden).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_implementation_sufficiency, empirical, 'Efficacy of mandated emission reduction technologies').

omega_variable(
    subsidy_adequacy_empirical,
    'Do government subsidies for equipment and transition support cover actual compliance costs for small-to-medium farms, or do they systematically underfund relative to large-scale operations?',
    'Detailed cost accounting across farm sizes; subsidy allocation analysis; comparison of net compliance burden (costs minus subsidies) by farm scale; farmer survey on perceived adequacy',
    'If underfunded for small farms: extraction mechanism hardens (Snare). If adequately funded: coordination function strengthens (Rope). Theater ratio may decline if transparent accounting replaces opaque subsidy allocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_adequacy_empirical, empirical, 'Whether subsidies adequately cover compliance costs across farm scales').

omega_variable(
    regulatory_capture_depth,
    'To what extent does agricultural industry influence policy design, enforcement priorities, and subsidy allocation through lobbying and regulatory capture?',
    'Transparency analysis of LTO Nederland (farmers union) involvement in policy drafting; funding flows from agribusiness to political parties; regulatory discretion patterns in enforcement (variation in violation detection across regions/farm types)',
    'If capture high: policy reclassifies as institutional-beneficiary Rope with asymmetric victim side effects (tangled_rope with stronger extraction). If capture low: validates stated coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Extent of agricultural industry influence over policy design and enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fine_particle_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fpp_tr_t0, fine_particle_policy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fpp_tr_t5, fine_particle_policy, theater_ratio, 5, 0.5).
narrative_ontology:measurement(fpp_tr_t10, fine_particle_policy, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(fpp_be_t0, fine_particle_policy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fpp_be_t5, fine_particle_policy, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fpp_be_t10, fine_particle_policy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fine_particle_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(fine_particle_policy, eu_air_quality_directive).
narrative_ontology:affects_constraint(fine_particle_policy, cap_subsidy_system).
narrative_ontology:affects_constraint(fine_particle_policy, agricultural_consolidation).

% DUAL FORMULATION NOTE:
% Dutch fine particle policy is downstream of EU National Emissions Ceiling Directive (higher ε, mountain-like constraint from EU perspective) and mediates between EU environmental mandate and agricultural subsidy system (which has contradictory incentives). The policy's extractiveness depends on relative power of regulatory versus agricultural constituencies — if farming consolidation accelerates, policy reclassifies toward snare; if subsidy adequacy improves, policy reclassifies toward rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fine_particle_policy, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
