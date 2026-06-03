% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Climate Response via Degrowth Economic Transformation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth transformation reading of climate response proposes that
 *   adequate emissions reduction and intergenerational justice require
 *   restructuring wealthy economies away from GDP growth toward throughput
 *   reduction, sufficiency, equity, and universal basic services. This
 *   reading explicitly rejects technological-substitution logics (carbon
 *   markets, renewable electrification, carbon removal) as sufficient,
 *   claiming these enable 'decoupling' that is actually accounting illusion.
 *   The reading demands deep institutional transformation: working time
 *   reduction to lower material throughput, democratic ownership of
 *   productive assets, universal provision of essential services, and wealth
 *   redistribution from Global North to Global South. The constraint is
 *   classified as snare because it imposes severe extraction pressure on
 *   multiple actor categories (wealthy populations, fossil fuel industries,
 *   incumbent financial institutions) while claiming to benefit others
 *   (Global South nations, future generations, workers in sustainable
 *   sectors). However, the classification is contested: the reading may be
 *   tangled_rope if the coordination benefits (reduced working time,
 *   universal services, ecological stability) to affected populations
 *   sufficiently offset the extraction pressures; it may be false-summit
 *   mountain if the constraint reflects biophysical limits rather than
 *   political choice; it may be analytically unstable if the transition
 *   mechanism is infeasible. The suppression metric (0.72) reflects
 *   substantial barriers: incumbent power defending growth model, ideological
 *   framing of degrowth as 'impossible' or 'authoritarian,' locked-in
 *   infrastructure and financial systems, and the absence of historical
 *   precedent for voluntary acceptance of such restructuring by wealthy
 *   populations. The theater ratio (0.55) reflects that degrowth propositions
 *   currently operate largely through academic, activist, and policy
 *   discourse without institutional instantiation — the constraint is more
 *   vision than enforcement mechanism.
 *
 * KEY AGENTS:
 *   - Global South Nations: Structurally trapped by debt, export dependency, and climate vulnerability; degrowth offers nominally increased development rights but provides no mechanism to exit immediate extraction pressure
 *   - Workers in High-Throughput Sectors: Immediate livelihood threat from structural contraction (fossil fuels, manufacturing, petrochemicals); trapped by geographic immobility and absence of transition mechanisms
 *   - Climate-Concerned Middle-Income Households (Global North): Intellectually aligned with degrowth but constrained by asset exposure, status signaling dependence, and fear of downward mobility
 *   - Degrowth Advocacy Institutions: NGOs, economists, climate justice movements that benefit from expanded authority and funding as the reading gains attention
 *   - Fossil Fuel and Extraction Industries: Forecloses their business model directly; trapped by sunk capital and fiduciary obligations despite nominal diversification options
 *   - Wealthy Asset Holders (Global North): Explicitly targeted for wealth reduction through redistribution mechanisms; constrained but not trapped by regulatory and democratic enforcement
 *   - International Climate Governance Apparatus: Experiences the degrowth reading as threatening to institutional legitimacy; maintains theater of climate action while defending growth model
 *   - Analytical Observer: Civilizational perspective recognizing biophysical limits and thermodynamic impossibility of infinite growth; risks naturalizing contingent institutional choices as inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.68).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.72).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, snare).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Climate Response via Degrowth Economic Transformation").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, '21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148').
narrative_ontology:cs_kernel_codification('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', distributed).
narrative_ontology:cs_authority_grounding('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', distributed).
narrative_ontology:cs_reading_relation('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', climate_response_action__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', foundational, growth_incompatible_with_climate_stability).
narrative_ontology:cs_axiom_status(growth_incompatible_with_climate_stability, holdable).
narrative_ontology:cs_axiom_grounding('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', growth_incompatible_with_climate_stability, empirically_contingent).
narrative_ontology:cs_axiom('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', foundational, intergenerational_justice_requires_consumption_reduction_global_north).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_consumption_reduction_global_north, holdable).
narrative_ontology:cs_axiom_grounding('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', intergenerational_justice_requires_consumption_reduction_global_north, deontological).
narrative_ontology:cs_reference_frame('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', planetary_justice_baseline).
narrative_ontology:cs_drift_state('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', contemporary_2024_2026, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('21e31a4c-b0ac-4e1f-b9b2-b93bcd0b2148', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_south_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, nonhuman_ecosystems).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, workers_in_high_throughput_sectors).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, wealthy_populations_global_north).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, fossil_fuel_extractors).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, high_consumption_sectors).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, incumbent_financial_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL SOUTH VICTIM (SNARE) — Structurally trapped by colonial resource extraction legacies, debt servicing obligations, and export dependency. The degrowth reading offers nominal 'development rights' but provides no mechanism to exit the immediate extraction pressure. Current wealthy nations block technology transfer and maintain tariff barriers. Exit options are functionally zero: defaulting on debt triggers collapse; refusing export growth triggers capital flight; refusing participation in carbon markets forfeits the only compensation mechanism offered. Experiences maximum extraction.
constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED WORKER (SNARE) — Immediate threat to livelihood from structural contraction of extraction, manufacturing, and petrochemical sectors. The degrowth reading promises 'universal basic services' but provides no transition mechanism, retraining pathway, or guarantee of temporal overlap between job loss and service availability. Geographic immobility compounds trap: cannot relocate to regions where new economic structures have already formed. Suppression operates through both economic necessity (must work to survive) and ideological messaging (degrowth is 'impossible' or 'authoritarian'). Maximal experienced extraction.
constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: CONSTRAINED BENEFICIARY (TANGLED ROPE) — Intellectually aligned with degrowth logic and would benefit from reduced work hours, universal services, and ecological stability. However, structurally constrained by asset exposure (housing, pensions tied to equity markets), reliance on consumption-based status signaling, and fear of downward mobility. Experiences some coordination benefit (lower working hours, reduced throughput would reduce stress, pollution exposure) alongside extraction pressure (wealth loss, status loss, consumption constraint). Exit options exist but at high cost: could advocate for transformation, but doing so risks professional isolation or reputational damage in networks where degrowth is dismissed as fringe.
constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BENEFICIARY INSTITUTIONS (ROPE) — Environmental NGOs, degrowth economists, climate justice movements, and post-growth policy institutes benefit from the constraint's articulation: expanded funding, intellectual authority, policy influence. The constraint functions as pure coordination from this perspective — it solves the collective action problem of coordinating climate action without pretending technological substitution will suffice. These institutions have arbitrage options (they can scale to accommodate growth in sustainable sectors, can influence policy, can redirect capital flows). They experience the constraint as coordination with substantial co-benefits and low suppression from their vantage point.
constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: VICTIM INCUMBENT INDUSTRY (SNARE) — The degrowth reading directly forecloses the business model: no carbon markets to monetize, no technological substitution pathway to maintain extractive rent, no growth in consumption to expand markets. Exit options appear available (diversify into renewable energy, battery minerals, sustainable agriculture) but are functionally constrained by sunk capital, locked-in supply chains, and fiduciary obligations to return value to shareholders. Suppression operates through political power (industry lobbying, regulatory capture, funding denial to competing analyses). However, the constraint imposes extraction pressure on this agent despite its power — the reading targets them for structural decline. Paradoxically high-power + high-extraction because the power is increasingly directed at suppressing the constraint itself rather than adapting within it.
constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, snare,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: WEALTHY VICTIM (SNARE) — The degrowth reading explicitly targets accumulated wealth as the mechanism for redistribution. Faces pressure to liquidate assets, accept lower returns on capital, abandon growth-dependent pension models, and accept consumption constraint. Exit options exist in theory (capital flight, tax havens, resource hoarding) but constrained by border regimes, regulatory coordination, and enforcement mechanisms (wealth tax proposals, carbon rationing, resource allocation systems). Experiences high extraction (wealth reduction, status loss, constraint on consumption) with suppression enforced through democratic process and regulatory mechanisms rather than physical coercion.
constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DEGRADED INSTITUTIONAL BODY (PITON) — UNFCCC, IPCC, World Bank climate finance mechanisms maintain the theater of climate action while defending the growth model. The degrowth reading exposes this as performative: carbon markets, emissions trading, technological-substitution narratives, and 'green growth' are ritual compliance with climate commitments while preserving the extraction model. The governance apparatus experiences the constraint as threatening its legitimacy and operational model. However, from the apparatus's perspective, maintaining its own institutional continuity requires defending the growth framework — the theater has become the primary function. The apparatus has arbitrage options (could legitimize degrowth, could reallocate governance functions) but chooses institutional preservation over functional adequacy. Theater ratio reflects that climate conferences, net-zero pledges, and carbon accounting rituals persist despite low functional impact on emissions.
constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a thermodynamic and biophysical perspective, infinite growth on a finite planet is physically impossible. Degrowth is not a political choice but a recognition of physical constraint. Any sufficiently long temporal horizon reveals that some form of throughput reduction is inevitable — the only variables are whether it is planned (degrowth) or catastrophic (overshoot + collapse). The analytical perspective sees the constraint as an immutable law of thermodynamics and ecology. However, this perspective risks falsely naturalizing the specific institutional and redistributive mechanisms proposed under degrowth (UBS, working time reduction, democratic ownership) as inevitable rather than contingent. The mountain classification may be a false summit concealing political choices within biophysical constraints.
constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_action__degrowth_transformation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_action__degrowth_transformation, TR),
    TR >= 0.70.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The reading imposes substantial costs on multiple powerful actor categories — wealthy populations (wealth reduction), fossil fuel industries (business model foreclosure), incumbent financial institutions (asset revaluation), and high-consumption sectors. The extraction is enforced through democratic policy mechanisms (wealth taxation, carbon rationing, resource allocation) and regulatory constraints (energy transformation, production standards). The upward trajectory (0.42 → 0.68 over 20 time periods) reflects that as the reading moves from academic proposal toward policy consideration, the extraction pressure on incumbent actors intensifies — their suppression efforts increase precisely because the reading becomes more credible. Suppression (0.72): High and increasing. Multiple mechanisms suppress the degrowth reading: incumbent industry lobbying, mainstream media framing of degrowth as 'unrealistic' or 'authoritarian,' financial system dependence on growth assumptions, and psychological barriers to envisioning consumption reduction as desirable. Suppression is structural — built into incentive systems, expertise hierarchies, and institutional continuity — rather than merely rhetorical. Theater ratio (0.55, declining): The reading currently operates primarily through discourse (academic papers, activist campaigns, policy proposals) without institutional enforcement. However, theater is declining as the reading gains policy traction in some contexts (Ecuador, Costa Rica, EU degrowth research networks) — institutional mechanisms are beginning to form. The declining theater reflects movement from pure advocacy toward institutionalization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The Global South nation sees snare (trapped in current extraction, offered nominal future benefits with no transition mechanism). The displaced worker sees snare (immediate livelihood loss, promised future services with no overlap timing). The wealthy asset holder sees snare (direct wealth and status extraction). The fossil fuel industry sees snare (business model foreclosure). But the degrowth advocacy institution sees rope (coordination function, low suppression, clear alignment). The climate governance apparatus sees piton (its own functions degraded, forced to choose between institutional preservation and functional adequacy). The middle-income household sees tangled rope (benefits and costs intermingled, constrained exit). The analytical observer sees mountain (an immutable biophysical law). This divergence is not merely observational — it reflects real structural differences in how the constraint operates for different actors. The gap reveals that the degrowth reading is genuinely snare-type for large populations (extraction without offsetting benefits) while functioning as rope for advocacy institutions (pure coordination). The constraint does not have a single 'true' classification — it has a perspectival presheaf where different indexical contexts produce different types.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is derived from base extractiveness (0.68) scaled by directionality d ∈ [0,1] and scope modifier σ. The deriv​ation chain prioritizes structural data: agents declared as victims with constrained exit (Global South nations, workers, asset holders) receive high d values (0.75–0.95) producing high chi; agents declared as beneficiaries with arbitrage options (advocacy institutions) receive low d values (0.10–0.20) producing low/negative chi. Global institutional scope (σ=1.2) amplifies chi across all contexts. The wealthy Global North asset holder is simultaneously an institutional actor (typically associated with low d) but also explicitly victimized by the reading's redistributive mechanism — the directionality override is essential here: override d from institutional canonical (0.00) to 0.85 (victim with constrained exit despite power). Similarly, the fossil fuel industry appears as organized actor (canonical d=0.40) but experiences foreclosure (override d to 0.92). These overrides ensure that the structural data — this reading directly targets certain actors for extraction — flows through to chi values.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATHWAY: The degrowth reading's extractiveness (0.68) exceeds the mandatrophy threshold (>0.70 requires resolution). Mandatrophy resolution appears through explicit acknowledgment of the distributional asymmetry: the reading is classified as snare rather than attempting to hide extraction behind coordination framing. The analytical commentary (perspectival_gap and key_agents sections) documents exactly which actors bear extraction costs and which experience benefits. The omega variables document irreducible uncertainties about feasibility, institutional mechanisms, and kernel contest positioning rather than attempting to resolve them into false certainty. This is the appropriate mandatrophy resolution for a high-extraction policy proposal: be explicit about who pays, provide theoretical and historical justification for the redistribution, and document the uncertainty. The alternative resolution pathway (reclassify to tangled_rope) would require demonstrating that Global North populations genuinely experience offsetting coordination benefits (lower working hours, universal services, ecological stability) sufficient to justify the wealth extraction — this may be true as a counterfactual but is not the current baseline and cannot be asserted without empirical demonstration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_transition_mechanism,
    'What is the actual enforcement mechanism that would compel wealthy populations and incumbent industries to accept degrowth restructuring?',
    'Historical analysis of comparable economic restructurings (post-WWII conversion, Cold War transition, post-apartheid redistribution); political economy analysis of feasibility conditions for wealth redistribution; examination of whether democratic voluntary acceptance or coercive enforcement is required',
    'If voluntary consensus is possible: classification remains snare but with lower suppression (actors accept constraint voluntarily). If coercive enforcement required: suppression remains high and classification may shift to pure extraction if enforcement mechanisms are themselves extractive. If transition is infeasible: the constraint cannot be instantiated and the classification becomes analytically vacuous.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_transition_mechanism, empirical, 'Mechanism of enforcement for degrowth transition in wealthy nations').

omega_variable(
    universal_services_coordination,
    'Can universal basic services (healthcare, education, transport, housing, energy) be coordinated globally without the price signal mechanism of markets, and what are the failure modes?',
    'Comparative analysis of existing universal service coordination (Nordic models, Cuban rationing, post-WWII British NHS); simulation studies of resource allocation without market mechanisms; examination of information aggregation and responsiveness in non-market systems',
    'If coordination is feasible: degrowth classification as snare is confirmed (extraction pressure on high-consumption actors balanced by genuine coordination benefits). If coordination fails: the constraint becomes self-defeating (collapse of services harms exactly the populations meant to benefit) and should reclassify toward chaos/fragmentation rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_services_coordination, empirical, 'Feasibility of global universal services coordination without market mechanisms').

omega_variable(
    global_north_south_asymmetry,
    'Does degrowth in wealthy nations actually transfer development capacity to Global South, or does it contract global capital availability and harm those nations regardless of redistributive intent?',
    'Counterfactual analysis: compare degrowth + redistribution scenario against continued growth + development aid; examine historical cases where wealthy nations contracted (1970s stagflation, 2008 crisis) and impacts on Global South capital availability and development trajectories',
    'If transfer is effective: Global South benefits genuinely shift from victims to beneficiaries across perspectives. If contraction harms Global South regardless: degrowth reading may create illusion of solving intergenerational injustice while maintaining or deepening international inequality. Classification could shift from benefiting Global South to further entrapping them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_north_south_asymmetry, empirical, 'Whether degrowth in Global North enables or prevents Global South development').

omega_variable(
    kernel_contest_overrepresentation,
    'Is the degrowth reading assigned higher extractiveness (0.68) relative to the mitigation_priority reading (expected ~0.35) because degrowth genuinely extracts more, or because the analysis center of gravity is weighted toward the Global North perspective where extraction IS higher?',
    'Comparative epsilon analysis across all three kernel readings (mitigation_priority, adaptation_priority, degrowth_transformation); weight epsilon values by population groups affected and aggregate cross-reading; examine whether ε variance is structural or observational',
    'If structural: degrowth genuinely imposes higher extraction costs on larger populations. If observational: the reading may be analytically misplaced in the snare category and should be reclassified as tangled_rope (coordination benefits to Global South balance extraction burdens on Global North; the constraint solves a genuine collective action problem despite high cost to some actors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_overrepresentation, conceptual, 'Whether high extractiveness reflects structural asymmetry or observational bias toward Global North').

omega_variable(
    kernel_legitimacy_grounding,
    'Does the degrowth reading ground its legitimacy claim in biophysical limits (a natural law), normative commitments to equity and sustainability (deontological), or instrumental efficacy at climate mitigation (means-ends reasoning)?',
    'Discourse analysis of degrowth literature: categorize core justifications by type (empirical claims about carrying capacity, normative claims about justice, efficiency arguments about resource allocation); assess whether the reading could survive falsification of each grounding type independently',
    'If grounded in biophysical limits alone (empirically_contingent): falsification of overshoot projections or emergence of unanticipated capacity increases could foreclose the reading. If grounded deontologically: empirical changes do not affect legitimacy, but the reading may be inflexible to evolving contexts. If instrumental: competing technologies achieving the same climate outcome would reduce the reading''s force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_legitimacy_grounding, conceptual, 'What type of claim grounds the degrowth reading''s legitimacy').

omega_variable(
    technological_foreclosure,
    'Does the degrowth reading genuinely foreclose technological-substitution pathways (renewables + electrification + efficiency), or does it coexist with them as a complementary rather than alternative strategy?',
    'Energy systems analysis: can IPCC 1.5°C pathways be achieved through technological substitution alone without throughput reduction? Examine whether ''degrowth + technology'' is faster, cheaper, or more equitable than either pathway alone.',
    'If foreclosure is genuine: the reading forecloses mitigation_priority. If coexistence is possible: both readings remain live options and the kernel contest is a genuine disagreement about necessary conditions rather than a logical contradiction. This determines the reading_relation value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_foreclosure, empirical, 'Whether degrowth forecloses or coexists with technological substitution pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_degrowth_tr_t0, climate_response_action__degrowth_transformation, theater_ratio, 0, 0.7).
narrative_ontology:measurement(clim_degrowth_tr_t10, climate_response_action__degrowth_transformation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(clim_degrowth_tr_t20, climate_response_action__degrowth_transformation, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(clim_degrowth_be_t0, climate_response_action__degrowth_transformation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_degrowth_be_t10, climate_response_action__degrowth_transformation, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(clim_degrowth_be_t20, climate_response_action__degrowth_transformation, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_degrowth_su_t0, climate_response_action__degrowth_transformation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_degrowth_su_t10, climate_response_action__degrowth_transformation, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(clim_degrowth_su_t20, climate_response_action__degrowth_transformation, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__degrowth_transformation, 0.12).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, carbon_market_architecture).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, fossil_fuel_subsidy_lock_in).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, global_debt_sustainability).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, working_time_norms_and_throughput).

% DUAL FORMULATION NOTE:
% The degrowth_transformation reading is one of three structurally distinct readings of the climate_response_action kernel. Each reading has its own extractiveness, beneficiary/victim structure, and feasibility profile. The mitigation_priority reading (~ε=0.35, tangled rope) coordinates emissions reductions while maintaining growth incentives; the adaptation_priority reading (~ε=0.45, tangled rope) coordinates resilience investment accepting temperature rise; the degrowth_transformation reading (ε=0.68, snare) demands structural redistribution and throughput reduction. The three readings compete but do not fully foreclose each other. They should be authored as separate constraint stories linked by this network node to enable comparative analysis of their governance logics, distributional impacts, and feasibility conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, institutional, 0.85).
constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, organized, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
