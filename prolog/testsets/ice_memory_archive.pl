% ============================================================================
% CONSTRAINT STORY: ice_memory_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ice_memory_archive, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ice_memory_archive
 *   human_readable: The imperative to create a global ice core archive before glaciers melt
 *   domain: technological/environmental
 *
 * SUMMARY:
 *   The global ice core archival imperative emerged in the 1990s as
 *   paleoclimate science recognized that rapidly melting glaciers would
 *   destroy irreplaceable paleoclimate records encoded in ice. The constraint
 *   structures a tension between scientific urgency (capture before melt),
 *   institutional resource competition (climate science institutions control
 *   funding and priority), and distributive justice (developing nations and
 *   alpine communities bear melt costs while developed-world institutions
 *   capture the data). The constraint exhibits hybrid extraction-coordination
 *   characteristics: it solves a genuine collective action problem (no
 *   individual researcher can preserve ice cores) while simultaneously
 *   extracting resources and priority from communities whose glaciers are
 *   being sampled. Theater has increased over 20 years as the narrative
 *   shifted from scientific opportunity to climate crisis rhetoric, creating
 *   performative archival commitments alongside genuine scientific sampling.
 *
 * KEY AGENTS:
 *   - Climate Science Institutions: Primary beneficiary (institutional/arbitrage) — control ice core acquisition, archival standards, and access protocols; capture funding and publication priority from the constraint
 *   - Paleoclimate Research Community: Primary beneficiary (institutional/arbitrage) — access standardized archived samples; reduce free-rider problem in ice preservation; benefit from suppression of alternative paleoclimate proxies
 *   - Alpine Communities: Primary victim (powerless/trapped) — bear meltwater risks, habitat loss, and infrastructure disruption; excluded from benefit distribution; no exit from glacial melting or scientific sampling demands
 *   - Developing Nations Climate Adaptation Programs: Secondary victim (moderate/constrained) — depend on paleoclimate data but excluded from governance; bear adaptation costs while developed nations prioritize archival research
 *   - International Climate Crisis Coalition: Organized actors (organized/constrained) — IPCC, WMO, Earth system science agencies building archival infrastructure with inherent sunset (either glaciers melt or climate action succeeds)
 *   - Ice Core Science Historical Institutions: Institutional inertia (institutional/arbitrage) — maintain ice core methods despite methodological displacement by remote sensing and modeling; theater-dependent funding
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing an institutional extraction regime as an immutable thermodynamic constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ice_memory_archive, 0.38).
domain_priors:suppression_score(ice_memory_archive, 0.48).
domain_priors:theater_ratio(ice_memory_archive, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_memory_archive, extractiveness, 0.38).
narrative_ontology:constraint_metric(ice_memory_archive, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ice_memory_archive, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ice_memory_archive, tangled_rope).
narrative_ontology:human_readable(ice_memory_archive, "The imperative to create a global ice core archive before glaciers melt").
narrative_ontology:topic_domain(ice_memory_archive, "technological/environmental").

domain_priors:requires_active_enforcement(ice_memory_archive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ice_memory_archive, climate_science_institutions).
narrative_ontology:constraint_beneficiary(ice_memory_archive, paleoclimate_research_community).
narrative_ontology:constraint_victim(ice_memory_archive, developing_nations_climate_adaptation).
narrative_ontology:constraint_victim(ice_memory_archive, alpine_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALPINE COMMUNITIES (SNARE) — Trapped between extraction of ice cores for scientific priority and the accelerating loss of water resources, glacial stability, and livelihood. Extraction occurs without compensation: glaciers are harvested for climate data while communities bear meltwater risks, habitat loss, and infrastructure costs. No exit from climate forcing; no choice in ice core priority allocation. Maximal experienced extraction.
constraint_indexing:constraint_classification(ice_memory_archive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS CLIMATE ADAPTATION (TANGLED ROPE) — Constrained by resource scarcity and dependence on climate data for adaptation planning, yet excluded from ice core archive governance. The constraint has coordination value: paleoclimate data informs adaptation timelines and risk models. But extraction is asymmetric — developed nations access the archive first; developing nations fund adaptation through borrowed models. Coordination benefit + asymmetric extraction = tangled rope.
constraint_indexing:constraint_classification(ice_memory_archive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CLIMATE SCIENCE INSTITUTIONS (ROPE) — Primary beneficiary. Experiences the archive imperative as coordination: distributed ice core sampling solves the collective action problem of capturing paleoclimate data before it melts. Institutional actors can arbitrage (shift sampling priority, access early data, build institutional prestige). Extraction flows toward them; suppression of alternative sampling strategies benefits their monopoly on interpretation. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(ice_memory_archive, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PALEOCLIMATE RESEARCH COMMUNITY (ROPE) — Primary beneficiary. Benefits from coordinated ice core acquisition, archival standardization, and access protocols. Suppression mechanism: alternative paleoclimate proxies (lake sediments, cave formations, tree rings) are deprioritized in funding and publication. Coordination function is genuine: the constraint solves the free-rider problem in ice core preservation (individual researchers cannot preserve cores alone). Effective extraction is low; suppression favors this constraint over alternatives.
constraint_indexing:constraint_classification(ice_memory_archive, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL CLIMATE CRISIS COALITION (SCAFFOLD) — Organized actors (IPCC, WMO, Earth system agencies) see the ice core archive as a temporary coordination mechanism with a hard deadline. Sunset is built in: if climate action succeeds, future ice preservation becomes unnecessary (less ice to archive, melt rate slows). If action fails, the deadline passes (ice is gone). Theater is moderate: the archive has genuine scientific function but also performative elements (symbolic commitment to climate science, donor visibility). Coalition experiences suppression but sees an exit path: successful climate mitigation ends the constraint.
constraint_indexing:constraint_classification(ice_memory_archive, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL ICE CORE SCIENCE (PITON) — From a civilizational view, much of the ice core archival imperative is institutional inertia from 20th-century glaciology. Ice cores were the primary paleoclimate proxy; newer remote sensing, model reconstruction, and isotope geochemistry provide paleoclimate data without ice. The urgency narrative (archive before melt) is partly theater — it maintains funding flows to traditional ice core programs despite methodological displacement. Theater ratio reflects persistent institutional practice despite reduced functional necessity.
constraint_indexing:constraint_classification(ice_memory_archive, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: THERMODYNAMIC CONSTRAINT / NATURAL LAW VIEW (MOUNTAIN) — From a physical universality perspective, the melting of glacial ice is a natural consequence of atmospheric greenhouse gas concentrations and planetary energy balance. The constraint is the irreversible thermodynamic loss of paleoclimate information encoded in ice: once melted, the isotopic, chemical, and particulate record cannot be recovered. This appears as a natural law — entropy, diffusion, melting — and thus an unchangeable constraint. However, the structural data reveals this as a false summit: the 'imperative to archive' is contingent on human institutional choices (funding, priority, governance) layered atop the natural melting process. The constraint is the institutional extraction, not the thermodynamics.
constraint_indexing:constraint_classification(ice_memory_archive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_memory_archive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ice_memory_archive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ice_memory_archive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ice_memory_archive, TR),
    TR >= 0.70.

:- end_tests(ice_memory_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The archival imperative extracts resources (funding, institutional priority, data governance) from developing nations and communities toward developed-world climate science institutions. But the extraction is not severe (ε ≥ 0.46 would trigger snare classification) because the coordination function is genuine — ice core preservation solves a real collective action problem that individual researchers cannot solve alone. The trajectory shows increasing extractiveness over 20 years (0.18 → 0.38) reflecting growing resource concentration in major climate institutions and increasing exclusion of developing-world researchers from governance. Suppression (0.48): Moderate. Barriers to alternative ice archival strategies include funding concentration, publication bias toward established ice core programs, governance structures that exclude developing nations, and institutional reluctance to de-prioritize traditional paleoclimate methods. Suppression is not total because alternative paleoclimate proxies exist and are improving; alpine communities have some voice (though minimal power) in site selection; and some developing-world researchers do participate. Theater ratio (0.55): Moderate-high and rising. The constraint includes genuine scientific function (paleoclimate information has real value) but also performative elements: the 'archive before melt' narrative has become a symbol of climate crisis commitment, enabling institutions to demonstrate urgency without addressing root causes (emissions reduction). The theater ratio rose from 0.35 to 0.55 as the political climate crisis narrative strengthened relative to pure paleoclimate urgency. Remote sensing and climate modeling have reduced the scientific necessity of ice cores, yet funding and priority have not shifted — institutional inertia maintains the ice core program beyond its functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap appears between the powerless alpine community perspective (snare: χ high, experienced extraction severe) and the institutional climate science perspective (rope: χ low or negative, net beneficiary). The piton and mountain perspectives reveal false naturalizations: the thermodynamic inevitability of melting is used to justify institutional priority allocation, but the actual constraint is institutional, not physical. This is a diagnostic case for why beneficiary/victim declarations and power-differentials matter: the same physical phenomenon (glacial melting) looks like an unchangeable natural law from the analytical perspective but like institutional extraction from the powerless and moderate perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent structural position. Alpine communities are trapped (d = 0.95) — no exit, victim status, low power. Their experienced extraction is maximal. Developing-nation adaptation programs are constrained (d = 0.65-0.75) — they depend on paleoclimate data (some benefit) but have no governance voice (victim status). Climate science institutions have arbitrage exit (d = 0.05-0.15) — beneficiary status, high power, can shift priorities or redirect archival resources. The scaffold perspective sees moderate extraction (d = 0.50-0.55) because the coalition is organized but constrained by the hard deadline and real scientific urgency. Directionality overrides are not needed — the structural derivation from beneficiary/victim declarations and exit options produces the correct perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the classification depends entirely on the observer's structural position relative to the benefit and cost flows. From the developed-world climate science institution's view, it is legitimate coordination (rope) solving a collective action problem. From the alpine community's view, it is extraction (snare) — their glaciers are harvested for data while they bear the costs. From the coalition view, it is temporary (scaffold) — the constraint is time-bounded by physical melting and potential climate action success. From the historical science view, it is degraded (piton) — institutional inertia maintains ice core programs despite methodological displacement. The false summit (mountain) reveals the risk that rhetorical naturalization ('we must archive because melting is inevitable') can mask institutional choices. The mandatrophy is resolved not by choosing one classification but by recognizing that all six are legitimate perspectival readings, and the presheaf structure IS the answer: the constraint manifests differently depending on where the observer stands, and that difference IS the structural fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archival_sufficiency_threshold,
    'What volume and distribution of ice cores constitutes sufficient paleoclimate coverage for centennial-scale prediction? Is current archival strategy scientifically adequate or driven by funding availability?',
    'Proxy reconstruction comparison: paleoclimate model output using existing ice core datasets vs. model output using hypothetical future expanded archives; empirical test of whether additional cores improve skill beyond statistical noise',
    'If sufficient: the extraction mechanism becomes clearer (funding distribution to favored institutions). If insufficient: the imperative is justified scientifically, and the constraint shifts toward rope or scaffold. If impossible to determine: the archive becomes pure theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archival_sufficiency_threshold, empirical, 'Whether current ice core archival strategy is scientifically adequate').

omega_variable(
    alternative_proxy_viability,
    'Do alternative paleoclimate proxies (cave formations, lacustrine sediments, tree rings, ocean sediment cores, isotopic leaf wax) provide equivalent or superior paleoclimate information to ice cores, such that the preservation imperative is overstated?',
    'Comparative reconstruction validation: paleoclimate estimates from ice cores vs. alternative proxies for the same time intervals; correlation of uncertainty bounds; publication bias analysis in paleoclimate literature toward ice core methods',
    'If alternatives are equivalent: the ice core archive is partly piton (institutional inertia). If alternatives are superior or lower-cost: the constraint is extractive rent-seeking by ice core programs. If ice cores remain necessary: the tangled rope classification stands and the constraint is legitimate hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_proxy_viability, empirical, 'Whether alternative paleoclimate proxies are as viable as ice cores').

omega_variable(
    governance_equity_capture,
    'Do developing nations have meaningful governance voice in ice core archive priorities, site selection, and data access? Or is governance capture by developed-world research institutions embedded in the constraint structure?',
    'Governance structure audit: formal vs. actual decision-making power in international ice core consortia; data access latency and cost for developing-world researchers; funding distribution to non-Western institutions; co-authorship patterns in archive-dependent publications',
    'If capture is embedded: the victims (developing nations) have no exit, confirming high suppression and snare classification. If governance is genuinely distributed: the constraint shifts toward rope or tangled rope with lower experienced extraction for victims. If partially captured: the institutional perspective overrides are needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_equity_capture, empirical, 'Whether ice core archive governance is equitable or captured').

omega_variable(
    climate_action_success_correlation,
    'If global climate action succeeds and warming trajectory flattens, does the ice core archival imperative decline? Or does the constraint persist through institutional inertia independent of climate outcomes?',
    'Counterfactual analysis: scenario modeling of archival priorities under 1.5°C vs. 3°C climate futures; funding trajectories for ice core programs under success vs. failure scenarios; institutional commitments to phased sunset of archival operations',
    'If true sunset: scaffold classification is correct. If persistence despite success: piton classification dominates, revealing the constraint as theater maintained by institutional inertia rather than genuine urgency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_action_success_correlation, preference, 'Whether ice core archival imperative will sunset with successful climate action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ice_memory_archive, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ice_tr_t0, ice_memory_archive, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ice_tr_t10, ice_memory_archive, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ice_tr_t20, ice_memory_archive, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(ice_be_t0, ice_memory_archive, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ice_be_t10, ice_memory_archive, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(ice_be_t20, ice_memory_archive, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ice_memory_archive, global_infrastructure).
narrative_ontology:affects_constraint(ice_memory_archive, climate_model_paleoclimate_dependency).
narrative_ontology:affects_constraint(ice_memory_archive, alpine_infrastructure_adaptation).
narrative_ontology:affects_constraint(ice_memory_archive, paleoclimate_proxy_substitution).

% DUAL FORMULATION NOTE:
% The ice core archival imperative can be decomposed into three distinct constraints: (1) the genuine collective action problem of ice core preservation (coordination, ε ≈ 0.15, rope), (2) the institutional resource extraction from developing nations and communities (asymmetric extraction, ε ≈ 0.42, snare from victim perspective), and (3) the methodological displacement of ice cores by modeling and remote sensing (institutional inertia, ε ≈ 0.12, piton). The JSON story integrates all three, producing the tangled rope classification. Constraint families downstream (climate model paleoclimate dependency, alpine adaptation) depend on how archive governance and access are structured; upstream (paleoclimate proxy substitution) shows the alternative methods that compress the extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
