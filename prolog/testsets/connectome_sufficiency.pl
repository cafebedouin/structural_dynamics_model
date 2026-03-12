% ============================================================================
% CONSTRAINT STORY: connectome_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_connectome_sufficiency, []).

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
 *   constraint_id: connectome_sufficiency
 *   human_readable: Connectome Sufficiency for Whole-Brain Emulation
 *   domain: computational_neuroscience/whole_brain_emulation/philosophy_of_mind
 *
 * SUMMARY:
 *   The connectome sufficiency thesis claims that a complete structural
 *   wiring diagram of the brain is sufficient to specify behavior and enable
 *   whole-brain emulation. This constraint exhibits the full range of DR
 *   classifications depending on observer position. Connectomics research
 *   programs experience it as coordination (organizing data collection around
 *   a clear target). Alternative paradigms emphasizing embodiment,
 *   plasticity, and neuromodulation experience it as extraction (resource
 *   concentration in connectomics suppresses competing approaches). The
 *   whole-brain emulation industry increasingly experiences it as a degraded
 *   commitment (piton) as empirical challenges accumulate. The analytical
 *   observer risks naturalizing it as a logical necessity of physicalism
 *   (false mountain). The constraint's theater ratio (0.58) reflects
 *   increasing performativity: connectome projects continue to attract
 *   funding and institutional support despite persistent failures to predict
 *   behavior from structure alone (C. elegans connectome complete since 1986,
 *   yet behavior remains incompletely specified). The extractiveness has
 *   grown from 0.22 to 0.38 over 20 years as the paradigm has concentrated
 *   resources while empirical validation has stalled.
 *
 * KEY AGENTS:
 *   - Connectomics Research Programs: Primary beneficiary (institutional/arbitrage) — capture large-scale funding, infrastructure investment, and paradigm-setting authority during the connectome mapping era
 *   - Alternative Neuroscience Paradigms: Primary victim (powerless/trapped) — embodied cognition, dynamical systems neuroscience, and developmental approaches structurally suppressed by funding concentration in connectomics
 *   - Embodied Cognition Researchers: Secondary victim (moderate/constrained) — face career barriers and resource asymmetry but benefit from connectomics infrastructure (imaging tools, computational methods, data standards)
 *   - Integrative Neuroscience Coalition: Organized agents (organized/mobile) — researchers building multi-scale models that integrate connectome, dynamics, and embodiment; see both coordination (structural data) and extraction (paradigm dominance)
 *   - Whole-Brain Emulation Industry: Institutional actor (institutional/arbitrage) — maintains commitment to connectome sufficiency through sunk costs and investment narratives despite empirical challenges; sees own roadmap as degraded (piton perspective)
 *   - Open Neuroscience Data Movement: Organized agents (organized/mobile) — advocates for open connectome data see the sufficiency claim as temporary coordination mechanism with sunset as empirical evidence forces paradigm expansion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing connectome sufficiency as logical consequence of physicalism, missing that structural determinism does not follow from physical determinism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(connectome_sufficiency, 0.38).
domain_priors:suppression_score(connectome_sufficiency, 0.62).
domain_priors:theater_ratio(connectome_sufficiency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(connectome_sufficiency, extractiveness, 0.38).
narrative_ontology:constraint_metric(connectome_sufficiency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(connectome_sufficiency, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(connectome_sufficiency, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(connectome_sufficiency, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(connectome_sufficiency, tangled_rope).
narrative_ontology:human_readable(connectome_sufficiency, "Connectome Sufficiency for Whole-Brain Emulation").
narrative_ontology:topic_domain(connectome_sufficiency, "computational_neuroscience/whole_brain_emulation/philosophy_of_mind").

domain_priors:requires_active_enforcement(connectome_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(connectome_sufficiency, connectomics_research_programs).
narrative_ontology:constraint_beneficiary(connectome_sufficiency, emulation_technology_investors).
narrative_ontology:constraint_beneficiary(connectome_sufficiency, computational_substrate_vendors).
narrative_ontology:constraint_victim(connectome_sufficiency, alternative_neuroscience_paradigms).
narrative_ontology:constraint_victim(connectome_sufficiency, embodied_cognition_research).
narrative_ontology:constraint_victim(connectome_sufficiency, dynamical_systems_neuroscience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE PARADIGMS (SNARE) — Research programs emphasizing embodiment, neuromodulation, and developmental plasticity are structurally trapped by funding concentration in connectomics. Cannot exit the paradigm competition; bear full cost of resource allocation asymmetry. The connectome sufficiency claim suppresses alternative approaches by framing them as secondary details rather than fundamental mechanisms.
constraint_indexing:constraint_classification(connectome_sufficiency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMBODIED COGNITION RESEARCHERS (TANGLED ROPE) — Constrained by career incentives and funding structures that privilege connectomics, but also benefit from the infrastructure (imaging technology, computational tools, data standards) developed by connectomics programs. Mixed experience: genuine coordination on methods alongside extraction via paradigm dominance.
constraint_indexing:constraint_classification(connectome_sufficiency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONNECTOMICS PROGRAMS (ROPE) — Primary beneficiaries experiencing the constraint as coordination: the sufficiency claim organizes research priorities, justifies large-scale funding, and provides a clear success criterion (complete wiring diagram). Net beneficiary with arbitrage exit — can pivot to alternative frameworks if connectome-only approaches fail, having already captured resources and infrastructure.
constraint_indexing:constraint_classification(connectome_sufficiency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEGRATIVE COALITION (TANGLED ROPE) — Organized researchers advocating for multi-scale models (connectome + dynamics + embodiment) see both coordination and extraction. The connectome provides essential structural data (coordination) but the sufficiency claim suppresses integration of other mechanisms (extraction). Mobile exit: can build integrative frameworks outside connectomics funding streams, but at career cost.
constraint_indexing:constraint_classification(connectome_sufficiency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: WBE INDUSTRY (PITON) — The emulation industry's commitment to connectome sufficiency has become increasingly theatrical as empirical challenges accumulate (C. elegans connectome complete since 1986, yet behavior not fully predicted; synaptic weights change faster than imaging resolution). The claim persists through institutional inertia and sunk costs rather than empirical validation. Industry sees its own roadmap as degraded but maintains it because alternative approaches lack comparable investment narratives.
constraint_indexing:constraint_classification(connectome_sufficiency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REDUCTIONIST VIEW (MOUNTAIN) — From a strong reductionist position, connectome sufficiency appears as a logical necessity: if the brain is a physical system, and the connectome specifies all structural relationships, then behavior must supervene on structure. This perspective sees the constraint as an immutable consequence of physicalism. However, the structural data contradicts mountain classification — the engine will compute this as a false summit, revealing that 'physical determinism' does not entail 'structural determinism' (dynamics, history-dependence, and body-environment coupling are also physical).
constraint_indexing:constraint_classification(connectome_sufficiency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: OPEN DATA MOVEMENT (SCAFFOLD) — Organized advocates for open connectome data, standardized formats, and reproducible pipelines see the sufficiency claim as a temporary coordination mechanism with a sunset: as open data accumulates and emulation attempts fail to match biological behavior, the paradigm will naturally evolve toward integrative multi-scale models. The constraint serves a transitional function (organizing data collection) that will dissolve as empirical evidence forces paradigm expansion.
constraint_indexing:constraint_classification(connectome_sufficiency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(connectome_sufficiency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(connectome_sufficiency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(connectome_sufficiency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(connectome_sufficiency, TR),
    TR >= 0.70.

:- end_tests(connectome_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Connectomics programs capture significant funding and paradigm authority, creating resource asymmetry that suppresses alternative approaches. However, extraction is not as severe as pure rent-seeking — connectome data does provide genuine scientific value, and the sufficiency claim organizes legitimate coordination problems (data standards, imaging protocols, computational infrastructure). The value reflects real but not maximal extraction: career advantages and funding concentration alongside genuine scientific contribution. Suppression (0.62): Moderate-high. Significant barriers prevent alternative paradigms from competing on equal terms: funding concentration in large-scale connectome projects, institutional prestige of structural mapping programs, computational infrastructure optimized for connectome analysis, publication bias favoring connectomics results. But suppression is not total — embodied cognition and dynamical systems research continue, and integrative approaches are emerging. Theater ratio (0.58): Moderate-high. The commitment to connectome sufficiency has become increasingly performative as empirical challenges accumulate. C. elegans connectome has been complete for nearly 40 years, yet behavior is not fully predictable from structure. Synaptic weights change on timescales comparable to behavior. Neuromodulation and body-environment coupling show causal relevance. Yet connectome projects continue to attract funding based on the sufficiency narrative rather than demonstrated behavioral prediction. The theater has increased over the interval as the gap between structural mapping progress and behavioral understanding has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural claim — connectome sufficiency for behavior — appears as coordination, extraction, degraded commitment, or natural law depending on observer position. Connectomics programs see legitimate scientific organization (rope). Alternative paradigms see resource suppression (snare). Embodied cognition researchers see mixed infrastructure benefit and paradigm extraction (tangled rope). Integrative coalitions see both genuine data value and paradigm overreach (tangled rope). The WBE industry sees its own degraded roadmap (piton). The analytical observer risks seeing logical necessity (mountain as false summit). The perspectival gap is not 'which view is correct' but 'which structural position are you measuring from.' The presheaf over observation sites captures all perspectives simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Connectomics programs are primary beneficiaries: they capture funding, infrastructure, and paradigm authority. The constraint runs in their favor — they experience low effective extraction (rope classification). Alternative paradigms are primary victims: they face resource suppression and paradigm marginalization. The constraint extracts from them — they experience high effective extraction (snare classification). Embodied cognition researchers are secondary victims with constrained exit: they face career barriers but also benefit from connectomics infrastructure, producing mixed experience (tangled rope). Integrative coalitions have mobile exit and see both coordination (structural data value) and extraction (paradigm dominance), also producing tangled rope. The WBE industry has arbitrage exit but sees its own commitment as degraded (piton from theater gate, not from high chi). The analytical observer risks seeing immutable physical law (mountain) but structural data reveals this as false summit — physicalism does not entail structural determinism.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves mandatrophy by showing that multiple classifications are legitimate perspectival readings of the same structural data. The question is not 'is connectome sufficiency a mountain, rope, or snare' but 'from which position are you observing.' The analytical observer's mountain is a false summit (naturalizes contingent paradigm choice as physical necessity). The beneficiary's rope is their genuine experience (connectome data does coordinate research). The scaffold is a real structural feature (open data movement sees empirical sunset). The piton is a real observation (WBE industry sees degraded commitment). The snare is the powerless paradigm's structural reality (resource suppression). The tangled rope is the moderate agent's mixed experience (infrastructure benefit plus paradigm extraction). The mandatrophy dissolves when we recognize that classification is indexical — the constraint's type depends on the observer's structural relationship to it, not on a single objective property.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    timescale_separation,
    'Are synaptic weight dynamics and neuromodulatory state changes slow enough relative to behavioral timescales that a static connectome captures functionally relevant structure?',
    'Longitudinal imaging of synaptic weights during learning tasks; comparison of weight change timescales to behavioral adaptation timescales; measurement of neuromodulator concentration dynamics during decision-making',
    'If timescales separate cleanly (weights change slowly, behavior emerges fast): connectome sufficiency strengthened, extraction reduced. If timescales overlap (weights change during behavior): dynamic state is causally relevant, extraction confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(timescale_separation, empirical, 'Whether synaptic dynamics are slow enough to treat connectome as static').

omega_variable(
    body_loop_necessity,
    'Do sensorimotor loops through the body constitute necessary causal structure for behavior, or are they implementable as boundary conditions on a brain-only simulation?',
    'Comparison of emulated agents with vs without body simulation across novel motor tasks; identification of behaviors that fail without proprioceptive feedback loops; analysis of whether body dynamics can be approximated by input-output mappings',
    'If body loops are approximable: connectome sufficiency viable, coordination interpretation strengthened. If body loops are constitutive: emulation requires body, sufficiency claim is extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(body_loop_necessity, empirical, 'Whether body-environment loops are necessary or boundary conditions').

omega_variable(
    developmental_contingency,
    'Is adult connectome structure sufficient to specify behavior, or does behavioral fidelity require developmental history (activity-dependent wiring, critical periods, experiential scaffolding)?',
    'Comparison of emulations initialized from adult connectomes vs emulations that simulate developmental trajectories; identification of behaviors that emerge only through developmental learning; analysis of whether critical period effects can be captured in static adult structure',
    'If adult structure suffices: sufficiency claim strengthened. If developmental history is necessary: connectome is endpoint of process, not specification of process; extraction confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_contingency, empirical, 'Whether developmental history is encoded in adult connectome').

omega_variable(
    c_elegans_counterexample,
    'Does the failure to predict C. elegans behavior from its complete connectome (known since 1986) constitute a decisive counterexample to connectome sufficiency, or is it explained by missing data (gap junctions, neuromodulation, muscle properties)?',
    'Systematic addition of missing mechanisms to C. elegans models; determination of which additions are necessary and sufficient for behavioral prediction; assessment of whether necessary additions are ''details'' or ''fundamental mechanisms''',
    'If missing data explains failure: sufficiency claim survives, extraction reduced. If fundamental mechanisms are missing: C. elegans is decisive counterexample, sufficiency claim is false, extraction confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(c_elegans_counterexample, empirical, 'Whether C. elegans failure falsifies sufficiency or reveals missing data').

omega_variable(
    substrate_independence,
    'Is behavioral function substrate-independent (implementable in silicon given correct connectome) or does it depend on specific biophysical properties (ion channel kinetics, dendritic computation, glial signaling)?',
    'Comparison of emulations using abstract neuron models vs biophysically detailed models; identification of behaviors that require specific biophysical mechanisms; analysis of whether these mechanisms can be abstracted to functional equivalents',
    'If substrate-independent: sufficiency claim viable, coordination interpretation. If substrate-dependent: connectome alone insufficient, extraction confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substrate_independence, conceptual, 'Whether behavior requires specific biophysical substrate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(connectome_sufficiency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_2000, connectome_sufficiency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_2005, connectome_sufficiency, theater_ratio, 5, 0.42).
narrative_ontology:measurement(theater_2010, connectome_sufficiency, theater_ratio, 10, 0.48).
narrative_ontology:measurement(theater_2015, connectome_sufficiency, theater_ratio, 15, 0.53).
narrative_ontology:measurement(theater_2020, connectome_sufficiency, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_2000, connectome_sufficiency, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(extract_2005, connectome_sufficiency, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(extract_2010, connectome_sufficiency, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(extract_2015, connectome_sufficiency, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(extract_2020, connectome_sufficiency, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(connectome_sufficiency, information_standard).
narrative_ontology:affects_constraint(connectome_sufficiency, computational_substrate_independence).
narrative_ontology:affects_constraint(connectome_sufficiency, embodied_cognition_necessity).
narrative_ontology:affects_constraint(connectome_sufficiency, synaptic_plasticity_timescales).

% DUAL FORMULATION NOTE:
% Connectome sufficiency is part of a constraint family in philosophy of mind and neuroscience. Related constraints include computational substrate independence (whether behavior is multiply realizable), embodied cognition necessity (whether body-environment loops are constitutive), and synaptic plasticity timescales (whether weights change slowly enough to treat connectome as static). Each has its own epsilon value reflecting different empirical and conceptual status. Connectome sufficiency sits upstream: if it fails, substrate independence and static structure assumptions also fail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
