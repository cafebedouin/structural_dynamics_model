% ============================================================================
% CONSTRAINT STORY: brain_network_paradigm_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brain_network_paradigm_2026, []).

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
 *   constraint_id: brain_network_paradigm_2026
 *   human_readable: Distributed Brain Network Scientific Paradigm
 *   domain: neuroscience/computational_biology
 *
 * SUMMARY:
 *   The distributed brain network paradigm represents the dominant
 *   theoretical and methodological framework in contemporary neuroscience
 *   (2015–2026). Built on graph-theoretic foundations and enabled by
 *   high-throughput neuroimaging and connectomics technologies, this paradigm
 *   positions the brain as a complex dynamical network where function emerges
 *   from connectivity structure. The BRAIN Initiative, Human Connectome
 *   Project, and open-science neuroimaging platforms have institutionalized
 *   network thinking. However, the paradigm exhibits classical tangled-rope
 *   structure: it genuinely solves coordination problems (standardized data
 *   formats, shared computational infrastructure, multi-lab collaboration
 *   frameworks) while simultaneously suppressing alternative explanatory
 *   frameworks (molecular mechanisms, embodied cognition, single-cell
 *   dynamical systems). The constraint operates through grant-review
 *   gatekeeping, journal editorial preferences, and training pipeline
 *   effects. Theater ratio has risen from 0.38 (2010, when network thinking
 *   was legitimately novel) to 0.61 (2026), indicating increasing
 *   performative use of 'network complexity' language divorced from
 *   mechanistic necessity. Extractiveness has grown from 0.22 to 0.38 as the
 *   paradigm consolidates, suggesting rising costs to researchers working
 *   outside the framework.
 *
 * KEY AGENTS:
 *   - Network Methodology Researchers: Primary beneficiary (institutional/arbitrage) — connectomics, graph theory, dynamical systems modeling communities exist to operationalize this paradigm
 *   - Computational Neuroscience Labs: Primary beneficiary (institutional/arbitrage) — machine learning and systems-level modeling benefit from paradigm's standardized data and common research problems
 *   - Molecular Neuroscientists: Primary victim (powerless/trapped) — mechanistic research requires network-level reframing for funding and publication even when mechanism is the actual question
 *   - Alternative Theoretical Frameworks: Victim (organized/constrained) — embodied cognition, quantum biology, developmental dynamics research faces suppression through review gates but some exit through interdisciplinary venues
 *   - Neuroimaging Technology Vendors: Secondary beneficiary (institutional/arbitrage) — network paradigm drives sustained demand for fMRI, two-photon microscopy, electron microscopy, calcium imaging infrastructure
 *   - Funding Agencies: Institutional actor (institutional/arbitrage) — NIH BRAIN Initiative, ERC, NSF maintain paradigm through grant categories and review-panel composition; maintain paradigm partly through institutional inertia
 *   - Integrative Neuroscience Coalition: Emerging organized agent (powerful/mobile) — multi-scale modeling, informatics-bridging approaches building exit ramps; sees 15-20 year sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brain_network_paradigm_2026, 0.38).
domain_priors:suppression_score(brain_network_paradigm_2026, 0.48).
domain_priors:theater_ratio(brain_network_paradigm_2026, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brain_network_paradigm_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brain_network_paradigm_2026, tangled_rope).
narrative_ontology:human_readable(brain_network_paradigm_2026, "Distributed Brain Network Scientific Paradigm").
narrative_ontology:topic_domain(brain_network_paradigm_2026, "neuroscience/computational_biology").

domain_priors:requires_active_enforcement(brain_network_paradigm_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, network_methodology_researchers).
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, computational_neuroscience_labs).
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, neuroimaging_technology_vendors).
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, funding_agencies_favoring_systems_approaches).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, single_cell_biology_research).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, molecular_mechanism_investigators).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, alternative_theoretical_frameworks).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, neurobiological_reductionism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOLECULAR NEUROSCIENTIST (SNARE) — Single-cell and molecular researchers cannot exit the paradigm without career destruction. Publishing in top venues requires network-level framing even for mechanistic discoveries. Funding agencies (NIH BRAIN Initiative, ERC Synergy Grants) explicitly privilege connectomics and network dynamics over molecular detail. Career progression depends on adopting the network language regardless of actual research focus. Maximum experienced extraction.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE THEORETICAL FRAMEWORKS (TANGLED ROPE) — Critics of pure network approaches (dynamical systems, embodied cognition, quantum biological effects) benefit from network paradigm's infrastructure (data availability, computational tools) but face systematic rejection. Constrained by review gate (network framework reviewers), but some exit possible through specialized venues and interdisciplinary collaboration. Mixed extraction and coordination.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NETWORK METHODOLOGY COMMUNITIES (ROPE) — Graph theory, dynamical systems modeling, connectomics infrastructure builders experience the paradigm as pure coordination: standardized data formats, shared datasets (Allen Brain Atlas, Open Connectome), collaborative frameworks (Brain Connectivity Toolbox) enable their work. Net beneficiary without extraction cost — the paradigm's existence is necessary for their existence.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NEUROIMAGING VENDORS (ROPE) — fMRI manufacturers, calcium imaging system makers, electron microscopy vendors benefit from the network paradigm's requirement for high-throughput measurement infrastructure. The paradigm creates sustained market demand. Arbitrage available: can sell to any neuroscience subfield, but network paradigm is a growth driver.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTEGRATIVE NEUROSCIENCE COALITION (SCAFFOLD) — Emerging multi-scale integration approaches (bridging molecular, cellular, and network levels) see the pure network paradigm as a transitional stage. Machine learning for cross-scale translation, informatics pipelines linking genes to circuits, and embodied AI frameworks are building exit ramps. Sunset visible: within 15-20 years, sufficiently powerful multi-scale models will render isolated network analysis incomplete. Coalition has agency and sees a real sunset.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: FUNDING AGENCY STRUCTURES (PITON) — NIH and ERC funding mechanisms are partially performative theater around network paradigm. Review panels use 'network complexity' and 'connectome mapping' as proxies for significance even when research questions don't require network-level answers. Theater ratio elevated (0.61) because paradigm persistence is maintained partly through grant language and administrative categories rather than through empirical necessity. Funding structures have arbitrage — they could redirect without cost — but maintain the paradigm through institutional inertia.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, distributed networks ARE a fundamental feature of brain organization: neurons are nodes, synapses are edges, this is simply what neural tissue is. The brain has distributed structure independent of any paradigm or observer. However, this risks false summit — confusing the map (network description) with territory (actual causal organization). Network description is valid but may not exhaust explanatory necessity.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brain_network_paradigm_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(brain_network_paradigm_2026, TR),
    TR >= 0.70.

:- end_tests(brain_network_paradigm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The network paradigm extracts resources (grant funding, journal space, career opportunities) from researchers working at different scales, but this extraction is not maximal because: (1) network research is genuinely productive — connectome mapping has revealed brain organization principles; (2) molecular research remains fundable in specialized venues (Neuron, PNAS); (3) training in molecular biology is still valued. The extraction increases when research is forced into network-language frames unnecessarily. Suppression (0.48): Moderate-high. Barriers to non-network research include: (1) top-tier journal editorial preferences for systems-level framing; (2) grant-review panels weighted toward network methodology experts; (3) NIH BRAIN Initiative structure privileging connectomics and circuit mapping; (4) training programs emphasizing network thinking; (5) publication bias against 'mere mechanism' papers lacking network context. However, suppression is not complete — molecular biology funding exists, alternative venues exist, some prestigious researchers challenge the paradigm. Theater ratio (0.61): Moderate-high. Growing performative use of network language: (1) grant abstracts invoke 'complex networks' and 'circuit dynamics' where actual research is molecular; (2) funding categories use 'connectome,' 'connectomics,' 'neural circuits' as proxy filters without explicit mechanistic justification; (3) review panels assess network-framing as indicator of significance rather than question-specificity; (4) training emphasizes visualization of networks regardless of explanatory necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The molecular neuroscientist and network researcher experience opposite classifications (snare vs rope) from identical base metrics because their structural position determines their experienced extraction. The integrative coalition's scaffold perspective reveals that the constraint is institutionally contingent, not inherent. The piton perspective on funding structures reveals that paradigm persistence is maintained partly through administrative categories rather than empirical necessity. The false mountain perspective exposes the risk of naturalizing institutional arrangements as laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent derives from their structural position relative to the paradigm constraint. Network researchers and computational labs benefit (d ≈ 0.1-0.2, low extraction). Molecular scientists are victims trapped by career necessity (d ≈ 0.9, high extraction). Alternative frameworks are organized victims with partial escape routes (d ≈ 0.65, moderate extraction). Funding institutions are beneficiaries with arbitrage options (d ≈ 0.05, negative extraction experienced). The piton classification for funding structures derives from theater_ratio (0.61) exceeding the functional necessity — institutional categories ('connectome research') persist despite not explaining why those categories are necessary for the stated scientific goals. The scaffold classification for integrative approaches derives from genuinely observable exit routes (multi-scale modeling, informatics pipelines) with clear sunset timelines (15-20 years for maturity).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through perspectival decomposition. The question 'is the network paradigm coordination or extraction?' cannot be answered universally — it depends on the agent's structural position. For network researchers, it is pure coordination (rope). For molecular researchers without exit, it is extraction (snare). For technology vendors, it is rent-seeking on necessary infrastructure (rope). For alternative frameworks, it is suppression with partial coordination benefits (tangled rope). The integration community sees a temporary coordination problem with a definite sunset. The funding institutions see their own degraded ritual (piton). None of these perspectives is 'wrong' — the presheaf of observations over the constraint is the complete answer. The mandatrophy resolves by recognizing that 'the network paradigm' is not a single constraint but a family of structural relationships differentiated by agent position. The base extractiveness (0.38) and suppression (0.48) are the aggregate metrics. The classification differences reflect real differences in experienced constraint structure, not measurement ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_vs_correlation_in_networks,
    'Does network structure determine brain function, or does network analysis merely correlate with function while missing actual causal mechanisms at molecular/cellular scales?',
    'Experimental perturbation studies (optogenetics, chemogenetics) correlating network predictions with actual behavioral outcomes; comparison of network-model predictions to ground-truth molecular mechanisms in well-characterized systems (C. elegans, larval zebrafish)',
    'If networks are causal: tangled rope classification holds; suppression justified by coordination benefit. If networks are correlational: snare classification gains weight; suppression revealed as gatekeeping without function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_vs_correlation_in_networks, empirical, 'Whether network structure determines function or merely correlates with it').

omega_variable(
    scale_specificity_requirement,
    'For what class of neuroscience questions is network-level description necessary vs sufficient vs misleading?',
    'Systematic literature analysis: identify research questions answerable only at network level vs answerable at molecular level; map question types to empirical productivity; identify cases where network focus delayed mechanistic understanding',
    'If most significant questions are network-scale: paradigm is legitimately dominant. If many important questions are molecular-scale: paradigm represents forced frame-switching and unjustified suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_specificity_requirement, empirical, 'Question-scale mapping: which neuroscience questions require network-level answers').

omega_variable(
    multi_scale_integration_feasibility,
    'Can computational and informatic methods sufficiently bridge molecular, cellular, and network scales to make the network paradigm''s dominance obsolete within 15 years?',
    'Progress tracking in multi-scale modeling (e.g., OpenWorm, whole-brain Drosophila models, mammalian organoid simulations); feasibility studies in cross-scale prediction; timeline assessment from current computational capacity growth rates',
    'If feasible: scaffold classification confirmed — sunset is real and approaching. If infeasible: open question about permanent constraint status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_scale_integration_feasibility, empirical, 'Feasibility and timeline for multi-scale integration bridging paradigm gap').

omega_variable(
    publication_bias_vs_paradigm_superiority,
    'Is the network paradigm''s dominance in high-impact journals due to its empirical superiority or due to review-gate bias favoring network-framed research?',
    'Meta-analysis of citation impact and reproducibility: compare network vs non-network papers matched for experimental quality; examine reviewer comments identifying paradigm-driven rejections; study acceptance rates for identical research framed in different paradigms',
    'If superiority: paradigm dominance is justified. If bias: suppression metrics underestimate actual institutional gatekeeping cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(publication_bias_vs_paradigm_superiority, empirical, 'Publication bias toward network paradigm vs genuine empirical superiority').

omega_variable(
    embodied_cognition_incompatibility,
    'Are network-based models fundamentally incompatible with embodied/enactive/sensorimotor theories of cognition, or can they be integrated?',
    'Theoretical analysis of model compatibility; review of attempts at integration; assessment of whether incompatibility is structural or merely terminological',
    'If incompatible: suppression of alternative frameworks is epistemic cost. If integrable: current suppression is temporary coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_cognition_incompatibility, conceptual, 'Compatibility of network models with embodied cognition theories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brain_network_paradigm_2026, 2010, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brnp_tr_t0, brain_network_paradigm_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(brnp_tr_t8, brain_network_paradigm_2026, theater_ratio, 8, 0.5).
narrative_ontology:measurement(brnp_tr_t16, brain_network_paradigm_2026, theater_ratio, 16, 0.61).

% Extraction over time
narrative_ontology:measurement(brnp_be_t0, brain_network_paradigm_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(brnp_be_t8, brain_network_paradigm_2026, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(brnp_be_t16, brain_network_paradigm_2026, base_extractiveness, 16, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brain_network_paradigm_2026, information_standard).
narrative_ontology:affects_constraint(brain_network_paradigm_2026, connectome_data_standardization).
narrative_ontology:affects_constraint(brain_network_paradigm_2026, neuroimaging_technology_accessibility).
narrative_ontology:affects_constraint(brain_network_paradigm_2026, single_cell_mechanistic_research_funding).

% DUAL FORMULATION NOTE:
% The brain network paradigm can be decomposed into two structurally distinct constraints: (1) NETWORK_DESCRIPTION (ε ≈ 0.10, Mountain) — the brain has distributed connectivity structure independent of any paradigm; this is simply observable anatomy. (2) NETWORK_PARADIGM_DOMINANCE (ε = 0.38, Tangled Rope) — the institutional privileging of network-level explanation over other scales and the gatekeeping mechanisms enforcing this privilege. The first is a natural law; the second is an institutional arrangement. Confusing them produces false mountains. This story focuses on the second.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(brain_network_paradigm_2026, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
