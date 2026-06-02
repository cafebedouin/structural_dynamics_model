% ============================================================================
% CONSTRAINT STORY: scaling_feasibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scaling_feasibility, []).

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
 *   constraint_id: scaling_feasibility
 *   human_readable: Scaling Feasibility in Whole-Brain Emulation
 *   domain: computational_neuroscience/whole_brain_emulation/philosophy_of_mind
 *
 * SUMMARY:
 *   The scaling feasibility constraint in whole-brain emulation coordinates
 *   research effort across organism scales (C. elegans → Drosophila →
 *   zebrafish → mouse → primate → human) while managing exponential growth in
 *   complexity, validation requirements, and ethical constraints. This
 *   constraint exhibits primarily rope characteristics from most
 *   perspectives: it solves a genuine coordination problem (how to approach
 *   an intractable problem through tractable milestones) with minimal
 *   extractive overhead. The linear scaling of mapping technology (electron
 *   microscopy throughput, automated segmentation, synapse detection)
 *   encounters exponential barriers in biological complexity (synaptic
 *   diversity increases, neuromodulator systems proliferate, developmental
 *   history dependence deepens) and validation methodology (behavioral
 *   repertoires expand, functional verification becomes multidimensional,
 *   ground-truth establishment requires cross-scale integration). The
 *   constraint channels research effort productively rather than extracting
 *   rents, but does impose opportunity costs on actors focused on human-scale
 *   outcomes. Theater ratio (0.35) reflects moderate performative content:
 *   some scaling milestones are celebrated as progress toward whole-brain
 *   emulation when they primarily advance basic neuroscience; the emulation
 *   framing attracts funding but the actual research output serves broader
 *   scientific goals.
 *
 * KEY AGENTS:
 *   - Connectome Mapping Consortia: Primary beneficiary (institutional/mobile) — coordinate incremental scaling, standardize protocols, allocate resources across organism scales
 *   - Neuroscience Research Community: Primary beneficiary (organized/mobile) — gain validation targets, methodological lessons, and publishable milestones at each scale
 *   - Computational Infrastructure Providers: Beneficiary (powerful/mobile) — benefit from predictable scaling milestones that coordinate investment timing and capacity planning
 *   - Validation Methodology Developers: Organized agents (organized/constrained) — see constraint as temporary; building multi-scale validation frameworks with sunset logic
 *   - Whole-Brain Emulation Advocates: Mixed position (moderate/constrained) — experience both coordination value (incremental progress) and extraction (indefinite delay of human-scale emulation)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as coordinating research effort across tractable milestones while avoiding premature resource commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scaling_feasibility, 0.18).
domain_priors:suppression_score(scaling_feasibility, 0.22).
domain_priors:theater_ratio(scaling_feasibility, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scaling_feasibility, extractiveness, 0.18).
narrative_ontology:constraint_metric(scaling_feasibility, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(scaling_feasibility, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scaling_feasibility, rope).
narrative_ontology:human_readable(scaling_feasibility, "Scaling Feasibility in Whole-Brain Emulation").
narrative_ontology:topic_domain(scaling_feasibility, "computational_neuroscience/whole_brain_emulation/philosophy_of_mind").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scaling_feasibility, connectome_mapping_consortia).
narrative_ontology:constraint_beneficiary(scaling_feasibility, neuroscience_research_community).
narrative_ontology:constraint_beneficiary(scaling_feasibility, computational_infrastructure_providers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONNECTOME MAPPING CONSORTIA (ROPE) — Institutional actors coordinating incremental scaling from C. elegans (302 neurons) through Drosophila (~100K neurons) to mouse (~75M neurons). Experience the constraint as a coordination problem: standardizing protocols, sharing data, allocating resources across scales. Low extraction — the scaling curve creates genuine coordination value through methodological refinement at each scale.
constraint_indexing:constraint_classification(scaling_feasibility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: NEUROSCIENCE RESEARCH COMMUNITY (ROPE) — Organized researchers benefit from incremental scaling: each organism scale provides validation targets, methodological lessons, and publishable milestones. The scaling constraint coordinates research effort across tractable intermediate targets rather than forcing premature attempts at human-scale emulation. Low extraction — the constraint channels effort productively.
constraint_indexing:constraint_classification(scaling_feasibility, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPUTATIONAL INFRASTRUCTURE PROVIDERS (ROPE) — Hardware and software providers benefit from predictable scaling milestones: each organism scale defines concrete computational requirements, enabling targeted infrastructure development. The scaling curve coordinates investment timing and capacity planning. Low extraction — infrastructure development tracks genuine capability growth.
constraint_indexing:constraint_classification(scaling_feasibility, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: VALIDATION METHODOLOGY DEVELOPERS (SCAFFOLD) — Organized methodologists see the scaling constraint as temporary: current validation bottlenecks (behavioral comparison, functional verification, ground-truth establishment) are being solved through multi-scale validation frameworks. As validation methods mature across organism scales, the constraint's coordination function will sunset — validation will become routine rather than rate-limiting. Estimated sunset: 15-25 years as cross-scale validation protocols standardize.
constraint_indexing:constraint_classification(scaling_feasibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WHOLE-BRAIN EMULATION ADVOCATES (TANGLED ROPE) — Moderate-power actors (transhumanist researchers, longevity-focused funders) experience mixed coordination and extraction. The scaling constraint coordinates incremental progress but also delays human-scale emulation indefinitely — each organism scale reveals new complexity barriers (glial cell function, neuromodulator dynamics, developmental plasticity) that weren't visible at smaller scales. The constraint both enables methodological progress and extracts opportunity cost from those focused on human-scale outcomes.
constraint_indexing:constraint_classification(scaling_feasibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the scaling constraint coordinates research effort across tractable milestones while avoiding premature resource commitment to intractable problems. The exponential barriers (synaptic diversity, neuromodulation, developmental history dependence) are real structural features that make incremental scaling the only viable path. Low extraction — the constraint reflects genuine complexity rather than artificial barriers.
constraint_indexing:constraint_classification(scaling_feasibility, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scaling_feasibility_tests).
:- end_tests(scaling_feasibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The scaling constraint coordinates research effort productively with minimal rent extraction. The primary 'extraction' is opportunity cost for whole-brain emulation advocates who must wait for incremental scaling rather than attempting human-scale emulation directly, but this reflects genuine technical barriers rather than artificial constraints. The slight increase over time (0.12 → 0.18) reflects growing gap between mapping capability and validation methodology — as connectome data accumulates faster than validation protocols mature, some extraction emerges from researchers who have data but cannot validate it. Suppression (0.22): Low. Exit options are relatively open: researchers can work at any organism scale, pursue alternative approaches (theory, simulation, hybrid models), or exit to adjacent fields. The barriers are primarily technical (complexity, validation, computation) rather than institutional or coercive. Theater ratio (0.35): Moderate-low. Some performative content exists: scaling milestones are framed as progress toward whole-brain emulation to attract funding, but much of the actual research output serves basic neuroscience goals independent of emulation. The theater has increased modestly over time as the emulation framing has become more prominent in grant applications and public communication, but the underlying research remains substantive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits primarily rope characteristics from most perspectives, with one tangled_rope perspective (whole-brain emulation advocates) and one scaffold perspective (validation methodology developers). The gap between rope and tangled_rope reflects the difference between actors who value incremental neuroscience progress (rope — the constraint coordinates their work productively) and actors focused specifically on human-scale emulation (tangled_rope — the constraint both enables and delays their goal). The scaffold perspective reflects genuine structural change: validation methodology is maturing across scales, and the constraint's coordination function will sunset as validation becomes routine rather than rate-limiting. The analytical observer sees rope at civilizational scale: the exponential complexity barriers are real structural features, not artificial constraints, and incremental scaling is the only viable path. No perspective sees snare or mountain — the constraint is neither purely extractive nor immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   All primary agents are beneficiaries with mobile or constrained exit options, producing low directionality values and low effective extraction. The connectome mapping consortia (institutional/mobile/beneficiary) experience the constraint as pure coordination — they are solving the legitimate problem of how to approach human-scale emulation through tractable intermediate steps. The neuroscience research community (organized/mobile/beneficiary) benefits from the scaling structure: each organism scale provides concrete research targets and methodological advances. Computational infrastructure providers (powerful/mobile/beneficiary) benefit from predictable scaling milestones that coordinate investment. The validation methodology developers (organized/constrained/beneficiary) see a sunset: current validation bottlenecks are temporary and being actively solved. Only the whole-brain emulation advocates (moderate/constrained/mixed) experience significant extraction: the constraint delays their primary goal indefinitely while coordinating incremental progress. Their mixed beneficiary-victim status produces moderate directionality and moderate effective extraction, yielding the tangled_rope classification from their perspective. No agent is fully trapped or powerless — all have exit options and agency, consistent with the low suppression score.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates rope classification with minimal mandatrophy risk. The low extractiveness (0.18) and low suppression (0.22) place it firmly in coordination territory. The beneficiary declarations are clear: mapping consortia, research community, and infrastructure providers all gain coordination value. The absence of victims (no agent is structurally trapped or bearing asymmetric costs) confirms the rope classification. The one tangled_rope perspective (whole-brain emulation advocates) reflects genuine mixed experience: they benefit from methodological progress but bear opportunity cost from delayed human-scale emulation. This is not mislabeled extraction — it is legitimate coordination with differential value to different actors. The scaffold perspective (validation methodology developers) adds temporal nuance: the constraint's coordination function has a sunset as validation methods mature. The theater ratio (0.35) is moderate but not high enough to trigger piton classification — some performative framing exists but the underlying research is substantive. The constraint coordinates research effort across tractable milestones while managing exponential complexity barriers, with minimal extractive overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    validation_sufficiency_threshold,
    'At what organism scale does validation methodology become the primary bottleneck rather than mapping throughput?',
    'Comparative analysis of mapping completion timelines vs validation protocol development across C. elegans, Drosophila, zebrafish, and mouse scales; identification of validation gaps that persist despite complete connectome data',
    'If threshold < Drosophila scale: current rope classification understates extraction from validation-focused researchers. If threshold > mouse scale: mapping throughput remains the primary coordination challenge and rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_sufficiency_threshold, empirical, 'Organism scale at which validation becomes rate-limiting').

omega_variable(
    complexity_barrier_predictability,
    'Are the exponential complexity barriers (glial function, neuromodulation, plasticity) discoverable from smaller-scale studies or do they emerge only at larger scales?',
    'Historical analysis of complexity surprises at each organism scale transition; assessment of whether mouse-scale studies predict primate-scale barriers or whether each scale reveals fundamentally new phenomena',
    'If predictable: scaling constraint remains pure coordination (rope). If emergent: each scale transition carries hidden extraction risk for researchers who assumed smaller-scale lessons would transfer, potentially shifting toward tangled_rope from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_barrier_predictability, empirical, 'Whether complexity barriers are predictable from smaller scales').

omega_variable(
    ethical_constraint_trajectory,
    'Do ethical constraints on primate and human-scale research tighten or relax as validation methodology matures?',
    'Longitudinal tracking of institutional review board standards, international regulatory frameworks, and public acceptance as non-invasive imaging and post-mortem mapping techniques improve; correlation between methodological capability and ethical permission',
    'If tightening: ethical barriers may become the dominant constraint at primate scale, adding suppression and potentially shifting classification toward tangled_rope or scaffold with ethical sunset. If relaxing: technological scaling remains the primary constraint and rope classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_constraint_trajectory, preference, 'Trajectory of ethical constraints as technology matures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scaling_feasibility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scale_theater_2000, scaling_feasibility, theater_ratio, 0, 0.25).
narrative_ontology:measurement(scale_theater_2010, scaling_feasibility, theater_ratio, 10, 0.3).
narrative_ontology:measurement(scale_theater_2020, scaling_feasibility, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(scale_extract_2000, scaling_feasibility, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(scale_extract_2010, scaling_feasibility, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(scale_extract_2020, scaling_feasibility, base_extractiveness, 20, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scaling_feasibility, resource_allocation).

% DUAL FORMULATION NOTE:
% The scaling_feasibility constraint is downstream of connectome_sufficiency (whether connectome data alone suffices for emulation). The upstream constraint addresses the fundamental question of what data is necessary; the scaling constraint addresses the practical question of how to acquire that data across organism scales. The two constraints have different extractiveness values: connectome_sufficiency (tangled_rope, higher extraction) reflects contested theoretical claims about sufficiency, while scaling_feasibility (rope, lower extraction) reflects practical coordination of incremental research effort.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
