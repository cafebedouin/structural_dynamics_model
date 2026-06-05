% ============================================================================
% CONSTRAINT STORY: model_invisibility_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_invisibility_mechanism, []).

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
 *   constraint_id: model_invisibility_mechanism
 *   human_readable: Model Invisibility Through Repeated Use
 *   domain: epistemology/cognitive_science/philosophy_of_mind
 *
 * SUMMARY:
 *   Model invisibility through repeated use is the cognitive mechanism by
 *   which mental models become transparent to their users: we see with them
 *   rather than seeing them. This is the process by which the chess master
 *   sees threats directly rather than consciously applying rules, the
 *   radiologist sees tumors rather than pixel patterns, and the driver
 *   executes mirror-signal-maneuver automatically rather than deliberatively.
 *   The constraint is primarily a coordination mechanism — it solves the
 *   cognitive bottleneck of conscious deliberation and enables fluent
 *   expertise. However, it exhibits a mild extraction profile in
 *   paradigm-shift contexts: when the domain changes, the expert's
 *   automaticity becomes rigidity. The constraint is downstream of
 *   theory_laden_perception (the mountain-level fact that all observation is
 *   mediated by conceptual frameworks) but represents a distinct structural
 *   phenomenon: the specific process by which repeated use makes frameworks
 *   invisible. The extractiveness is low (0.18) because the mechanism is
 *   genuinely beneficial in stable environments and only becomes costly
 *   during paradigm transitions. The suppression is low (0.22) because
 *   metacognitive interventions can surface invisible models, though at a
 *   coordination cost. The theater_ratio is low (0.15) because the mechanism
 *   is functional: model invisibility genuinely enables skilled performance,
 *   not performative expertise.
 *
 * KEY AGENTS:
 *   - Expert Practitioners: Primary beneficiaries (powerful/mobile) — gain fluency and processing efficiency from model invisibility; experience it as skill acquisition
 *   - Learners Acquiring Expertise: Beneficiaries (moderate/mobile) — model invisibility is the goal of practice; they are gaining capacity through automaticity
 *   - Metacognitive Training Community: Organized agents (organized/constrained) — building interventions to surface invisible models; see the constraint as temporary with a sunset
 *   - Experts Facing Paradigm Shift: Mixed position (moderate/constrained) — experience model invisibility as both coordination (within-paradigm fluency) and extraction (paradigm-transition rigidity)
 *   - Scientific Community: Institutional beneficiaries (institutional/arbitrage) — shared invisible models enable distributed problem-solving and cumulative knowledge production
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees model invisibility as a structural feature of neural learning architectures; genuine mountain substrate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_invisibility_mechanism, 0.18).
domain_priors:suppression_score(model_invisibility_mechanism, 0.22).
domain_priors:theater_ratio(model_invisibility_mechanism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_invisibility_mechanism, extractiveness, 0.18).
narrative_ontology:constraint_metric(model_invisibility_mechanism, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(model_invisibility_mechanism, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_invisibility_mechanism, rope).
narrative_ontology:human_readable(model_invisibility_mechanism, "Model Invisibility Through Repeated Use").
narrative_ontology:topic_domain(model_invisibility_mechanism, "epistemology/cognitive_science/philosophy_of_mind").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(model_invisibility_mechanism, expert_practitioners).
narrative_ontology:constraint_beneficiary(model_invisibility_mechanism, domain_specialists).
narrative_ontology:constraint_beneficiary(model_invisibility_mechanism, skilled_cognizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPERT PRACTITIONER (ROPE) — Model invisibility enables fluent expertise. The chess master sees threats and opportunities directly rather than consciously applying rules. The radiologist sees tumors rather than pixel patterns. Model invisibility is the coordination mechanism that allows skilled performance — it solves the cognitive bottleneck of conscious deliberation. Low extraction: the practitioner benefits from automaticity and gains processing efficiency.
constraint_indexing:constraint_classification(model_invisibility_mechanism, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 2: LEARNER ACQUIRING EXPERTISE (ROPE) — Model invisibility is the goal of practice. The novice driver consciously attends to mirror-signal-maneuver; the experienced driver executes the sequence automatically while attending to traffic flow. Model invisibility coordinates the transition from effortful to fluent performance. The learner experiences this as skill acquisition, not extraction — they are gaining capacity, not losing it.
constraint_indexing:constraint_classification(model_invisibility_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: METACOGNITIVE TRAINING COMMUNITY (SCAFFOLD) — Organized efforts to make models visible (reflective practice protocols, peer review, adversarial collaboration, red-teaming) treat model invisibility as a temporary coordination problem with a sunset. The community develops techniques to surface invisible models: think-aloud protocols, contrastive cases, paradigm challenges. Model invisibility is not permanent — it can be reversed through deliberate metacognitive intervention. Scaffold classification reflects the sunset logic: as metacognitive norms mature, the invisibility mechanism loses force.
constraint_indexing:constraint_classification(model_invisibility_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: EXPERT FACING PARADIGM SHIFT (TANGLED ROPE) — When the domain changes, model invisibility becomes a mixed blessing. The expert's automaticity, which was coordination in a stable environment, becomes extraction when the environment shifts. The radiologist trained on film cannot see digital artifacts; the pilot trained on analog instruments struggles with glass cockpit automation. Model invisibility coordinates within-paradigm performance but extracts during paradigm transitions. The expert experiences both: fluency within the old paradigm (coordination) and rigidity when the paradigm shifts (extraction). Constrained exit: retraining is possible but costly.
constraint_indexing:constraint_classification(model_invisibility_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SCIENTIFIC COMMUNITY (ROPE) — Model invisibility enables cumulative knowledge production. Scientists internalize paradigms (Kuhn's normal science) so they can work efficiently within them. The invisibility of background assumptions coordinates research: everyone uses the same conceptual tools, enabling communication and collaboration. Extraction is minimal — the community benefits from shared invisible models that enable distributed problem-solving. Arbitrage exit: the community can surface models when needed (philosophy of science, foundational debates) but defaults to invisibility for efficiency.
constraint_indexing:constraint_classification(model_invisibility_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE ARCHITECTURE VIEW (MOUNTAIN) — From a civilizational/universal perspective, model invisibility through repeated use is a structural feature of how neural networks (biological or artificial) learn. Hebbian learning, synaptic consolidation, and procedural memory formation all produce automaticity through practice. The mechanism is not contingent on social arrangements — it emerges from the computational architecture of learning systems. This perspective sees model invisibility as an immutable property of embodied cognition. However, the mountain classification is genuine here, not a false summit: the cognitive mechanism is a natural law (accessibility_collapse high, resistance low, emerges_naturally true). The constraint is a coordination mechanism built on a mountain substrate.
constraint_indexing:constraint_classification(model_invisibility_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_invisibility_mechanism_tests).
:- end_tests(model_invisibility_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Model invisibility is primarily a coordination mechanism that enables fluent expertise. The extraction component is real but mild: experts facing paradigm shifts experience rigidity, and the cost of retraining is non-trivial. However, this extraction is not the dominant feature — most practitioners most of the time benefit from automaticity. The low extractiveness reflects that the mechanism is genuinely functional in stable environments and only becomes costly during rare paradigm transitions. Suppression (0.22): Low. Metacognitive interventions (reflective practice, think-aloud protocols, adversarial collaboration) can surface invisible models. The suppression is not zero because these interventions have coordination costs (time, cognitive effort, institutional support), but the barriers are surmountable. The low suppression reflects that model invisibility is reversible through deliberate effort. Theater ratio (0.15): Low. Model invisibility is functional, not performative. The chess master's automaticity genuinely enables skilled play; the radiologist's pattern recognition genuinely detects tumors. The theater component is minimal: some expert performance may be ritualized rather than functional, but this is not the dominant pattern. The low theater ratio reflects that the mechanism delivers real cognitive benefits.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single cognitive mechanism can be experienced as pure coordination (Rope) by most agents most of the time, as a temporary problem with a sunset (Scaffold) by organized metacognitive communities, as mixed coordination-extraction (Tangled Rope) by experts during paradigm shifts, and as an immutable natural law (Mountain) from the analytical perspective. The perspectival gap is not about disagreement over whether model invisibility exists — all perspectives agree on the phenomenon. The gap is about whether the mechanism is beneficial (coordination) or costly (extraction), and whether it is reversible (scaffold/tangled_rope) or permanent (mountain). The expert practitioner sees pure benefit; the expert facing a paradigm shift sees mixed benefit and cost; the metacognitive community sees a reversible problem; the analytical observer sees an immutable substrate. All perspectives are structurally accurate — the presheaf over the observation site captures the full complexity.
 *
 * DIRECTIONALITY LOGIC:
 *   Expert practitioners and learners are beneficiaries: they gain fluency and processing efficiency from model invisibility. Their directionality values are low (beneficiary status + mobile/powerful exit options), producing low or negative effective extraction — they experience the constraint as coordination. The metacognitive training community is organized with constrained exit: they can surface models but at a coordination cost. Their directionality is moderate, producing moderate effective extraction — they see the constraint as a solvable problem. Experts facing paradigm shifts are in a mixed position: beneficiaries within the old paradigm (low d) but victims during the transition (high d). Their perspective is tangled_rope because they experience both coordination and extraction depending on environmental stability. The scientific community is an institutional beneficiary with arbitrage exit: they can surface models when needed but default to invisibility for efficiency. Their directionality is low, producing low effective extraction — they experience the constraint as coordination. The analytical observer sees a genuine mountain: the cognitive mechanism is a natural law of neural learning. No victims are declared because the constraint does not systematically extract from any agent group — the paradigm-shift extraction is episodic and affects experts who were previously beneficiaries, not a distinct victim class.
 *
 * MANDATROPHY ANALYSIS:
 *   COORDINATION MECHANISM WITH EPISODIC EXTRACTION: This constraint resolves the mandatrophy by showing that model invisibility is primarily a coordination mechanism (it solves the cognitive bottleneck of conscious deliberation) with a mild, episodic extraction profile (experts facing paradigm shifts experience rigidity). The low extractiveness (0.18) and low suppression (0.22) reflect that the mechanism is genuinely beneficial in stable environments and only becomes costly during rare paradigm transitions. The constraint is not mislabeled coordination masking extraction (the Rope classification is accurate for most agents most of the time) nor is it mislabeled extraction masking coordination (the Tangled Rope classification is accurate for experts during paradigm shifts). The mandatrophy is resolved by recognizing that the constraint's type varies by perspective and environmental stability: Rope in stable environments, Tangled Rope during paradigm shifts, Scaffold from the metacognitive community's view, and Mountain from the analytical substrate view. The classification is indexical, not absolute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paradigm_shift_frequency,
    'How frequently do paradigm shifts occur in a given domain, and does this frequency change the extraction profile of model invisibility?',
    'Historical analysis of paradigm shifts across domains; correlation between shift frequency and expert obsolescence rates; measurement of retraining costs and success rates',
    'If shifts are rare (>50 years): model invisibility remains pure coordination (Rope) for most practitioners. If shifts are frequent (<10 years): model invisibility becomes extraction mechanism (Tangled Rope or Snare) as experts face repeated obsolescence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_shift_frequency, empirical, 'Paradigm shift frequency and its effect on extraction profile').

omega_variable(
    metacognitive_intervention_effectiveness,
    'Do metacognitive interventions (reflective practice, think-aloud protocols, adversarial collaboration) actually surface invisible models at rates that justify the coordination cost?',
    'Controlled studies comparing error rates and innovation rates in groups with vs without metacognitive training; longitudinal tracking of paradigm-shift adaptation in metacognitively trained vs untrained experts',
    'If effective: scaffold perspective confirmed — metacognitive norms provide a real sunset to model invisibility''s extraction risk. If ineffective: the interventions are theater, and model invisibility remains a permanent coordination-extraction tradeoff.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metacognitive_intervention_effectiveness, empirical, 'Whether metacognitive interventions effectively surface invisible models').

omega_variable(
    individual_variation_in_model_flexibility,
    'Do some individuals retain model visibility (or can more easily resurface models) despite extensive practice, and if so, what cognitive or dispositional factors predict this?',
    'Psychometric studies of cognitive flexibility, need for cognition, and openness to experience correlated with model resurfacing ability; neuroimaging studies of experts who retain metacognitive access vs those who don''t',
    'If individual variation is high: model invisibility is not a universal constraint but a population-level distribution, and interventions should target high-rigidity individuals. If variation is low: the mechanism is universal, and systemic interventions (training norms, institutional practices) are appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_variation_in_model_flexibility, empirical, 'Individual differences in model flexibility and resurfacing capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_invisibility_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(model_invis_tr_t0, model_invisibility_mechanism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(model_invis_tr_t5, model_invisibility_mechanism, theater_ratio, 5, 0.12).
narrative_ontology:measurement(model_invis_tr_t10, model_invisibility_mechanism, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(model_invis_be_t0, model_invisibility_mechanism, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(model_invis_be_t5, model_invisibility_mechanism, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(model_invis_be_t10, model_invisibility_mechanism, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_invisibility_mechanism, information_standard).

% DUAL FORMULATION NOTE:
% Model invisibility is downstream of theory_laden_perception (the mountain-level fact that all observation is mediated by conceptual frameworks) but represents a distinct structural phenomenon: the specific process by which repeated use makes frameworks invisible. Theory_laden_perception is the substrate (mountain); model_invisibility_mechanism is the coordination process built on that substrate (rope with episodic extraction during paradigm shifts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
