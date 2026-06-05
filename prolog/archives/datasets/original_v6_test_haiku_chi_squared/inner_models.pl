% ============================================================================
% CONSTRAINT STORY: inner_models
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inner_models, []).

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
 *   constraint_id: inner_models
 *   human_readable: Confirmation Bias in Inner Model Updating
 *   domain: social/cognitive_systems
 *
 * SUMMARY:
 *   Confirmation bias in inner model updating is a structural constraint on
 *   how agents incorporate evidence into their mental models. The phenomenon
 *   creates a systematic asymmetry: evidence confirming existing beliefs is
 *   accepted readily, while disconfirming evidence is scrutinized,
 *   reinterpreted, or forgotten. This constraint exhibits characteristics of
 *   both a natural law (arising from bounded rationality and finite
 *   processing capacity) and a socially enforced extraction mechanism
 *   (institutional structures amplify and maintain bias through publication
 *   bias, funding alignment, and status quo defaults). The constraint's
 *   theater_ratio (0.58) reflects that much institutional rhetoric treats
 *   confirmation bias as an inevitable consequence of human cognition, while
 *   simultaneously constructing social structures that maximize its effect.
 *   Over the interval [0,10], both base extractiveness and theater_ratio have
 *   increased, indicating that as information environments became more
 *   complex, the performance of naive confirmation bias declined while
 *   institutions invested more rhetorical effort in naturalizing it as
 *   unchangeable.
 *
 * KEY AGENTS:
 *   - Adaptive Agent: Primary victim (powerless/trapped) — seeks accurate inner models but is structurally trapped by bias in cognitive architecture; cannot exit without abandoning belief-updating altogether
 *   - Minority Evidence Generators: Secondary victim (moderate/constrained) — researchers or agents producing disconfirming evidence face filtering, reinterpretation, and suppression; exit constrained by resource costs
 *   - Research Teams/Institutional Paradigms: Mixed (organized/constrained beneficiary) — experience confirmation bias both as coordination mechanism (shared assumptions accelerate discovery) and as extraction (sunk costs suppress paradigm shifts)
 *   - Dominant Institutional Narratives: Primary beneficiary (institutional/arbitrage) — established models and institutions benefit from confirmation bias through stabilized expectations and justified resource distributions; can arbitrage between narratives
 *   - Metacognitive Reform Movements: Organized agents (organized/mobile) — employ debiasing techniques and see confirmation bias as temporary; building alternative epistemological pathways with sunset logic
 *   - Evolutionary Legacy: Institutional system (institutional/arbitrage) — confirmation bias persists as inertial mechanism from evolutionary adaptation; maintains throughculturally reinforced 'nature of human cognition' narratives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent cognitive architecture and institutional structures as immutable limits on knowing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inner_models, 0.52).
domain_priors:suppression_score(inner_models, 0.65).
domain_priors:theater_ratio(inner_models, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inner_models, extractiveness, 0.52).
narrative_ontology:constraint_metric(inner_models, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(inner_models, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inner_models, tangled_rope).
narrative_ontology:human_readable(inner_models, "Confirmation Bias in Inner Model Updating").
narrative_ontology:topic_domain(inner_models, "social/cognitive_systems").

domain_priors:requires_active_enforcement(inner_models).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inner_models, existing_belief_holders).
narrative_ontology:constraint_beneficiary(inner_models, institutional_narratives).
narrative_ontology:constraint_beneficiary(inner_models, status_quo_maintainers).
narrative_ontology:constraint_victim(inner_models, epistemic_accuracy).
narrative_ontology:constraint_victim(inner_models, adaptive_agents).
narrative_ontology:constraint_victim(inner_models, disconfirming_evidence_generators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADAPTIVE AGENT (SNARE) — An agent attempting to update their inner model toward accuracy is structurally trapped by confirmation bias. Disconfirming evidence is filtered, reinterpreted, or forgotten. The agent perceives freedom (I can update my beliefs), but the constraint operates through cognitive architecture itself. Cannot exit without abandoning the internal model altogether. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(inner_models, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MINORITY EVIDENCE GENERATOR (SNARE) — Researchers or agents producing disconfirming evidence experience structural suppression. Their findings are sought selectively, interpreted charitably only when confirming dominant models, and forgotten rapidly. Exit is constrained: producing evidence costs resources; having it ignored is the likely outcome. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.62.
constraint_indexing:constraint_classification(inner_models, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RESEARCH TEAM (TANGLED ROPE) — Groups working within a paradigm experience both coordination and extraction. Confirmation bias enables rapid model building (coordination: shared assumptions speed hypothesis generation). But the constraint also extracts: sunk costs in the current model suppress exploration of alternatives. Exit is constrained by career risk and funding alignment. d≈0.58, f(d)≈0.65, σ=0.9 → χ≈0.30.
constraint_indexing:constraint_classification(inner_models, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DOMINANT NARRATIVE (ROPE) — A well-established institutional model experiences confirmation bias as a coordination mechanism. Shared priors enable efficient communication, resource allocation, and collective action. The institution benefits from the constraint: it stabilizes expectations and justifies existing resource distributions. Arbitrage exit: leadership can shift narratives when incentives align. d≈0.10, f(d)≈0.02, σ=1.2 → χ≈0.01.
constraint_indexing:constraint_classification(inner_models, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: METACOGNITIVE REFORM (SCAFFOLD) — Agents employing debiasing techniques (pre-registration, adversarial collaboration, Bayesian updating frameworks, explicit disconfirmation search) see confirmation bias as a temporary constraint with a sunset clause. Modern epistemology includes tools to partially overcome bias: structured debate, empirical replication, diverse team composition. These reduce confirmation bias extraction by enforcing confrontation with disconfirming evidence. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.22.
constraint_indexing:constraint_classification(inner_models, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: EVOLUTIONARY BIOLOGY (PITON) — From a civilizational perspective, confirmation bias appears as an inertial constraint maintained by evolutionary legacy. Humans evolved with limited information-processing capacity; rapid model-building (even with bias) outperformed exhaustive evidence weighing. This evolutionary utility has degraded in modern environments with information abundance, but the cognitive mechanism persists through institutional inertia. theater_ratio≈0.68. The 'bias is natural' framing naturalizes what is now a contingent institutional artifact.
constraint_indexing:constraint_classification(inner_models, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: COMPUTATIONAL LIMITS / NATURAL LAW VIEW (MOUNTAIN) — An analytical observer might see confirmation bias as an irreducible consequence of finite computational resources: any bounded agent must filter information, and filtering creates bias. This perspective risks naturalizing a contingent cognitive architecture as immutable. The structural data (ε=0.52, suppression=0.65, theater=0.58) contradicts the mountain classification — the constraint is not a law of information theory but a specific architectural choice in how humans and institutions process evidence. The engine detects this as a false summit.
constraint_indexing:constraint_classification(inner_models, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inner_models_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inner_models, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inner_models, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inner_models, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inner_models, TR),
    TR >= 0.70.

:- end_tests(inner_models_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Confirmation bias extracts epistemic accuracy from those who update their beliefs. The original estimate of 0.72 was overstated — not all confirmation bias represents pure loss, as some bias provides coordination value. Revised to 0.52 reflects that extractiveness is genuine but mixed with coordination benefits. The extractiveness has increased over the interval as information complexity grew and institutional biases became more entrenched. Suppression (0.65): Moderate-high. Significant barriers to escaping confirmation bias include: cognitive architecture (automatic filtering), institutional incentives (publication bias rewards confirming findings), social reinforcement (confirmation of group priors valued), and career dependencies (paradigm shifts risk status loss). But suppression is not total — debiasing interventions exist and show measurable effects. Theater_ratio (0.58): Moderate-high. Institutional discourse treats confirmation bias as 'just how humans are' (natural law framing), masking the fact that institutional structures actively construct and amplify it. The theater has increased over the interval as institutions invested more rhetorical effort in naturalizing bias.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a critical perspectival gap between the beneficiary (dominant institutional narrative) and the victim (the adaptive agent). The dominant narrative sees confirmation bias as a coordination mechanism enabling shared priors and efficient collective action (Rope perspective). The adaptive agent sees it as a trap preventing accurate model updating (Snare perspective). The research team experiences both: bias speeds hypothesis generation (coordination value) but locks teams into paradigms (extraction cost). The metacognitive reform movement sees confirmation bias as a temporary problem being solved through structural epistemological change (Scaffold perspective). The evolutionary biology view risks naturalizing bias as immutable (Piton perspective). The analytical observer risks seeing confirmation bias as a law of bounded rationality (false Mountain perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Adaptive Agent: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Cannot exit the constraint without abandoning belief-updating mechanisms. Minority Evidence Generators: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction. Can exit (stop producing disconfirming evidence) but at high cost (career termination, resource loss). Research Team: Mixed (organized beneficiary + organized victim) + constrained → d≈0.58, f(d)≈0.65. Moderate extraction. Benefit from paradigm stability and shared assumptions, but trapped by sunk costs. Dominant Narrative: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Net beneficiary. Can shift narratives when optimal. Metacognitive Reform: Organized + mobile → d≈0.35, f(d)≈0.35. Low effective extraction; has agency and exit path through debiasing practices.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: Confirmation bias in inner model updating is a genuine Tangled Rope, not mislabeled pure extraction. The key mandatrophy resolution is that confirmation bias IS a coordination mechanism (shared belief priors enable rapid collective action and communication), AND it IS extractive (it traps agents in inaccurate models and privileges confirming evidence). Both properties are structural, not observational artifacts. The tangled rope classification prevents misframing bias as either 'merely natural human cognition' (false mountain) or 'purely extractive institutional conspiracy' (snare without coordination function). The scaffold perspective (metacognitive reform) represents a real sunset clause: debiasing techniques (pre-registration, adversarial collaboration, Bayesian frameworks, explicit disconfirmation protocols) are progressively reducing the extraction component while preserving coordination benefits through more explicit assumption-sharing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_versus_rational_priors,
    'Is confirmation bias a departure from rational Bayesian updating, or is it rational limited-information inference under resource constraints?',
    'Formal comparison of Bayesian posterior updates with observed human updating patterns; identification of whether human bias exceeds expected value from a resource-constrained Bayesian agent',
    'If bias exceeds rational limits: constraint is Snare (structural extraction of accuracy). If bias approximates rational limits: constraint is Rope (coordination benefit from shared priors exceeds bias cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_versus_rational_priors, conceptual, 'Whether confirmation bias exceeds rational inference under constraints').

omega_variable(
    institutional_enforcement_mechanism,
    'What fraction of confirmation bias is intrinsic to cognitive architecture versus enforced by institutional structures (publication bias, funding alignment, status quo defaults)?',
    'Comparison of bias rates in individual vs institutional contexts; measurement of bias reduction in organizations with explicit disconfirmation protocols; analysis of how institutional incentives shape evidence filtering',
    'If mostly intrinsic (>70%): constraint is unavoidable, scaffold perspective is aspirational. If mostly institutional (>60%): constraint is structurally engineered and could be dismantled, snare perspective confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_mechanism, empirical, 'Institutional versus intrinsic sources of confirmation bias').

omega_variable(
    debiasing_effectiveness_plateau,
    'Do debiasing interventions (pre-registration, adversarial collaboration, structured debate) reduce confirmation bias below some stable floor, or can they drive it near zero?',
    'Meta-analysis of debiasing intervention effect sizes across domains; long-term longitudinal studies of teams using structured debiasing; identification of residual bias even under optimal conditions',
    'If significant plateau (ε> 0.20 residual after optimization): scaffold has limited sunset. If near-zero achievable: scaffold sunset is real, and confirmation bias can be largely resolved through institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debiasing_effectiveness_plateau, empirical, 'Whether debiasing can reduce confirmation bias to near-zero').

omega_variable(
    model_initialization_dependency,
    'Is confirmation bias''s strength intrinsically tied to the quality of the initial inner model, such that false starting models inherently trigger stronger bias than true ones?',
    'Experimental manipulation of initial model accuracy; measurement of bias magnitude as a function of initial model distance from ground truth; identification of whether accurate priors reduce bias or merely change its direction',
    'If true: confirmation bias is a coordination mechanism for stabilizing accurate models (Rope). If false: confirmation bias equally traps agents in accurate and inaccurate models (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(model_initialization_dependency, empirical, 'Whether confirmation bias strength depends on initial model accuracy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inner_models, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imcb_tr_t0, inner_models, theater_ratio, 0, 0.42).
narrative_ontology:measurement(imcb_tr_t5, inner_models, theater_ratio, 5, 0.5).
narrative_ontology:measurement(imcb_tr_t10, inner_models, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(imcb_be_t0, inner_models, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(imcb_be_t5, inner_models, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(imcb_be_t10, inner_models, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inner_models, information_standard).
narrative_ontology:affects_constraint(inner_models, institutional_narrative_lock).
narrative_ontology:affects_constraint(inner_models, paradigm_shift_resistance).
narrative_ontology:affects_constraint(inner_models, evidence_hierarchy_bias).

% DUAL FORMULATION NOTE:
% Confirmation bias can be decomposed into cognitive architecture constraints (ε≈0.35, Mountain-candidate) and institutional enforcement structures (ε≈0.52, Tangled Rope). These are distinct constraints linked by amplification: institutions exploit cognitive bias to maintain narratives, which reinforce bias through behavioral loops. This story focuses on the tangled system; a parallel story could isolate institutional narrative lock (downstream constraint amplifying the cognitive substrate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inner_models, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
