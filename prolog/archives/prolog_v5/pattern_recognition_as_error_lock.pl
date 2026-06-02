% ============================================================================
% CONSTRAINT STORY: pattern_recognition_as_error_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pattern_recognition_as_error_lock, []).

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
 *   constraint_id: pattern_recognition_as_error_lock
 *   human_readable: Pattern Recognition as Cognitive Error Lock
 *   domain: cognitive_science/evolutionary_psychology/philosophy_of_mind
 *
 * SUMMARY:
 *   Pattern recognition as cognitive error lock represents a structural
 *   tension inherent to human cognition: the same neural architecture that
 *   enables rapid, efficient categorization and heuristic reasoning also
 *   generates and reinforces systematic epistemic errors. This constraint is
 *   downstream of temporal_perception_mismatch (the brain's need to construct
 *   coherent narratives from asynchronous sensory input drives
 *   pattern-seeking behavior) and exhibits the tangled_rope signature:
 *   genuine coordination function (cognitive efficiency, rapid
 *   decision-making under uncertainty) inseparably coupled with asymmetric
 *   extraction (confirmation bias, belief perseverance, resistance to
 *   disconfirming evidence). The constraint operates at multiple scales:
 *   individual cognition (immediate bias in belief formation), biographical
 *   learning (accumulated false patterns resistant to correction),
 *   institutional structures (scientific method as error-correction
 *   scaffold), and evolutionary timescales (ancestral environment calibration
 *   vs modern information ecology mismatch). The theater_ratio is relatively
 *   low (0.35) because the cognitive processes are functional, not
 *   performative — pattern recognition genuinely solves coordination problems
 *   even as it generates systematic error. The modest increase in theater
 *   over the interval reflects growing awareness that some 'critical thinking
 *   training' is performative (teaching bias labels without improving actual
 *   reasoning) rather than functional.
 *
 * KEY AGENTS:
 *   - Epistemic Accuracy: Primary victim (powerless/trapped) — abstract goal of forming true beliefs; cannot exit the cognitive architecture that generates systematic bias
 *   - Cognitive Efficiency: Primary beneficiary (institutional/arbitrage in ancestral context) — rapid decision-making, heuristic reasoning, pattern completion enable survival-relevant speed
 *   - Reflective Practitioner: Mixed position (moderate/constrained) — benefits from domain expertise pattern matching while bearing cost of bias in novel domains; partial exit through metacognitive override at high cognitive cost
 *   - Ideologically Committed Agent: Identity-locked victim (moderate/identity_locked) — structurally mobile but cognitively trapped by identity fusion with belief system; pattern recognition reinforces in-group/out-group boundaries
 *   - Scientific Community: Organized scaffold (organized/mobile) — institutional error-correction structures compensate for individual bias; sunset logic as cognitive prosthetics mature
 *   - Analytical Observer: Sees irreducible dual function (analytical/analytical) — pattern recognition is both genuine adaptation and systematic error source
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pattern_recognition_as_error_lock, 0.38).
domain_priors:suppression_score(pattern_recognition_as_error_lock, 0.52).
domain_priors:theater_ratio(pattern_recognition_as_error_lock, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pattern_recognition_as_error_lock, extractiveness, 0.38).
narrative_ontology:constraint_metric(pattern_recognition_as_error_lock, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(pattern_recognition_as_error_lock, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pattern_recognition_as_error_lock, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(pattern_recognition_as_error_lock, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pattern_recognition_as_error_lock, tangled_rope).
narrative_ontology:human_readable(pattern_recognition_as_error_lock, "Pattern Recognition as Cognitive Error Lock").
narrative_ontology:topic_domain(pattern_recognition_as_error_lock, "cognitive_science/evolutionary_psychology/philosophy_of_mind").

domain_priors:requires_active_enforcement(pattern_recognition_as_error_lock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pattern_recognition_as_error_lock, cognitive_efficiency).
narrative_ontology:constraint_beneficiary(pattern_recognition_as_error_lock, rapid_decision_making).
narrative_ontology:constraint_beneficiary(pattern_recognition_as_error_lock, evolutionary_fitness_ancestral_environment).
narrative_ontology:constraint_victim(pattern_recognition_as_error_lock, epistemic_accuracy).
narrative_ontology:constraint_victim(pattern_recognition_as_error_lock, belief_revision_capacity).
narrative_ontology:constraint_victim(pattern_recognition_as_error_lock, scientific_reasoning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC ACCURACY (SNARE) — Individual attempting to form accurate beliefs in real-time encounters systematic bias with no immediate exit. The same neural architecture that enables rapid categorization locks in false patterns. Maximum extraction: the cognitive system actively resists correction through confirmation bias and belief perseverance.
constraint_indexing:constraint_classification(pattern_recognition_as_error_lock, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: REFLECTIVE PRACTITIONER (TANGLED ROPE) — Agent with metacognitive training (scientist, critical thinker, trained skeptic) experiences both benefit and cost. Benefits from rapid pattern matching in domain expertise while bearing cost of systematic bias in novel domains. Can partially exit through deliberate System 2 override, but at significant cognitive cost and incomplete success.
constraint_indexing:constraint_classification(pattern_recognition_as_error_lock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ANCESTRAL ENVIRONMENT (ROPE) — In the evolutionary context where this cognitive architecture was selected, rapid pattern matching solved genuine coordination problems: predator detection, social coalition tracking, food source recognition. Speed-accuracy tradeoff favored false positives (see predator that isn't there) over false negatives (miss predator that is). Net beneficiary in ancestral context.
constraint_indexing:constraint_classification(pattern_recognition_as_error_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: SCIENTIFIC COMMUNITY (SCAFFOLD) — Organized institutional structures (peer review, replication requirements, adversarial collaboration, pre-registration) create temporary scaffolding that compensates for individual cognitive bias. These structures have sunset logic: as individual metacognitive training improves and AI-assisted reasoning tools mature, the need for institutional error-correction decreases. The constraint's extraction mechanism loses force as cognitive prosthetics become available.
constraint_indexing:constraint_classification(pattern_recognition_as_error_lock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: IDENTITY-LOCKED BELIEVER (TANGLED ROPE) — Agent whose self-concept is constituted through a belief system experiences pattern recognition as both coordination (in-group signaling, worldview coherence) and extraction (epistemic closure, resistance to disconfirming evidence). Structurally mobile (could encounter disconfirming evidence, could engage with alternative frameworks) but identity-fused with the pattern. Exit would require abandoning the identity frame, not just updating beliefs.
constraint_indexing:constraint_classification(pattern_recognition_as_error_lock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, pattern recognition is a genuine cognitive adaptation that solves real coordination problems (rapid categorization, heuristic reasoning, social cognition) while simultaneously generating systematic epistemic costs (confirmation bias, stereotyping, belief perseverance). Both functions are structural features of the same neural architecture. The constraint is not a bug or a feature — it is both, irreducibly.
constraint_indexing:constraint_classification(pattern_recognition_as_error_lock, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pattern_recognition_as_error_lock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pattern_recognition_as_error_lock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pattern_recognition_as_error_lock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(pattern_recognition_as_error_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. Pattern recognition generates genuine cognitive efficiency benefits (rapid categorization, heuristic reasoning, social cognition) that partially offset the epistemic costs. The extraction is real — confirmation bias, belief perseverance, and stereotyping impose systematic costs on epistemic accuracy — but not as severe as pure extraction mechanisms because the underlying function is adaptive. The value reflects that in many contexts, the speed-accuracy tradeoff favors speed appropriately (ancestral environment, time-pressured decisions, domain expertise), and extraction dominates only when accuracy requirements exceed the system's calibration (novel domains, complex causal reasoning, ideologically charged beliefs). Suppression (0.52): Moderate. Significant barriers to exit include: (1) architectural embedding (pattern recognition is a core feature of neural processing, not a separable module), (2) automaticity (System 1 processes run by default, System 2 override requires deliberate effort), (3) metacognitive difficulty (recognizing one's own bias is cognitively demanding), and (4) identity fusion (beliefs become constitutive of self-concept). But suppression is not total — metacognitive training, institutional structures, and deliberate reasoning strategies provide partial exit paths. Theater ratio (0.35): Low-moderate. Pattern recognition is genuinely functional, not performative. The cognitive processes solve real coordination problems (rapid threat detection, social coalition tracking, category-based inference). The modest theater component reflects that some 'bias awareness training' and 'critical thinking courses' are performative (teaching labels without improving reasoning) and that some belief perseverance is maintained for social signaling rather than epistemic reasons. The slight increase over the interval reflects growing recognition that bias training often fails to transfer to real-world reasoning.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a clear perspectival gap driven by temporal horizon and exit options. At immediate timescales with no exit (powerless/trapped), the constraint appears as pure extraction (snare) — the agent attempting to form accurate beliefs encounters systematic bias with no escape. At biographical timescales with constrained exit (moderate/constrained), the constraint appears as tangled_rope — the reflective practitioner experiences both benefit (domain expertise) and cost (bias in novel domains), with partial exit through metacognitive override. At generational timescales with organized exit (organized/mobile), the constraint appears as scaffold — institutional structures provide temporary error-correction mechanisms with sunset logic as cognitive prosthetics mature. From the ancestral environment perspective (institutional/arbitrage in evolutionary context), the constraint appears as rope — pattern recognition solved genuine coordination problems with net benefit. The analytical observer sees the irreducible dual function: pattern recognition is both a genuine cognitive adaptation and a systematic error source, and these are not separable features but two aspects of the same neural architecture. The identity-locked perspective reveals a distinct binding mechanism: the agent is structurally mobile but cognitively trapped by identity fusion, demonstrating that suppression can be internalized (the agent carries the constraint with them) rather than purely external.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the constraint's dual function. Cognitive efficiency is the primary beneficiary — pattern recognition exists because it solved ancestral survival problems, and it continues to provide decision-making speed in modern contexts. Epistemic accuracy is the primary victim — the same architecture that enables rapid categorization generates systematic false positives (seeing patterns that aren't there) and false negatives (missing patterns that are there, but less frequently due to the evolutionary bias toward false positives). The reflective practitioner occupies a mixed position: benefits from pattern matching in domains of expertise (a chess master's intuition, a radiologist's rapid diagnosis) while bearing costs in novel domains where patterns are unreliable. The identity-locked agent is structurally mobile (could encounter disconfirming evidence) but functionally trapped by identity fusion — the pattern recognition system reinforces in-group/out-group boundaries that constitute the agent's self-concept. The scientific community is organized and mobile — institutional structures provide collective exit paths (peer review catches individual bias, replication requirements filter false positives, adversarial collaboration forces engagement with disconfirming evidence) that individual agents cannot access alone.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the tangled_rope classification is structurally necessary, not a failure to disambiguate. The coordination function (cognitive efficiency, rapid decision-making) and the extraction mechanism (confirmation bias, belief perseverance) are not separable features that could be decomposed into rope + snare. They are two aspects of the same neural architecture: the speed-accuracy tradeoff that enables rapid pattern matching necessarily generates systematic false positives, and the pattern completion mechanisms that enable heuristic reasoning necessarily resist disconfirming evidence. Attempting to eliminate the extraction (perfect epistemic accuracy) would eliminate the coordination function (cognitive efficiency). Attempting to maximize the coordination function (maximum speed) would maximize the extraction (maximum bias). The constraint is genuinely hybrid — neither pure coordination nor pure extraction, but an irreducible coupling of both. The perspectival gap (snare from powerless/trapped, rope from institutional/arbitrage in ancestral context, scaffold from organized/mobile, tangled_rope from analytical) demonstrates that the classification depends on the observer's structural position, not on disambiguation of the constraint's 'true' nature. The omega variables identify the empirical uncertainties (speed-accuracy optimality, metacognitive override cost, belief perseverance mechanism, AI prosthetic sufficiency) that would refine the extractiveness estimate but would not resolve the fundamental dual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speed_accuracy_optimality,
    'Is the speed-accuracy tradeoff calibrated optimally for modern information environments, or does it reflect ancestral environment parameters that are now maladaptive?',
    'Comparative analysis of decision accuracy under time pressure across ancestral-like vs modern information-rich environments; measurement of false positive/false negative rates in contemporary contexts vs evolutionary models',
    'If optimally calibrated for modern environments: lower extractiveness (coordination function dominates). If ancestral calibration persists: higher extractiveness (systematic error in modern contexts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speed_accuracy_optimality, empirical, 'Whether speed-accuracy tradeoff is adaptive in modern environments').

omega_variable(
    metacognitive_override_cost,
    'What is the cognitive cost of System 2 override of pattern recognition biases, and does this cost scale with bias strength or remain constant?',
    'Dual-task paradigm studies measuring cognitive load during deliberate bias correction; longitudinal training studies tracking override cost reduction with practice',
    'If cost is high and constant: exit options remain constrained even for trained agents. If cost decreases with training: scaffold perspective strengthened (institutional structures can sunset as individual capacity improves).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metacognitive_override_cost, empirical, 'Cognitive cost of deliberate bias correction').

omega_variable(
    belief_perseverance_mechanism,
    'Is belief perseverance after disconfirmation a feature of pattern recognition architecture itself, or a downstream consequence of identity fusion and motivated reasoning?',
    'Experimental dissociation of belief perseverance in identity-neutral vs identity-relevant domains; neural imaging studies comparing activation patterns for disconfirming evidence in neutral vs valued beliefs',
    'If architectural: suppression is higher (constraint is more deeply embedded). If identity-driven: suppression is lower (constraint can be addressed through identity interventions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(belief_perseverance_mechanism, empirical, 'Whether belief perseverance is architectural or identity-driven').

omega_variable(
    ai_prosthetic_sufficiency,
    'Can AI-assisted reasoning tools effectively compensate for human pattern recognition biases, or do they inherit and amplify the same biases through training data?',
    'Comparative studies of human-only vs human-AI collaborative reasoning on tasks with known bias vulnerabilities; analysis of AI training data for embedded human cognitive biases',
    'If AI tools compensate effectively: scaffold sunset is real (technological exit path exists). If AI inherits biases: scaffold perspective is aspirational (no technological exit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_prosthetic_sufficiency, empirical, 'Whether AI tools provide genuine cognitive prosthetics or amplify bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pattern_recognition_as_error_lock, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pattern_err_tr_t0, pattern_recognition_as_error_lock, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pattern_err_tr_t25, pattern_recognition_as_error_lock, theater_ratio, 25, 0.32).
narrative_ontology:measurement(pattern_err_tr_t50, pattern_recognition_as_error_lock, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(pattern_err_be_t0, pattern_recognition_as_error_lock, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pattern_err_be_t25, pattern_recognition_as_error_lock, base_extractiveness, 25, 0.37).
narrative_ontology:measurement(pattern_err_be_t50, pattern_recognition_as_error_lock, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pattern_recognition_as_error_lock, identity_coordination).
narrative_ontology:affects_constraint(pattern_recognition_as_error_lock, scientific_method_as_error_correction).
narrative_ontology:affects_constraint(pattern_recognition_as_error_lock, ideological_belief_systems).
narrative_ontology:affects_constraint(pattern_recognition_as_error_lock, stereotype_formation).

% DUAL FORMULATION NOTE:
% Pattern recognition as error lock is downstream of temporal_perception_mismatch (the brain's need to construct coherent narratives from asynchronous sensory input drives pattern-seeking behavior) and upstream of multiple domain-specific constraints where pattern recognition generates systematic bias (scientific reasoning, ideological belief formation, social stereotyping). Each downstream constraint has its own extractiveness value reflecting the specific domain's error costs, but all share the common structural feature that the same cognitive architecture enabling function also generates error.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
