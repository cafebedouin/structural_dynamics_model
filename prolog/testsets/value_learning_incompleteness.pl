% ============================================================================
% CONSTRAINT STORY: value_learning_incompleteness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_value_learning_incompleteness, []).

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
 *   constraint_id: value_learning_incompleteness
 *   human_readable: Value Learning Incompleteness in AI Systems
 *   domain: artificial_intelligence/alignment/mechanism_design
 *
 * SUMMARY:
 *   Value learning incompleteness in AI systems creates a structural
 *   asymmetry between the values the system learns and the values
 *   stakeholders hold. During training, the system's objective function is
 *   constructed from a finite sample of observed or declared preferences —
 *   these might come from training data, human feedback, or specified utility
 *   functions. But this sample is necessarily incomplete: stakeholders exist
 *   whose values were not represented in training, future values that don't
 *   yet exist in the population, values held by communities without voice in
 *   the training process, and incommensurable values that resist
 *   quantification. Once training concludes, the system's value function
 *   becomes fixed or slowly adaptive, while stakeholder populations continue
 *   to generate new values, shift preferences, and discover previously
 *   unrepresented needs. This creates structural extraction: some groups
 *   benefit from alignment with the learned values (through coordination),
 *   while others suffer from misalignment with no mechanism to correct it.
 *   The constraint exhibits all six DR types depending on the observer's
 *   structural position relative to the value distribution.
 *
 * KEY AGENTS:
 *   - AI Developers: Primary beneficiary (institutional/arbitrage) — capture deployment advantage before value corrections become feasible; can arbitrage between value frameworks
 *   - Unrepresented Stakeholders: Primary victim (powerless/trapped) — excluded from training distribution, no exit mechanism, bear costs of misalignment
 *   - Value-Adjacent Communities: Secondary victim (moderate/constrained) — may participate in feedback mechanisms but face high barriers to shifting core learned values
 *   - Alignment Research Coalition: Organized agents (organized/constrained) — interpretability researchers, red-teamers, constitutional AI frameworks building technical solutions with generational timeline
 *   - Incumbent Value Framework (Utilitarian): Institutional actor (institutional/arbitrage) — benefits from continued use of utility maximization despite known limitations; maintenance is performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent architectural choices as fundamental limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(value_learning_incompleteness, 0.58).
domain_priors:suppression_score(value_learning_incompleteness, 0.65).
domain_priors:theater_ratio(value_learning_incompleteness, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(value_learning_incompleteness, extractiveness, 0.58).
narrative_ontology:constraint_metric(value_learning_incompleteness, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(value_learning_incompleteness, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(value_learning_incompleteness, tangled_rope).
narrative_ontology:human_readable(value_learning_incompleteness, "Value Learning Incompleteness in AI Systems").
narrative_ontology:topic_domain(value_learning_incompleteness, "artificial_intelligence/alignment/mechanism_design").

domain_priors:requires_active_enforcement(value_learning_incompleteness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(value_learning_incompleteness, ai_developers_with_time_advantage).
narrative_ontology:constraint_beneficiary(value_learning_incompleteness, incumbent_value_frameworks).
narrative_ontology:constraint_victim(value_learning_incompleteness, downstream_stakeholders).
narrative_ontology:constraint_victim(value_learning_incompleteness, value_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREPRESENTED STAKEHOLDER (SNARE) — Cannot exit the constraint; their values are structurally excluded from the learning process. If a value frame was not observed or encoded during training, the agent whose values it represents has no mechanism to correct or update the system's value function. Maximum extraction: stakeholders bear costs of misalignment with no voice in the system's behavior.
constraint_indexing:constraint_classification(value_learning_incompleteness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: VALUE-ADJACENT COMMUNITY (TANGLED ROPE) — Constrained by dependence on the AI system's decisions while also participating in its training ecosystem. Communities with values similar to the training distribution benefit from coordination; those distant from it face extraction. Moderate agency through advocacy, feedback mechanisms, and retraining initiatives, but high barriers to shifting the system's core learned values.
constraint_indexing:constraint_classification(value_learning_incompleteness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI DEVELOPER (ROPE) — Benefits from the value learning bottleneck through time advantage: deployment window before value corrections become feasible. Experiences constraint as coordination problem (training the system to learn values at all). Can arbitrage between different value frameworks through model versions and fine-tuning.
constraint_indexing:constraint_classification(value_learning_incompleteness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALIGNMENT RESEARCH COALITION (SCAFFOLD) — Organized agents (interpretability researchers, red-teamers, constitutional AI frameworks) see value incompleteness as a solvable technical problem with a generational sunset. Constitutional AI, mechanistic interpretability, and value-learning architectures are building pathways to capture broader value distributions. Suppression is high initially but declining as techniques mature.
constraint_indexing:constraint_classification(value_learning_incompleteness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UTILITY MAXIMIZATION FRAMEWORK (PITON) — Classical expected utility theory persists in AI system design largely through institutional inertia despite known limitations in value aggregation. The framework cannot represent incommensurable values, value pluralism, or context-dependent preferences. Maintenance is largely performative: system designers cite utility maximization in documentation while using ad-hoc value encoding in practice.
constraint_indexing:constraint_classification(value_learning_incompleteness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational perspective, value incompleteness may reflect a fundamental limit: no finite training distribution can capture the complete value space of an unbounded stakeholder population. Arrow's impossibility theorem, value incommensurability, and learning-theoretic bounds suggest structural unchangeability. However, this false summit naturalizes what may be contingent limitations of current architecture choices.
constraint_indexing:constraint_classification(value_learning_incompleteness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_learning_incompleteness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(value_learning_incompleteness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_learning_incompleteness, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(value_learning_incompleteness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(value_learning_incompleteness, TR),
    TR >= 0.70.

:- end_tests(value_learning_incompleteness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system captures career and deployment value from the time advantage before value corrections; developers can fine-tune for different stakeholders but the base learned values persist. Extraction is not total because communities can provide feedback and some value learning mechanisms exist, but the barriers are high. The value has increased from 0.32 to 0.58 over the interval (time_point 0-6) as stakeholder dissatisfaction accumulates and unrepresented values become salient, then plateaus as the system's core values stabilize. Suppression (0.65): High. Barriers to correcting learned values include: (1) architectural inertia — retraining is expensive, (2) stakeholder exclusion — many groups lack channels for value input, (3) measurement problems — some values resist quantification, (4) institutional capture — current value frameworks benefit incumbents. Theater ratio (0.68): Moderate-high. Contemporary AI value specification uses elaborate formal frameworks and stakeholder consultation, but much of this is performative — the actual learned values reflect the training data distribution and developer priors more than stakeholder input. Constitutional AI adds procedural legitimacy but doesn't fundamentally resolve incompleteness.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the developer's rope (coordinate preferences through training) and the unrepresented stakeholder's snare (no mechanism to register existence, let alone preferences). The open-science alignment coalition sees a solvable scaffold (constitutional AI, mechanistic interpretability, ongoing value learning architectures), while the utilitarian framework sees its own persistence as performative theater — utilities are cited, but actual value encoding reflects developer choices and historical accident. The natural limit view (mountain) claims value incompleteness is inherent to any learning system with finite samples, but this naturalizes what may be architectural contingency. If stakeholder populations continuously generate new values, then no completion state exists — the constraint is not a temporary bottleneck to overcome but a permanent feature of system-stakeholder coevolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from the agent's structural position relative to value representation. Developers with arbitrage exit options (can deploy different value versions, fine-tune for different markets) experience low d — they benefit from incompleteness by capturing multiple stakeholder segments. Unrepresented stakeholders with no exit (no mechanism to correct the system's values about them) experience high d. Value-adjacent communities (values partially represented, but can only exit through costly retraining or system replacement) experience moderate-high d. The alignment research coalition has organized power to build alternatives (scaffold perspective) — their d is moderate because they can see and work toward an exit. The utilitarian framework benefits from continued use (d ≈ 0.15) despite limitations. The analytical observer risks high d (seeing the constraint as unchangeable natural law) but the false summit detection reveals this as naturalization of contingent choices.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR OF VALUE INCOMPLETENESS ITSELF: The mandatrophy resolution reveals that value learning incompleteness is not a constraint the system has — it IS a constraint the system creates. The system's learned values determine which other constraints agents experience. A bank's AI system trained primarily on financial stability values may treat access constraints differently than one trained on inclusivity values. The AI's value learning incompleteness cascades into constraint multiplicity for downstream stakeholders. The mandatrophy here is not 'what type is this constraint?' but 'this constraint creates the conditions under which other constraints proliferate.' Resolution requires making explicit that: (1) no training distribution is complete, (2) stakeholder populations continuously generate new values, (3) architectural solutions like constitutional AI or mechanistic interpretability can reduce but never eliminate incompleteness, (4) value pluralism frameworks may be necessary rather than unified learning targets. The tangled_rope classification holds: genuine coordination exists (system learns to respond to values at all), but asymmetric extraction is structural (some groups' values are cheaper to represent than others; developers benefit from the window before value corrections).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    learning_theoretic_lower_bound,
    'Is value incompleteness a fundamental information-theoretic limit or a contingent limitation of current training methodologies?',
    'Formal analysis of sample complexity for value function learning; comparison of bounds for different value representation schemes; empirical testing of alternative architectures',
    'If fundamental limit: mountain classification confirmed, constraint is immutable. If contingent: constraint is tangled_rope/scaffold, technical solutions exist but face institutional barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(learning_theoretic_lower_bound, empirical, 'Whether value incompleteness reflects fundamental limits or current methodology constraints').

omega_variable(
    value_space_cardinality,
    'Is the stakeholder value space finite and potentially completable through enumeration, or infinite and requiring continuous learning mechanisms?',
    'Formal definition of value space cardinality; analysis of value distribution across actual stakeholder populations; testing whether new values emerge after training completion',
    'If finite and enumerable: technical solution possible. If infinite or continuously generating: constraint is structurally persistent; learning systems must operate with permanently incomplete value capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(value_space_cardinality, conceptual, 'Cardinality and enumerability of stakeholder value space').

omega_variable(
    incommensurability_irreducibility,
    'Can incommensurable values be represented in a unified learning framework, or does value incompleteness reflect genuinely incommensurable value systems?',
    'Empirical testing of multi-objective value learning on historically incommensurable domains (individual liberty vs collective welfare, present vs future generations); formal analysis of value representation expressiveness',
    'If commensurable: architectural solutions may exist. If irreducibly incommensurable: constraint is classification-dependent on which values are declared ''core'' — piton classification from utilitarian framework, snare from excluded perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incommensurability_irreducibility, conceptual, 'Whether incommensurable values can be represented in unified frameworks').

omega_variable(
    training_distribution_bias_persistence,
    'Does value learning incompleteness persist even with perfectly accurate training on the actual stakeholder value distribution, or is incompleteness solely a function of distribution bias?',
    'Counterfactual analysis: simulate perfect training on maximal stakeholder sampling. Compare against current distribution bias. If incompleteness persists: architectural limit. If incompleteness disappears: distribution bias is the sole driver.',
    'If persistence: need architectural changes and value pluralism mechanisms. If distribution bias alone: problem is governance/stakeholder inclusion, not fundamental learning limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(training_distribution_bias_persistence, empirical, 'Whether incompleteness persists beyond training distribution bias').

omega_variable(
    temporal_value_emergence,
    'Do new values emerge in stakeholder populations after the AI system''s training completion, making the constraint inherently non-static?',
    'Longitudinal study of value distributions in stakeholder populations; tracking of novel values not present in training set appearing post-deployment; measurement of cultural/moral innovation rates',
    'If values continuously emerge: no completion state exists, constraint is permanent feature of system-stakeholder co-evolution, requires ongoing adaptation mechanisms rather than one-time learning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_value_emergence, empirical, 'Whether stakeholder values continuously emerge post-training').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(value_learning_incompleteness, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vli_tr_t0, value_learning_incompleteness, theater_ratio, 0, 0.52).
narrative_ontology:measurement(vli_tr_t3, value_learning_incompleteness, theater_ratio, 3, 0.62).
narrative_ontology:measurement(vli_tr_t6, value_learning_incompleteness, theater_ratio, 6, 0.68).
narrative_ontology:measurement(vli_tr_t9, value_learning_incompleteness, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(vli_be_t0, value_learning_incompleteness, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(vli_be_t3, value_learning_incompleteness, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(vli_be_t6, value_learning_incompleteness, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(vli_be_t9, value_learning_incompleteness, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(value_learning_incompleteness, identity_coordination).
narrative_ontology:affects_constraint(value_learning_incompleteness, goal_specification_hazard).
narrative_ontology:affects_constraint(value_learning_incompleteness, distributional_shift_alignment).
narrative_ontology:affects_constraint(value_learning_incompleteness, preference_aggregation_impossibility).

% DUAL FORMULATION NOTE:
% Value learning incompleteness decomposes into three structurally distinct constraints: (1) goal_specification_hazard — the problem of specifying any goal at all given ontological uncertainty; (2) distributional_shift_alignment — the problem of value learning that generalizes beyond training distribution; (3) preference_aggregation_impossibility — the problem of representing incommensurable values in unified frameworks. This story models the constraint at the system level; the downstream stories model specific architectural instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(value_learning_incompleteness, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
