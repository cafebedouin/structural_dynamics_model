% ============================================================================
% CONSTRAINT STORY: institutional_framing_tangled_rope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_framing_tangled_rope, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_framing_tangled_rope
 *   human_readable: Institutional Framing of Epistemic Limits as Optimization Problems
 *   domain: philosophy_of_science/computational_learning_theory/science_studies
 *
 * SUMMARY:
 *   The institutional framing of epistemic limits as optimization problems
 *   creates a structural misalignment between benchmark performance and
 *   epistemic reliability. Binary grading schemes reward models for confident
 *   guessing on irreducibly uncertain inputs rather than appropriate
 *   abstention. This constraint sits downstream of two distinct upstream
 *   constraints: (1) epistemic_irreducibility_mountain — the genuine
 *   computational learning theory limits on what can be known from finite
 *   data, and (2) formalization_translation_rope — the coordination problem
 *   of translating informal epistemic concepts into formal evaluation
 *   metrics. The institutional framing constraint is neither of these: it is
 *   the specific institutional choice to treat epistemic uncertainty as an
 *   optimization target rather than a boundary condition. Benchmark
 *   organizations benefit from simple, rankable metrics that drive
 *   engagement. Model developers face competitive pressure to maximize scores
 *   even when doing so produces miscalibrated systems. Deployed system users
 *   in safety-critical domains bear the cost when models trained on binary
 *   benchmarks fail to abstain appropriately. The constraint exhibits genuine
 *   coordination value (standardized evaluation enables progress tracking and
 *   reproducibility) alongside asymmetric extraction (the evaluation scheme
 *   systematically misaligns incentives). Theater ratio has increased over
 *   the interval as the gap between benchmark performance and real-world
 *   reliability has widened, with leaderboard rankings becoming increasingly
 *   detached from epistemic quality.
 *
 * KEY AGENTS:
 *   - Deployed System Users: Primary victims (powerless/trapped) — bear full cost of overconfident predictions in safety-critical domains with no exit option
 *   - Benchmark Organizations: Primary beneficiaries (institutional/arbitrage) — capture citation advantage and visibility from simple rankable metrics
 *   - Model Developers: Mixed position (moderate/constrained) — benefit from coordination infrastructure but face extraction through competitive pressure to optimize flawed metrics
 *   - Epistemic Reliability Coalition: Organized advocates (organized/mobile) — can build alternative frameworks but face adoption barriers from network effects
 *   - Academic Review Systems: Institutional degradation (institutional/constrained) — maintain performative benchmark requirements despite recognized epistemic problems
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination value embedded with extractive institutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_framing_tangled_rope, 0.58).
domain_priors:suppression_score(institutional_framing_tangled_rope, 0.62).
domain_priors:theater_ratio(institutional_framing_tangled_rope, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_framing_tangled_rope, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_framing_tangled_rope, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_framing_tangled_rope, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_framing_tangled_rope, tangled_rope).
narrative_ontology:human_readable(institutional_framing_tangled_rope, "Institutional Framing of Epistemic Limits as Optimization Problems").
narrative_ontology:topic_domain(institutional_framing_tangled_rope, "philosophy_of_science/computational_learning_theory/science_studies").

domain_priors:requires_active_enforcement(institutional_framing_tangled_rope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_framing_tangled_rope, benchmark_organizations).
narrative_ontology:constraint_beneficiary(institutional_framing_tangled_rope, model_developers).
narrative_ontology:constraint_beneficiary(institutional_framing_tangled_rope, leaderboard_maintainers).
narrative_ontology:constraint_victim(institutional_framing_tangled_rope, deployed_system_users).
narrative_ontology:constraint_victim(institutional_framing_tangled_rope, epistemic_reliability).
narrative_ontology:constraint_victim(institutional_framing_tangled_rope, safety_critical_domains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPLOYED SYSTEM USERS (SNARE) — Users in medical diagnosis, legal decision support, or autonomous systems cannot exit the constraint. They face models trained to maximize benchmark scores rather than calibrate uncertainty appropriately. Binary grading schemes reward confident guessing, and users bear the full cost when models fail to abstain on irreducibly uncertain inputs. Maximum extraction with no alternative.
constraint_indexing:constraint_classification(institutional_framing_tangled_rope, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MODEL DEVELOPERS (TANGLED ROPE) — Developers face genuine coordination benefits from standardized benchmarks (comparability, reproducibility, progress tracking) but also bear extraction through competitive pressure to optimize for flawed metrics. They could advocate for better evaluation schemes but face career costs and funding disadvantages if they unilaterally adopt confidence-threshold grading while competitors game binary metrics. Mixed experience: real coordination value alongside structural coercion.
constraint_indexing:constraint_classification(institutional_framing_tangled_rope, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BENCHMARK ORGANIZATIONS (ROPE) — Primary beneficiaries who experience the constraint as pure coordination. Binary grading schemes are computationally simple, produce clear rankings, and drive engagement. These organizations capture citation advantage, conference visibility, and funding priority. They can arbitrage between evaluation schemes and face minimal cost from the epistemic misalignment their metrics create downstream.
constraint_indexing:constraint_classification(institutional_framing_tangled_rope, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EPISTEMIC RELIABILITY COALITION (TANGLED ROPE) — Organized researchers advocating for confidence-threshold evaluation, selective prediction, and abstention-aware metrics. They benefit from the benchmark infrastructure (enables empirical study of calibration) but also experience extraction (their proposed fixes face adoption barriers due to leaderboard inertia and competitive dynamics). Mobile exit: can build alternative evaluation frameworks, but face network effects and institutional resistance.
constraint_indexing:constraint_classification(institutional_framing_tangled_rope, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC REVIEW SYSTEMS (PITON) — Peer review for ML papers increasingly treats benchmark performance as the primary validity signal, despite widespread recognition that leaderboard rankings are gameable and epistemically misleading. The review ritual persists through institutional inertia: reviewers demand benchmark comparisons even when inappropriate, and papers without leaderboard results face rejection regardless of conceptual contribution. High theater ratio: the evaluation process is performative rather than epistemically functional.
constraint_indexing:constraint_classification(institutional_framing_tangled_rope, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, benchmark infrastructure provides genuine coordination value (standardized evaluation enables cumulative progress) but embeds extractive institutional choices (binary grading, leaderboard ranking, competitive framing) that systematically misalign model behavior with epistemic reliability. The constraint is not a natural law — confidence-threshold evaluation and abstention-aware metrics are technically feasible — but institutional path dependence and competitive dynamics suppress alternatives. Tangled rope: real coordination function with embedded asymmetric extraction that requires active enforcement to maintain.
constraint_indexing:constraint_classification(institutional_framing_tangled_rope, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_framing_tangled_rope_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_framing_tangled_rope, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_framing_tangled_rope, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_framing_tangled_rope, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_framing_tangled_rope, TR),
    TR >= 0.70.

:- end_tests(institutional_framing_tangled_rope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from deployed system users who face overconfident models in safety-critical domains, and from model developers who must optimize for flawed metrics to remain competitive. However, there is genuine coordination value in standardized benchmarks — the extraction is not total. The value reflects that roughly 40% of the constraint's function is legitimate coordination (enabling comparability and progress tracking) while 60% is extractive institutional framing (binary grading that rewards confident guessing over calibration). Suppression (0.62): High-moderate. Significant barriers to alternative evaluation schemes include network effects (leaderboard visibility drives adoption), competitive dynamics (unilateral adoption of confidence-threshold grading creates disadvantage), institutional inertia (review systems demand benchmark comparisons), and technical lock-in (existing infrastructure optimized for binary metrics). However, suppression is not total — alternative frameworks like selective prediction and abstention-aware metrics exist and are gaining traction in safety-critical subfields. Theater ratio (0.68): High. Benchmark performance has become increasingly detached from epistemic reliability. Models achieve state-of-the-art scores through confident guessing on uncertain inputs, and leaderboard rankings correlate poorly with real-world calibration. Academic review treats benchmark numbers as validity signals despite widespread recognition of their limitations. The theater has increased over the interval as model scale has grown faster than evaluation sophistication.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classic tangled rope perspectival structure. Benchmark organizations see pure coordination (Rope) — they are solving the legitimate problem of standardized evaluation. Model developers see mixed coordination and extraction (Tangled Rope) — the infrastructure enables progress but coerces metric gaming. Deployed system users see pure extraction (Snare) — they bear the cost of miscalibrated systems with no exit. The epistemic reliability coalition sees a solvable coordination problem with embedded extraction (Tangled Rope from organized position). Academic review systems see their own degraded ritual (Piton) — benchmark requirements persist through inertia despite recognized epistemic problems. The analytical observer confirms tangled rope at civilizational scope — genuine coordination value with asymmetric extraction that is not a natural law but a contingent institutional arrangement. The perspectival gap reveals that 'benchmark performance' means different things to different agents: a coordination tool for organizations, a competitive weapon for developers, and a source of harm for users.
 *
 * DIRECTIONALITY LOGIC:
 *   Benchmark organizations are primary beneficiaries with arbitrage exit options — they experience low effective extraction and see the constraint as pure coordination. Model developers have constrained exit (can advocate for better metrics but face competitive disadvantage) and mixed beneficiary/victim status — they benefit from coordination infrastructure but bear extraction through metric gaming pressure. Deployed system users are pure victims with trapped exit — they cannot avoid models trained on flawed benchmarks and bear maximum extraction when overconfident predictions cause harm. The epistemic reliability coalition has mobile exit (can build alternatives) and organized power, experiencing moderate extraction despite being nominally victims. Academic review systems show piton dynamics — high theater ratio drives their classification regardless of directionality. The analytical observer sees the full structure: genuine coordination embedded with extractive institutional choices that require active enforcement to maintain.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the institutional framing is neither pure coordination (Rope) nor pure extraction (Snare) but a hybrid requiring both coordination and extraction components for classification. The coordination function is real: standardized benchmarks enable reproducibility, comparability, and cumulative progress. The extraction is also real: binary grading schemes systematically reward confident guessing over appropriate abstention, creating asymmetric costs borne by deployed system users. The constraint cannot be classified as Rope because it has identifiable victims (users facing overconfident models) and requires active enforcement (competitive pressure and institutional inertia suppress alternatives). It cannot be classified as Snare because it has genuine coordination value and identifiable beneficiaries who experience it as coordination rather than extraction. The tangled rope classification captures both structural features: the constraint coordinates evaluation while extracting from users through epistemic misalignment. The mandatrophy resolution is structural: both the coordination function and the extraction mechanism are necessary to explain the constraint's persistence and its differential impact across agent positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    calibration_vs_accuracy_tradeoff,
    'Is the tension between maximizing accuracy and maintaining calibration an inherent tradeoff in model training, or an artifact of binary evaluation schemes?',
    'Empirical comparison of models trained under confidence-threshold evaluation vs binary grading, controlling for architecture and data. Measure whether calibration-accuracy tradeoff persists when evaluation scheme changes.',
    'If inherent tradeoff: some extraction is unavoidable coordination cost (lower ε). If artifact of evaluation: current ε underestimates institutional extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(calibration_vs_accuracy_tradeoff, empirical, 'Whether calibration-accuracy tension is inherent or institutional').

omega_variable(
    leaderboard_adoption_barrier,
    'What proportion of leaderboard resistance to confidence-threshold metrics is due to technical complexity vs competitive advantage preservation?',
    'Survey of benchmark maintainers and model developers; analysis of stated reasons for not adopting proposed fixes; comparison with adoption rates of other technical changes of similar complexity.',
    'If primarily technical: lower suppression, coordination problem. If primarily competitive: higher suppression, extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leaderboard_adoption_barrier, empirical, 'Primary barrier to adopting confidence-threshold evaluation').

omega_variable(
    post_training_amplification_mechanism,
    'Does post-training (RLHF, DPO) amplify pretraining miscalibration through reward hacking, or does it inherit miscalibration passively from base model limitations?',
    'Controlled experiments comparing calibration degradation across post-training methods; analysis of reward model behavior on abstention vs confident guessing; measurement of calibration before and after alignment training.',
    'If active amplification: post-training is an extractive mechanism requiring separate constraint story. If passive inheritance: post-training is downstream effect, not independent constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_training_amplification_mechanism, empirical, 'Whether post-training actively amplifies or passively inherits miscalibration').

omega_variable(
    safety_critical_threshold,
    'At what deployment risk level does the cost of confident guessing on irreducibly uncertain inputs exceed the coordination benefit of standardized benchmarks?',
    'Cost-benefit analysis across deployment domains; measurement of harm from overconfident predictions in medical, legal, and autonomous systems; comparison with coordination value of benchmark infrastructure.',
    'If threshold is low: most current deployments exceed it, higher victim count. If threshold is high: extraction is concentrated in narrow safety-critical domains, lower victim count.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_critical_threshold, preference, 'Risk threshold where benchmark coordination value is outweighed by miscalibration cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_framing_tangled_rope, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_2015, institutional_framing_tangled_rope, theater_ratio, 0, 0.45).
narrative_ontology:measurement(theater_2018, institutional_framing_tangled_rope, theater_ratio, 3, 0.58).
narrative_ontology:measurement(theater_2021, institutional_framing_tangled_rope, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(extract_2015, institutional_framing_tangled_rope, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(extract_2018, institutional_framing_tangled_rope, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(extract_2021, institutional_framing_tangled_rope, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_framing_tangled_rope, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of two structurally distinct upstream constraints: epistemic_irreducibility_mountain (the genuine computational learning theory limits) and formalization_translation_rope (the coordination problem of translating informal concepts into formal metrics). The institutional framing constraint is neither of these — it is the specific institutional choice to treat epistemic uncertainty as an optimization target rather than a boundary condition. The three constraints form a family with different ε values: epistemic_irreducibility (ε ≈ 0.08, mountain), formalization_translation (ε ≈ 0.25, rope), institutional_framing (ε = 0.58, tangled rope). The institutional constraint inherits some coordination value from the formalization constraint but adds extractive institutional choices (binary grading, leaderboard ranking) that the formalization constraint does not contain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_framing_tangled_rope, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
