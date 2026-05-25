% ============================================================================
% CONSTRAINT STORY: parable_as_transmission_layer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parable_as_transmission_layer, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: parable_as_transmission_layer
 *   human_readable: Parable as Transmission Layer in Epistemic Warning Propagation
 *   domain: epistemology/philosophy_of_science/cognitive_science
 *
 * SUMMARY:
 *   The parable transmission layer describes a cognitive and cultural
 *   constraint on how epistemic warnings propagate across disciplinary and
 *   cultural boundaries. Warnings that compress into portable narratives —
 *   Plato's Cave (epistemic humility about perception), Schrödinger's Cat
 *   (quantum measurement problem), the Prisoner's Dilemma (cooperation
 *   failure), Gödel's Incompleteness (limits of formal systems) — achieve
 *   cross-disciplinary citation, textbook inclusion, and popular recognition.
 *   Warnings of equal validity that resist narrative compression — formal
 *   frameworks, technical proofs, discipline-specific methodologies — remain
 *   local to their originating communities despite equivalent epistemic
 *   content. This is not an institutional choice or a correctable bias but an
 *   emergent property of how information propagates through populations with
 *   bounded rationality and cultural transmission bottlenecks. The constraint
 *   arises from: (1) working memory limits (parables fit in ~7 chunks; formal
 *   frameworks exceed capacity), (2) narrative schema processing (humans
 *   encode and retrieve stories more efficiently than abstract structures),
 *   (3) oral transmission fitness (parables survive retelling; technical
 *   precision degrades), and (4) cultural evolution selection pressure
 *   (high-fidelity low-bandwidth signals outcompete low-fidelity
 *   high-bandwidth signals in noisy channels). The structural delta is not
 *   that parables are better epistemology than formal frameworks — both are
 *   valid within their domains — but that the transmission medium selects for
 *   compression regardless of content validity.
 *
 * KEY AGENTS:
 *   - Discipline-Local Researcher: Experiences immutable transmission barrier (powerless/trapped) — cannot make formal frameworks propagate cross-disciplinarily at parable rates
 *   - Science Communication Institution: Recognizes structural feature (institutional/arbitrage) — can amplify specific ideas but cannot change memetic fitness landscape
 *   - Analytical Observer: Sees cognitive universal (analytical/analytical) — transmission differential emerges from working memory constraints and cultural evolution
 *   - Interdisciplinary Research Consortium: Faces bandwidth limit (organized/constrained) — pedagogy and outreach cannot override cognitive architecture
 *   - Public Intellectual: Translates but cannot equalize (moderate/mobile) — can create parables from formal content but cannot make untranslated versions propagate equally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parable_as_transmission_layer, 0.12).
domain_priors:suppression_score(parable_as_transmission_layer, 0.03).
domain_priors:theater_ratio(parable_as_transmission_layer, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parable_as_transmission_layer, extractiveness, 0.12).
narrative_ontology:constraint_metric(parable_as_transmission_layer, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(parable_as_transmission_layer, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(parable_as_transmission_layer, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(parable_as_transmission_layer, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parable_as_transmission_layer, mountain).
narrative_ontology:human_readable(parable_as_transmission_layer, "Parable as Transmission Layer in Epistemic Warning Propagation").
narrative_ontology:topic_domain(parable_as_transmission_layer, "epistemology/philosophy_of_science/cognitive_science").

domain_priors:emerges_naturally(parable_as_transmission_layer).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCIPLINE-LOCAL RESEARCHER (MOUNTAIN) — Cannot change the cognitive architecture that makes compressed narratives more transmissible than formal frameworks. Experiences the constraint as an immutable feature of human communication: parables propagate, technical precision doesn't, regardless of effort invested in clarity.
constraint_indexing:constraint_classification(parable_as_transmission_layer, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SCIENCE COMMUNICATION INSTITUTION (MOUNTAIN) — Recognizes the transmission differential as a structural feature of cultural evolution. Can choose which ideas to amplify but cannot change the underlying selection pressure favoring narrative compression. The constraint is a law of memetic fitness, not an institutional choice.
constraint_indexing:constraint_classification(parable_as_transmission_layer, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the parable transmission layer is a cognitive universal arising from working memory constraints, narrative schema processing, and cultural evolution dynamics. Parables compress epistemic warnings into portable units that fit human cognitive architecture; formal frameworks exceed working memory capacity and resist oral transmission. This is not a contingent institutional arrangement but an emergent property of how information propagates through populations with bounded rationality.
constraint_indexing:constraint_classification(parable_as_transmission_layer, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: INTERDISCIPLINARY RESEARCH CONSORTIUM (MOUNTAIN) — Organized groups attempting to propagate formal frameworks cross-disciplinarily face the same transmission barrier. Can invest in pedagogy, visualization, and outreach, but cannot override the cognitive architecture that makes parables more portable. The constraint is experienced as an immutable limit on knowledge transfer bandwidth.
constraint_indexing:constraint_classification(parable_as_transmission_layer, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: PUBLIC INTELLECTUAL (MOUNTAIN) — Can choose to translate formal frameworks into parables (Plato's Cave, Schrödinger's Cat, the Prisoner's Dilemma) but cannot make the untranslated formal version propagate at the same rate. The transmission differential is a constraint on the medium, not on the message's validity. Mobility in career terms does not grant escape from cognitive architecture.
constraint_indexing:constraint_classification(parable_as_transmission_layer, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parable_as_transmission_layer_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(parable_as_transmission_layer, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parable_as_transmission_layer, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(parable_as_transmission_layer, ExtMetricName, E),
    domain_priors:suppression_score(parable_as_transmission_layer, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(parable_as_transmission_layer),
    narrative_ontology:constraint_metric(parable_as_transmission_layer, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(parable_as_transmission_layer, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(parable_as_transmission_layer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint does not extract rents or create asymmetric advantage. It is a neutral feature of cognitive architecture and cultural transmission. The slight non-zero value reflects that parable-compressible ideas gain citation advantage and cultural persistence, which could be framed as a mild selection bias, but this is not extraction in the DR sense — it is memetic fitness. Suppression (0.03): Negligible. No active enforcement prevents formal frameworks from propagating; the barrier is passive (cognitive limits, not coercion). Researchers are free to attempt cross-disciplinary transmission of technical content; the constraint is that such attempts face higher friction, not prohibition. Theater ratio (0.15): Very low. The transmission differential is functional, not performative. Parables genuinely compress epistemic content into portable units; the selection for compression is doing real cognitive work (enabling cross-disciplinary knowledge transfer within working memory constraints), not theater. Accessibility collapse (0.92): Very high. The constraint is accessible to all observers once articulated — the phenomenon is immediately recognizable across disciplines (everyone has experienced the asymmetry between parable propagation and technical framework propagation). Resistance (0.08): Very low. No plausible intervention changes the underlying cognitive architecture. Artificial intelligence, extended cognition tools, and institutional reforms can reduce friction but cannot eliminate the working memory bottleneck or the narrative schema advantage. The constraint emerges naturally from human cognitive limits and cultural evolution dynamics.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as mountain because the constraint is genuinely immutable across all observation contexts. The powerless researcher, the institutional actor, the organized consortium, the public intellectual, and the analytical observer all experience the same transmission barrier. There is no perspectival gap in classification type — the gap is in *response capacity*. The institutional actor can choose which parables to amplify; the public intellectual can create new parables; the organized consortium can invest in pedagogy. But none can override the underlying selection pressure. This uniform classification across perspectives is the diagnostic signature of a true natural law constraint, not a false summit. The constraint passes all mountain gates: emerges naturally (cognitive architecture + cultural evolution), accessibility collapse (immediately recognizable once articulated), low resistance (no intervention changes working memory limits), low extractiveness (memetic fitness is not rent extraction), low suppression (no active enforcement), low theater (functional compression, not performative).
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims in the structural sense. The transmission differential is not extraction — it is a neutral feature of the medium. All agents experience the same constraint: parables propagate, formal frameworks don't, regardless of the agent's power or exit options. The slight extractiveness (0.12) reflects memetic fitness advantage, not rent extraction. No directionality overrides are needed because no agent occupies a structurally asymmetric position relative to the constraint. The cognitive architecture applies universally.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CERTIFICATION: This constraint is a genuine natural law, not a naturalized institutional arrangement. The mandatrophy resolution is straightforward: the transmission differential is not mislabeled coordination (no coordination function exists — the constraint is a passive feature of cognitive architecture) and not mislabeled extraction (no rents are captured — memetic fitness is not extraction). The constraint is an emergent property of bounded rationality and cultural evolution, analogous to the speed of light or the halting problem. It is not contingent on institutional choices, policy frameworks, or power asymmetries. The analytical observer's mountain classification is not a false summit — it is the correct identification of a cognitive universal. The constraint's low extractiveness and suppression, combined with high accessibility collapse and low resistance, place it firmly in the mountain category from all perspectives. No agent perceives this as rope, snare, or tangled rope because no agent experiences coordination benefit, extractive harm, or mixed dynamics. The constraint is simply a limit on information propagation bandwidth through populations with working memory constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parable_as_transmission_layer, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parable_as_transmission_layer, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is not part of a decomposed family. It is a single structural claim: narrative compression determines cross-disciplinary propagation fitness. No alternative observable yields a different epsilon value because the constraint is defined precisely by the transmission differential itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
