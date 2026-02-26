% ============================================================================
% CONSTRAINT STORY: antikythera_planetary_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antikythera_planetary_model, []).

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
 *   constraint_id: antikythera_planetary_model
 *   human_readable: Antikythera Mechanism's Geocentric Planetary Model
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   The Antikythera Mechanism's planetary model represents a sophisticated
 *   technological embodiment of a scientific paradigm. As a constraint, it is
 *   not a rule or law, but a cognitive and technical framework for
 *   prediction. Its geocentric basis, while flawed, allowed for remarkably
 *   complex calculations. This created a powerful tool that simultaneously
 *   enabled astronomical prediction (a coordination function) while
 *   reinforcing a complex and ultimately incorrect model of the cosmos,
 *   thereby suppressing simpler alternatives (an extractive function). The
 *   history of this model, from its creation as a cutting-edge device to its
 *   ossification into dogma and eventual replacement, provides a rich example
 *   of how a single constraint can be perceived across all six DR categories
 *   depending on the observer's temporal and structural position.
 *
 * KEY AGENTS:
 *   - Ancient Astronomers: Primary beneficiaries (powerful/constrained) — gained unprecedented predictive capability.
 *   - Geocentric School Proponents: Institutional beneficiaries (institutional/constrained) — saw their worldview physically validated and reinforced.
 *   - Heliocentric Proponents: Primary victims (powerless/trapped) — their simpler model was suppressed by the technical and social weight of the geocentric paradigm.
 *   - Long-Term Astronomical Progress: Abstract victim (powerless/trapped) — encumbered by the complexity of epicycles for over a millennium.
 *   - Renaissance Astronomers: Organized agents (organized/mobile) — acted as the mechanism for the constraint's sunset clause.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antikythera_planetary_model, 0.35).
domain_priors:suppression_score(antikythera_planetary_model, 0.45).
domain_priors:theater_ratio(antikythera_planetary_model, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antikythera_planetary_model, extractiveness, 0.35).
narrative_ontology:constraint_metric(antikythera_planetary_model, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(antikythera_planetary_model, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antikythera_planetary_model, tangled_rope).
narrative_ontology:human_readable(antikythera_planetary_model, "Antikythera Mechanism's Geocentric Planetary Model").
narrative_ontology:topic_domain(antikythera_planetary_model, "technological/scientific").

domain_priors:requires_active_enforcement(antikythera_planetary_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antikythera_planetary_model, ancient_astronomers).
narrative_ontology:constraint_beneficiary(antikythera_planetary_model, geocentric_school_proponents).
narrative_ontology:constraint_victim(antikythera_planetary_model, heliocentric_proponents).
narrative_ontology:constraint_victim(antikythera_planetary_model, long_term_astronomical_progress).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANCIENT USER (ROPE) — For an astronomer or navigator in 100 BC, the mechanism is a pure coordination tool of immense power. It solves the problem of predicting celestial events with unprecedented accuracy. The underlying flaws of the model are invisible, as no better alternative exists. They are a beneficiary with no viable exit, experiencing it as pure utility.
constraint_indexing:constraint_classification(antikythera_planetary_model, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: SUPPRESSED HELIOCENTRIST (SNARE) — For a follower of Aristarchus's heliocentric ideas, the mechanism is a powerful snare. It physically embodies and reinforces the complex, incorrect geocentric model, making the simpler truth harder to argue for. They are trapped by the dominant paradigm, which this technology makes tangible and persuasive, extracting cognitive and social capital from any attempt to dissent.
constraint_indexing:constraint_classification(antikythera_planetary_model, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEDIEVAL SCHOLASTIC (PITON) — By the Middle Ages, the specific technology is lost, but the geocentric model it represents has become dogma (the Ptolemaic system). The model's function has atrophied from a dynamic computational system to an inertial belief system maintained by institutional authority. The theater of a divinely-ordered, Earth-centered cosmos is high, while the practical, predictive function is maintained by scholastic repetition, not innovation.
constraint_indexing:constraint_classification(antikythera_planetary_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: RENAISSANCE ASTRONOMER (SCAFFOLD) — For Copernicus or Galileo, the geocentric model is a temporary scaffold that supported astronomical prediction for centuries but is now obsolete and hindering progress. They are actively building its replacement, creating the 'sunset clause' through new observations and a more elegant mathematical framework. They see it as a system to be dismantled and replaced.
constraint_indexing:constraint_classification(antikythera_planetary_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The modern analyst sees the full structure. The model provided a genuine coordination function (predicting events) while also asymmetrically extracting cognitive effort and suppressing simpler alternatives. It required active enforcement by the scientific consensus of its time. This perspective recognizes both its utility and its long-term cost, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(antikythera_planetary_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: UNDERLYING PHYSICAL LAWS (MOUNTAIN) — This perspective distinguishes the model from the reality it describes. The actual laws of celestial mechanics are a Mountain. The Antikythera model is a human-constructed approximation of that Mountain. The model itself is a Tangled Rope, but it attempts to map an unchangeable physical reality. This highlights the difference between a constraint of physics and a constraint of knowledge.
constraint_indexing:constraint_classification(antikythera_planetary_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antikythera_planetary_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antikythera_planetary_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antikythera_planetary_model, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(antikythera_planetary_model, TR),
    TR >= 0.70.

:- end_tests(antikythera_planetary_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Represents the 'cognitive tax' of the geocentric model. To achieve predictive accuracy, users were forced into the complex world of epicycles, a significant expenditure of intellectual effort compared to a heliocentric model. Suppression (0.45): The existence of such a complex, functional device created a high barrier to entry for alternative theories. It wasn't just an idea; it was a working machine that 'proved' the paradigm, making dissent difficult. Theater Ratio (0.60): The mechanism was both a functional calculator and a stunning piece of scientific theater, demonstrating the power and correctness of the Greek cosmological view. Over time, as the model became dogma, its theatrical/symbolic value began to outweigh its innovative function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is primarily temporal and epistemological. For the initial user, it's a pure Rope; for the dissenter, a Snare. For the inheritor of the tradition, it becomes an inertial Piton. For the revolutionary who dismantles it, it was a temporary Scaffold. The modern analyst, with full hindsight, sees the combination of coordination and extraction and classifies it as a Tangled Rope. The underlying physics it attempts to model remains a Mountain. This demonstrates how a constraint's classification evolves as knowledge and power dynamics shift around it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ancient astronomers) saw a powerful tool that solved their problems, hence they experienced it as coordination (Rope). Victims (heliocentrists, future progress) were structurally trapped by a paradigm that extracted cognitive effort and delayed a simpler truth (Snare). The analytical view balances these, recognizing the genuine coordination function alongside the suppressive/extractive element, leading to the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a strong resolver of mandatrophy. It shows that a single object can be correctly classified in multiple ways without contradiction. The error is to insist on a single 'true' classification. The Antikythera model *was* a Rope for its users, and it *was* a Snare for its opponents, and it *did* become a Piton. Deferential Realism correctly models this by indexing classification to the observer's structural position, capturing the complete, multi-faceted nature of the constraint's lifecycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_predictive_accuracy,
    'What was the real-world predictive accuracy of the mechanism, accounting for manufacturing tolerances and potential jamming?',
    'Physical reconstruction and long-term simulation of the gear train based on modern imaging, testing its error accumulation over decades.',
    'High accuracy would strengthen its classification as a Rope from the user perspective. Low accuracy would increase its theater_ratio, pushing it towards a Piton, suggesting its primary function was demonstrative rather than practical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_predictive_accuracy, empirical, 'The true predictive accuracy of the Antikythera Mechanism').

omega_variable(
    cultural_suppression_level,
    'To what extent was the existence of this device and the geocentric model used to actively suppress heliocentric thought?',
    'Textual analysis of philosophical and astronomical texts from the period for evidence of debate and dismissal of alternative cosmological models.',
    'Strong evidence of active suppression would increase the ''suppression'' metric, reinforcing the Snare perspective. A lack of evidence would suggest passive dominance, lowering suppression and favoring the Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_suppression_level, empirical, 'Level of active suppression of alternative cosmological models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antikythera_planetary_model, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antikythera_planetary_model, theater_ratio, 0, 0.3).
narrative_ontology:measurement(anti_tr_t900, antikythera_planetary_model, theater_ratio, 900, 0.75).
narrative_ontology:measurement(anti_tr_t1800, antikythera_planetary_model, theater_ratio, 1800, 0.6).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antikythera_planetary_model, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t900, antikythera_planetary_model, base_extractiveness, 900, 0.4).
narrative_ontology:measurement(anti_be_t1800, antikythera_planetary_model, base_extractiveness, 1800, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antikythera_planetary_model, information_standard).
narrative_ontology:affects_constraint(antikythera_planetary_model, ptolemaic_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
