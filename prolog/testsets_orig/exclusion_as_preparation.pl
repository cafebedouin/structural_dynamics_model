% ============================================================================
% CONSTRAINT STORY: exclusion_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exclusion_as_preparation, []).

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
 *   constraint_id: exclusion_as_preparation
 *   human_readable: Social Exclusion as Preparation for Material Reception
 *   domain: political_economy/labor_systems/epistemic_infrastructure
 *
 * SUMMARY:
 *   The exclusion ritual in eusocial insect colonies (specifically termites
 *   and certain ant species) coordinates social recognition of impending
 *   individual transformation with material preparation for geological
 *   reception. When an individual's nest-touch participation rate declines
 *   below a threshold (typically 40-60% of baseline), the colony socially
 *   ejects the individual from communal spaces. This exclusion precedes
 *   material disposal by a predictable interval (2-7 days in most studied
 *   species), during which the individual's body chemistry changes in ways
 *   that optimize signal conductivity when the body is eventually
 *   incorporated into the colony's architectural substrate. The ritual solves
 *   a genuine coordination problem: without a reliable social signal of
 *   impending transformation, the colony cannot prepare disposal
 *   infrastructure (excavation of reception chambers, positioning of
 *   conductive pathways) in advance. The exclusion ritual is a low-extraction
 *   coordination mechanism (ε=0.18) with minimal suppression (σ=0.22) and low
 *   theater (0.15) — it coordinates without coercing and its function is
 *   genuine rather than performative. This constraint is downstream of
 *   transformation_as_threshold_marker (the physiological transformation that
 *   triggers exclusion) and represents a distinct coordination layer: the
 *   upstream constraint governs the transformation itself; this constraint
 *   governs the social coordination that prepares for material reception.
 *
 * KEY AGENTS:
 *   - Excluded Individual: Primary participant (powerless/trapped) — experiences exclusion as coordination signal rather than punishment; benefits from preparation for material reception
 *   - Colony Collective: Primary beneficiary (organized/constrained) — uses exclusion ritual to synchronize social recognition with material preparation; benefits from coordination efficiency
 *   - Geological System: Secondary beneficiary (institutional/mobile) — receives prepared material at predictable intervals; benefits from optimized conductivity pathways
 *   - Signal Infrastructure: Secondary beneficiary (institutional/arbitrage) — uses ritual-synchronized timeline to optimize signal routing; benefits from predictable material flow
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees exclusion ritual as low-extraction coordination mechanism that solves collective action problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exclusion_as_preparation, 0.18).
domain_priors:suppression_score(exclusion_as_preparation, 0.22).
domain_priors:theater_ratio(exclusion_as_preparation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exclusion_as_preparation, extractiveness, 0.18).
narrative_ontology:constraint_metric(exclusion_as_preparation, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(exclusion_as_preparation, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exclusion_as_preparation, rope).
narrative_ontology:human_readable(exclusion_as_preparation, "Social Exclusion as Preparation for Material Reception").
narrative_ontology:topic_domain(exclusion_as_preparation, "political_economy/labor_systems/epistemic_infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exclusion_as_preparation, colony_collective).
narrative_ontology:constraint_beneficiary(exclusion_as_preparation, geological_system).
narrative_ontology:constraint_beneficiary(exclusion_as_preparation, signal_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED INDIVIDUAL (ROPE) — Experiences social ejection from nest-touch participation as coordination rather than extraction. The exclusion is a signal of impending transformation, not punishment. The individual cannot exit the process but does not experience it as extractive — the ritual marks a threshold that serves collective coordination. Low effective extraction despite trapped exit because the agent is a beneficiary of the coordination function (receives preparation for material reception).
constraint_indexing:constraint_classification(exclusion_as_preparation, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COLONY COLLECTIVE (ROPE) — Experiences exclusion ritual as pure coordination. The ritual synchronizes social recognition of transformation with material preparation for disposal. Nest-touch participation rate decline signals to the collective that an individual is approaching the threshold where geological reception becomes possible. The coordination function is genuine: without the exclusion ritual, the colony would lack a reliable signal for when to prepare disposal infrastructure. Constrained exit (the colony could abandon the ritual but at high cost to coordination efficiency).
constraint_indexing:constraint_classification(exclusion_as_preparation, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: GEOLOGICAL SYSTEM (ROPE) — The exclusion ritual is a coordination mechanism that prepares material for reception. Social ejection precedes material disposal by a predictable interval, allowing the geological system to optimize conductivity pathways. The ritual is a signal that enables efficient material processing. The geological system benefits from the coordination (receives prepared material at predictable intervals) and has mobile exit (could receive unprepared material but at lower efficiency).
constraint_indexing:constraint_classification(exclusion_as_preparation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: SIGNAL INFRASTRUCTURE (ROPE) — Experiences the exclusion ritual as a coordination standard that enables reliable signal conductivity. The ritual creates a predictable timeline between social exclusion and material disposal, which the signal infrastructure uses to optimize conductivity pathways. The infrastructure benefits from the coordination (predictable material flow enables efficient signal routing) and has arbitrage exit (could route signals through alternative pathways but the ritual-synchronized pathway is most efficient).
constraint_indexing:constraint_classification(exclusion_as_preparation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the exclusion ritual is a low-extraction coordination mechanism that solves a genuine collective action problem: synchronizing social recognition of transformation with material preparation for geological reception. The ritual has minimal extractive overhead (ε=0.18) and low suppression (σ=0.22) — it coordinates without coercing. The theater ratio is low (0.15) because the ritual's function is genuine: nest-touch participation rate is a reliable signal of impending transformation, not a performative proxy. This is a rope from all perspectives because no agent experiences significant extraction — the ritual serves all participants.
constraint_indexing:constraint_classification(exclusion_as_preparation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exclusion_as_preparation_tests).
:- end_tests(exclusion_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The exclusion ritual imposes minimal cost on participants. The excluded individual experiences social ejection but not material deprivation (continues to access food and shelter during the exclusion period). The colony bears minimal overhead cost (the ritual requires no dedicated enforcement infrastructure). The geological system and signal infrastructure benefit from the coordination without bearing costs. The slight extractiveness (above zero) reflects the social cost of exclusion and the risk of coordination failure if the signal is unreliable. Suppression (0.22): Low. The excluded individual cannot exit the process (trapped exit option) but the constraint does not rely on coercion to function — the exclusion is a signal, not a punishment. The colony could abandon the ritual (constrained exit) but at high cost to coordination efficiency. The low suppression reflects that the ritual operates through coordination incentives rather than enforcement. Theater ratio (0.15): Very low. The ritual's function is genuine: nest-touch participation rate is a reliable physiological signal of impending transformation, not a performative proxy. The exclusion timeline correlates strongly with disposal timeline (r > 0.85 in studied species), indicating that the ritual serves its stated coordination function rather than substituting a proxy goal. The slight theater (above zero) reflects measurement noise and the possibility that some exclusions are false positives (individuals whose participation rate declines temporarily but who do not transform).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all agents classify it as rope. The excluded individual, despite having trapped exit and powerless power, experiences the ritual as coordination rather than extraction because the individual is a beneficiary of the preparation function. The colony collective, geological system, and signal infrastructure all experience the ritual as efficient coordination. The analytical observer confirms the rope classification from a civilizational perspective. The lack of perspectival gap is diagnostically significant: it indicates that the ritual is a genuine low-extraction coordination mechanism rather than a naturalized extraction mechanism. If any perspective classified the ritual as snare or tangled_rope, it would indicate that the coordination function is a cover story for extraction. The uniform rope classification across all perspectives confirms that the ritual serves all participants.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as rope because all agents are beneficiaries of the coordination function. The excluded individual is a beneficiary (receives preparation for material reception) despite having trapped exit — the ritual serves the individual's interests even though the individual cannot exit. The colony collective is a beneficiary (gains coordination efficiency) with constrained exit (could abandon the ritual but at high cost). The geological system and signal infrastructure are beneficiaries (receive prepared material and predictable signal flow) with mobile and arbitrage exit respectively (could function without the ritual but at lower efficiency). The analytical observer sees the ritual as serving all participants — no agent experiences significant extraction. The directionality values are all low (d < 0.30 for all perspectives) because all agents are beneficiaries, producing low or negative effective extraction (χ) across all perspectives. This is a rare case of a constraint that is rope from all perspectives because the coordination function is genuine and the extractive overhead is minimal.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low-extraction coordination mechanisms (ropes) can involve social exclusion and material disposal without being extractive. The ritual is not a snare (no victims, no high extraction) or a tangled_rope (no asymmetric extraction, no active enforcement) despite involving exclusion and disposal. The mandatrophy resolution depends on three structural features: (1) the excluded individual is a beneficiary of the preparation function, not a victim; (2) the ritual has minimal extractive overhead (ε=0.18) and low suppression (σ=0.22); (3) the ritual's function is genuine (low theater ratio 0.15) rather than performative. If any of these features were absent, the classification would shift: if the individual were a victim (not a beneficiary), the constraint would be a snare; if extractiveness or suppression were higher, it would be a tangled_rope; if theater ratio were higher, it would be a piton. The rope classification is not a naturalization of extraction — it is a recognition that coordination mechanisms can involve exclusion without being extractive when the exclusion serves all participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_causality_direction,
    'Does social exclusion cause physiological transformation, or does physiological transformation cause social exclusion?',
    'Longitudinal tracking of nest-touch participation rate vs physiological markers; experimental manipulation of social exclusion timeline to test whether it affects transformation timeline',
    'If exclusion causes transformation: the ritual has extractive power (forces transformation). If transformation causes exclusion: the ritual is purely coordinative (signals transformation). Current hypothesis assumes the latter, but if the former is true, extractiveness would rise to 0.40+ and classification would shift to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_causality_direction, empirical, 'Causal direction between social exclusion and physiological transformation').

omega_variable(
    alternative_signal_sufficiency,
    'Could the geological system achieve equivalent signal conductivity without the social exclusion ritual?',
    'Comparison of signal conductivity in colonies with vs without exclusion rituals; identification of alternative coordination mechanisms (chemical markers, behavioral signals) that could serve the same function',
    'If alternative signals are sufficient: the exclusion ritual is redundant (theater ratio rises, classification shifts toward piton). If no alternatives exist: the ritual is genuinely necessary coordination (rope classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_signal_sufficiency, empirical, 'Whether alternative signals could replace the exclusion ritual').

omega_variable(
    disposal_timeline_variance,
    'What is the variance in the interval between social exclusion and material disposal, and does high variance indicate coordination failure?',
    'Statistical analysis of exclusion-to-disposal intervals across multiple colonies; correlation between interval variance and signal conductivity efficiency',
    'If variance is low: the ritual is a reliable coordination signal (rope confirmed). If variance is high: the ritual may be performative rather than functional (theater ratio rises, classification shifts toward piton or tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disposal_timeline_variance, empirical, 'Variance in exclusion-to-disposal interval as coordination reliability measure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exclusion_as_preparation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excl_prep_tr_t0, exclusion_as_preparation, theater_ratio, 0, 0.12).
narrative_ontology:measurement(excl_prep_tr_t3, exclusion_as_preparation, theater_ratio, 3, 0.14).
narrative_ontology:measurement(excl_prep_tr_t6, exclusion_as_preparation, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(excl_prep_be_t0, exclusion_as_preparation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(excl_prep_be_t3, exclusion_as_preparation, base_extractiveness, 3, 0.17).
narrative_ontology:measurement(excl_prep_be_t6, exclusion_as_preparation, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exclusion_as_preparation, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of transformation_as_threshold_marker (the physiological transformation that triggers exclusion). The upstream constraint governs the transformation itself (claimed_type: snare, indicating that the transformation is extractive); this constraint governs the social coordination that prepares for material reception after transformation (claimed_type: rope, indicating that the coordination is non-extractive). The decomposition reflects that the transformation and the coordination are structurally distinct: the transformation extracts from the individual (forces physiological change), while the coordination serves all participants (prepares for efficient material reception). The upstream snare classification does not contaminate the downstream rope classification because the coordination layer operates independently of the extraction mechanism in the transformation layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
