% ============================================================================
% CONSTRAINT STORY: stoic_logos_governance
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_stoic_logos_governance, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: stoic_logos_governance
 * human_readable: The Stoic Logos as an Imperial Governance Framework
 * domain: philosophical/political
 * * SUMMARY:
 * The Stoic framework, as practiced by Emperor Marcus Aurelius, represents a
 * constraint where the ruler submits his personal will to the Logos (divine
 * reason/natural law) for the good of the state. This system coordinates the
 * Pax Romana but extracts extreme personal autonomy and vitality from the ruler,
 * functioning as a self-imposed governance mechanism.
 * * KEY AGENTS:
 * - Marcus Aurelius: The Emperor, subject to the constraint (Institutional Power, Trapped Exit).
 * - Roman Citizen: Beneficiary of stability, but subject to imperial law (Powerless).
 * - The Roman State: The abstract entity benefiting from the coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(stoic_logos_governance, 0.75). % Snare extraction >= 0.46
domain_priors:suppression_score(stoic_logos_governance, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(stoic_logos_governance, 0.10).       % Low theater; this is a deeply internalized belief system.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(stoic_logos_governance, extractiveness, 0.75).
narrative_ontology:constraint_metric(stoic_logos_governance, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(stoic_logos_governance, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Stoicism claims to be alignment with universal reason, i.e., natural law.
narrative_ontology:constraint_claim(stoic_logos_governance, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(stoic_logos_governance). % Requires constant self-discipline and state power.

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(stoic_logos_governance, roman_state). % Derives has_coordination_function/1
narrative_ontology:constraint_victim(stoic_logos_governance, marcus_aurelius_personal_will). % Derives has_asymmetric_extraction/1

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE EMPEROR (INSTITUTIONAL, TRAPPED) - SNARE
% Despite holding supreme power, Marcus Aurelius is personally trapped by his
% duty to the Logos. The framework extracts his personal autonomy and vitality
% for the sake of the state, making it a Snare from his perspective.
constraint_indexing:constraint_classification(stoic_logos_governance, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CITIZEN (POWERLESS, TRAPPED) - MOUNTAIN
% To a subject of the Roman Empire, the Emperor's rule, the laws, and the
% cosmic order (Logos) are perceived as an unchangeable, natural reality.
% They are fixed features of the world, hence a Mountain.
constraint_indexing:constraint_classification(stoic_logos_governance, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER - TANGLED ROPE
% An analyst sees both the massive coordination function (maintaining the Pax
% Romana) and the severe, asymmetric extraction of the ruler's autonomy. It
% requires active enforcement (self-discipline, state power) and is not a
% natural law. This dual nature defines it as a Tangled Rope.
constraint_indexing:constraint_classification(stoic_logos_governance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stoic_logos_governance_tests).

test(perspectival_gap_ruler_vs_subject) :-
    constraint_indexing:constraint_classification(stoic_logos_governance, TypeRuler, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(stoic_logos_governance, TypeSubject, context(agent_power(powerless), _, _, _)),
    assertion(TypeRuler == snare),
    assertion(TypeSubject == mountain),
    TypeRuler \= TypeSubject.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(stoic_logos_governance, tangled_rope, context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression_thresholds) :-
    narrative_ontology:constraint_metric(stoic_logos_governance, extractiveness, E),
    narrative_ontology:constraint_metric(stoic_logos_governance, suppression_requirement, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(stoic_logos_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system of immense power and cost. The base_extractiveness
 * (0.75) is high because the constraint demands the total sublimation of the
 * ruler's personal will and desires to the abstract 'good of the state'. The
 * suppression_score (0.80) is also high because viable alternatives for a ruler
 * (e.g., tyranny, Epicurean detachment) are philosophically and politically
 * suppressed as paths to ruin.
 *
 * The Perspectival Gap is stark: The Emperor (institutional) experiences this
 * as a personal 'Snare', a duty from which there is no escape that consumes his
 * life. The citizen (powerless) experiences the result as a 'Mountain'—the

 * stability of the Empire is a seemingly permanent and unchangeable feature
 * of their world.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case for Mandatrophy resolution. A naive
 * analysis focusing only on the high extraction (0.75) and suppression (0.80)
 * would classify it as a pure Snare. However, this would ignore its profound
 * and effective coordination function: the maintenance of the Pax Romana, a
 * system of law and stability for millions. The 'Tangled Rope' classification
 * correctly captures this duality. It acknowledges that the system is both a
 * genuine coordination mechanism (benefiting the 'roman_state') and a source
 * of severe, asymmetric extraction (cost borne by 'marcus_aurelius_personal_will').
 * This prevents the system from incorrectly dismissing a functional, albeit costly,
 * governance model as pure predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_stoic_logos_governance_1,
    'Is the high extraction a functional necessity for Roman stability, or a self-imposed psychological snare by Marcus?',
    'Comparative analysis of Roman stability and imperial welfare under Stoic vs. non-Stoic "good" emperors.',
    'If necessity -> closer to Mountain properties. If choice -> confirms Snare/Tangled Rope properties.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(stoic_logos_governance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This was a mature, stable philosophical system during the interval of
% Marcus Aurelius's reign. The measurements are flat, indicating no significant
% drift in its core properties during this period.
%
% Theater ratio over time:
narrative_ontology:measurement(stoic_logos_tr_t0, stoic_logos_governance, theater_ratio, 0, 0.10).
narrative_ontology:measurement(stoic_logos_tr_t5, stoic_logos_governance, theater_ratio, 5, 0.10).
narrative_ontology:measurement(stoic_logos_tr_t10, stoic_logos_governance, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(stoic_logos_ex_t0, stoic_logos_governance, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(stoic_logos_ex_t5, stoic_logos_governance, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(stoic_logos_ex_t10, stoic_logos_governance, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The Stoic framework acts as a moral and ethical enforcement mechanism for
% imperial policy and personal conduct.
narrative_ontology:coordination_type(stoic_logos_governance, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */