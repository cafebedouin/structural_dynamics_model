% ============================================================================
% CONSTRAINT STORY: delta_force_selection_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_delta_force_selection_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: delta_force_selection_2026
 * human_readable: Delta Force (1st SFOD-D) Selection & Assessment
 * domain: military/special_operations
 * * SUMMARY:
 * This constraint models the extreme exclusionary pressure of the 1st Special
 * Forces Operational Detachment-Delta selection process. With an attrition
 * rate averaging 90%, the process extracts maximum physiological and
 * psychological output from candidates to identify "absolute reliability".
 * * KEY AGENTS:
 * - [Candidates]: Subject (Powerless) - Elite soldiers from Rangers/SF
 * attempting to endure the "Long Walk".
 * - [1st SFOD-D Command]: Beneficiary (Institutional) - The unit maintains
 * its Tier 1 lethality through this high-extraction filter.
 * - [Unit Psychologists]: Auditor (Analytical) - Evaluate character and
 * mental resilience.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extremely high extraction (0.92) due to the intentional "breakdown" of
% candidates, reflecting the ~90% attrition rate.
domain_priors:base_extractiveness(delta_force_selection_2026, 0.92).

% High suppression (0.85); candidates are "kept in the dark" and forbidden
% from speaking to assess individual grit.
domain_priors:suppression_score(delta_force_selection_2026, 0.85).

% High theater ratio (0.75); instructors (cadre) maintain a performative
% silence/anonymity to increase psychological stress.
domain_priors:theater_ratio(delta_force_selection_2026, 0.75).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(delta_force_selection_2026, extractiveness, 0.92).
narrative_ontology:constraint_metric(delta_force_selection_2026, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(delta_force_selection_2026, theater_ratio, 0.75).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination mechanism (a filter) but operates via pure enforcement.
narrative_ontology:constraint_claim(delta_force_selection_2026, tangled_rope).
narrative_ontology:human_readable(delta_force_selection_2026, "Delta Force (1st SFOD-D) Selection & Assessment").

% Binary flags
domain_priors:requires_active_enforcement(delta_force_selection_2026). % Required for Tangled Rope

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(delta_force_selection_2026, '1st_sfod_d_command').
narrative_ontology:constraint_victim(delta_force_selection_2026, 'selection_candidates').

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the candidate, selection is a snare: a trap of physical punishment
% and psychological "darkness" where the only exit is to quit.
% χ = 0.92 * 1.5 (powerless) * 0.9 (regional) = 1.242.
constraint_indexing:constraint_classification(delta_force_selection_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the Institutional Command, the process is a rope: it coordinates
% human talent into a functional, ultra-reliable unit.
% χ = 0.92 * -0.2 (institutional) * 1.2 (global) = -0.22.
constraint_indexing:constraint_classification(delta_force_selection_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the genuine coordination function (finding elite
% operators) and the asymmetric, brutal extraction required to achieve it.
% The high extraction (0.92) and active enforcement make it a Tangled Rope.
% χ = 0.92 * 1.15 (analytical) * 1.2 (global) = 1.2696.
constraint_indexing:constraint_classification(delta_force_selection_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(delta_force_selection_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(delta_force_selection_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(delta_force_selection_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(delta_force_selection_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_analytical_view) :-
    % The analytical observer must classify this as a Tangled Rope.
    constraint_indexing:constraint_classification(delta_force_selection_2026, tangled_rope,
        context(agent_power(analytical),
                time_horizon(civilizational),
                exit_options(analytical),
                spatial_scope(global))).

test(extraction_threshold) :-
    narrative_ontology:constraint_metric(delta_force_selection_2026, extractiveness, E),
    E > 0.46. % Correctly identifies high-extraction exclusionary filters.

:- end_tests(delta_force_selection_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.92) is anchored in the 90% attrition rate—a process
 * where the "input" (elite soldiers) is almost entirely "consumed" (extracted)
 * to find a tiny fraction of "output". The theater_ratio (0.75) is high because
 * the cadre intentionally uses performative "grey-man" tactics to simulate an
 * environment of ambiguity and unreliability, testing for the candidate's
 * internal drive. The original classification of Piton for the analytical view
 * was incorrect; a Piton must have low base extraction (<= 0.10). This process
 * is brutally functional, not inertial, making Tangled Rope the correct
 * classification.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system avoids Mandatrophy by classifying this as a Tangled Rope. A naive
 * analysis would see the extreme extraction (0.92) and suppression (0.85) and
 * label it a pure Snare. However, the Tangled Rope classification correctly
 * acknowledges that this brutal extraction is coupled to a genuine, non-trivial
 * coordination function: identifying individuals with the psychological and
 * physical resilience required for Tier 1 operations. The extraction is not
 * arbitrary; it is the filtering mechanism itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_delta_selection_2026,
    'Is mental toughness a trainable Scaffold or a fixed Mountain of personality?',
    'Analysis of successful selection candidates over 20 years vs. early-life resilience markers.',
    'If trainable, the process is a Scaffold; if genetic/formative, it is a Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(delta_force_selection_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the selection window from T=0 (Initial Screening) to T=10 (Final Board).
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time: Cadre silence (theater) increases as candidates enter
% the stress phase and "Long Walk".
narrative_ontology:measurement(delta_force_selection_2026_tr_t0, delta_force_selection_2026, theater_ratio, 0, 0.40).
narrative_ontology:measurement(delta_force_selection_2026_tr_t5, delta_force_selection_2026, theater_ratio, 5, 0.65).
narrative_ontology:measurement(delta_force_selection_2026_tr_t10, delta_force_selection_2026, theater_ratio, 10, 0.75).

% Extraction over time: Physiological extraction peaks during the 40-mile "Long Walk"
% and psychological extraction peaks at the Commander's Board.
narrative_ontology:measurement(delta_force_selection_2026_ex_t0, delta_force_selection_2026, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(delta_force_selection_2026_ex_t5, delta_force_selection_2026, base_extractiveness, 5, 0.88).
narrative_ontology:measurement(delta_force_selection_2026_ex_t10, delta_force_selection_2026, base_extractiveness, 10, 0.92).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The selection process is a mechanism for allocating scarce human capital.
narrative_ontology:coordination_type(delta_force_selection_2026, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */