% ============================================================================
% CONSTRAINT STORY: scientific_paradigm_lifecycle
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_scientific_paradigm_lifecycle, []).

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: scientific_paradigm_lifecycle
 *   human_readable: The Crisis of a Scientific Paradigm
 *   domain: scientific/sociological
 *
 * SUMMARY:
 *   Models the "crisis" phase of a dominant scientific paradigm, following the
 *   structure described by Thomas Kuhn. A paradigm that was once a powerful
 *   coordination tool (Rope) now faces accumulating anomalies. The establishment
 *   defends it through institutional inertia (Piton), suppressing dissent and
 *   creating a conflict (Tangled Rope) that is experienced as a Snare by those
 *   with contradictory findings. This story snapshots the paradigm at its peak
 *   of crisis and internal conflict.
 *
 * KEY AGENTS (by structural relationship):
 *   - Junior Researchers / Anomaly Discoverers: Primary targets (powerless/trapped) — their careers and findings are suppressed by the establishment.
 *   - "Normal Scientists": Beneficiaries of the paradigm's remaining puzzle-solving framework (moderate/mobile).
 *   - The Scientific Establishment: The institutional beneficiary, defending the paradigm's inertia (institutional/arbitrage).
 *   - A Historian of Science: The analytical observer seeing the full conflict structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS) - Represents T=13 (Peak Crisis)
   ========================================================================== */

% --- Numerical metrics (Peak Crisis State) ---
domain_priors:base_extractiveness(scientific_paradigm_lifecycle, 0.48). % High professional cost of dissent.
domain_priors:suppression_score(scientific_paradigm_lifecycle, 0.75).   % Active suppression of anomalous results.
domain_priors:theater_ratio(scientific_paradigm_lifecycle, 0.75).       % High performative defense of the paradigm.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, extractiveness, 0.48).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, theater_ratio, 0.75).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(scientific_paradigm_lifecycle, tangled_rope).
narrative_ontology:human_readable(scientific_paradigm_lifecycle, "The Crisis of a Scientific Paradigm").

% --- Binary flags ---
domain_priors:requires_active_enforcement(scientific_paradigm_lifecycle). % Peer review, funding allocation, tenure committees.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
narrative_ontology:constraint_beneficiary(scientific_paradigm_lifecycle, scientific_establishment).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, anomaly_discoverers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ANOMALY DISCOVERER (SNARE)
% For a junior researcher or graduate student whose findings contradict the
% paradigm, the system is a Snare. It extracts their labor and career prospects,
% suppresses their work, and offers no path to success. They are trapped.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: A "NORMAL SCIENTIST" WORKING WITHIN THE PARADIGM (ROPE)
% For a tenured scientist whose career is built on the dominant paradigm, it
% remains a useful Rope for generating puzzles and publications, even as the
% crisis brews. As a beneficiary, their derived directionality `d` is low,
% leading to a low effective extraction `χ`.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: A SCIENTIST DEFENDING THE OLD PARADIGM (PITON)
% For the establishment figure, the accumulation of anomalies has turned the
% paradigm into a Piton. Its primary function is no longer discovery, but
% self-preservation through institutional defense and high "theater"
% (conferences, keynote speeches, editorial control).
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: A HISTORIAN OF SCIENCE (TANGLED ROPE)
% The analytical observer sees the entire crisis, recognizing it as a Tangled
% Rope: a conflict between an old, extractive establishment (Piton/Snare aspects)
% and the remaining coordination function for normal science (Rope aspect).
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scientific_paradigm_lifecycle_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless victim and the moderate beneficiary.
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, rope, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, piton, context(agent_power(institutional), _, _, _)).

test(crisis_state_metrics_are_high) :-
    narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, extractiveness, E),
    narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, suppression_requirement, S),
    narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, theater_ratio, T),
    E >= 0.46,
    S >= 0.60,
    T >= 0.70.

:- end_tests(scientific_paradigm_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story models the Kuhnian cycle of science by focusing on the "peak
 *   crisis" phase, where the perspectival gaps are widest. The base properties
 *   (ε=0.48, S=0.75, T=0.75) are set to this moment of maximum conflict.
 *   - The high suppression (0.75) and extraction (0.48) reflect the professional
 *     cost imposed on dissenters, making it a Snare for them.
 *   - The high theater (0.75) reflects the establishment's defensive posture,
 *     making it a Piton from their perspective.
 *   - The system still has a coordination function for "normal scientists,"
 *     who are beneficiaries, allowing it to appear as a Rope to them.
 *   - The combination of all these functions makes it a canonical Tangled Rope
 *     from an analytical view.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. The powerless researcher sees a career-destroying Snare.
 *   The tenured insider still sees a productive Rope. The institutional leader
 *   sees a legacy to be defended (Piton). This divergence is the hallmark of a
 *   paradigm crisis. The directionality derivation `d` is key: for the
 *   beneficiary, `d` is low, suppressing the high base extraction `ε` into a
 *   low effective extraction `χ`. For the victim, `d` is high, amplifying `ε`
 *   into a high `χ`.
 *
 * MANDATROPHY ANALYSIS:
 *   This model avoids mislabeling the crisis. A naive analysis might see only
 *   the suppression and call it a pure Snare, ignoring the real coordination
 *   that still occurs. Another might see only the coordination and call it a
 *   Rope, ignoring the victims. The Tangled Rope classification correctly
 *   identifies that both are happening simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_paradigm_incommensurability,
    'Are competing paradigms truly incommensurable (a Mountain between them), or is there always a logical bridge (a Rope) that can be found?',
    'Formal analysis of the logical structure of historical paradigm shifts (e.g., Newtonian to Einsteinian physics).',
    'If incommensurable, science is not purely rational but involves "conversion." If commensurable, paradigm shifts are exercises in logic and evidence.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scientific_paradigm_lifecycle, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the paradigm's decay from a functional Rope into a
% crisis-ridden Tangled Rope. The interval ends at the peak crisis (T=13).

% T=0: Normal Science (Healthy Rope)
narrative_ontology:measurement(sci_ex_t0, scientific_paradigm_lifecycle, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sci_tr_t0, scientific_paradigm_lifecycle, theater_ratio, 0, 0.05).

% T=8: Anomaly & Inertia (Piton phase begins)
narrative_ontology:measurement(sci_ex_t8, scientific_paradigm_lifecycle, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(sci_tr_t8, scientific_paradigm_lifecycle, theater_ratio, 8, 0.70).

% T=13: Peak Crisis (Tangled Rope / Snare) - Final values match base properties
narrative_ontology:measurement(sci_ex_t13, scientific_paradigm_lifecycle, base_extractiveness, 13, 0.48).
narrative_ontology:measurement(sci_tr_t13, scientific_paradigm_lifecycle, theater_ratio, 13, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scientific_paradigm_lifecycle, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */