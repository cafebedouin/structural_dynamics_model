% ============================================================================
% CONSTRAINT STORY: civilizational_lifecycle_solara
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-14
% ============================================================================

:- module(constraint_civilizational_lifecycle_solara, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: civilizational_lifecycle_solara
 *   human_readable: The Lifecycle of Solaran Civilization
 *   domain: social/political
 *
 * SUMMARY:
 *   Models the complete lifecycle of a fictional civilization, Solara. The
 *   "social contract" constraint begins as a pure coordination mechanism for
 *   survival, evolves through institutional growth and sclerosis, and finally
 *   decays into a purely extractive system prior to collapse. This story is
 *   designed to be temporally deep to test drift and fixed-point analysis.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Founders: Early-stage beneficiaries (institutional/mobile) — benefit from coordination.
 *   - The Citizenry: Mid-to-late stage targets (powerless/constrained) — bear the costs of complexity and extraction.
 *   - The High Council: Late-stage beneficiaries (institutional/arbitrage) — benefit from extraction.
 *   - The Historian: Analytical observer — sees the full structure across time.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS) - Represents T=20 (End State)
   ========================================================================== */

% --- Numerical metrics (End State: Collapse) ---
domain_priors:base_extractiveness(civilizational_lifecycle_solara, 0.85). % Near total extraction
domain_priors:suppression_score(civilizational_lifecycle_solara, 0.90).   % Alternatives are brutally suppressed.
domain_priors:theater_ratio(civilizational_lifecycle_solara, 0.80).       % High ritual, low function.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, extractiveness, 0.85).
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, theater_ratio, 0.80).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(civilizational_lifecycle_solara, tangled_rope).
narrative_ontology:human_readable(civilizational_lifecycle_solara, "The Lifecycle of Solaran Civilization").
narrative_ontology:topic_domain(civilizational_lifecycle_solara, "social/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(civilizational_lifecycle_solara). % The late-stage system requires a vast apparatus.
narrative_ontology:has_sunset_clause(civilizational_lifecycle_solara). % The early scaffold had one, but it's long since ignored.


% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(civilizational_lifecycle_solara, solaran_high_council).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, solaran_citizenry).


/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LATE-STAGE CITIZEN / DISSIDENT (SNARE)
% For a citizen at the end of the lifecycle, the social contract is a trap
% that extracts all value and punishes dissent.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE HIGH COUNCIL (ROPE)
% For the late-stage elite, the system is a perfectly functioning rope for
% coordinating wealth extraction and maintaining power. The negative chi value
% reflects their position as net beneficiaries.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE HISTORIAN (TANGLED ROPE)
% The analytical observer sees the full picture: a system that still has
% some coordination function (it runs, barely) but is overwhelmingly
% extractive and coercive. This matches the constraint_claim.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_lifecycle_solara_tests).

test(perspectival_gap_deep) :-
    % Verify the massive gap between the Dissident (Snare) and the High Council (Rope).
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, tangled_rope, context(agent_power(analytical), _, _, _)).

test(end_state_is_high_extraction) :-
    narrative_ontology:constraint_metric(civilizational_lifecycle_solara, extractiveness, E),
    E > 0.8.

test(end_state_is_high_suppression) :-
    narrative_ontology:constraint_metric(civilizational_lifecycle_solara, suppression_requirement, S),
    S > 0.8.

:- end_tests(civilizational_lifecycle_solara_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story uses a 20-step lifecycle to model civilizational drift.
 *   - It begins as a low-extraction ROPE (T=0-3).
 *   - Becomes a SCAFFOLD as institutions are built (T=4-8).
 *   - Degrades into a PITON as institutions lose function but persist (T=9-13), marked by rising theater.
 *   - Morphs into a TANGLED_ROPE as complexity is exploited for asymmetric gain (T=14-17).
 *   - Ends as a high-extraction SNARE from the citizen's perspective (T=18-20).
 *   The base properties reflect the final, collapsed state at T=20.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal at the end. The High Council experiences the system as a negative-chi Rope, a tool that subsidizes their existence. The Citizen experiences it as a high-chi Snare, a trap that drains their livelihood. The Historian sees the messy reality: a Tangled Rope, a system with both coordination and immense extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: 'solaran_high_council'. Victim: 'solaran_citizenry'. This clear opposition drives the directionality calculation, yielding a low 'd' for the council (beneficiary) and a high 'd' for the citizenry (victim), creating the massive chi gap.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] The extremely high base extraction (0.85) at the
 *   end of the lifecycle risks a simplistic classification of the entire
 *   system as a Snare. However, the Deferential Realism framework resolves
 *   this by indexing to different agents. For the High Council, the system
 *   is a perfect Rope, coordinating extraction for their benefit (negative
 *   chi). For the Historian, it's a Tangled Rope, acknowledging the residual
 *   (though failing) coordination function alongside the overwhelming
 *   extraction. This prevents the mischaracterization of a complex,
 *   asymmetric system as pure, functionless coercion, which is the core
 *   purpose of Mandatrophy resolution. The framework correctly identifies
 *   that even in its terminal phase, the Solaran state is not just a Snare;
 *   it is a Rope *for someone*.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_solara_collapse,
    'Was the collapse inevitable due to the physics of complexity (a Mountain), or was it a contingent failure of a specific elite (a Snare)?',
    'Comparative analysis of other civilizational collapses with different institutional structures.',
    'If Mountain, all complex civilizations are doomed. If Snare, collapse is avoidable with better governance.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civilizational_lifecycle_solara, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% T=0-3: Founding (Rope)
narrative_ontology:measurement(solara_ex_t0, civilizational_lifecycle_solara, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(solara_th_t0, civilizational_lifecycle_solara, theater_ratio, 0, 0.05).
narrative_ontology:measurement(solara_su_t0, civilizational_lifecycle_solara, suppression_requirement, 0, 0.10).

% T=4-8: Expansion (Scaffold)
narrative_ontology:measurement(solara_ex_t4, civilizational_lifecycle_solara, base_extractiveness, 4, 0.20).
narrative_ontology:measurement(solara_th_t4, civilizational_lifecycle_solara, theater_ratio, 4, 0.10).
narrative_ontology:measurement(solara_su_t4, civilizational_lifecycle_solara, suppression_requirement, 4, 0.30).
narrative_ontology:measurement(solara_ex_t8, civilizational_lifecycle_solara, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(solara_th_t8, civilizational_lifecycle_solara, theater_ratio, 8, 0.20).
narrative_ontology:measurement(solara_su_t8, civilizational_lifecycle_solara, suppression_requirement, 8, 0.35).

% T=9-13: Stagnation (Piton)
narrative_ontology:measurement(solara_ex_t9, civilizational_lifecycle_solara, base_extractiveness, 9, 0.20). % Stays low
narrative_ontology:measurement(solara_th_t9, civilizational_lifecycle_solara, theater_ratio, 9, 0.75). % Theater spikes
narrative_ontology:measurement(solara_su_t9, civilizational_lifecycle_solara, suppression_requirement, 9, 0.40).
narrative_ontology:measurement(solara_ex_t13, civilizational_lifecycle_solara, base_extractiveness, 13, 0.25).
narrative_ontology:measurement(solara_th_t13, civilizational_lifecycle_solara, theater_ratio, 13, 0.85). % Theater maxes out
narrative_ontology:measurement(solara_su_t13, civilizational_lifecycle_solara, suppression_requirement, 13, 0.45).

% T=14-17: Complexity (Tangled Rope)
narrative_ontology:measurement(solara_ex_t14, civilizational_lifecycle_solara, base_extractiveness, 14, 0.50). % Extraction begins to climb
narrative_ontology:measurement(solara_th_t14, civilizational_lifecycle_solara, theater_ratio, 14, 0.60). % Theater decreases as raw extraction replaces it
narrative_ontology:measurement(solara_su_t14, civilizational_lifecycle_solara, suppression_requirement, 14, 0.60).
narrative_ontology:measurement(solara_ex_t17, civilizational_lifecycle_solara, base_extractiveness, 17, 0.65).
narrative_ontology:measurement(solara_th_t17, civilizational_lifecycle_solara, theater_ratio, 17, 0.50).
narrative_ontology:measurement(solara_su_t17, civilizational_lifecycle_solara, suppression_requirement, 17, 0.75).

% T=18-20: Collapse (Snare)
narrative_ontology:measurement(solara_ex_t18, civilizational_lifecycle_solara, base_extractiveness, 18, 0.80).
narrative_ontology:measurement(solara_th_t18, civilizational_lifecycle_solara, theater_ratio, 18, 0.60). % Theater is now just for show
narrative_ontology:measurement(solara_su_t18, civilizational_lifecycle_solara, suppression_requirement, 18, 0.85).
narrative_ontology:measurement(solara_ex_t20, civilizational_lifecycle_solara, base_extractiveness, 20, 0.85). % Final value
narrative_ontology:measurement(solara_th_t20, civilizational_lifecycle_solara, theater_ratio, 20, 0.80). % Final value
narrative_ontology:measurement(solara_su_t20, civilizational_lifecycle_solara, suppression_requirement, 20, 0.90). % Final value

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civilizational_lifecycle_solara, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */