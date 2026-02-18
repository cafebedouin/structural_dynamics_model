% ============================================================================
% CONSTRAINT STORY: taiwan_storm_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2023-10-27
% ============================================================================

:- module(constraint_taiwan_storm_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: taiwan_storm_2026
 * human_readable: The 2026 Taiwan "Perfect Storm" Geopolitical Convergence
 * domain: geopolitical/political
 * * SUMMARY:
 * This constraint represents the alignment of three factors creating a window of
 * heightened risk for Taiwan: a potential 2027 legacy deadline for Xi Jinping,
 * a temporary window of perceived U.S. strategic ambiguity or distraction, and
 * the normalization of PLA blockade rehearsals as a coercive tool. The
 * constraint is the structure of incentives and pressures that makes a
 * non-kinetic or kinetic action against Taiwan appear viable to planners.
 * * KEY AGENTS:
 * - Taiwanese Citizenry: Subject (Powerless/Trapped in the "Window")
 * - CCP Leadership: Beneficiary (Institutional/Coordinating Reunification)
 * - Geopolitical Strategist: Auditor (Analytical/Monitoring the "Davidson Window")
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.82) because the goal is "reunification," which
% extracts total sovereignty from the Taiwanese state.
domain_priors:base_extractiveness(taiwan_storm_2026, 0.82).
% Suppression is high (0.78) as military and economic pressure actively
% suppresses alternatives like formal independence or deeper alliances.
domain_priors:suppression_score(taiwan_storm_2026, 0.78).
% Theater ratio is moderate (0.45), representing significant rehearsal and
% performative military drills that are not yet full-scale action.
domain_priors:theater_ratio(taiwan_storm_2026, 0.45).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(taiwan_storm_2026, extractiveness, 0.82).
narrative_ontology:constraint_metric(taiwan_storm_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(taiwan_storm_2026, theater_ratio, 0.45).

% Constraint self-claim (what does the constraint claim to be?)
% Beijing frames the pressure as necessary "coordination" for national reunification.
narrative_ontology:constraint_claim(taiwan_storm_2026, tangled_rope).
narrative_ontology:topic_domain(taiwan_storm_2026, "geopolitical/political").
narrative_ontology:human_readable(taiwan_storm_2026, "The 2026 Taiwan \"Perfect Storm\" Geopolitical Convergence").

% Binary flags
domain_priors:requires_active_enforcement(taiwan_storm_2026). % Blockades, drills require active military enforcement.

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(taiwan_storm_2026, ccp_leadership).
narrative_ontology:constraint_victim(taiwan_storm_2026, taiwanese_citizenry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE TAIWANESE CITIZEN (SNARE)
% The subject experiences the "inevitability" of reunification as a predatory trap.
% χ = 0.82 * π(powerless=1.5) * σ(national=1.0) = 1.23. This is a clear Snare.
constraint_indexing:constraint_classification(taiwan_storm_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CCP LEADERSHIP (ROPE)
% Viewed as the essential coordination of the "great rejuvenation," a historical imperative.
% χ = 0.82 * π(institutional=-0.2) * σ(regional=0.9) = -0.1476. Negative extraction is a Rope.
constraint_indexing:constraint_classification(taiwan_storm_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (for Beijing's goals) and the
% severe asymmetric extraction (from Taiwan), supported by active enforcement.
% This is the definition of a Tangled Rope.
% χ = 0.82 * π(analytical=1.15) * σ(global=1.2) = 1.13. High extraction.
constraint_indexing:constraint_classification(taiwan_storm_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_storm_2026_tests).

test(perspectival_gap_snare_rope) :-
    % Verify the core perspectival gap between the subject and beneficiary.
    constraint_indexing:constraint_classification(taiwan_storm_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_storm_2026, rope, context(agent_power(institutional), _, _, _)).

test(analytical_observer_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope structure.
    constraint_indexing:constraint_classification(taiwan_storm_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % A Tangled Rope requires enforcement, a beneficiary, and a victim.
    domain_priors:requires_active_enforcement(taiwan_storm_2026),
    narrative_ontology:constraint_beneficiary(taiwan_storm_2026, _),
    narrative_ontology:constraint_victim(taiwan_storm_2026, _).

:- end_tests(taiwan_storm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extraction score (0.82) is high, reflecting the existential threat to
 * Taiwanese sovereignty. The suppression score (0.78) reflects the active
 * military and diplomatic pressure used to close off other outcomes.
 * The core of this model is the perspectival gap: for the CCP leadership
 * (institutional), the pressure campaign is a 'Rope'—a necessary tool to
 * coordinate the historical goal of reunification. For the Taiwanese citizen
 * (powerless), it is a 'Snare'—an existential trap with no easy exit.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system avoids misclassifying this complex geopolitical issue as a pure
 * Snare by using the Tangled Rope classification for the analytical observer.
 * This classification correctly identifies that the structure possesses BOTH a
 * genuine (from the beneficiary's perspective) coordination function AND a
 * severe, asymmetric extraction function. This prevents the collapse of analysis
 * into a simplistic "good vs. evil" frame and instead focuses on the structural
 * properties that enable the conflict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_taiwan_storm_2026,
    'Is the 2027 deadline a hard constraint for Xi, or a flexible tool for internal consolidation?',
    'Observation of personnel appointments and policy priorities at the 21st Party Congress.',
    'Hard constraint -> High risk of action. Flexible tool -> Continued coercion without invasion.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. The interval represents the "Davidson
% Window," from his 2021 testimony to the 2027 deadline.
narrative_ontology:interval(taiwan_storm_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint models the intensification
% of pressure and rehearsals over the 2021-2027 period.
% T=0 maps to 2021, T=5 to mid-2024, T=10 to 2027.

% Theater ratio over time (normalization of military drills):
narrative_ontology:measurement(ts26_tr_t0, taiwan_storm_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ts26_tr_t5, taiwan_storm_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ts26_tr_t10, taiwan_storm_2026, theater_ratio, 10, 0.45).

% Extraction over time (intensification of the sovereignty threat):
narrative_ontology:measurement(ts26_ex_t0, taiwan_storm_2026, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(ts26_ex_t5, taiwan_storm_2026, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ts26_ex_t10, taiwan_storm_2026, base_extractiveness, 10, 0.82).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a coercive enforcement mechanism for a geopolitical claim.
narrative_ontology:coordination_type(taiwan_storm_2026, enforcement_mechanism).

% Network relationships: The Taiwan situation directly impacts global
% semiconductor supply chains, which is modeled as a separate constraint.
narrative_ontology:affects_constraint(taiwan_storm_2026, semiconductor_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */