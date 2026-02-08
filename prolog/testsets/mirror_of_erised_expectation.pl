% ============================================================================
% CONSTRAINT STORY: erised_expectation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_erised_expectation, []).

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
 * * constraint_id: erised_expectation
 * human_readable: The Erised Career/Stability Mirror
 * domain: psychological/economic
 * * SUMMARY:
 * This constraint represents the internalised Millennial expectation of a
 * "magical" meritocracy where ordinary people defeat evil and achieve
 * stability, based on cultural narratives from the 1990s. It acts as a
 * beautiful but deceptive reflection of a world that was "too good to be
 * true," encouraging participation in increasingly precarious economic systems.
 * * KEY AGENTS:
 * - The Graying Millennial: Subject (Powerless/Entranced)
 * - The Gig Economy/Market: Beneficiary (Institutional/Extractive)
 * - The Disillusioned Zoomer: Observer (Analytical/Jaded)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.65) because the hope of "meritocratic magic" keeps
% the subject participating in declining systems, extracting surplus labor and hope.
domain_priors:base_extractiveness(erised_expectation, 0.65).
% Suppression is high, representing the psychological difficulty of shattering
% a deeply held worldview and accepting a harsher reality.
domain_priors:suppression_score(erised_expectation, 0.70).
% Theater ratio is very high; the belief is maintained through nostalgic
% performance rather than functional outcomes.
domain_priors:theater_ratio(erised_expectation, 0.85).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(erised_expectation, extractiveness, 0.65).
narrative_ontology:constraint_metric(erised_expectation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(erised_expectation, theater_ratio, 0.85).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordinating principle for achieving success.
narrative_ontology:constraint_claim(erised_expectation, piton).

% Binary flags
% The belief system is self-enforcing through cognitive dissonance and social reinforcement.
domain_priors:requires_active_enforcement(erised_expectation).

% Structural property derivation hooks (Required for Tangled Rope)
% The market benefits from a compliant, hopeful workforce.
narrative_ontology:constraint_beneficiary(erised_expectation, gig_economy_platforms).
% Millennials who internalize this expectation are the primary victims of the extraction.
narrative_ontology:constraint_victim(erised_expectation, indebted_millennials).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENTRANCED SUBJECT (SNARE)
% The subject "wastes away" before the mirror, unable to distinguish the
% possible from the real, trapped in a system that extracts their labor.
% χ = 0.65 * 1.5 (powerless) * 1.0 (national) = 0.975.
constraint_indexing:constraint_classification(erised_expectation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE MODERN MARKET (TANGLED ROPE)
% Uses the "optimistic vision" as a coordination mechanism to maintain worker
% faith while extraction remains high. This classification is valid because
% it has beneficiaries (coordination), victims (asymmetric extraction), and
% requires enforcement.
% χ = 0.65 * -0.2 (institutional) * 1.2 (global) = -0.156.
constraint_indexing:constraint_classification(erised_expectation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE EXTERNAL CRITIC / ZOOMER (PITON)
% Sees a "naive" politics and worldview that is "dated" and non-functional,
% maintained only by theatrical nostalgia. Triggered by theater_ratio > 0.7.
constraint_indexing:constraint_classification(erised_expectation, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(erised_expectation, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(erised_expectation_tests).

test(perspectival_gap_snare_vs_tangled_rope) :-
    % Verify the gap between the subject (Snare) and beneficiary (Tangled Rope).
    constraint_indexing:constraint_classification(erised_expectation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(erised_expectation, tangled_rope, context(agent_power(institutional), _, _, _)).

test(piton_classification_from_high_theater) :-
    % Verify the analytical observer sees a Piton due to high theater ratio.
    domain_priors:theater_ratio(erised_expectation, TR),
    ( TR > 0.7 ->
        constraint_indexing:constraint_classification(erised_expectation, piton, context(agent_power(analytical), _, _, _))
    ; fail("Theater ratio is not high enough for Piton classification.")
    ).

test(tangled_rope_structural_requirements_met) :-
    % Verify that all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(erised_expectation, _),
    narrative_ontology:constraint_victim(erised_expectation, _),
    domain_priors:requires_active_enforcement(erised_expectation).

:- end_tests(erised_expectation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a cultural artifact—the optimistic, meritocratic
 * worldview of the 1990s—that has become maladaptive. The base extraction of
 * 0.65 reflects the immense value (labor, compliance, political inaction)
 * extracted from those who still cling to this belief in an economic reality
 * that no longer supports it. The high theater ratio (0.85) is key, as it
 * shows the belief is now maintained by performance and nostalgia, not results,
 * leading to the Piton classification from an analytical view.
 *
 * The perspectival gap is stark: for the Millennial subject, it's a Snare that
 * consumes their prime years. For the institutional market, it's a Tangled
 * Rope—a useful (if degrading) tool for coordinating labor expectations that
 * also enables massive extraction.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction is resolved by recognizing the constraint's dual nature.
 * The Tangled Rope classification prevents mislabeling it as a pure Snare,
 * acknowledging that the "mirror" does perform a genuine (though deceptive)
 * coordination function for the beneficiary. The Piton classification further
 * clarifies that from a systemic, long-term view, the constraint is simply
 * inertial cultural baggage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_mirror_shatter,
    'At what level of economic decline does this belief system shatter for its adherents?',
    'Longitudinal study correlating consumer confidence indices and cost-of-living metrics with polling on generational optimism.',
    'If the belief shatters, it could trigger mass political realignment. If it persists, it implies continued economic stagnation and political quietism.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(erised_expectation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint degraded over time. It began as a plausible worldview in the
% 1990s (low extraction, low theater) and became an extractive, theatrical
% trap as economic conditions changed.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(erised_expectation_tr_t0, erised_expectation, theater_ratio, 0, 0.20).
narrative_ontology:measurement(erised_expectation_tr_t5, erised_expectation, theater_ratio, 5, 0.60).
narrative_ontology:measurement(erised_expectation_tr_t10, erised_expectation, theater_ratio, 10, 0.85).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(erised_expectation_ex_t0, erised_expectation, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(erised_expectation_ex_t5, erised_expectation, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(erised_expectation_ex_t10, erised_expectation, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a shared standard of belief and expectation.
narrative_ontology:coordination_type(erised_expectation, information_standard).

% This psychological constraint directly influences behavior in economic constraints.
narrative_ontology:affects_constraint(erised_expectation, housing_market_entry).
narrative_ontology:affects_constraint(erised_expectation, student_debt_burden).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */