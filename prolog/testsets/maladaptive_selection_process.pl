% ============================================================================
% CONSTRAINT STORY: maladaptive_selection_process
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_maladaptive_selection_process, []).

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
 * * constraint_id: maladaptive_selection_process
 * human_readable: The Evolutionary Dead-End
 * domain: organizational/technological
 * * SUMMARY:
 * A scenario where the criteria for "success" or "fitness" in a system (which claims to be a Rope)
 * become decoupled from long-term survival or functional utility.
 * This coordination mechanism for resource allocation becomes a "Snare"
 * as it optimizes for traits that are locally beneficial but systemically
 * terminal, liquidating the subject's primary survival agency by forcing
 * participation in a race toward a structural cliff.
 *
 * * KEY AGENTS:
 * - Competitive Agent: Subject (Powerless)
 * - Selection Platform Operators: Beneficiary (Institutional)
 * - Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of the subject's long-term
% viability to satisfy the immediate "fitness" metrics of the platform.
domain_priors:base_extractiveness(maladaptive_selection_process, 0.86).
domain_priors:suppression_score(maladaptive_selection_process, 0.74). % Alternative "fitness" strategies are suppressed by the network effect of the dominant selection process.
domain_priors:theater_ratio(maladaptive_selection_process, 0.90).    % Extreme theater: "Meritocracy Dashboards" that mask the systemic decay.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(maladaptive_selection_process, extractiveness, 0.86).
narrative_ontology:constraint_metric(maladaptive_selection_process, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(maladaptive_selection_process, theater_ratio, 0.90).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(maladaptive_selection_process, tangled_rope).
narrative_ontology:human_readable(maladaptive_selection_process, "The Evolutionary Dead-End").

% Binary flags and structural properties for Tangled Rope
domain_priors:requires_active_enforcement(maladaptive_selection_process).
narrative_ontology:constraint_beneficiary(maladaptive_selection_process, selection_platform_operators).
narrative_ontology:constraint_victim(maladaptive_selection_process, competitive_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The agent is trapped: to survive the current interval, they must optimize
% for traits that guarantee long-term failure, liquidating their future agency.
constraint_indexing:constraint_classification(maladaptive_selection_process, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform operators view the selection criteria as a Rope—the essential coordination
% substrate for sorting and ranking global-scale performance data.
constraint_indexing:constraint_classification(maladaptive_selection_process, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) and suppression (0.74) masking as functional
% coordination. The system requires active enforcement and has clear victims
% and beneficiaries, meeting all criteria for a Tangled Rope. The high theater
% ratio (0.90) is a symptom of the extraction being masked, not a sign of inertia.
constraint_indexing:constraint_classification(maladaptive_selection_process, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maladaptive_selection_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless agent vs Rope for the institutional environment.
    constraint_indexing:constraint_classification(maladaptive_selection_process, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maladaptive_selection_process, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(maladaptive_selection_process, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(maladaptive_selection_process, _),
    narrative_ontology:constraint_victim(maladaptive_selection_process, _),
    domain_priors:requires_active_enforcement(maladaptive_selection_process).

test(extraction_threshold) :-
    % High extraction (> 0.46) requires omega variable and temporal data.
    domain_priors:base_extractiveness(maladaptive_selection_process, E),
    E > 0.46.

:- end_tests(maladaptive_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the
 * "coordination" benefit of a universal sorting mechanism is achieved by
 * liquidating the subject's primary capacity for long-term survival. The
 * suppression score (0.74) is high because network effects and platform
 * lock-in make alternative survival strategies non-viable. The extreme
 * theater ratio (0.90) indicates that the performative signaling of "merit"
 * has almost entirely replaced any connection to actual systemic health.
 *
 * * PERSPECTIVAL GAP:
 * The Competitive Agent feels a Snare because they are forced into a
 * "Red Queen" race that hollows out their actual utility. The Selection
 * Platform Operators see a Rope because the criteria coordinate massive behavioral
 * alignment and resource efficiency in the immediate term, generating value for them.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] This is resolved via the Tangled Rope classification.
 * An analysis that only saw the extraction would label this a pure Snare,
 * missing the genuine (though perverse) coordination function. An analysis
 * that only saw the coordination claim would label it a Rope. Tangled Rope
 * correctly identifies that a coordination mechanism is being used as the
 * substrate for asymmetric extraction, which is the core of the mandatrophy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_selection_reversal,
    'Is the maladaptive selection a reversible policy (Tangled Rope) or an emergent, irreversible law of complex scaled systems (Mountain)?',
    'Tracking the survival rate of "counter-trend" organizations that explicitly reject the dominant fitness metrics in hyper-optimized market sectors.',
    'If counter-trends survive: Tangled Rope of current technique. If they collapse: Mountain of Evolutionary Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(maladaptive_selection_process, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a system that degraded over time, with both the
% performative theater and the underlying extraction intensifying as the
% link to genuine utility was severed.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(msp_tr_t0, maladaptive_selection_process, theater_ratio, 0, 0.30).
narrative_ontology:measurement(msp_tr_t5, maladaptive_selection_process, theater_ratio, 5, 0.65).
narrative_ontology:measurement(msp_tr_t10, maladaptive_selection_process, theater_ratio, 10, 0.90).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(msp_ex_t0, maladaptive_selection_process, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(msp_ex_t5, maladaptive_selection_process, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(msp_ex_t10, maladaptive_selection_process, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's coordination function is primarily about sorting and allocating.
narrative_ontology:coordination_type(maladaptive_selection_process, resource_allocation).

% This type of process has downstream effects on labor stability and innovation.
narrative_ontology:affects_constraint(maladaptive_selection_process, labor_market_precarity).
narrative_ontology:affects_constraint(maladaptive_selection_process, technological_lock_in).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */