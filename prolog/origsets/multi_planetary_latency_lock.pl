% ============================================================================
% CONSTRAINT STORY: multi_planetary_latency_lock
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(multi_planetary_latency_lock, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: multi_planetary_latency_lock
 * human_readable: The Relativistic Governance Gap
 * domain: technological/political
 * * SUMMARY:
 * This scenario explores a constraint at the universal spatial scope. As 
 * civilization expands to Mars and beyond, the speed of light creates an 
 * irreducible communication latency (3 to 22 minutes for Mars). This delay 
 * renders Earth-centric real-time governance and financial clearing impossible, 
 * forcing the emergence of autonomous local "nodes" that are physically 
 * incapable of real-time coordination with the home planet.
 * * KEY AGENTS:
 * - Martian Colonist: Subject (Powerless)
 * - Earth-Based Central Bank: Beneficiary (Institutional)
 * - Deep-Space Signal Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is 0.04 (Mountain) because the constraint is a physical constant ($c$).
% However, if Earth attempts to enforce Earth-time legal clearing, it spikes to 0.85 (Snare).
domain_priors:base_extractiveness(multi_planetary_latency_lock, 0.04). 
domain_priors:suppression_score(multi_planetary_latency_lock, 0.99). % Alternatives to the speed of light are non-existent.
domain_priors:theater_ratio(multi_planetary_latency_lock, 0.10).    % Low theater; the physics is the enforcement.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(multi_planetary_latency_lock, extractiveness, 0.04).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, suppression_requirement, 0.99).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, theater_ratio, 0.1).

% Speed of light is not a scaffold.
% narrative_ontology:has_sunset_clause(multi_planetary_latency_lock). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the colonist, the delay is not a "rule"; it is a Mountain of physics.
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(multi_planetary_latency_lock, E), E =< 0.05.

% PERSPECTIVE 2: THE BENEFICIARY (TANGLED ROPE)
% If Earth attempts to maintain "Universal Time" clearing, they extract massive 
% premiums for "certainty," creating a hybrid coordination/extraction trap.
constraint_indexing:constraint_classification(multi_planetary_latency_lock, tangled_rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(universal))) :-
    domain_priors:suppression_score(multi_planetary_latency_lock, S), S > 0.40.

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a civilizational view, $c$ is the ultimate non-negotiable coordinate.
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(analytical), 
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multi_planetary_latency_lock_tests).

test(universal_scope_physics) :-
    % Verify that the speed of light is correctly indexed as a Mountain for the subject.
    constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain, 
        context(agent_power(powerless), _, _, spatial_scope(universal))).

test(extraction_limit) :-
    % Ensure base extraction remains low to satisfy the Mountain logic for physics.
    domain_priors:base_extractiveness(multi_planetary_latency_lock, E),

    E =< 0.05.

:- end_tests(multi_planetary_latency_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.04 extraction score reflects that the "cost" is a thermodynamic and 
 * relativistic necessity, not a policy choice. At the universal scope, 
 * humans lose the ability to "exit" the constraint because the speed of 
 * light ($c \approx 3 \times 10^8$ m/s) is constant.
 * * 
 * * * PERSPECTIVAL GAP:
 * The Colonist sees a Mountain because they cannot bargain with photons. 
 * The Institution (Earth) experiences a Tangled Rope because they use the 
 * Mountain's existence to justify a "Latency Surcharge" for interplanetary 
 * financial clearing—turning physics into extraction.
 * * [RESOLVED MANDATROPHY]:
 * This is resolved by keeping the base extraction low. By labeling it a 
 * Mountain at the powerless level, we acknowledge that it is not a 
 * "trap" (Snare) that can be solved with better policy, but an irreducible 
 * constraint of the species' physical expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Although extraction is low, the scope (universal) triggers high-level scrutiny.
omega_variable(
    omega_quantum_entanglement,
    'Can non-local quantum state transfers bypass the latency lock?',
    'Experimental validation of superluminal information transfer via EPR pairs.',
    'If superluminal: Transition from Mountain to Snare (if regulated). If subluminal: Permanent Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(multi_planetary_latency_lock, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
