% ============================================================================
% CONSTRAINT STORY: adversarial_truth_decay
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(adversarial_truth_decay, []).

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
 * * constraint_id: adversarial_truth_decay
 * human_readable: The Epistemic Siege
 * domain: social/technological/political
 * * SUMMARY:
 * A scenario where the cost of generating high-fidelity misinformation via 
 * generative AI is significantly lower than the cost of verifying truth. This 
 * "Rope" for adversarial actors to coordinate influence operations becomes 
 * a "Snare" for the public, liquidating the shared informational commons 
 * and forcing subjects into a state of low-trust paralysis where 
 * collective action is impossible.
 * * KEY AGENTS:
 * - Information Consumer: Subject (Powerless)
 * - Influence Architect: Beneficiary (Institutional)
 * - Epistemic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.89) because the decay liquidates the subject's ability 
% to make informed decisions, siphoning their cognitive autonomy into 
% a managed reality controlled by the highest-volume producer.
domain_priors:base_extractiveness(adversarial_truth_decay, 0.89). 
domain_priors:suppression_score(adversarial_truth_decay, 0.78). % High suppression: factual alternatives are drowned out.
domain_priors:theater_ratio(adversarial_truth_decay, 0.94).    % Extreme theater: "fact-checking" rituals that lack the velocity to catch up.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(adversarial_truth_decay, extractiveness, 0.89).
narrative_ontology:constraint_metric(adversarial_truth_decay, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(adversarial_truth_decay, theater_ratio, 0.94).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The consumer is trapped: verifying every piece of data is biologically 
% impossible, yet acting on unverified data is increasingly dangerous.
constraint_indexing:constraint_classification(adversarial_truth_decay, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the decay as a Rope—the most efficient way to 
% coordinate mass sentiment and suppress counter-narratives without 
% the friction of traditional censorship.
constraint_indexing:constraint_classification(adversarial_truth_decay, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.94) > 0.70 triggers Piton: "Truth-verification" 
% protocols are an inertial spike; they provide the optics of security 
% without the functional capacity to resolve the flood.
constraint_indexing:constraint_classification(adversarial_truth_decay, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.89) and coordination necessity as a Tangled Rope.
constraint_indexing:constraint_classification(adversarial_truth_decay, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(adversarial_truth_decay, E), E >= 0.50,
    domain_priors:suppression_score(adversarial_truth_decay, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adversarial_truth_decay_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(adversarial_truth_decay, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_truth_decay, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.94) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(adversarial_truth_decay, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.89) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(adversarial_truth_decay, E),

    E > 0.70.

:- end_tests(adversarial_truth_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.89) reflects a "Mandatrophy" state where the 
 * "coordination" of social consensus is achieved by liquidating the 
 * subject's access to reality.
 * 
 * * PERSPECTIVAL GAP:
 * The Information Consumer feels a Snare because their cognitive environment 
 * is weaponized against them. The Influence Architect sees a Rope because 
 * the decay coordinates the dissolution of opposition and the 
 * consolidation of attention.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "verification" is no longer functional (Theater 0.94); 
 * it is an inert spike siphoning 0.89 of the species' collective agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_epistemic_velocity,
    'Can verification speed ever match generation speed (Snare vs Mountain)?',
    'Tracking the delta between "misinformation release" and "effective debunk penetration" over time.',
    'If delta is fixed: Mountain of Information Theory. If delta can shrink: Snare of current tech.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(adversarial_truth_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
:- multifile narrative_ontology:measurement/5.

% Theater ratio rising: "fact-checking" theater intensifying over time.
% T=0: Pre-AI era, modest theater. T=5: Early deepfake era. T=10: Current state.
narrative_ontology:measurement(atd_m1, adversarial_truth_decay, theater_ratio, 0, 0.40).
narrative_ontology:measurement(atd_m2, adversarial_truth_decay, theater_ratio, 5, 0.70).
narrative_ontology:measurement(atd_m3, adversarial_truth_decay, theater_ratio, 10, 0.94).

% Extraction rising: information commons being progressively liquidated.
narrative_ontology:measurement(atd_m4, adversarial_truth_decay, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(atd_m5, adversarial_truth_decay, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(atd_m6, adversarial_truth_decay, base_extractiveness, 10, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
