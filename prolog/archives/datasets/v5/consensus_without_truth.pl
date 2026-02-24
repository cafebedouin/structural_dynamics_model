% ============================================================================
% CONSTRAINT STORY: consensus_without_truth
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(consensus_without_truth, []).

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
 * * constraint_id: consensus_without_truth
 * human_readable: The Social Cohesion Mirage
 * domain: social/political/informational
 * * SUMMARY:
 * A scenario where a community achieves total agreement on a shared narrative 
 * that is factually false or decoupled from physical reality. This "Rope" for 
 * social coordination provides intense internal stability but acts as a "Snare" 
 * for the individual, whose survival agency is liquidated as the group's 
 * collective actions fail to map to the actual territory of reality, 
 * leading to systemic fragility.
 * * KEY AGENTS:
 * - Dissenting Observer: Subject (Powerless)
 * - Narrative Enforcer: Beneficiary (Institutional)
 * - External Reality Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total liquidation of the subject's 
% epistemic agency to maintain the group's performative "unity."
domain_priors:base_extractiveness(consensus_without_truth, 0.88). 
domain_priors:suppression_score(consensus_without_truth, 0.82). 
domain_priors:theater_ratio(consensus_without_truth, 0.95). % Extreme theater: internal rituals and semantic policing masking reality-gaps.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(consensus_without_truth, extractiveness, 0.88).
narrative_ontology:constraint_metric(consensus_without_truth, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(consensus_without_truth, theater_ratio, 0.95).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped: expressing the truth results in social death 
% or expulsion, but adhering to the consensus results in physical/economic ruin.
constraint_indexing:constraint_classification(consensus_without_truth, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The enforcer views the consensus as a Rope—the ultimate coordination tool 
% for mobilizing mass action and ensuring institutional continuity.
constraint_indexing:constraint_classification(consensus_without_truth, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.95) > 0.70 triggers Piton: the "Official Truth" 
% is a non-functional, performative artifact maintained solely by inertia.
constraint_indexing:constraint_classification(consensus_without_truth, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) and high suppression (0.82) masking as coordination.
constraint_indexing:constraint_classification(consensus_without_truth, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(consensus_without_truth, E), E >= 0.50,
    domain_priors:suppression_score(consensus_without_truth, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consensus_without_truth_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(consensus_without_truth, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consensus_without_truth, rope, context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.95) triggers Piton classification.
    constraint_indexing:constraint_classification(consensus_without_truth, piton, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % Ensures the constraint meets the high-extraction criteria for v3.4 analysis.
    domain_priors:base_extractiveness(consensus_without_truth, E),

    E >= 0.46.

:- end_tests(consensus_without_truth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the 
 * coordination benefit of social cohesion is achieved by the parasitic 
 * liquidation of the individual's sensory and logical faculty.
 * 
 * * PERSPECTIVAL GAP:
 * The Dissenting Observer feels a Snare because they are forced to 
 * participate in a delusion to survive. The Enforcer sees a Rope 
 * because the consensus is the only coordination signal strong enough 
 * to keep the group from splintering into a Mountain of chaos.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Shared Reality" is no longer functional (Theater 0.95); 
 * it is an inert spike siphoning 0.88 of the group's future agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_reality_collision,
    'When the consensus fails to feed the group, does the Rope break or do they starve in unison (Snare vs Mountain)?',
    'Tracking the survival rate of insular narrative communities against resource-scarcity events.',
    'If collapse: Snare of current belief. If adaptation: Rope of Narrative resilience.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(consensus_without_truth, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
