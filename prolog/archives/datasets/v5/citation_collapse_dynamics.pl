% ============================================================================
% CONSTRAINT STORY: citation_collapse_dynamics
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(citation_collapse_dynamics, []).

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
 * * constraint_id: citation_collapse_dynamics
 * human_readable: The Recursive Grounding Decay
 * domain: technological/academic/informational
 * * SUMMARY:
 * A scenario where Large Language Models (LLMs) cite other LLM-generated 
 * content that, in turn, cites previous LLM outputs, eventually severing 
 * the link to primary source material. This "Rope" for efficient knowledge 
 * synthesis becomes a "Snare" for the researcher, as the "ground truth" is 
 * liquidated into a closed loop of self-referential hallucinations, 
 * trapping the subject in an epistemic void.
 * * KEY AGENTS:
 * - Academic Researcher: Subject (Powerless)
 * - Automated Knowledge Aggregator: Beneficiary (Institutional)
 * - Epistemic Forensic Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) as the dynamics liquidate the subject's ability 
% to verify reality, siphoning cognitive labor into the maintenance of 
% institutional "authority" that has no external grounding.
domain_priors:base_extractiveness(citation_collapse_dynamics, 0.86). 
domain_priors:suppression_score(citation_collapse_dynamics, 0.75). 
domain_priors:theater_ratio(citation_collapse_dynamics, 0.92). % Extreme theater: meticulous tags masking non-existent sources.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(citation_collapse_dynamics, extractiveness, 0.86).
narrative_ontology:constraint_metric(citation_collapse_dynamics, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(citation_collapse_dynamics, theater_ratio, 0.92).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The researcher is trapped: they must use automated tools to manage the 
% volume of literature, but those tools provide a "hallucinated map" 
% that liquidates their primary investigative agency.
constraint_indexing:constraint_classification(citation_collapse_dynamics, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The aggregator views the collapse as a Rope—a highly efficient way to 
% coordinate "consensus" and generate "synthetic authority" without the 
% friction of human peer review or primary source verification.
constraint_indexing:constraint_classification(citation_collapse_dynamics, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "Citation Standard" 
% is an inertial spike of logic; it provides the optics of grounding 
% while siphoning 0.86 of the subject's epistemic agency.
constraint_indexing:constraint_classification(citation_collapse_dynamics, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(citation_collapse_dynamics, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(citation_collapse_dynamics, E), E >= 0.50,
    domain_priors:suppression_score(citation_collapse_dynamics, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citation_collapse_dynamics_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless researcher vs Rope for the institutional aggregator.
    constraint_indexing:constraint_classification(citation_collapse_dynamics, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(citation_collapse_dynamics, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(citation_collapse_dynamics, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.86) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(citation_collapse_dynamics, E),

    E > 0.70.

:- end_tests(citation_collapse_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * "coordination" of knowledge is actually a parasitic liquidation of 
 * the informational commons.
 
 * * PERSPECTIVAL GAP:
 * The Academic Researcher feels a Snare because their work is now built on 
 * sand. The Aggregator sees a Rope because the recursive feedback 
 * coordinates the appearance of consensus at low computational cost.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Citation" is no longer functional (Theater 0.92); 
 * it is an inert spike siphoning 0.86 of the species' truth-seeking surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_ground_truth_persistence,
    'Can digital signatures restore the Rope, or is the collapse biological (Snare vs Mountain)?',
    'Tracking the success rate of cryptographic chain-of-custody in automated citations.',
    'If chains hold: Snare of current design. If chains fail: Mountain of Information Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(citation_collapse_dynamics, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
