% ============================================================================
% CONSTRAINT STORY: moral_outsourcing
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(moral_outsourcing, []).

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
 * * constraint_id: moral_outsourcing
 * human_readable: The Ethical Externalization Loop
 * domain: social/economic/technological
 * * SUMMARY:
 * A scenario where a system or institution delegitimizes individual ethical 
 * agency by automating decision-making or deferring responsibility to 
 * algorithmic or bureaucratic frameworks. This functions as a "Rope" for 
 * large-scale coordination and liability mitigation but acts as a "Snare" 
 * for the subject, whose moral intuition is siphoned into a "follow-protocol" 
 * mandate, liquidating their ability to act on localized context.
 * * KEY AGENTS:
 * - Protocol Adherent: Subject (Powerless)
 * - Institutional Architect: Beneficiary (Institutional)
 * - Ethical Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the system siphons the subject's 
% primary moral agency to maintain the liability-free coordination 
% of the institution.
domain_priors:base_extractiveness(moral_outsourcing, 0.84). 
domain_priors:suppression_score(moral_outsourcing, 0.76). 
domain_priors:theater_ratio(moral_outsourcing, 0.88). % Extreme theater: "Ethical AI" or "Compliance" branding.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(moral_outsourcing, extractiveness, 0.84).
narrative_ontology:constraint_metric(moral_outsourcing, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(moral_outsourcing, theater_ratio, 0.88).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the powerless agent, the outsourcing is a snare: they are legally 
% and socially punished if they override the protocol, even when it is 
% clearly harmful.
constraint_indexing:constraint_classification(moral_outsourcing, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views outsourcing as a vital Rope—the only way to 
% coordinate ethical standards across millions of transactions with 
% near-zero variance.
constraint_indexing:constraint_classification(moral_outsourcing, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 triggers Piton: the "ethical framework" 
% is an inert spike of logic that protects the institution from 
% liability rather than generating moral outcomes.
constraint_indexing:constraint_classification(moral_outsourcing, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(moral_outsourcing, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) masking as coordination (Rope).
constraint_indexing:constraint_classification(moral_outsourcing, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(moral_outsourcing, E), E >= 0.50,
    domain_priors:suppression_score(moral_outsourcing, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_outsourcing_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(moral_outsourcing, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_outsourcing, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_detection) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(moral_outsourcing, piton, 
        context(agent_power_analytical), _, _, _).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(moral_outsourcing, E),

    E > 0.70.

:- end_tests(moral_outsourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" is actually a parasitic liquidation of the subject's 
 * moral responsibility.
 * 
 * * PERSPECTIVAL GAP:
 * The Protocol Adherent feels a Snare because they are forced to 
 * suppress their conscience to keep their job. The Institutional 
 * Architect sees a Rope because the protocol ensures global 
 * consistency and eliminates individual "ethical noise".
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an 
 * analytical observer, the "ethical coordination" is no longer 
 * functional relative to human values (Theater 0.88); it is an 
 * inert spike siphoning 0.84 of the species' moral agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_moral_residue,
    'Can an automated system simulate moral responsibility (Snare vs Mountain)?',
    'Tracking the frequency of "unforeseen harm" events in protocol-driven vs human-discretion environments.',
    'If protocols prevent harm: Rope of Progress. If harm persists: Snare of Outsourcing.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(moral_outsourcing, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
