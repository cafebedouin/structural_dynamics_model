% ============================================================================
% CONSTRAINT STORY: prestige_signal_inflation
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(prestige_signal_inflation, []).

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
 * * constraint_id: prestige_signal_inflation
 * human_readable: The Credential Red Queen
 * domain: social/economic/educational
 * * SUMMARY:
 * This constraint models the devaluation of status markers (degrees, titles, 
 * luxury symbols) as they become more accessible or mandatory. What was once 
 * a Rope for high-fidelity talent coordination becomes a Snare as subjects 
 * must acquire ever-higher "quantities" of prestige just to maintain their 
 * current social or economic position.
 * * KEY AGENTS:
 * - Early-Career Professional: Subject (Powerless)
 * - Credentialing Institution: Beneficiary (Institutional)
 * - Labor Market Sociologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.81) because the inflation siphons the subject's 
% lifetime earnings and time into the acquisition of signals that no 
% longer correlate with increased utility, only baseline entry.
domain_priors:base_extractiveness(prestige_signal_inflation, 0.81). 
domain_priors:suppression_score(prestige_signal_inflation, 0.70). 
domain_priors:theater_ratio(prestige_signal_inflation, 0.85). % High theater: performative "excellence" branding.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(prestige_signal_inflation, extractiveness, 0.81).
narrative_ontology:constraint_metric(prestige_signal_inflation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(prestige_signal_inflation, theater_ratio, 0.85).

% This is a structural property of competitive signaling games.
% narrative_ontology:has_sunset_clause(prestige_signal_inflation). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the early-career subject, inflation is a snare: they are trapped in 
% an "arms race" where the cost of participation rises while the 
% reward (relative status) stays flat.
constraint_indexing:constraint_classification(prestige_signal_inflation, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the signal as a Rope—it is the coordination 
% substrate that allows them to sort populations and capture 
% institutional prestige.
constraint_indexing:constraint_classification(prestige_signal_inflation, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: the "signal" of prestige 
% is no longer a functional coordination tool; it is an inert spike 
% maintained by institutional vanity.
constraint_indexing:constraint_classification(prestige_signal_inflation, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.81) and coordination intent as a Tangled Rope.
constraint_indexing:constraint_classification(prestige_signal_inflation, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(prestige_signal_inflation, E), E >= 0.50,
    domain_priors:suppression_score(prestige_signal_inflation, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prestige_signal_inflation_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(prestige_signal_inflation, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prestige_signal_inflation, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection for analytical auditors.
    constraint_indexing:constraint_classification(prestige_signal_inflation, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.81) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(prestige_signal_inflation, E),

    E > 0.70.

:- end_tests(prestige_signal_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.81) reflects a "Mandatrophy" state where the 
 * "coordination" is actually a parasitic liquidation of the subject's 
 * temporal and financial capital.
 * 
 * * PERSPECTIVAL GAP:
 * The Professional feels a Snare because they are running a Red Queen's 
 * race to stay in the same economic place. The Credentialing Institution 
 * sees a Rope because the inflation of signals ensures a perpetual 
 * demand for their "status-issuing" services.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the signal is no longer functional relative to talent 
 * discovery (Theater 0.85); it is an inert spike siphoning 0.81 of the 
 * species' productive youth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_signal_collapse,
    'Can the signal be replaced by "proof-of-work" or is prestige an irreducible human need (Snare vs Mountain)?',
    'Tracking the market adoption of alternative certification vs traditional degrees over a 15-year horizon.',
    'If alternatives scale: Snare of current policy. If degrees persist: Mountain of Status Hierarchy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(prestige_signal_inflation, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
