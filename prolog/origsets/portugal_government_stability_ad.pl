% ============================================================================
% CONSTRAINT STORY: PORTUGAL_GOVERNMENT_STABILITY_AD
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_portugal_ad_stability_2026, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: portugal_ad_stability_2026
 * human_readable: The AD Minority Government Stability (The "Presidential" Scaffold)
 * domain: political
 * * SUMMARY:
 * The stability of the Aliança Democrática (AD) minority government functions 
 * as a Scaffold. It relies on the mediating influence of the outgoing President, 
 * Marcelo Rebelo de Sousa, and a temporary truce between the PS and PSD. 
 * This constraint has an implicit sunset clause: the January 2026 Presidential 
 * election. The outcome of the election—specifically whether the new President 
 * is a "stabilizer" or a "disruptor"—will determine if this scaffold is 
 * replaced by a more permanent coordination (Rope) or collapses into a Snare.
 * * KEY AGENTS:
 * - Luís Montenegro: Architect (Organized) - Managing the fragile minority balance.
 * - The New President (Candidate): Beneficiary (Institutional) - Inherits the power to dissolve parliament.
 * - The Civil Service/Economy: Subject (Powerless) - Dependent on the non-collapse of the budget.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is moderate; the government coordinates but requires suppression of dissent.
domain_priors:base_extractiveness(portugal_ad_stability_2026, 0.35). 
% Suppression: Significant; parties are discouraged from "rocking the boat" before the election.
domain_priors:suppression_score(portugal_ad_stability_2026, 0.45).   
% Theater: Moderate; constant negotiations are partially performative for the electorate.
domain_priors:theater_ratio(portugal_ad_stability_2026, 0.30).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(portugal_ad_stability_2026, extractiveness, 0.35).
narrative_ontology:constraint_metric(portugal_ad_stability_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(portugal_ad_stability_2026, theater_ratio, 0.3).

% Binary flags
domain_priors:requires_active_enforcement(portugal_ad_stability_2026).
% Mandatory for Scaffold status.
narrative_ontology:has_sunset_clause(portugal_ad_stability_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ARCHITECT (SCAFFOLD)
% Tolerated high suppression of political rivalry only because it expires with the new mandate.
constraint_indexing:constraint_classification(portugal_ad_stability_2026, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(portugal_ad_stability_2026).

% PERSPECTIVE 2: THE INSTITUTIONAL LOYALIST (ROPE)
% Viewed as necessary coordination to avoid a mid-cycle constitutional crisis.
constraint_indexing:constraint_classification(portugal_ad_stability_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE POLICY CRITIC (SNARE)
% Viewed as a trap where the need for "stability" is used to extract concessions.
constraint_indexing:constraint_classification(portugal_ad_stability_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portugal_ad_stability_2026_tests).

test(scaffold_expiry) :-
    % Verify that the scaffold is correctly identified with its sunset clause.
    constraint_indexing:constraint_classification(portugal_ad_stability_2026, scaffold, context(agent_power(organized), _, _, _)),
    narrative_ontology:has_sunset_clause(portugal_ad_stability_2026).

test(type_variance) :-
    % Ensure at least two different types across indices for v3.4 compliance.
    constraint_indexing:constraint_classification(portugal_ad_stability_2026, Type1, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(portugal_ad_stability_2026, Type2, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2.

:- end_tests(portugal_ad_stability_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The AD government is classified as a Scaffold because its survival is 
 * conditional on the current presidential term. The sunset clause 
 * is the January 24th election date. Once the new President is sworn 
 * in, the "coordination" provided by Marcelo's mediation expires. 
 * If the new President uses the "atomic bomb" (dissolution power), the 
 * Scaffold collapses immediately.
 *
 * PERSPECTIVAL GAP:
 * The government (Architect) sees a necessary Scaffold to pass budgets. 
 * Opposition voters (Subject) see a Snare that limits their ability to 
 * challenge austerity or specific AD policies under the guise of "national 
 * interest".
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_presidential_dissolution,
    'Will the winner of the 2026 election dismantle the AD stability scaffold by calling early legislative elections?',
    'Observation of the new President\'s first speech to the Council of State.',
    'Collapse (Scaffold removed) vs Solidification (Scaffold becomes Rope)',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portugal_ad_stability_2026, 2024, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
