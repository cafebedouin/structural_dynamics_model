% ============================================================================
% CONSTRAINT STORY: cascading_uncertainty_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_cascading_uncertainty_2026, []).

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
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cascading_uncertainty_2026
 * human_readable: The Sovereignty Gap (Cascading Uncertainty)
 * domain: political/social/geopolitical
 * * SUMMARY:
 * As of Feb 5, 2026, a "Cascading Uncertainty" has emerged from the 
 * simultaneous expiration of New START and the domestic enforcement crisis 
 * in Minnesota. While the state projects power through $151B defense 
 * contracts, 65% of the population views its domestic operations as 
 * excessive, creating a widening "sovereignty gap."
 * * KEY AGENTS:
 * - General US Population: Subject (Powerless)
 * - Federal Executive/Defense Apparatus: Beneficiary (Institutional)
 * - Institutional Auditors/Pollsters: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.75) as the state extracts massive capital for 
% defense while social legitimacy (the ultimate coordination asset) is depleted.
domain_priors:base_extractiveness(cascading_uncertainty_2026, 0.75). 

% Suppression is high (0.88) due to the "Operation Metro Surge" 
% tactics and the lack of response to nuclear extension offers.
domain_priors:suppression_score(cascading_uncertainty_2026, 0.88).   

% Theater ratio is high (0.80) because "drawdown" announcements and 
% "strategic ambiguity" mask the underlying growth of technological power.
domain_priors:theater_ratio(cascading_uncertainty_2026, 0.80).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cascading_uncertainty_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(cascading_uncertainty_2026, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(cascading_uncertainty_2026, theater_ratio, 0.8).

% Primary keys for the classification engine
% Stakeholder declarations
narrative_ontology:constraint_beneficiary(cascading_uncertainty_2026, federal_defense_contractors).
narrative_ontology:constraint_victim(cascading_uncertainty_2026, american_social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the 65% of Americans who feel ICE has "gone too far," the state's 
% growth is a Snare—a predatory trap of high-tech surveillance and force.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The executive branch views this as a Rope: coordinating "SHIELD" defense 
% and domestic "Metro Surge" to ensure national survival.
constraint_indexing:constraint_classification(cascading_uncertainty_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts view this "Sovereignty Gap" as a Piton: Inertial growth of 
% technological power (SHIELD) that has lost its functional social 
% legitimacy (65% disapproval).
constraint_indexing:constraint_classification(cascading_uncertainty_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cascading_uncertainty_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cascading_uncertainty_2026, rope, context(agent_power(institutional), _, _, _)).

test(extraction_accumulation) :-
    domain_priors:base_extractiveness(cascading_uncertainty_2026, E),
    E > 0.46.

test(theater_dominance) :-
    domain_priors:theater_ratio(cascading_uncertainty_2026, TR),
    TR > 0.70.

:- end_tests(cascading_uncertainty_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) is extremely high because the state is 
 * actively burning its social capital (legitimacy) to fund its 
 * technological power (SHIELD). The high theater_ratio (0.80) is 
 * justified by the performative nature of the "700-agent drawdown" 
 * while the core enforcement apparatus remains at 2,300 agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_sovereignty_gap_2026,
    'Can a state maintain global technological "Ropes" while domestic legitimacy is a "Snare"?',
    'Longitudinal study of tax compliance and enrollment in federal programs.',
    'Success maintains the state; Failure leads to a fragmentation of sovereignty.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cascading_uncertainty_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rises as the gap between state rhetoric and social polls widens.
narrative_ontology:measurement(cas_tr_t0, cascading_uncertainty_2026, theater_ratio, 0, 0.30).
narrative_ontology:measurement(cas_tr_t5, cascading_uncertainty_2026, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cas_tr_t10, cascading_uncertainty_2026, theater_ratio, 10, 0.80).

% Extraction rises as defense contracts scale and domestic approval drops.
narrative_ontology:measurement(cas_ex_t0, cascading_uncertainty_2026, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cas_ex_t5, cascading_uncertainty_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cas_ex_t10, cascading_uncertainty_2026, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
