% ============================================================================
% CONSTRAINT STORY: litchfield_sensitive_locations_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_litchfield_sensitive_locations_2026, []).

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
 * * constraint_id: litchfield_sensitive_locations_2026
 * human_readable: Litchfield School Perimeter Crisis
 * domain: political/social
 * * SUMMARY:
 * On Feb 5, 2026, federal agents were spotted within a block of the School of 
 * St. Philip in Litchfield, MN, despite a publicized 700-agent drawdown. 
 * This presence at "sensitive locations" triggers a Snare classification for 
 * residents, as it extracts student mental health and attendance to maintain 
 * a 2,300-agent enforcement baseline.
 * * KEY AGENTS:
 * - Parents/Students (School of St. Philip): Subject (Powerless)
 * - Federal Executive (Trump Administration): Beneficiary (Institutional)
 * - MN School Districts (Litigants): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.78) due to direct impact on child mental health 
% and fatal force incidents.
domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, 0.78). 

% Suppression is near-total (0.94) given the use of federal force in 
% non-combatant sensitive zones.
domain_priors:suppression_score(litchfield_sensitive_locations_2026, 0.94).   

% Theater ratio is high (0.88) because the 700-agent drawdown is a 
% performative mask for a sustained 2,300-agent presence.
domain_priors:theater_ratio(litchfield_sensitive_locations_2026, 0.88).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, extractiveness, 0.78).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, suppression_requirement, 0.94).
narrative_ontology:constraint_metric(litchfield_sensitive_locations_2026, theater_ratio, 0.88).

% Metadata facts
% Core indicators
domain_priors:requires_active_enforcement(litchfield_sensitive_locations_2026).
narrative_ontology:constraint_beneficiary(litchfield_sensitive_locations_2026, federal_enforcement_agencies).
narrative_ontology:constraint_victim(litchfield_sensitive_locations_2026, litchfield_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Local parents view the school perimeter presence as a Snare: a predatory 
% environment that traps children between educational needs and federal threat.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Administration views the "Metro Surge" as a Rope: essential coordination 
% for national security, regardless of local proximity to schools.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(historical), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts (School Districts) see a Piton: The "security" function has 
% atrophied into theatrical enforcement that damages local stability.
constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(litchfield_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(litchfield_sensitive_locations_2026, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(litchfield_sensitive_locations_2026, E),
    E > 0.46.

test(theater_dominance) :-
    domain_priors:theater_ratio(litchfield_sensitive_locations_2026, TR),
    TR > 0.70.

:- end_tests(litchfield_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) reflects the measurable drop in attendance 
 * and mental health. The Theater Ratio (0.88) is justified by the 
 * discrepancy between the "700-agent drawdown" announcement and the 
 * tactical reality of agents stationed near the School of St. Philip.
 * * MANDATROPHY ANALYSIS:
 * This case is a classic Piton where the primary function (coordination) 
 * has been replaced by inertial enforcement theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_litchfield_2026,
    'Will the school district lawsuit establish a legal Mountain against federal entry?',
    'Judicial ruling on federal activity in "sensitive locations".',
    'Success creates a Mountain (limit); Failure reinforces the Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(litchfield_sensitive_locations_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rises as the drawdown rhetoric is deployed.
narrative_ontology:measurement(lit_tr_t0, litchfield_sensitive_locations_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lit_tr_t5, litchfield_sensitive_locations_2026, theater_ratio, 5, 0.60).
narrative_ontology:measurement(lit_tr_t10, litchfield_sensitive_locations_2026, theater_ratio, 10, 0.88).

% Extraction spikes upon school perimeter deployment.
narrative_ontology:measurement(lit_ex_t0, litchfield_sensitive_locations_2026, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(lit_ex_t5, litchfield_sensitive_locations_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(lit_ex_t10, litchfield_sensitive_locations_2026, base_extractiveness, 10, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
