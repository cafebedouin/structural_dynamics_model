% ============================================================================
% CONSTRAINT STORY: geopolitical_insularity_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_geopolitical_insularity_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: geopolitical_insularity_2026
 * human_readable: Geopolitical Nationalist Insularity
 * domain: geopolitical/economic
 * SUMMARY: 
 * A world order defined by the "Great Realignment" where trust is a zero-sum 
 * nationalist asset. 65% of people fear foreign disinformation, and 34% 
 * would actively reduce foreign market presence to protect domestic stability.
 * KEY AGENTS:
 * - Local Citizen: Subject (Powerless) - Fears job loss from trade wars (66%).
 * - Multinational Corp: Beneficiary (Institutional) - Must pivot to "Polynational" models.
 * - Global Auditor: Auditor (Analytical) - Tracks the 31pt domestic trust gap in Canada.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction (0.60) reflects the 34% productivity hit and "protectionist tax".
domain_priors:base_extractiveness(geopolitical_insularity_2026, 0.60). 
% Suppression (0.70) scale: 1/3rd of people want foreign companies removed entirely.
domain_priors:suppression_score(geopolitical_insularity_2026, 0.70).   
% Theater ratio (0.55) reflects high "Brand Nationalism" vs actual coordination.
domain_priors:theater_ratio(geopolitical_insularity_2026, 0.55).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(geopolitical_insularity_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(geopolitical_insularity_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geopolitical_insularity_2026, theater_ratio, 0.55).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LOCAL WORKER (SNARE)
% Sees global trade as a trap; 66% worry trade policies will hurt their employer.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE MULTINATIONAL STRATEGIST (ROPE/SCAFFOLD)
% Views the local circle as the only way to coordinate progress; uses "Polynational" 
% structures to survive nationalism.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, scaffold, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects how "My CEO" (66% trust) replaces distant global institutions.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_insularity_2026_tests).

test(domestic_gap_validation) :-
    % Gaps in Canada (31pts) and Japan (29pts) confirm high extractiveness of foreign labels.
    domain_priors:base_extractiveness(geopolitical_insularity_2026, E),
    E > 0.46.

test(insularity_threshold) :-
    % Global 70% unwilling to trust different actors.
    domain_priors:suppression_score(geopolitical_insularity_2026, S),

    S >= 0.60.

:- end_tests(geopolitical_insularity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.70 suppression score is driven by the "Me over We" mentality, where 
 * 42% are unwilling to invest in companies that do not share their values. 
 * The move from Multinational to "Polynational" is the suggested resolution, 
 * treating each country as its own "local circle" to bypass foreign distrust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_geo_2026,
    'Will the "Polynational" model resolve the 29pt trust gap in developed markets?',
    'Tracking trust scores of foreign companies that hire 100% local leadership.',
    'Success implies a path to global trade; failure implies a return to autarky.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_insularity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising as "Brand America" or "Brand China" becomes a liability.
narrative_ontology:measurement(geo_tr_t0, geopolitical_insularity_2026, theater_ratio, 0, 0.30).
narrative_ontology:measurement(geo_tr_t5, geopolitical_insularity_2026, theater_ratio, 5, 0.45).
narrative_ontology:measurement(geo_tr_t10, geopolitical_insularity_2026, theater_ratio, 10, 0.55).

% Extraction: Sharp increase linked to 2026 disinformation all-time highs (65%).
narrative_ontology:measurement(geo_ex_t0, geopolitical_insularity_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(geo_ex_t5, geopolitical_insularity_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(geo_ex_t10, geopolitical_insularity_2026, base_extractiveness, 10, 0.60).

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================
