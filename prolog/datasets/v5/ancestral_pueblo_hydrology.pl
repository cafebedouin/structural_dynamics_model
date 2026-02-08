% ============================================================================
% CONSTRAINT STORY: ANCESTRAL_PUEBLO_HYDROLOGY
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_pueblo_hydrology, []).

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
 * * constraint_id: ancestral_pueblo_hydrology
 * human_readable: Ancestral Puebloan Hydrological Debt
 * domain: environmental/social
 * * SUMMARY:
 * This constraint models the collapse of the Chaco Canyon and Mesa Verde 
 * urban centers (c. 1276-1299 CE). It tracks the transition of water 
 * infrastructure from a coordination Rope to a survival Snare during the 
 * "Great Drought."
 * * KEY AGENTS:
 * - The Pueblo Farmer: Subject (Powerless/Trapped by sedentary agriculture)
 * - The Priest/Leadership: Beneficiary (Institutional/Coordinating infrastructure)
 * - The Modern Archaeologist: Auditor (Analytical/Detection of Mandatrophy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.82) as the "Great Drought" effectively extracted the 
% caloric basis for the entire civilization's survival. [RESOLVED MANDATROPHY]
domain_priors:base_extractiveness(ancestral_pueblo_hydrology, 0.82). 
domain_priors:suppression_score(ancestral_pueblo_hydrology, 0.75).   
domain_priors:theater_ratio(ancestral_pueblo_hydrology, 0.15).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, extractiveness, 0.82).
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, theater_ratio, 0.15).

% Mandatory keys for classification engine v3.4
% REQUIRED: Sunset clause for Scaffold classification (Far View Reservoir).
% Rationale: Infrastructure serves as a temporary support (Scaffold) that 
% eventually sunsets as the climate shifts into a terminal Mountain.
narrative_ontology:has_sunset_clause(ancestral_pueblo_hydrology).

domain_priors:requires_active_enforcement(ancestral_pueblo_hydrology).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(ancestral_pueblo_hydrology, priesthood_leadership).
narrative_ontology:constraint_victim(ancestral_pueblo_hydrology, pueblo_farmers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CLIFF-DWELLER (SNARE)
% During the 23-year drought, the fixed investment in stone cities and 
% corn agriculture became a trap that prevented rapid adaptation.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE THEOCRATIC LEADERSHIP (ROPE)
% Viewed as the essential coordination of reservoirs and ritual labor 
% that sustained the "Golden Age" for three centuries.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ARID ENVIRONMENT (MOUNTAIN)
% The baseline moisture limit of the Southwest is an irreducible physical 
% law that dictates the ultimate "interest rate" of the debt.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(ancestral_pueblo_hydrology, E), E > 0.70.

% PERSPECTIVE 4: THE FAR VIEW RESERVOIR (SCAFFOLD)
% Acts as a temporary support structure (Scaffold) that functions as long 
% as annual recharge exceeds a minimal threshold.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pueblo_hydrology_tests).

test(perspectival_shift) :-
    % Verify the infrastructure is a Rope for the leadership but a Snare for the farmer.
    constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, snare, context(agent_power(powerless), _, _, _)).

test(mountain_validation) :-
    domain_priors:base_extractiveness(ancestral_pueblo_hydrology, E),
    E >= 0.80. % Confirms it meets the v3.4 "Mountain" / High-Extraction Snare threshold.

:- end_tests(pueblo_hydrology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) reflects the existential "debt" called by the 
 * Great Drought. The Perspectival Gap is defined by the transition from 
 * high coordination (Rope) to a zero-degree-of-freedom trap (Snare). 
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system prevents mislabeling this as pure extraction by acknowledging the 
 * 'Rope' classification at the institutional level, which represents the 
 * 300+ years of successful coordination before climate-driven collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_collapse_causality,
    'Was the collapse a Snare of rigid social hierarchy or a Mountain of environmental limits?',
    'Analysis of Rio Grande migration sites to determine if social complexity was retained or shed.',
    'Social = Recoverable Rope; Environmental = Irreducible Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
% Tracks the primary urban phase to the final abandonment.
narrative_ontology:interval(ancestral_pueblo_hydrology, 850, 1300). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Remains low as the infrastructure was primarily functional (irrigation)
% rather than performative (theater).
narrative_ontology:measurement(pueblo_tr_t0, ancestral_pueblo_hydrology, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pueblo_tr_t5, ancestral_pueblo_hydrology, theater_ratio, 5, 0.10).
narrative_ontology:measurement(pueblo_tr_t10, ancestral_pueblo_hydrology, theater_ratio, 10, 0.15).

% Extraction: Tracking the intensification of "hydrological debt" as the 
% Great Drought (c. 1276-1299 CE) liquidates the caloric surplus.
narrative_ontology:measurement(pueblo_ex_t0, ancestral_pueblo_hydrology, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(pueblo_ex_t5, ancestral_pueblo_hydrology, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(pueblo_ex_t10, ancestral_pueblo_hydrology, base_extractiveness, 10, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
