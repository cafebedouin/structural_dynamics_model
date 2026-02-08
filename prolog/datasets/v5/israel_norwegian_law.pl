% ============================================================================
% CONSTRAINT STORY: israel_norwegian_law
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_israel_norwegian_law, []).

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
 * constraint_id: israel_norwegian_law
 * human_readable: The Norwegian Law (Amendment to Article 42c)
 * domain: political
 * SUMMARY:
 * A mechanism allowing ministers to resign from the Knesset to focus on 
 * executive duties, replaced by the next list member. While it expands 
 * committee capacity, it creates "conditional" MKs whose tenure depends 
 * entirely on the minister's cabinet status, effectively extracting their 
 * political independence.
 * KEY AGENTS:
 * - Replacement MK: Subject (Powerless) - Vulnerable to the "revolving door."
 * - Coalition Leadership: Beneficiary (Institutional) - Staffs committees.
 * - Public Auditor: Auditor (Analytical) - Tracks fiscal and representative costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.52) due to the NIS 1.95M annual cost per new MK 
% and the extraction of voting independence from replacement MKs.
domain_priors:base_extractiveness(israel_norwegian_law, 0.52). 

% Suppression of alternatives is high; the system mandates a 120-seat cap, 
% making this the primary bypass for cabinet-heavy coalitions.
domain_priors:suppression_score(israel_norwegian_law, 0.70).   

domain_priors:theater_ratio(israel_norwegian_law, 0.25). % Functional governance bypass.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(israel_norwegian_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(israel_norwegian_law, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(israel_norwegian_law, theater_ratio, 0.25).

domain_priors:requires_active_enforcement(israel_norwegian_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE REPLACEMENT MK (SNARE)
% Viewed as a precarious trap where independence is traded for a seat 
% that can be revoked at any moment by a minister's whim.
constraint_indexing:constraint_classification(israel_norwegian_law, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE COALITION (ROPE)
% Viewed as essential infrastructure to prevent the "onerous" workload 
% of staffing committees in a small 120-member parliament.
constraint_indexing:constraint_classification(israel_norwegian_law, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid efficiency of governance and the extraction of fiscal resources.
constraint_indexing:constraint_classification(israel_norwegian_law, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(israel_norwegian_law, E), E >= 0.50.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_norwegian_law_tests).

test(perspectival_gap) :-
    % Verify the law is a Snare for the vulnerable MK but a Rope for the leadership.
    constraint_indexing:constraint_classification(israel_norwegian_law, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_norwegian_law, rope, context(agent_power(institutional), _, _, _)).

:- end_tests(israel_norwegian_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 2023 expansion demonstrates the Tangled Rope's "Asymmetric Extraction." 
 * While it solves the coordination problem of a too-small Knesset, it 
 * does so at a high fiscal cost (NIS 35M+ per year) and reduces the 
 * independence of MKs who know they will be "sent packing" if their 
 * minister leaves the cabinet (as seen with Sharon Roffe-Ofir).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_israel_norwegian_law,
    'Does the law increase committee efficiency enough to offset the loss of direct ministerial oversight in the plenary?',
    'Comparative audit of committee session attendance vs. plenary question time frequency.',
    'Determines if the constraint is a genuine Rope or a Piton (inertial waste).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_norwegian_law, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
