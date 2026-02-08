% ============================================================================
% CONSTRAINT STORY: technological_point_of_no_return
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(technological_point_of_no_return, []).

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
 * * constraint_id: technological_point_of_no_return
 * human_readable: The Autocatalytic Singularity Gate
 * domain: technological/social
 * * SUMMARY:
 * This constraint represents the threshold at which a technological system 
 * becomes so deeply integrated into the biological or cognitive infrastructure 
 * of a species that "opting out" results in immediate systemic death or 
 * civilizational collapse. It transforms from a chosen utility (Rope) into 
 * an environmental necessity (Mountain).
 * * KEY AGENTS:
 * - Bio-Digital Citizen: Subject (Powerless)
 * - Infrastructure Hegemon: Beneficiary (Institutional)
 * - Evolutionary Biologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.86) because the cost of "exit" is total (death/collapse).
domain_priors:base_extractiveness(technological_point_of_no_return, 0.86). 
domain_priors:suppression_score(technological_point_of_no_return, 0.98). % Zero alternatives once crossed.
domain_priors:theater_ratio(technological_point_of_no_return, 0.15).    % Low theater; the dependency is physical/biological.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(technological_point_of_no_return, extractiveness, 0.86).
narrative_ontology:constraint_metric(technological_point_of_no_return, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(technological_point_of_no_return, theater_ratio, 0.15).

% This is not a scaffold; it is a permanent evolutionary transition.
% narrative_ontology:has_sunset_clause(technological_point_of_no_return). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% For the powerless agent, the technology is now as unalterable as the atmosphere.
constraint_indexing:constraint_classification(technological_point_of_no_return, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views this as the ultimate Rope—perfect, unbreakable coordination.
constraint_indexing:constraint_classification(technological_point_of_no_return, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Viewed historically, this is a snare that has permanently closed off 
% all other possible evolutionary paths for the species.
constraint_indexing:constraint_classification(technological_point_of_no_return, snare, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% High extraction (0.86) triggers the hybrid Tangled Rope signature at the civilizational scale.
constraint_indexing:constraint_classification(technological_point_of_no_return, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(technological_point_of_no_return, E), E >= 0.50,
    domain_priors:suppression_score(technological_point_of_no_return, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technological_point_of_no_return_tests).

test(perspectival_shift) :-
    % Verify the transition from "Necessity" (Mountain) to "Trap" (Snare) based on the observer's time horizon.
    constraint_indexing:constraint_classification(technological_point_of_no_return, mountain, 
        context(agent_power(powerless), time_horizon(biographical), _, _)),
    constraint_indexing:constraint_classification(technological_point_of_no_return, snare, 
        context(agent_power(analytical), time_horizon(historical), _, _)).

test(extraction_mandatrophy) :-
    % Ensure extreme extraction (0.86) is correctly captured.
    domain_priors:base_extractiveness(technological_point_of_no_return, E),

    E > 0.80.

:- end_tests(technological_point_of_no_return_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects the total liquidation of species-level 
 * optionality. This is "Mandatrophy" at its limit—the system is so efficient 
 * at coordination (Rope) that it becomes a permanent cage (Snare).
 * * PERSPECTIVAL GAP:
 * 
 * The individual experiences a Mountain because they cannot biologically 
 * exist without the system. The institution sees a Rope because they 
 * have achieved total alignment and predictability.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that the 
 * "trap" is also the "foundation"—the extraction is the price of continued 
 * existence in the new technological niche.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_biological_decoupling,
    'Can the species biologically decouple from the tech-stack (Snare vs Mountain)?',
    'Long-term isolated cohort studies with high-fidelity metabolic monitoring.',
    'If decoupling = death: Mountain of Biology. If survivable: Snare of Policy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(technological_point_of_no_return, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
