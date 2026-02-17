% ============================================================================
% CONSTRAINT STORY: antifragility
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility, []).

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
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: antifragility
 * human_readable: Antifragility (Gaining from Disorder)
 * domain: technological/economic/biological
 * * SUMMARY:
 * Antifragility describes systems that increase in capability or resilience 
 * in response to stressors and volatility. For the species, it is a 
 * Mountain; for the informed practitioner, a Rope; and for the fragile 
 * subject, it is a Snare.
 * * KEY AGENTS:
 * - The Optimized Serf: Subject (Powerless)
 * - The Barbell Practitioner: Beneficiary (Moderate)
 * - The Fragilista/Bureaucrat: Institutional Beneficiary/Enforcer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(antifragility, 0.75). 
domain_priors:suppression_score(antifragility, 0.65).   
domain_priors:theater_ratio(antifragility, 0.55). % Reflects performative stability.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(antifragility, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility, theater_ratio, 0.55).

% Constraint classification claim
narrative_ontology:constraint_claim(antifragility, tangled_rope).
narrative_ontology:human_readable(antifragility, "Antifragility (Gaining from Disorder)").

% Constraint metric facts used by the classification engine.
domain_priors:requires_active_enforcement(antifragility).

% Beneficiaries and Victims
narrative_ontology:constraint_beneficiary(antifragility, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility, fragile_institutions).
narrative_ontology:constraint_victim(antifragility, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OPTIMIZED SERF (SNARE)
constraint_indexing:constraint_classification(antifragility, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BARBELL PRACTITIONER (ROPE)
constraint_indexing:constraint_classification(antifragility, rope, 
    context(agent_power(individual_moderate), 
            time_horizon(biographical), 
            exit_options(arbitrage), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE BUREAUCRAT (TANGLED ROPE)
constraint_indexing:constraint_classification(antifragility, tangled_rope, 
    context(agent_power(institutional), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(national))).

% PERSPECTIVE 4: THE EVOLUTIONARY OBSERVER (MOUNTAIN)
constraint_indexing:constraint_classification(antifragility, mountain, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(analytical), 
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility, TypeModerate, context(agent_power(individual_moderate), _, _, _)),
    TypePowerless \= TypeModerate.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) reflects the "convexity bias" where the antifragile 
 * agent harvests upside while downside is externalized to the fragile.
 * * MANDATROPHY ANALYSIS:
 * Resolved via the Tangled Rope classification for the Institutional agent, 
 * showing how coordination of short-term order creates predatory long-term debt.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the 0.75 extraction a functional necessity for evolution or predatory?',
    'Audit of Skin in the Game metrics',
    'If necessity: Mountain. If predatory: Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional stressors (0.10) to performative 
% "Fragilista" stability theater (0.55).
narrative_ontology:measurement(anti_tr_t0, antifragility, theater_ratio, 0, 0.10).
narrative_ontology:measurement(anti_tr_t5, antifragility, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility, theater_ratio, 10, 0.55).

% Extraction: Tracking the intensification of risk-asymmetry as buffers 
% are removed from the fragile population.
narrative_ontology:measurement(anti_ex_t0, antifragility, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_ex_t5, antifragility, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_ex_t10, antifragility, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
