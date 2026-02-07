% ============================================================================
% CONSTRAINT STORY: metabolic_constraint_cognition
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(metabolic_constraint_cognition, []).

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
 * * constraint_id: metabolic_constraint_cognition
 * human_readable: The ATP Ceiling
 * domain: biological/cognitive
 * * SUMMARY:
 * This constraint represents the hard biological limit of the human brain's 
 * metabolic capacity. The brain consumes ~20% of body energy despite being 
 * 2% of mass; high-order cognition is metabolically expensive and 
 * subject to rapid fatigue. This functions as a Mountain for the individual, 
 * while being leveraged as a Snare by systems that induce "decision fatigue" 
 * to bypass critical thinking.
 * * KEY AGENTS:
 * - Cognitive Laborer: Subject (Powerless)
 * - Attention Architect: Beneficiary (Institutional)
 * - Neuro-metabolic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.79) as systems intentionally exhaust the subject's 
% limited glucose/ATP reserves to reduce resistance to predatory defaults.
domain_priors:base_extractiveness(metabolic_constraint_cognition, 0.79). 
domain_priors:suppression_score(metabolic_constraint_cognition, 0.60). 
domain_priors:theater_ratio(metabolic_constraint_cognition, 0.30). % Low theater; the exhaustion is physiologically real.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(metabolic_constraint_cognition, extractiveness, 0.79).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(metabolic_constraint_cognition, theater_ratio, 0.3).

% Biological limits are not temporary scaffolds.
% narrative_ontology:has_sunset_clause(metabolic_constraint_cognition). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the individual, the need for sleep and food to restore cognition is 
% an unalterable natural law.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutions view this limit as a Rope—a way to coordinate human 
% schedules and predict when labor/attention will be most compliant.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.79) triggers the hybrid Tangled Rope signature, 
% identifying the exploitation of biological necessity.
constraint_indexing:constraint_classification(metabolic_constraint_cognition, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(metabolic_constraint_cognition, E), E >= 0.50,
    domain_priors:suppression_score(metabolic_constraint_cognition, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metabolic_constraint_cognition_tests).

test(perspectival_gap) :-
    % Verify the Mountain vs Rope conflict at the biological level.
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, mountain, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metabolic_constraint_cognition, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.79) triggers the required logic resolution.
    domain_priors:base_extractiveness(metabolic_constraint_cognition, E),

    E > 0.70.

:- end_tests(metabolic_constraint_cognition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.79) reflects the "Mandatrophy" of cognitive labor, 
 * where systemic demands exceed the subject's biological replenishment rates.
 * 
 * * PERSPECTIVAL GAP:
 * The Laborer feels a Mountain because they cannot work 24/7; biology 
 * dictates an "exit" into sleep. The Architect sees a Rope because they 
 * can engineer "choice architecture" to hit users when their ATP is low, 
 * effectively ensuring coordination with the system's preferred outcomes.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Tangled Rope classification. This identifies that the 
 * system's "coordination" is built on the predatory drainage of the 
 * subject's metabolic optionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_metabolic_augmentation,
    'Can exogenous glucose or neural-interfacing expand the ATP ceiling (Mountain or Snare)?',
    'Longitudinal metabolic study of cognitive performance under varying caloric/stimulant loads.',
    'If ceiling rises: Snare of policy. If ceiling remains fixed: Mountain of Thermodynamics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(metabolic_constraint_cognition, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
