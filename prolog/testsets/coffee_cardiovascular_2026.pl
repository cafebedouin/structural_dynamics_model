% ============================================================================
% CONSTRAINT STORY: coffee_cardiovascular_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_coffee_cardiovascular_2026, []).

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
 * * constraint_id: coffee_cardiovascular_2026
 * human_readable: The Caffeine Paradox Realignment
 * domain: medical/health
 * * SUMMARY:
 * For years, cardiologists cautioned against coffee for patients with atrial 
 * fibrillation (AF). New randomized trial data (Feb 2026) reveals that daily 
 * coffee reduces AF recurrence by 17% and improves metabolic activity. This 
 * transforms coffee from a "Snare" of jitter-inducing risk into a "Rope" of 
 * protective coordination for heart health and gut microbiome diversity.
 * * KEY AGENTS:
 * - AF Patients/Coffee Drinkers: Subject (Powerless against previous medical dogma)
 * - Cardiologists (Adelaide Study): Beneficiary (Institutional - New Knowledge)
 * - British Heart Foundation: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is low (0.12). The shift to coffee-as-protection reduces the 
% economic and social "tax" of medical over-restriction.
domain_priors:base_extractiveness(coffee_cardiovascular_2026, 0.12). 

% Suppression is moderate (0.40) as the new evidence suppresses the 
% long-held "jittery heart" dogma and restrictive clinical advice.
domain_priors:suppression_score(coffee_cardiovascular_2026, 0.40).   

% Theater ratio is low (0.10) because the "coin-flip" randomized trial 
% provides functional clinical data over performative health guru advice.
domain_priors:theater_ratio(coffee_cardiovascular_2026, 0.10).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, extractiveness, 0.12).
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(coffee_cardiovascular_2026, theater_ratio, 0.1).

% Constraint classification claim
narrative_ontology:constraint_claim(coffee_cardiovascular_2026, scaffold).
narrative_ontology:human_readable(coffee_cardiovascular_2026, "The Caffeine Paradox Realignment").

% Primary keys for the classification engine
% High-fidelity stakeholders
narrative_ontology:constraint_beneficiary(coffee_cardiovascular_2026, public_health_outcomes).
narrative_ontology:constraint_victim(coffee_cardiovascular_2026, medical_precautionary_dogma).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FORMER PATIENT (SNARE)
% Previously, patients viewed caffeine as a Snare: an addictive substance 
% that trapped them in a cycle of jitters and heart risk.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE MODERN CLINICIAN (ROPE)
% Modern cardiologists see daily coffee as a Rope: essential coordination 
% for weight loss, reduced inflammation, and a 17% reduction in AF recurrence.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE BIO-SYSTEM AUDITOR (SCAFFOLD)
% Analysts view this as a Scaffold: a transitional discovery that 
% supports the "Gut-Heart Axis" while full mechanisms are mapped.
constraint_indexing:constraint_classification(coffee_cardiovascular_2026, scaffold,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(coffee_cardiovascular_2026).

% Mandatory sunset clause for transitional Scaffold
narrative_ontology:has_sunset_clause(coffee_cardiovascular_2026).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coffee_cardiovascular_2026_tests).

test(low_extraction_validation) :-
    domain_priors:base_extractiveness(coffee_cardiovascular_2026, E), E < 0.20.

test(functional_signal) :-
    domain_priors:theater_ratio(coffee_cardiovascular_2026, TR), TR < 0.15.

test(perspectival_shift) :-
    % Verify the shift from Snare (Powerless) to Rope (Institutional)
    constraint_indexing:constraint_classification(coffee_cardiovascular_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coffee_cardiovascular_2026, rope, context(agent_power(institutional), _, _, _)).

:- end_tests(coffee_cardiovascular_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.12) is low because coffee is an abundant 
 * dietary component that provides non-extractive biological benefits. 
 * The low Theater Ratio (0.10) is justified by randomized trial 
 * results that found 17% lower risk in AF recurrence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_gut_heart_correlation,
    'Does the 6-8x increase in gut-friendly microbes directly prevent atherosclerosis?',
    'BHF-funded longitudinal study of gut bacteria and artery hardening.',
    'Confirmation establishes a permanent biological Rope for heart health.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coffee_cardiovascular_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio drops as anecdotal "jitter" warnings are replaced by clinical data.
narrative_ontology:measurement(cf_tr_t0, coffee_cardiovascular_2026, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cf_tr_t5, coffee_cardiovascular_2026, theater_ratio, 5, 0.20).
narrative_ontology:measurement(cf_tr_t10, coffee_cardiovascular_2026, theater_ratio, 10, 0.10).

% Extraction (medical restriction) drops as coffee is identified as a health aid.
narrative_ontology:measurement(cf_ex_t0, coffee_cardiovascular_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cf_ex_t5, coffee_cardiovascular_2026, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(cf_ex_t10, coffee_cardiovascular_2026, base_extractiveness, 10, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
