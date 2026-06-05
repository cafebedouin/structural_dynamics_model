% ============================================================================
% CONSTRAINT STORY: future_dsm_integration_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_future_dsm_integration_2026, []).

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
 * * constraint_id: future_dsm_integration
 * human_readable: Future DSM Strategic Vision (SCE-DoH & Intersectionality)
 * domain: technological/political
 * * SUMMARY:
 * The American Psychiatric Association (APA) has empaneled a subcommittee to 
 * integrate socioeconomic, cultural, and environmental determinants (SCE-DoH) 
 * and intersectionality into the DSM. This marks a transition from 
 * a focus on isolated symptoms to a framework accounting for systemic 
 * oppression, economic stability, and built environments as causal and 
 * prognostic factors in mental health.
 * * KEY AGENTS:
 * - Minoritized/Diverse Patients: Subject (Powerless)
 * - APA / Future DSM Strategic Committee: Beneficiary (Institutional)
 * - Future DSM Strategic Committee Subcommittee: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is moderate-high (0.55). The historical lack of context extracts 
% health equity by ignoring systemic risk and protective factors.
domain_priors:base_extractiveness(future_dsm_integration, 0.55). 

% Suppression is high (0.70). Traditional diagnostic frameworks suppress 
% lived experience in favor of standardized symptomatology.
domain_priors:suppression_score(future_dsm_integration, 0.70).   

% Theater ratio is moderate (0.40). Existing V and Z codes provide a 
% theatrical proxy for context without consistent clinical integration.
domain_priors:theater_ratio(future_dsm_integration, 0.40).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(future_dsm_integration, extractiveness, 0.55).
narrative_ontology:constraint_metric(future_dsm_integration, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(future_dsm_integration, theater_ratio, 0.4).

% Primary keys for the classification engine
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(future_dsm_integration, psychiatric_research_institutions).
narrative_ontology:constraint_victim(future_dsm_integration, historically_underserved_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For patients in marginalized communities, the lack of context-sensitive 
% diagnosis is a Snare: a trap where structural disadvantages are 
% misidentified as individual pathology.
constraint_indexing:constraint_classification(future_dsm_integration, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The APA views the integration as a Rope: a coordination tool to 
% establish a common language and common criteria across diverse 
% stakeholders and cultures.
constraint_indexing:constraint_classification(future_dsm_integration, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analysts view the current transition as a Tangled Rope: Genuine 
% coordination for diagnostic equity mixed with asymmetric extraction 
% of patient data for risk-adjusted modeling.
constraint_indexing:constraint_classification(future_dsm_integration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(future_dsm_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(future_dsm_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(future_dsm_integration, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(extraction_threshold) :-
    domain_priors:base_extractiveness(future_dsm_integration, E),
    E >= 0.46.

:- end_tests(future_dsm_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.55) reflects the "Sovereignty Gap" in mental 
 * health, where structural disadvantages (housing, poverty, racism) 
 * create vulnerabilities that current frameworks fail to quantify 
 * adequately. The shift toward "Risk-Adjusted Diagnostic Models" 
 * acknowledges that two patients with the same symptoms may warrant 
 * different intensities of care based on environmental stressors.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system identifies the Tangled Rope by distinguishing between the 
 * functional aim of "Diagnostic Accuracy" and the extractive burden 
 * placed on underfunded health systems to collect SCE-DoH data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_dsm_scalability,
    'Can SCE-DoH integration add clinical value without overburdening practice?',
    'Analysis of scalable components and monitorable objective pilots.',
    'Success creates a functional global Rope; failure results in a theatrical Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(future_dsm_integration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (modeling the drift from V/Z codes to integrated screening)
narrative_ontology:measurement(dsm_tr_t0, future_dsm_integration, theater_ratio, 0, 0.60).
narrative_ontology:measurement(dsm_tr_t5, future_dsm_integration, theater_ratio, 5, 0.45).
narrative_ontology:measurement(dsm_tr_t10, future_dsm_integration, theater_ratio, 10, 0.40).

% Extraction over time (reflecting the accumulation of contextual evidence)
narrative_ontology:measurement(dsm_ex_t0, future_dsm_integration, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(dsm_ex_t5, future_dsm_integration, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dsm_ex_t10, future_dsm_integration, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
