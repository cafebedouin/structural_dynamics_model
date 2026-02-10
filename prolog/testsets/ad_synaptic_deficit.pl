% ============================================================================
% CONSTRAINT STORY: ad_synaptic_deficit
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(ad_synaptic_deficit, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ad_synaptic_deficit
 * human_readable: Synaptic Liquidation (Neuro-Degenerative Debt)
 * domain: biological/technological
 * * SUMMARY:
 * This constraint models the irreversible liquidation of synaptic connectivity 
 * within a biological or synthetic neural network. It tracks the drift from 
 * functional metabolic pruning (Rope) to an inescapable structural collapse 
 * (Snare) where the network's processing capacity is extracted to sustain 
 * baseline metabolic maintenance.
 * * KEY AGENTS:
 * - Neural Integrity: Subject (Powerless)
 * - Metabolic Efficiency: Beneficiary (Institutional)
 * - Systems Archaeologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ad_synaptic_deficit, 0.85). 
domain_priors:suppression_score(ad_synaptic_deficit, 0.95).   % Severe suppression; no biological exit.
domain_priors:theater_ratio(ad_synaptic_deficit, 0.10).       % Primarily functional biological loss.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ad_synaptic_deficit, extractiveness, 0.85).
narrative_ontology:constraint_metric(ad_synaptic_deficit, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(ad_synaptic_deficit, theater_ratio, 0.1).

% Constraint classification claim
narrative_ontology:constraint_claim(ad_synaptic_deficit, mountain).

% Mandatory keys for classification engine v3.4
% These resolve the [FAIL] Schema mismatch by anchoring the measurement keys.
domain_priors:requires_active_enforcement(ad_synaptic_deficit).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(ad_synaptic_deficit, metabolic_efficiency).
narrative_ontology:constraint_victim(ad_synaptic_deficit, neural_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL (SNARE)
% [RESOLVED PERSPECTIVAL_GAP]: Powerless agents see a predatory trap liquidating agency.
constraint_indexing:constraint_classification(ad_synaptic_deficit, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTION (ROPE)
% [RESOLVED PERSPECTIVAL_GAP]: The system views pruning as essential coordination for survival.
constraint_indexing:constraint_classification(ad_synaptic_deficit, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% [RESOLVED ONTOLOGICAL_MISMATCH]: Corrects false_summit by identifying irreducible law.
constraint_indexing:constraint_classification(ad_synaptic_deficit, mountain, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(analytical), 
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ad_synaptic_deficit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ad_synaptic_deficit, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ad_synaptic_deficit, rope, context(agent_power(institutional), _, _, _)).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ad_synaptic_deficit, E),
    E >= 0.80.

:- end_tests(ad_synaptic_deficit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects the near-total capture of synaptic agency.
 * The schema mismatch was caused by list-wrapping values in measurement/5 facts.
 * * MANDATROPHY ANALYSIS:
 * Resolved via the Rope classification for institutional agents, acknowledging 
 * that the pruning process coordinates metabolic stability even while 
 * extracting individual connectivity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_synaptic_regeneration,
    'Is the deficit a Mountain of biological decay or a Snare of metabolic mismanagement?',
    'Audit of ATP-resupply vectors in early-stage deficit cohorts.',
    'If ATP restores function: Snare. If not: Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ad_synaptic_deficit, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Fixed raw floats (Remove square brackets) to resolve [VALUE ERROR].
narrative_ontology:measurement(ad_syn_tr_t0, ad_synaptic_deficit, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ad_syn_tr_t5, ad_synaptic_deficit, theater_ratio, 5, 0.10).
narrative_ontology:measurement(ad_syn_tr_t10, ad_synaptic_deficit, theater_ratio, 10, 0.10).

% Extraction: Tracking the intensification of synaptic liquidation.
narrative_ontology:measurement(ad_syn_ex_t0, ad_synaptic_deficit, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ad_syn_ex_t5, ad_synaptic_deficit, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(ad_syn_ex_t10, ad_synaptic_deficit, base_extractiveness, 10, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
