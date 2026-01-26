% ============================================================================
% CONSTRAINT STORY: STRAIT_COERCION_2025
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_strait_coercion, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: strait_coercion_2025
 * human_readable: Normalized Taiwan Strait Military Coercion
 * domain: political/military
 * * SUMMARY:
 * Chinese military activity around Taiwan has decoupled from external
 * political triggers (signaling) and shifted to internal readiness cycles
 * and training schedules (preparation). By 2025, presence became near-continuous,
 * [cite_start]with lulls driven by weather or domestic holidays rather than diplomacy. [cite: 1]
 * * KEY AGENTS:
 * - [cite_start]The Taiwanese Citizen: Subject (Powerless/Trapped in normalization) [cite: 1]
 * - [cite_start]PLA High Command: Beneficiary (Institutional/Decoupled) [cite: 1]
 * - [cite_start]ASPI / Strategic Analysts: Auditor (Analytical/Detection of lulls) [cite: 1]
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.55) as the activity imposes constant security costs
% and psychological pressure on Taiwan without a coordination benefit.
domain_priors:base_extractiveness(strait_coercion_2025, 0.55).
domain_priors:suppression_score(strait_coercion_2025, 0.90).   % Vanishingly rare lulls [cite: 1]
domain_priors:theater_ratio(strait_coercion_2025, 0.35).       % Shift from 'theater' (signaling) to functional prep [cite: 1]

% Binary flags
domain_priors:requires_active_enforcement(strait_coercion_2025).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TAIWANESE VOTER (PITON)
% The military presence is now so "normalized" that it no longer sways behavior;
% [cite_start] it is a spike driven into the daily reality of the Strait. [cite: 1]
constraint_indexing:constraint_classification(strait_coercion_2025, piton,
    context(agent_power(individual_powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PLA STRATEGISTS (ROPE)
% Internal training schedules and readiness cycles act as the primary
% [cite_start] coordination mechanism for joint operations. [cite: 1]
constraint_indexing:constraint_classification(strait_coercion_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: STRATEGIC ANALYST (TANGLED ROPE)
% Detects the extraction of security resources from Taiwan while acknowledging
% [cite_start] the genuine coordination of PLA internal rhythms. [cite: 1]
constraint_indexing:constraint_classification(strait_coercion_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(strait_coercion_2025, E), E >= 0.50.

% PERSPECTIVE 4: DIPLOMATIC SIGNALERS (SNARE)
% Traditional signaling is retroactively framed (opportunistic justification)
% [cite_start] to trap external actors into a reactive loop. [cite: 1]
constraint_indexing:constraint_classification(strait_coercion_2025, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strait_coercion_tests).

test(normalization_piton) :-
    % Verify the constraint is a Piton for the powerless due to normalization.
    constraint_indexing:constraint_classification(strait_coercion_2025, piton, context(agent_power(individual_powerless), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(strait_coercion_2025, E),
    E >= 0.46. % Triggers Snare/Tangled logic.

:- end_tests(strait_coercion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.55) reflects the constant drain on Taiwan's defensive
 * [cite_start]readiness[cite: 1]. The perspectival gap is significant: the institution (PLA)
 * sees a 'Rope' for internal training efficiency, while the subject (Taiwan)
 * sees a 'Piton'—an immutable, normalized fact of life that no longer
 * [cite_start]functions as a tactical signal[cite: 1].
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * Labeling this as a 'Tangled Rope' acknowledges the internal coordination
 * benefits for the PLA (training/testing) while preventing the system from
 * [cite_start]ignoring the aggressive extraction practiced against Taiwan[cite: 1].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_opportunistic_signaling,
    'Is the signaling purely retroactive (opportunistic) or are some spikes still genuine tactical responses?',
    'Correlation analysis of internal PLA command directives vs external triggers over a 5-year period.',
    'Retroactive = Pure Readiness Constraint; Tactical = Hybrid Signaling Constraint.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(strait_coercion_2025, 2025, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
