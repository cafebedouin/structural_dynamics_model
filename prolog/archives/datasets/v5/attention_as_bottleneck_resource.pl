% ============================================================================
% CONSTRAINT STORY: attention_as_bottleneck_resource
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(attention_as_bottleneck_resource, []).

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
 * * constraint_id: attention_as_bottleneck_resource
 * human_readable: The Finite Cognitive Aperture
 * domain: social/technological/economic
 * * SUMMARY:
 * A scenario where the absolute biological limit of human attention becomes 
 * the primary bottleneck for all social and economic coordination. This 
 * "Rope" for organizing priority and value in an information-dense world 
 * becomes a "Snare" as competing autonomous agents and platforms 
 * aggressively liquidate the subject's cognitive surplus, trapping them 
 * in a state of perpetual distraction and decision fatigue.
 * * KEY AGENTS:
 * - Cognitive Laborer: Subject (Powerless)
 * - Attention Capture Platform: Beneficiary (Institutional)
 * - Human Factors Engineer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(attention_as_bottleneck_resource, 0.87). 
domain_priors:suppression_score(attention_as_bottleneck_resource, 0.76). 
domain_priors:theater_ratio(attention_as_bottleneck_resource, 0.84).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, extractiveness, 0.87).
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, theater_ratio, 0.84).

% Constraint metric facts — primary keys used by the classification engine v3.4.
% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(attention_as_bottleneck_resource, attention_capture_platform).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, cognitive_laborer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: their biological attention is a fixed asset being 
% mined by high-frequency nudges they cannot ignore.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the bottleneck as a Rope—the essential coordination 
% substrate for allocating visibility and value in an infinite data landscape.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.84) > 0.70 triggers Piton: "Attention Management" tools 
% are inert spikes; they perform the ritual of care without restoring agency.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.87) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(attention_as_bottleneck_resource, E), E >= 0.50,
    domain_priors:suppression_score(attention_as_bottleneck_resource, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_as_bottleneck_resource_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless laborer vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.84) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.87) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(attention_as_bottleneck_resource, E),
    E > 0.70.

:- end_tests(attention_as_bottleneck_resource_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * "coordination" of social attention has effectively consumed the 
 * cognitive substrate of the subject population.
 * 
 * * PERSPECTIVAL GAP:
 * The Cognitive Laborer feels a Snare because their life is a sequence of 
 * involuntary task-switches. The Platform sees a Rope because the 
 * bottleneck is the only way to coordinate a legible economy of merit 
 * in a zero-marginal-cost information environment.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "personalized notification" system is no longer functional 
 * (Theater 0.84); it is an inert spike siphoning 0.87 of the subject's agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_cognitive_saturation,
    'Can human attention be augmented, or is the bottleneck biological (Snare vs Mountain)?',
    'Tracking the delta between information intake and decision quality over 10 years.',
    'If quality plateaus: Mountain of Human Biology. If it rises: Snare of current interface.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(attention_as_bottleneck_resource, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional notification management (0.25) 
% to inertial "Digital Well-being" theater (0.84).
narrative_ontology:measurement(attn_tr_t0, attention_as_bottleneck_resource, theater_ratio, 0, 0.25).
narrative_ontology:measurement(attn_tr_t5, attention_as_bottleneck_resource, theater_ratio, 5, 0.55).
narrative_ontology:measurement(attn_tr_t10, attention_as_bottleneck_resource, theater_ratio, 10, 0.84).

% Extraction: Progressive accumulation of cognitive load as the 
% "Attention Economy" liquidates the subject's biological surplus.
narrative_ontology:measurement(attn_ex_t0, attention_as_bottleneck_resource, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(attn_ex_t5, attention_as_bottleneck_resource, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(attn_ex_t10, attention_as_bottleneck_resource, base_extractiveness, 10, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
