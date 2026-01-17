% ============================================================================
% CONSTRAINT STORY: protocol_r7_isolation (REFINED v2.0)
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini 2.0 Flash
% Source: "Rotation Seven" narrative
% ============================================================================

:- module(protocol_r7_isolation_v2, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor
narrative_ontology:interval(protocol_r7_isolation, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: protocol_r7_isolation
 * human_readable: R7 Medical Isolation Protocol
 * domain: political_technological
 * temporal_scope: Rotation Era
 * spatial_scope: Rotation Seven Station
 * * SUMMARY:
 * The R7 Protocol enforces the immediate removal and isolation of "flagged" 
 * individuals. While it presents itself as an unchangeable safety necessity 
 * (Mountain), it is a highly enforced, extractive mechanism (Noose) that 
 * removes social "friction" by erasing the sick from the station's population. 
 * * KEY AGENTS:
 * - Anna (Powerless): The subject being "extracted."
 * - Nurse Chen (Institutional): The enforcer maintaining the "Rope" of safety.
 * - Rina (Analytical): The observer who identifies the "Noose."
 */

/* ==========================================================================
   2. BASE PROPERTIES (Refined per Audit)
   ========================================================================== */

% Base extractiveness: 0.85 (Severe asymmetry)
% It extracts a human life/social unit and replaces it with a new "reassignment" 
% within 6 hours. 
domain_priors:base_extractiveness(protocol_r7_isolation, 0.85).

% Suppression: 0.70 (High)
% Alternatives (onsite care) are suppressed by claims of impossibility. 
domain_priors:suppression_score(protocol_r7_isolation, 0.70).

% Enforcement: Requires active maintenance.
% Requires card-swipes, isolation wards, and wristband monitoring. 
domain_priors:requires_active_enforcement(protocol_r7_isolation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ANNA - Noose (False Mountain)
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped
   
   WHY THIS CLASSIFICATION:
   Anna perceives a "Mountain" because the protocol is unyielding, but 
   structurally it is a Noose. She is trapped by active enforcement 
   (locked doors) and extractiveness (her removal). The "Mountain" 
   appearance is a byproduct of her total lack of power.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    protocol_r7_isolation,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NURSE CHEN - Rope
   --------------------------------------------------------------------------
   WHO: institutional
   WHEN: historical
   WHERE: constrained
   
   WHY THIS CLASSIFICATION:
   To Chen, the protocol is a functional "Rope" used to coordinate 
   station-wide health. She views the extraction of the sick as 
   necessary for the survival of the collective. 
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    protocol_r7_isolation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: RINA - Noose
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: biographical
   WHERE: trapped
   
   WHY THIS CLASSIFICATION:
   Rina's analysis identifies the "Noose" signature: high extractiveness 
   masked by a narrative of necessity. She sees the "erasure" of Anna 
   as a coercive act of the station's logic. 
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    protocol_r7_isolation,
    noose,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(protocol_r7_v2_tests).

test(false_mountain_resolution) :-
    % Powerless agents see it as a Noose, not a Mountain, 
    % due to active enforcement.
    domain_priors:requires_active_enforcement(protocol_r7_isolation),
    constraint_indexing:constraint_classification(protocol_r7_isolation, noose, context(agent_power(individual_powerless), _, _, _)).

test(perspectival_gap) :-
    % Validation: Institutional sees Rope, Powerless sees Noose.
    constraint_indexing:constraint_classification(protocol_r7_isolation, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(protocol_r7_isolation, noose, context(agent_power(individual_powerless), _, _, _)).

:- end_tests(protocol_r7_v2_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================= */

/**
 * LLM GENERATION NOTES
 * * KEY DECISIONS:
 * 1. FIXED TYPE 1 FALSE MOUNTAIN: Anna's perspective is now correctly 
 * mapped to 'noose'. The subjective "unyieldingness" is treated as 
 * a psychological effect of the Noose's suppression.
 * 2. MODULE UNIQUENESS: Module name set to 'protocol_r7_isolation_v2' 
 * to avoid collisions with historical tax test sets.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * CONCLUSION:
 * The protocol’s status as a "Noose" is confirmed by the immediate 
 * reassignment of Anna's bunk space. A "Rope" (true coordination) 
 * would seek to maintain the social unit; the R7 Noose seeks only 
 * to maintain the station's biological metrics. 
 */

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================
