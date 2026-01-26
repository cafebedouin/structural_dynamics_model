% ============================================================================
% CONSTRAINT STORY: rotation_seven_isolation
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "Rotation Seven" narrative
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_rotation_seven_isolation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: rotation_seven_isolation
 * human_readable: R7 Medical Isolation Protocol
 * domain: political/technological/social
 * temporal_scope: Rotation Era
 * spatial_scope: Rotation Seven Station
 * 
 * SUMMARY:
 * The R7 Protocol enforces the immediate removal and isolation of "flagged" 
 * individuals. While it presents itself as an unchangeable safety necessity 
 * (Mountain), it is a highly enforced, extractive mechanism (Snare) that 
 * removes social "friction" by erasing the sick from the station's population. 
 * 
 * KEY AGENTS:
 * - Anna (Individual Powerless): The subject being "extracted" and isolated.
 * - Nurse Chen (Institutional): The enforcer maintaining the "Rope" of safety protocols.
 * - Rina (Analytical): The observer who identifies the underlying "Snare" of the protocol.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(rotation_seven_isolation, 0, 10).
narrative_ontology:constraint_claim(rotation_seven_isolation, snare).

% Base extractiveness: 0.85 (Severe asymmetry)
% It extracts a human life/social unit and replaces it with a new "reassignment" 
% within 6 hours.
domain_priors:base_extractiveness(rotation_seven_isolation, 0.85).

% Suppression: 0.70 (High)
% Alternatives (onsite care) are suppressed by claims of impossibility.
domain_priors:suppression_score(rotation_seven_isolation, 0.70).

% Enforcement: Requires active maintenance.
% Requires card-swipes, isolation wards, and wristband monitoring.
domain_priors:requires_active_enforcement(rotation_seven_isolation).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(rotation_seven_isolation, station_health_metrics).
constraint_victim(rotation_seven_isolation, isolated_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ANNA - Snare (False Mountain)
   --------------------------------------------------------------------------
   WHO: individual_powerless (The subject being "extracted" and isolated)
   WHEN: immediate (The moment of flagging and removal)
   WHERE: trapped (By active enforcement like locked doors and wristband monitoring)
   
   WHY THIS CLASSIFICATION:
   Anna perceives a "Mountain" because the protocol is unyielding, but 
   structurally it is a 'Snare'. She is trapped by active enforcement 
   (locked doors) and extractiveness (her removal). The "Mountain" 
   appearance is a byproduct of her total lack of power and agency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_isolation,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: NURSE CHEN - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The enforcer maintaining the "Rope" of safety protocols)
   WHEN: historical (Upholding station-wide health standards)
   WHERE: constrained (Bound by the rigid protocols of the R7 system)
   
   WHY THIS CLASSIFICATION:
   To Nurse Chen, the protocol is a functional 'Rope' used to coordinate 
   station-wide health and safety. She views the extraction of the sick as 
   necessary for the survival of the collective, using the protocol as a
   tool to maintain order and prevent contagion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_isolation,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: RINA - Snare
   --------------------------------------------------------------------------
   WHO: analytical (The observer who identifies the underlying "Snare" of the protocol)
   WHEN: biographical (Personal analysis of the protocol's impact)
   WHERE: trapped (By the station's logic of erasure)
   
   WHY THIS CLASSIFICATION:
   Rina's analysis identifies the "Snare" signature: high extractiveness 
   masked by a narrative of necessity. She sees the "erasure" of Anna 
   as a coercive act of the station's logic, a system that strangles
   individual rights for perceived collective safety.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_isolation,
    snare,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rotation_seven_isolation_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(rotation_seven_isolation, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_isolation, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_isolation, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(rotation_seven_isolation_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. MANDATROPHY STATUS: High extractiveness (0.85) and suppression (0.70)
 *    trigger the mandatrophy protocol. This is 'RESOLVED' by recognizing that
 *    what is a 'Rope' (safety) for the institutional enforcer (Nurse Chen)
 *    is a 'Snare' for the individual (Anna) and the analytical observer (Rina).
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Anna (Snare): Forced isolation and erasure.
 *    - Nurse Chen (Rope): Protocol as a tool for collective health.
 *    - Rina (Snare): Protocol as a coercive act of systemic logic.
 * 
 * 3. CORE INSIGHT: The R7 Isolation Protocol exemplifies a severe perspectival
 *    asymmetry. What is presented as an unavoidable 'Mountain' of safety is,
 *    in practice, a 'Snare' for the individual, even as it functions as a
 *    'Rope' for institutional control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    protocol_legitimacy_vs_humanity,
    "At what point does the 'Rope' of collective safety (for Nurse Chen) become a 'Snare' of inhumanity for the isolated individual (Anna), and can this trade-off be quantitatively managed?",
    resolution_mechanism("Development of a 'Humanity Index' that measures the psychological and social impact of isolation protocols against epidemiological benefits. Ongoing ethical review boards and longitudinal studies of isolated individuals."),
    impact("If index too low: The 'Rope' is revealed as a 'Snare'. If balanced: The 'Rope' remains legitimate."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Onsite Medical Care / Advanced Treatment
 *    Viability: Providing full medical support and humane care within the general station population.
 *    Suppression: Explicitly suppressed by the protocol's claim of "impossibility" due to resource constraints or contagion risk, making isolation the only perceived option.
 *
 * CONCLUSION:
 * The R7 Isolation Protocol is a 'Snare' for the individual, created by the
 * suppression of more humane, albeit resource-intensive, alternatives. What
 * is presented as a 'Mountain' of necessity is a choice to prioritize
 * "station health metrics" over individual well-being.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/rotation_seven_isolation].
 * 2. Multi-perspective: ?- multi_index_report(rotation_seven_isolation).
 * 3. Run tests: ?- run_tests(rotation_seven_isolation_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */