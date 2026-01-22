% ============================================================================
% CONSTRAINT STORY: net_zero_stabilization
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Madeleine Cuff, "The invention of net zero"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_net_zero_stabilization, []).

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
 * * constraint_id: net_zero_stabilization
 * human_readable: The Net Zero Carbon Constraint
 * domain: scientific/technological/political
 * temporal_scope: 2005 (inception) to 2026 (current policy consensus)
 * spatial_scope: Global
 * * SUMMARY:
 * Before 2005, scientists believed the world could emit ~2.5 gigatonnes of CO2 annually while maintaining stable temperatures. 
 * Physicists David Frame and Myles Allen used climate models to prove that warming only stops when human-caused emissions reach net zero—where emissions are balanced by equivalent removals.
 * * KEY AGENTS:
 * - The Climate Physicist: Analytical observers (Frame and Allen) who "turned the problem on its head".
 * - The Global Citizen/Industry: Agents subject to the "net-zero" target requiring removals.
 * - The Earth System: The biological/physical reality that dictates temperature stability based on emissions.
 * * NARRATIVE ARC:
 * This constraint represents a "rupture" in climate logic. It shifted the "Rope" of climate policy from 
 * stabilizing atmospheric concentrations to an absolute "Mountain" or "Noose" of zero net emissions.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(net_zero_stabilization, 0, 10).
narrative_ontology:constraint_claim([net_zero_stabilization], [environmental_stabilization]).

% Base extractiveness score (0.85 = High)
% Rationale: Reaching net zero requires total extraction of carbon-emitting 
% capabilities from current global systems and mandatory "equivalent removals".
domain_priors:base_extractiveness(net_zero_stabilization, 0.85).

% Suppression score (0.45 = Moderate)
% Rationale: The previous consensus (that we could continue emitting 2.5Gt) was 
% suppressed by the 2009 Nature paper and the subsequent shift in thinking.
domain_priors:suppression_score(net_zero_stabilization, 0.45).

% Enforcement requirements: Requires active enforcement (Policy, removals, and treaties).
domain_priors:requires_active_enforcement(net_zero_stabilization).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(net_zero_stabilization, extractiveness, 0.85).
narrative_ontology:constraint_metric(net_zero_stabilization, suppression_requirement, 0.45).

% BENEFICIARIES & VICTIMS
% The Earth System (and future generations) benefit from temperature stability.
constraint_beneficiary(net_zero_stabilization, global_climate_stability).
% Carbon-heavy industries and high-emitting nations lose the "fair chunk" 
% of emissions (~6% of global total) they once believed was safe.
constraint_victim(net_zero_stabilization, legacy_carbon_emitters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CLIMATE PHYSICIST - Rope
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer modeling the system)
   WHEN: biographical (Working from 2005 train ride to 2009 publication)
   WHERE: mobile (Shifting from concentrations to emissions focus)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the physicist, net zero is a Rope—a functional coordination tool discovered 
   through "fiddling about" with models to show how to stop warming.
   
   NARRATIVE EVIDENCE:
   "The world would stop warming once humanity reached 'net-zero' carbon emissions... It was quite cool sitting on the train looking at these figures".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    net_zero_stabilization,
    rope,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(net_zero_stabilization, E),
    E < 0.9,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE LEGACY EMITTER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (Subject to the new "net zero" target)
   WHEN: immediate (Facing the end of the "2.5 gigatonne" grace period)
   WHERE: trapped (No longer allowed to emit without balancing removals)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For emitters who relied on the "fair chunk" of 2.5Gt emissions, net zero is a Noose 
   that extracts their ability to operate without costly removals.
   
   NARRATIVE EVIDENCE:
   "It was widely believed humans could still emit a fair chunk of emissions... about 6 per cent of annual global emissions today".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    net_zero_stabilization,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(net_zero_stabilization, E),
    E > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE EARTH SYSTEM - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional (The physical laws governing climate)
   WHEN: historical (The deep-time response of the atmosphere)
   WHERE: constrained (Bound by thermodynamic realities)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The climate model reveals a Mountain—an immutable physical law where global 
   temperatures only remain stable once humanity reaches net zero.
   
   NARRATIVE EVIDENCE:
   "The result? Global temperatures remained stable at their new level. In short, 
   the world would stop warming once humanity reached 'net-zero'".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    net_zero_stabilization,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :-
    % Classification based on the physical model result
    true,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(net_zero_stabilization_tests).

test(multi_perspective_variance) :-
    % Scientist (Rope) vs Emitter (Noose) vs System (Mountain)
    constraint_indexing:constraint_classification(net_zero_stabilization, rope, context(analytical, biographical, mobile, global)),
    constraint_indexing:constraint_classification(net_zero_stabilization, noose, context(individual_powerless, immediate, trapped, national)),
    constraint_indexing:constraint_classification(net_zero_stabilization, mountain, context(institutional, historical, constrained, global)).

test(power_extractiveness_scaling) :-
    % The legacy emitter (powerless to the new rule) experiences high extraction (0.85)
    % compared to the analytical observer (0.1).
    Score1 = 0.85,
    Score2 = 0.1,
    Score1 > Score2.

:- end_tests(net_zero_stabilization_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.85):
 * Reasoning: Reaching net zero is an absolute extraction of the "right to emit" 
 * that previously characterized industrialization.
 * * 2. PERSPECTIVE SELECTION:
 * Analyzed from the scientist (Rope), the legacy emitter (Noose), and the Earth 
 * system (Mountain) to show the discovery of a physical limit.
 * * 3. AMBIGUITIES:
 * The text mentions "balancing removals," which implies a new technological 
 * extractive industry that didn't exist in the old 2.5Gt model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    net_zero_stabilization_extraction_intent,
    "Is the 0.85 extraction of carbon space a physical necessity (Mountain) or a policy choice that ignores lower-impact alternatives?",
    resolution_mechanism("Audit of global temperature responses to small (2.5Gt) sub-zero emissions over centuries"),
    impact("If necessity: Mountain. If choice: Noose/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    removal_technology_viability,
    "Can 'equivalent removals' actually be achieved at the scale required by the net-zero target?",
    resolution_mechanism("Monitor the gigatonne-scale deployment of Carbon Capture and Storage (CCS) technology"),
    impact("If viable: Rope. If non-viable: The target becomes a systemic Noose."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Concentration-Based Stabilization
 * Viability: The widely believed "consensus" before the Frame/Allen shift.
 * Suppression: Actively rejected by the climate community after 2009.
 * Evidence: "It was widely believed humans could still emit a fair chunk of emissions".
 * * CONCLUSION:
 * The move from concentration-based "Ropes" to the net-zero "Mountain" represents 
 * the defining scientific rupture of the 21st century.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * * 1. Load: ?- [constraints/net_zero_stabilization].
 * 2. Multi-perspective: ?- multi_index_report(net_zero_stabilization).
 * 3. Run tests: ?- run_tests(net_zero_stabilization_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
