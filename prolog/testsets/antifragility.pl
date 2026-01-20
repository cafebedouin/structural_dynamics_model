% ============================================================================
% CONSTRAINT STORY: antifragility
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Nassim Nicholas Taleb (2012) / Risk Engineering
% ============================================================================

:- module(constraint_antifragility, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: antifragility
 * human_readable: Antifragility (Gaining from Disorder)
 * domain: technological/economic/biological
 * temporal_scope: Permanent (Universal Property)
 * spatial_scope: Global (Systems subject to Volatility)
 * * SUMMARY:
 * Antifragility is a property of systems that increase in capability, resilience, 
 * or robustness as a result of stressors, shocks, volatility, noise, mistakes, 
 * or failures. Unlike resilience (which resists shocks) or fragility (which 
 * breaks), antifragile systems actually require disorder to thrive and grow.
 * * KEY AGENTS:
 * - The Antifragile Practitioner: Seeks exposure to small, non-fatal stressors 
 * to build strength (e.g., the weightlifter or the venture capitalist).
 * - The Fragilista: The bureaucrat or planner who attempts to smooth out 
 * volatility, inadvertently making the system more fragile to large shocks.
 * - The Evolutionist: Nature itself, which uses the death of individuals to 
 * strengthen the species (the ultimate antifragile system).
 * * NARRATIVE ARC:
 * Antifragility acts as a "Rope" for those who understand how to harvest 
 * volatility. However, it becomes a "Noose" for those caught in fragile 
 * structures who are "extracted" by the system's volatility. To the 
 * observer of history, it is an immutable "Mountain" of evolutionary logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(antifragility_interval, 0, 10).
narrative_ontology:constraint_claim(antifragility, mountain).

% Base extractiveness: 0.4 (Moderate)
% Rationale: Antifragility often involves "Optionality." The antifragile agent 
% extracts disproportionate upside from volatility while leaving the downside 
% (shocks) to the environment or more fragile participants.
domain_priors:base_extractiveness(antifragility, 0.4).

% Suppression score: 0.6 (High)
% Rationale: "Fragilistas" (central planners) actively suppress volatility 
% and noise, which are the very inputs antifragile systems need. This 
% suppression hides the fragility of the system until a Black Swan event occurs.
domain_priors:suppression_score(antifragility, 0.6).

% Enforcement: Emerges naturally in biological/complex systems but requires 
% active effort to maintain in human-designed hierarchies.
domain_priors:emerges_naturally(antifragility).

% Metrics for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(antifragility, extractiveness, 0.4).
narrative_ontology:constraint_metric(antifragility, suppression_requirement, 0.6).

% Beneficiaries: Biological life, decentralized markets, and "Optionality" holders.
constraint_beneficiary(antifragility, antifragile_practitioner).

% Victims: Highly optimized, centralized, and debt-heavy "fragile" institutions.
constraint_victim(antifragility, fragilistas).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EVOLUTIONARY BIOLOGIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (Scientific observer).
   WHEN: civilizational (Long-term species survival).
   WHERE: trapped (Natural selection is a fixed law).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   From an evolutionary standpoint, antifragility is a Mountain. It is an 
   immutable law of nature. Species must be antifragile to survive 
   environmental shifts; those that are merely resilient eventually vanish. 
   Disorder is the natural landscape, not an obstacle.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    antifragility,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BARBELL INVESTOR - ROPE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A participant with strategic agency).
   WHEN: biographical (Achieving wealth/health in a lifetime).
   WHERE: arbitrage (Can move capital/attention between fragile and antifragile).
   SCOPE: national/regional.
   
   WHY THIS CLASSIFICATION:
   For the practitioner, antifragility is a Rope. It is a coordination tool 
   for leveraging reality. By building "Optionality" and a "Barbell 
   Strategy" (extreme safety + extreme risk), they use the system's 
   volatility to pull themselves upward while others are crushed by it.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    antifragility,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BUREAUCRATIC PLANNER - NOOSE
   --------------------------------------------------------------------------
   
   WHO: institutional (In charge of maintaining "order").
   WHEN: immediate (Quarterly stability and political optics).
   WHERE: constrained (Bound by rigid rules and expectations of stability).
   SCOPE: national.
   
   WHY THIS CLASSIFICATION:
   To the planner, antifragility is a Noose. The very volatility that 
   strengthens the system is seen as a threat to their job and the 
   perceived stability of the institution. As they try to "fix" the 
   disorder, the system's hidden fragility tightens, eventually leading 
   to a catastrophic "snap" when a large enough stressor arrives.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    antifragility,
    noose,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(antifragility_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(antifragility, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(antifragility, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(antifragility, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(volatility_extraction) :-
    % Antifragile agents extract "upside" from disorder.
    domain_priors:base_extractiveness(antifragility, E),
    E >= 0.4.

test(immutability_by_horizon) :-
    % Short term (Immediate) = Noose (struggle against disorder); 
    % Long term (Civilizational) = Mountain (the way things work).
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(antifragility_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SELECTION: Chose to contrast the "Practitioner" who loves 
 * volatility (Rope) with the "Planner" who fears it (Noose).
 * 2. EXTRACTIVENESS: Set to 0.4 because antifragility is asymmetric—the 
 * antifragile agent gains from others' failures (extraction of information 
 * or capital).
 * 3. NOOSE LOGIC: The "Noose" here is the psychological and institutional 
 * pressure to eliminate noise, which ironically makes the inevitable 
 * collapse more severe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    stressor_threshold_limit,
    "At what point does a stressor transition from 'antifragile-building' 
    (Hormesis) to 'catastrophically destructive' (Fragility)?",
    resolution_mechanism("Non-linear stress-response mapping in complex systems"),
    impact("If known: We can build perfect Ropes. If unknown: Antifragility 
    remains a dangerous Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Robustness (Resisting change)
 * Viability: High in the short term, but fails in the long term (Lindy effect).
 * Suppression: Moderate. Our culture over-values "resilience" over "antifragility."
 * * ALTERNATIVE 2: Redundancy (Having backups)
 * Viability: High. A key component of antifragility, but often seen as 
 * "inefficient" by planners.
 * * CONCLUSION:
 * Because "Antifragility" is often counter-intuitive and suppressed in favor 
 * of "Optimization" (Fragility), the principle acts as a Noose for those 
 * trapped in highly optimized systems.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_antifragility].
 * 2. Report: ?- multi_index_report(antifragility).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
