% ============================================================================
% CONSTRAINT STORY: viral_transmission_rates
% ============================================================================
% Generated: 2026-01-22
% Model: Gemini
% Source: Epidemiological Modeling / Public Health Policy
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_viral_transmission_rates, []).

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
 * * constraint_id: viral_transmission_rates
 * human_readable: Viral Transmission Dynamics (R0/Rt)
 * domain: technological/biological/political
 * temporal_scope: Pandemic Periods (e.g., 2020-2023)
 * spatial_scope: Global
 * * SUMMARY:
 * Viral transmission rates (R0/Rt) function as biological constraints that dictate 
 * the speed of pathogen spread. These rates force systemic societal 
 * reorganization, where biological necessity and political enforcement converge 
 * to extract labor, movement, and economic agency from the population.
 * * KEY AGENTS:
 * - The Epidemiologist: Analytical observer of mathematical growth laws.
 * - The Health Ministry: Institutional actor using transmission metrics as a coordination tool.
 * - The Precarious Worker: Individual powerless subject experiencing the rate as an extractive trap.
 * * NARRATIVE ARC:
 * Transmission begins as a "Mountain" of natural law. State intervention attempts 
 * to convert this into a "Rope" for coordination, but for the economically 
 * vulnerable, the policy-biological hybrid tightens into a "Snare".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(viral_transmission_rates, 0, 10).
narrative_ontology:constraint_claim(viral_transmission_rates, mountain).

% Base Properties
% Pathogens and lockdowns extract health and labor (0.7).
domain_priors:base_extractiveness(viral_transmission_rates, 0.7).
% Alternatives to isolation are actively suppressed or criminalized (0.8).
domain_priors:suppression_score(viral_transmission_rates, 0.8).

% Requires both biological emergence and active political maintenance.
domain_priors:emerges_naturally(viral_transmission_rates).
domain_priors:requires_active_enforcement(viral_transmission_rates).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(viral_transmission_rates, institutional_governance).
constraint_victim(viral_transmission_rates, individual_powerless).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE EPIDEMIOLOGIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical - Observer of biological laws.
   WHEN: historical - Evaluating outbreaks across decades.
   WHERE: analytical - Not personally constrained by policy.
   SCOPE: global - The virus ignores borders.
   
   WHY THIS CLASSIFICATION:
   To the analyst, R0 is a fixed feature of the biological landscape. Exponential 
   growth is non-negotiable; it is an unchangeable law of the environment.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    viral_transmission_rates,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HEALTH MINISTRY - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Rule-making power.
   WHEN: immediate - Managing 1-year hospital capacity.
   WHERE: arbitrage - Can shift resources to modify the rate.
   SCOPE: national - Country-wide mandates.
   
   WHY THIS CLASSIFICATION:
   For the institution, the effective reproduction number (Rt) is a tool for 
   population coordination. It justifies collective action to prevent systemic 
   collapse.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    viral_transmission_rates,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SMALL BUSINESS OWNER - Tangled Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate - Some agency, limited rule-shaping.
   WHEN: biographical - Lifetime economic impact.
   WHERE: constrained - Exit is costly (bankruptcy).
   SCOPE: regional - Local markets.
   
   WHY THIS CLASSIFICATION:
   The business owner needs the system to coordinate safety so customers return 
   (Rope), but the mandates asymmetrically extract their revenue and viability 
   (Snare).
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    viral_transmission_rates,
    tangled_rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE DAY LABORER - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - No savings, subject to mandates.
   WHEN: immediate - Survival depends on today's wages.
   WHERE: trapped - No physical or economic exit.
   SCOPE: local - Immediate worksite.
   
   WHY THIS CLASSIFICATION:
   The laborer is caught between infection risk and starvation. The policy 
   suppresses their ability to earn while the virus extracts their health. It 
   is a pure extractive trap.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    viral_transmission_rates,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(viral_transmission_rates_tests).

test(multi_perspective_variance) :-
    % Verify different indices yield different types
    constraint_indexing:constraint_classification(viral_transmission_rates, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(viral_transmission_rates, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(viral_transmission_rates, snare, context(agent_power(individual_powerless), _, _, _)).

test(power_extractiveness_scaling) :-
    % Powerless experience higher extraction than the institution
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, immediate, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(viral_transmission_rates, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(viral_transmission_rates, ContextPowerful, Score2),
    Score1 > Score2.

test(hybrid_tangled_rope_detection) :-
    % Ensure moderate power detects the coordination/extraction hybrid
    constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope, context(individual_moderate, biographical, constrained, regional)).

:- end_tests(viral_transmission_rates_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini
 * Date: 2026-01-22
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS (0.7):
 * The virus extracts biological life, while the lockdown extracts economic 
 * life. The combined effect is severely asymmetric.
 * * 2. PERSPECTIVE SELECTION:
 * Added Tangled Rope for the 'individual_moderate' to reflect the hybrid 
 * nature of pandemic policy (coordination vs. extraction).
 * * 3. MANDATROPHY RESOLUTION:
 * The [RESOLVED MANDATROPHY] status is justified by showing that while the 
 * system is a coordination tool (Rope) for the state, it is a Snare for 
 * the precarious worker.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    viral_transmission_extraction_intent,
    "Is the extraction of economic agency a functional necessity for biosecurity or a predatory centralization of power?",
    resolution_mechanism("Audit of state emergency power rollbacks vs. virus prevalence normalization"),
    impact("If necessity: Mountain (natural law). If predatory: Snare (Mandatrophy)."),
    confidence_without_resolution(medium)
).

omega_variable(
    herd_immunity_mechanism,
    "Is the threshold reached via natural infection (Snare) or vaccination (Rope)?",
    resolution_mechanism("Seroprevalence study comparison with vaccine uptake metrics"),
    impact("If natural: high victim extraction. If vaccine: coordination success."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Focused Protection
 * Viability: Shielding high-risk while low-risk maintain economy.
 * Suppression: Actively pathologized and excluded from official policy.
 * * CONCLUSION:
 * The active suppression of "Focused Protection" shifts the laborer's 
 * experience from a necessary Rope to an enforced Snare.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load into system: ?- [viral_transmission_rates].
% Run analysis: ?- constraint_indexing:multi_index_report(viral_transmission_rates).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
