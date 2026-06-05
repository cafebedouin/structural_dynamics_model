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

% --- Namespace Hooks (Updated for v3.4) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
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
   2. CORE SYSTEM INTEGRATION (REVISED)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(viral_transmission_rates, 0, 10).
narrative_ontology:constraint_claim(viral_transmission_rates, mountain).

% Base Properties
% Rationale: Pathogens and lockdowns extract biological and economic life (0.70).
domain_priors:base_extractiveness(viral_transmission_rates, 0.70). 
domain_priors:suppression_score(viral_transmission_rates, 0.80).   
domain_priors:theater_ratio(viral_transmission_rates, 0.40).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(viral_transmission_rates, extractiveness, 0.7).
narrative_ontology:constraint_metric(viral_transmission_rates, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(viral_transmission_rates, theater_ratio, 0.4).

% Mandatory keys for classification engine v3.4
% These resolve the [FAIL] Schema mismatch by anchoring the measurement keys.
domain_priors:requires_active_enforcement(viral_transmission_rates).
domain_priors:emerges_naturally(viral_transmission_rates).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(viral_transmission_rates, institutional_governance).
narrative_ontology:constraint_victim(viral_transmission_rates, precarious_laborers).

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
   WHO: powerless - No savings, subject to mandates.
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
        agent_power(powerless),
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
    constraint_indexing:constraint_classification(viral_transmission_rates, snare, context(agent_power(powerless), _, _, _)).

test(power_extractiveness_scaling) :-
    % Powerless experience higher extraction than the institution
    ContextPowerless = context(powerless, immediate, trapped, local),
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
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Corrected to raw floats to resolve schema mismatch.
narrative_ontology:measurement(viral_tr_t0, viral_transmission_rates, theater_ratio, 0, 0.10).
narrative_ontology:measurement(viral_tr_t5, viral_transmission_rates, theater_ratio, 5, 0.45).
narrative_ontology:measurement(viral_tr_t10, viral_transmission_rates, theater_ratio, 10, 0.40).

% Extraction: Tracking the intensification of biosecurity extraction.
narrative_ontology:measurement(viral_ex_t0, viral_transmission_rates, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(viral_ex_t5, viral_transmission_rates, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(viral_ex_t10, viral_transmission_rates, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. INTEGRATION HOOKS
   ========================================================================== */

% Load into system: ?- [viral_transmission_rates].
% Run analysis: ?- constraint_indexing:multi_index_report(viral_transmission_rates).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
