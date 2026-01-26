% ============================================================================
% CONSTRAINT STORY: antifragility
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini 2.0 Flash
% Source: Nassim Nicholas Taleb (2012) / Risk Engineering / Systems Theory
% Status: [RESOLVED MANDATROPHY]
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
    narrative_ontology:interval/3,
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
 * temporal_scope: Permanent / Evolutionary Time
 * spatial_scope: Global / Universal
 * * SUMMARY:
 * Antifragility describes systems that increase in capability or resilience 
 * in response to stressors, shocks, and volatility. Unlike the robust (which 
 * resists) or the fragile (which breaks), the antifragile requires disorder 
 * to thrive.
 * * KEY AGENTS:
 * - The Practitioner: Individuals using convex strategies to gain from chaos.
 * - The Fragilista: Institutional actors who suppress volatility to ensure 
 * short-term stability, creating hidden systemic debt.
 * - The Evolutionary Process: The civilizational backdrop where failures 
 * inform the strength of the whole.
 * * NARRATIVE ARC:
 * Antifragility functions as a natural law (Mountain) for the species, a 
 * strategic tool (Rope) for the informed agent, and a tightening trap 
 * (Snare) for those embedded in over-optimized, fragile institutions.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(antifragility, 0, 10).
narrative_ontology:constraint_claim(antifragility, mountain).

% Base extractiveness: 0.75 (High)
% Rationale: Antifragility in social systems is often "predatory" in its 
% asymmetry; the antifragile agent harvests the upside of volatility 
% while the downside is externalized to the fragile (victims).
domain_priors:base_extractiveness(antifragility, 0.75).

% Suppression score: 0.65 (High)
% Rationale: Institutional "Fragilistas" actively suppress small stressors 
% (volatility), making the underlying antifragile nature of reality 
% invisible until a catastrophic Black Swan event occurs.
domain_priors:suppression_score(antifragility, 0.65).

% Enforcement: Emerges naturally in biological systems but requires 
% active institutional suppression to "hide" it in social systems.
domain_priors:emerges_naturally(antifragility).

% Mandatory Asymmetry Hooks
constraint_beneficiary(antifragility, antifragile_practitioner).
constraint_beneficiary(antifragility, species_level_evolution).
constraint_victim(antifragility, fragile_institutions).
constraint_victim(antifragility, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE OPTIMIZED SERF - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_powerless (Subject to system shocks with no buffer)
   WHEN: biographical (One lifetime of labor/savings)
   WHERE: trapped (Debt-bound or specialized into a single fragile niche)
   SCOPE: local/national
   
   WHY THIS CLASSIFICATION:
   For the individual trapped in an "efficient" system, antifragility is a 
   Snare. They lack "optionality." When volatility strikes, they bear the 
   downside (job loss, inflation) while the system "learns" and 
   reconfigures at their expense.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(antifragility, snare, 
    context(agent_power(individual_powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(national))).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BARBELL PRACTITIONER - ROPE
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has agency and strategic buffers)
   WHEN: biographical (Lifetime wealth/skill building)
   WHERE: arbitrage (Can move between systems/assets)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the savvy practitioner, it is a Rope. By maintaining a "Barbell" 
   position (extreme safety + extreme risk/optionality), they use 
   volatility as a coordination mechanism to pull themselves into 
   higher states of capability.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(antifragility, rope, 
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(arbitrage), spatial_scope(global))).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE FRAGILISTA / BUREAUCRAT - TANGLED ROPE
   --------------------------------------------------------------------------
   WHO: institutional (Rule-making and stability-focused)
   WHEN: immediate (Quarterly cycles/elections)
   WHERE: constrained (Bound by KPIs and optics)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   The Bureaucrat sees a Tangled Rope. They use "stabilization" (coordination) 
   to maintain order, but this very act extracts future stability to pay 
   for current calm. It is a hybrid of necessary management and 
   unintentional predatory extraction of systemic resilience.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(antifragility, tangled_rope, 
    context(agent_power(institutional), time_horizon(immediate), exit_options(constrained), spatial_scope(national))) :-
    domain_priors:base_extractiveness(antifragility, E), E > 0.4,
    domain_priors:suppression_score(antifragility, S), S > 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE EVOLUTIONARY OBSERVER - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: analytical (Observing long-run dynamics)
   WHEN: civilizational (500+ years)
   WHERE: analytical (Not participating in the immediate shock)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From the view of deep time, antifragility is a Mountain. It is an 
   immutable law of information. Systems that do not gain from disorder 
   cease to exist. It is not a choice; it is the landscape of reality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(antifragility, mountain, 
    context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(antifragility_tests).

test(multi_perspective_antifragility) :-
    % Subjective suffering (Snare) vs. Strategic gain (Rope)
    constraint_indexing:constraint_classification(antifragility, snare, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility, rope, context(agent_power(individual_moderate), _, _, _)),
    % Long-term law (Mountain)
    constraint_indexing:constraint_classification(antifragility, mountain, context(_, time_horizon(civilizational), _, _)).

test(power_extractiveness_scaling) :-
    % The powerless (no buffers) experience the "extraction" of their 
    % stability by the system's need to learn from shocks.
    ContextPowerless = context(agent_power(individual_powerless), biographical, trapped, national),
    ContextPowerful = context(agent_power(individual_moderate), biographical, arbitrage, global),
    constraint_indexing:extractiveness_for_agent(antifragility, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(antifragility, ContextPowerful, Score2),
    Score1 > Score2.

test(mandatrophy_variance_check) :-
    % Verification: Does the predatory nature disappear for the beneficiary?
    % For the Practitioner (Beneficiary), it's a Rope (0 extraction felt).
    constraint_indexing:constraint_classification(antifragility, rope, context(agent_power(individual_moderate), _, _, _)).

:- end_tests(antifragility_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.75):
 * Reasoning: Antifragility is fundamentally asymmetric (the "Convexity 
 * Bias"). In a social context, for someone to gain from volatility, 
 * the volatility must usually be "paid for" by a fragile counterparty. 
 * This high score triggers the Mandatrophy Gate.
 * * 2. SUPPRESSION SCORE (0.65):
 * Reasoning: Our current institutional frameworks (Modernity) are 
 * built on the "Naive Intervention" of smoothing out cycles. This 
 * suppresses the visibility of antifragility, making it a "hidden" law.
 * * 3. CLASSIFICATION RATIONALE:
 * - Powerless -> Snare: They are the "transfer of fragility" targets.
 * - Institutional -> Tangled Rope: They coordinate (Rope) but their 
 * suppression of noise creates a predatory debt (Snare).
 * * 4. MANDATROPHY RESOLUTION:
 * The status [RESOLVED MANDATROPHY] is applied because the model 
 * demonstrates that the high extraction (0.75) is indexed. The 
 * Practitioner perceives a Rope (voluntary use of reality), while 
 * the Serf perceives a Snare (forced extraction by reality).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    antifragility_extraction_intent,
    "Is the 0.75 extraction a functional necessity for evolution or a predatory choice by the Barbell Practitioner?",
    resolution_mechanism("Audit of 'Skin in the Game' metrics: do practitioners suffer when their volatility-harvesting causes systemic collapse?"),
    impact("If necessity: Mountain (Lindy Law). If predatory choice: Snare (Parasitism)."),
    confidence_without_resolution(medium)
).

omega_variable(
    hormetic_threshold_uncertainty,
    "Where is the precise line between a 'strengthening stressor' and a 'terminal shock' for a specific population?",
    resolution_mechanism("Real-time physiological and economic stress-testing data"),
    impact("If known: Antifragility becomes a manageable Rope. If unknown: It remains a lethal Snare."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Robustness / Redundancy
 * Viability: High. Having backups (extra kidneys, extra cash) is a 
 * non-extractive way to survive volatility.
 * Suppression: High. Redundancy is often labeled "inefficiency" by 
 * corporate/state planners and pruned away.
 * * ALTERNATIVE 2: Fragility (The Status Quo)
 * Viability: Low. Only works in vacuum-sealed environments.
 * Suppression: N/A (It is the dominant, though failing, paradigm).
 * * CONCLUSION:
 * The active suppression of "Inefficient Redundancy" (Alternative 1) is 
 * what forces the "Antifragility" of the system to become a "Snare" for 
 * those within it.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * * 1. Load: ?- [antifragility].
 * 2. Run Tests: ?- run_tests(antifragility_tests).
 * 3. Audit: ?- multi_index_report(antifragility).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
