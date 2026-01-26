% ============================================================================
% CONSTRAINT STORY: bedouin_sedentary_transition
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The Bedouin Way of Life in Rub’ al Khali"
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_bedouin_sedentary_transition, []).

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
 * constraint_id: bedouin_sedentary_transition
 * human_readable: The Bedouin Sedentary Transition
 * domain: social/economic/technological
 * temporal_scope: 1938 - 2030+ (Oil discovery to Vision 2030)
 * spatial_scope: Regional (Rub’ al Khali, Saudi Arabia)
 * 
 * SUMMARY:
 * This constraint represents the shift of the nomadic Bedouin tribes from a
 * resilient, millennium-old pastoral lifestyle in the hyper-arid Empty Quarter
 * to a dependency-based existence in settled 'markaz' population centers fueled
 * by oil wealth. It highlights the systemic risk inherent in urban settlements
 * where specialization increases fragility and results in the loss of survival skills.
 * 
 * KEY AGENTS:
 * - Traditional Nomadic Bedouin (Individual Moderate): Characterized by freedom and resilience.
 * - Settled Bedouin (Individual Powerless): Dependent on state services and oil wealth.
 * - The Saudi State (Institutional): Uses oil wealth to consolidate population and manage development.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(bedouin_sedentary_transition, 0, 10).
narrative_ontology:constraint_claim(bedouin_sedentary_transition, snare).

% Base extractiveness: 0.75.
% The sedentary system extracts traditional survival skills and resilience 
% in exchange for temporary comfort, creating a deep asymmetric dependency on oil state budgets.
domain_priors:base_extractiveness(bedouin_sedentary_transition, 0.75).

% Suppression score: 0.6.
% Traditional nomadic strategies are undergoing "extinction" and are shunted 
% in favor of markaz settlements and town-based business.
domain_priors:suppression_score(bedouin_sedentary_transition, 0.6).

% Enforcement: Requires active enforcement (State budget management and infrastructure maintenance).
domain_priors:requires_active_enforcement(bedouin_sedentary_transition).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(bedouin_sedentary_transition, state_centralization).
constraint_victim(bedouin_sedentary_transition, bedouin_traditional_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: TRADITIONAL NOMAD (THESIGER ERA) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Significant agency over personal survival)
   WHEN: biographical (Lifelong nomadic satisfaction)
   WHERE: mobile (Constantly on the move looking for green patches)
   
   WHY THIS CLASSIFICATION:
   For the traditional Bedouin, the desert was a 'Rope'—a functional coordination mechanism 
   where hardship and abstinence were tools to achieve a "freedom unattainable in civilization".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bedouin_sedentary_transition,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MODERN SETTLED BEDOUIN - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Dependent on state oil money and services)
   WHEN: immediate (Day-to-day life in towns)
   WHERE: trapped (Dependent on a state budget tied to oil prices)
   
   WHY THIS CLASSIFICATION:
   For the modern settled Bedouin, the transition is a 'Snare.' They have "gone soft" and 
   "lost their edge," becoming trapped in settlements where survival is no longer 
   under their control but dependent on finite oil prices.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bedouin_sedentary_transition,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SAUDI STATE - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Provider of services, consolidator of population)
   WHEN: historical (From oil discovery to Vision 2030)
   WHERE: arbitrage (Balancing immediate development with long-term stability)
   
   WHY THIS CLASSIFICATION:
   For the Saudi State, the sedentary transition is a 'Tangled Rope'. It's a 'Rope'
   because it allows for centralization of control, provision of services, and
   management of the population. It's 'Tangled' because it creates a deep, systemic
   dependency on finite oil wealth, risking fragility and potential future collapse.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    bedouin_sedentary_transition,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(bedouin_sedentary_transition_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, Type1, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, Type2, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(bedouin_sedentary_transition_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Saudi State' to represent the
 *    institutional agent. Their perspective is a 'Tangled Rope' due to the
 *    benefits of centralization versus the risks of dependency.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Traditional Nomad (Rope): Freedom and resilience.
 *    - Settled Bedouin (Snare): Dependency on finite resources.
 *    - Saudi State (Tangled Rope): Centralization tool with systemic risks.
 * 
 * 3. MANDATROPHY STATUS: High extractiveness (0.75) is 'RESOLVED' because
 *    the sedentary transition provides immediate comfort (a 'Rope' for the state)
 *    but extracts long-term resilience, creating a 'Snare' for the individuals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the long-term viability of the sedentary model in a
 * post-oil future.
 */

omega_variable(
    bedouin_resilience_persistence,
    "Can the settled Bedouin re-learn the traditional skills necessary for survival if the oil revenue (and state services) collapses, or has their traditional resilience been permanently extracted?",
    resolution_mechanism("Observation of Bedouin response to hypothetical post-oil economic declines (e.g., Vision 2030 outcomes in 2040-2050)."),
    impact("If yes: The transition was a temporary 'Rope'. If no: It is a terminal 'Snare' from which there is no return."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Continued Nomadism
 *    Viability: The traditional millennium-old resilient strategy, proven to be sustainable in the region.
 *    Suppression: Actively made extinct by the "paradise" of oil money, state-provided housing, and medical care. This comfort suppressed the need for traditional survival skills.
 *
 * CONCLUSION:
 * The sedentary transition, while offering immediate comfort, ultimately created
 * a 'Snare' by suppressing the nomadic 'Rope' of resilience. This highlights the
 * hidden costs of "progress" when it replaces time-tested survival strategies.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/bedouin_sedentary_transition].
 * 2. Multi-perspective: ?- multi_index_report(bedouin_sedentary_transition).
 * 3. Run tests: ?- run_tests(bedouin_sedentary_transition_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */