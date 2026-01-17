% ============================================================================
% CONSTRAINT STORY: trade_secret_law
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Uniform Trade Secrets Act (UTSA) / Defend Trade Secrets Act (DTSA)
% ============================================================================

:- module(constraint_trade_secret, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: trade_secret_law
 * human_readable: Trade Secret Law (Information Ownership)
 * domain: legal/economic/technological
 * temporal_scope: Civilizational (Permanent feature of commerce)
 * spatial_scope: Global
 * * SUMMARY:
 * Trade Secret Law protects confidential business information (formulae, 
 * practices, designs, instruments, or patterns) that provides an enterprise 
 * a competitive advantage. It exists as long as the information remains 
 * secret and the owner takes reasonable steps to protect it.
 * * KEY AGENTS:
 * - The Information Owner: The firm that invested in the secret.
 * - The Departing Employee: The agent carrying "skills" (allowed) vs "secrets" (blocked).
 * - The Courts: The final arbiters of what constitutes "misappropriation."
 * * NARRATIVE ARC:
 * The constraint acts as a "Filtered Border." It allows agents to migrate 
 * across the labor market (Matching Market) but requires them to leave 
 * specific proprietary "objects" behind. It is a structural feature that 
 * theoretically balances innovation with individual mobility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(trade_secret_law, 0, 10).
narrative_ontology:constraint_claim(trade_secret_law, intellectual_property_limit).

% Base extractiveness: Low (0.2).
% It protects the firm's investment without extracting the employee's 
% general right to work or earn a living.
domain_priors:base_extractiveness(trade_secret_law, 0.2).

% Suppression: Low (0.3).
% The rules are generally transparent, though the specific "secrets" 
% are (by definition) suppressed/hidden.
domain_priors:suppression_score(trade_secret_law, 0.3).

% Enforcement: Emerges naturally as a market norm and legal baseline.
domain_priors:emerges_naturally(trade_secret_law).

% Beneficiaries: R&D Intensive Firms (protecting investment).
constraint_beneficiary(trade_secret_law, innovative_enterprises).

% Victims: Industrial Spies / Unethical Competitors.
constraint_victim(trade_secret_law, bad_faith_actors).

% Metrics
narrative_ontology:constraint_metric(trade_secret_law, extractiveness, 0.2).
narrative_ontology:constraint_metric(trade_secret_law, suppression_requirement, 0.3).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ETHICAL ENGINEER - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the ethical professional, Trade Secret Law is a "Rope." It provides 
   clear boundaries that actually *enable* their mobility. Because the 
   law protects the secret, the employer feels safe hiring and training 
   the engineer, and the engineer knows exactly what they can and cannot 
   take with them to their next "Match."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trade_secret_law,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(trade_secret_law, E),
    E < 0.4,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STARTUP FOUNDER - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerful
   WHEN: historical
   WHERE: arbitrage
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   A founder sees Trade Secret Law as a "Mountain"—a permanent feature 
   of the environment. They build their company's "moat" based on these 
   protections. It is an immutable law of the marketplace that allows 
   them to secure venture capital by proving their value is protected.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trade_secret_law,
    mountain,
    context(
        agent_power(individual_powerful),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ACCUSED WHISTLEBLOWER - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate
   WHERE: trapped
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   If a company uses "Trade Secret" claims to suppress a whistleblower 
   or block a departing employee's move through litigation, the law 
   becomes a "Noose." The high cost of legal defense and the ambiguity 
   of what constitutes a "secret" can strangle the individual's 
   career and mobility.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trade_secret_law,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(trade_secret_law, S),
    S > 0.2,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(trade_secret_tests).

test(mobility_preservation) :-
    % Testing that Trade Secret Law (unlike Non-Competes) allows for mobility.
    constraint_indexing:constraint_classification(trade_secret_law, rope, context(_, _, mobile, _)).

test(misuse_case) :-
    % Testing that for 'trapped' agents in litigation, it functions as a Noose.
    constraint_indexing:constraint_classification(trade_secret_law, noose, context(individual_powerless, _, trapped, _)).

:- end_tests(trade_secret_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * I set extractiveness low (0.2) because the law targets information, 
 * not people. It is a "cleaner" constraint than the Non-Compete. 
 * However, the "Noose" classification is necessary to acknowledge 
 * how the law is often weaponized in "tactical litigation."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    inevitable_disclosure_drift,
    "Will courts increasingly use 'Inevitable Disclosure' to treat Trade Secret Law as a de-facto Non-Compete?",
    resolution_mechanism("Monitoring case law trends in states like New York vs California"),
    impact("If Yes: The 'Mountain' turns into a 'Noose' for all high-skill labor."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
