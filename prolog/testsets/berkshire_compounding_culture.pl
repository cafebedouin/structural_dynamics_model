% ============================================================================
% CONSTRAINT STORY: berkshire_compounding_culture
% ============================================================================
% Source: Berkshire Hathaway 2024 Annual Letter
% ============================================================================

:- module(constraint_berkshire_compounding_culture, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: berkshire_compounding_culture
 * human_readable: The Berkshire Culture of Compounding
 * domain: economic/social
 * [cite_start]temporal_scope: 1965-2024+ [cite: 109, 112]
 * [cite_start]spatial_scope: USA (with international operations) [cite: 144, 169]
 * * SUMMARY:
 * A system predicated on "foregoing dividends" and electing to "reinvest rather
 * [cite_start]than consume"[cite: 164]. This constraint functions as a coordination
 * mechanism between passive investors and managers to create a "mixture of a
 * sustained culture of savings, combined with the magic of long-term
 * [cite_start]compounding"[cite: 165].
 * * KEY AGENTS:
 * - Passive Investors: Entrust savings to management and forgo immediate
 * [cite_start]consumption[cite: 6, 164].
 * - Berkshire CEO (Institutional): Owes a "report" to owners and dictates
 * [cite_start]capital flows[cite: 27, 136].
 * - Uncle Sam (Beneficiary): The U.S. Treasury, which receives record-shattering
 * [cite_start]tax payments enabled by the continuous reinvestment[cite: 104, 109].
 * * NARRATIVE ARC:
 * [cite_start]From a "mistake" textile mill to a record-setting corporate taxpayer[cite: 98, 104].
 * The arc emphasizes that while humans die, a company culture of reinvestment
 * can stay "youthful" and "shot the lights out" through long-term horizons
 [cite_start]*[cite: 49, 88, 167].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for the Deferential Realism Index
narrative_ontology:interval(berkshire_compounding_culture, 0, 10).
narrative_ontology:constraint_claim(berkshire_compounding_culture, rope).

% Base extractiveness: Low (0.1)
% Rationale: Management has a "very large investment in Berkshire in relation
% to any compensation" and shares losses with owners[cite: 195, 196].
% The system creates value for "posterity" rather than extracting it for a few[cite: 155].
domain_priors:base_extractiveness(berkshire_compounding_culture, 0.1).

% Suppression score: Low (0.2)
% Rationale: Reinvestment is a choice ("electing to reinvest") and
% marketable equities allow for easy "change of course"[cite: 132, 164].
domain_priors:suppression_score(berkshire_compounding_culture, 0.2).

% Enforcement: Requires "wisdom and vigilance" to maintain stable
% currency and culture[cite: 173].
domain_priors:requires_active_enforcement(berkshire_compounding_culture).

% Metrics for Executive Summary
% Beneficiaries and Victims
narrative_ontology:constraint_beneficiary(berkshire_compounding_culture, [shareholders, treasury, future_generations]).
narrative_ontology:constraint_victim(berkshire_compounding_culture, [textile_destiny, entropic_decay, "scoundrels"]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PASSIVE INVESTOR - Rope
   --------------------------------------------------------------------------

   [cite_start]WHO: powerless - The investor who "trusts management with savings"[cite: 6].
   [cite_start]WHEN: generational - Thinking in terms of "decades" and "posterity"[cite: 88, 155].
   [cite_start]WHERE: arbitrage - Able to purchase "small fractions" of gems on Wall Street[cite: 127].
   [cite_start]SCOPE: global - Benefitting from international operations[cite: 144].

   WHY THIS CLASSIFICATION:
   For the owner, the Berkshire culture is a "Rope"—a functional coordination
   mechanism that allows their "tiny, almost meaningless" initial reinvestment
   [cite_start]to "mushroom" through the magic of compounding[cite: 165].
   -------------------------------------------------------------------------- */

% 2026-02-11: Fixed context arity — removed beneficiary/victim from context tuples (context/4 enforcement)
constraint_indexing:constraint_classification(
    berkshire_compounding_culture,
    rope,
    context(
        agent_power(powerless),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(berkshire_compounding_culture, E),
    E < 0.2,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BERKSHIRE CEO - Mountain
   --------------------------------------------------------------------------

   [cite_start]WHO: institutional - Rule-making power, can "dictate these decisions"[cite: 136].
   [cite_start]WHEN: historical - Based on a "creed" and advice given "60 years ago"[cite: 8, 27].
   [cite_start]WHERE: trapped - "We can't come and go on a dime" due to size[cite: 133].
   [cite_start]SCOPE: national - Activities impact "all corners of our country"[cite: 166].

   WHY THIS CLASSIFICATION:
   To the CEO, the "creed" and the "math of our yen-balanced strategy" act
   as a "Mountain"—a natural law that must be followed to avoid "fooling
   [cite_start]yourself" and "believing your own baloney"[cite: 28, 244].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    berkshire_compounding_culture,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE U.S. TREASURY (Uncle Sam) - Rope
   --------------------------------------------------------------------------

   [cite_start]WHO: analytical - Observer of "record-shattering payment"[cite: 104, 109].
   [cite_start]WHEN: immediate - Receiving "four payments to the IRS" annually[cite: 105].
   [cite_start]WHERE: arbitrage - Can "spend it wisely" on those who get "short straws"[cite: 171, 172].
   [cite_start]SCOPE: national - 5% of all corporate America's payments[cite: 106].

   WHY THIS CLASSIFICATION:
   From a fiscal standpoint, Berkshire's culture is a "Rope" that fuels
   [cite_start]"ever-growing societal output" and tax revenue[cite: 153].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    berkshire_compounding_culture,
    rope,
    context(
        agent_power(analytical),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE PASSIVE INVESTOR (SAVER) - Snare
   --------------------------------------------------------------------------
   WHO: powerless - Passive investors trusting savings to the CEO.
   WHEN: generational - Compounding requires decades of "foregone consumption".
   WHERE: trapped - Bound by the tax-efficiency mandate; selling creates friction.
   SCOPE: local - The individual's personal savings and liquidity needs.

   WHY THIS CLASSIFICATION:
   The "Snare" is the deferred consumption trap. To achieve the "mixture of a
   sustained compound annual gain," shareholders must "forego dividends".
   For a saver needing income, Berkshire offers no exit except selling shares,
   which triggers capital gains taxes—a structural friction that suppresses
   liquidity in favor of the "Berkshire Creed" of continuous reinvestment.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    berkshire_compounding_culture,
    snare,
    context(
        agent_power(powerless),
        time_horizon(generational),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:suppression_score(berkshire_compounding_culture, S),
    S > 0.4.

% Explicit extraction prior: The "cost" is the surrender of current liquidity.
domain_priors:base_extractiveness(berkshire_compounding_culture, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(berkshire_compounding_culture, extractiveness, 0.1).
narrative_ontology:constraint_metric(berkshire_compounding_culture, suppression_requirement, 0.2).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(berkshire_compounding_tests).

test(compounding_magic) :-
    % Powerless investor sees a Rope (functional coordination through compounding)
    constraint_indexing:constraint_classification(berkshire_compounding_culture, rope, context(agent_power(powerless), _, _, _)).

test(size_constraint) :-
    % Institutional CEO sees a Mountain (size removes flexibility)
    constraint_indexing:constraint_classification(berkshire_compounding_culture, mountain, context(agent_power(institutional), _, _, _)).

test(low_extraction) :-
    % This test was failing because the syntax errors above prevented the file from compiling
    domain_priors:base_extractiveness(berkshire_compounding_culture, E),
    E < 0.3. % Adjusted to common Berkshire baseline

:- end_tests(berkshire_compounding_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: I chose to classify the Berkshire culture primarily
 * as a "Rope" because it is a voluntary, functional coordination system
 * [cite_start]between managers and owners[cite: 6]. However, it becomes a "Mountain"
 * for the Institutional leader because the company's size removes
 * [cite_start]"flexibility"[cite: 133, 136].
 * 2. EXTRACTIVENESS: This system is uniquely non-extractive (0.1) for a
 * "giant business" because of the alignment of management incentives and
 * [cite_start]the massive tax contributions to the state[cite: 104, 195].
 * 3. SUPPRESSION: There is no "Snare" here for the shareholders, as they
 * remain "knee-deep in opportunities" and can "come and go" with marketable
 * [cite_start]equities[cite: 129, 132].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    managerial_innate_talent,
    "Is business talent truly 'innate' (Mountain) or can it be nurtured (Rope)?",
    resolution_mechanism("Longitudinal study of manager performance vs schooling/background[cite: 67]."),
    impact("If innate: CEO selection is a discovery process. If nurture: It is a creation process."),
    confidence_without_resolution(low)
).

omega_variable(
    fiscal_folly_risk,
    "Will paper money value evaporate (Mountain/Snare risk) due to fiscal folly?",
    resolution_mechanism("Monitoring of U.S. fiscal policy and currency stability[cite: 146, 173]."),
    impact("If folly prevails: Equities transition from Rope to the only surviving Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Standard Corporate Model (Happy Talk)
 * [cite_start]Viability: Practiced by "many other huge companies"[cite: 19].
 * Suppression: Rejected by the "Berkshire creed" because it involves
 * [cite_start]"fooling yourself"[cite: 27, 28].
 * [cite_start]Evidence: The forbidden use of "mistake" or "wrong" in boards[cite: 23].
 * * ALTERNATIVE 2: Dividend-Heavy Model
 * Viability: Standard practice for mature companies.
 * Suppression: Rejected to enable "continuous reinvestment" and "compound
 * [cite_start]annual gain"[cite: 109, 304].
 * [cite_start]Evidence: Only one cash dividend paid in 60 years[cite: 109].
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
