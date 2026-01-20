% ============================================================================
% CONSTRAINT STORY: kidney_exchange_market
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: Alvin Roth / Market Design / Kidney Exchange Logic
% ============================================================================

:- module(constraint_kidney_exchange, []).

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
 * * constraint_id: kidney_exchange_market
 * human_readable: Kidney Exchange Cycles/Chains
 * domain: social/technological/biological
 * temporal_scope: Immediate (Critical Survival)
 * spatial_scope: Regional to National
 * * SUMMARY:
 * Kidney exchange solves the "incompatible donor" problem. When a willing donor 
 * cannot give to their intended recipient due to blood/tissue type, they enter 
 * a swap. This creates a matching market where "Price" is illegal (prohibited 
 * by the National Organ Transplant Act), necessitating algorithmic coordination.
 * * KEY AGENTS:
 * - The Patient: Needs a kidney; has a willing but incompatible donor.
 * - The Incompatible Donor: Willing to give, but only if their partner receives.
 * - The Altruistic Donor: A "non-directed" donor who starts a chain.
 * - The Algorithm: Computes optimal cycles and chains (Institutional power).
 * * NARRATIVE ARC:
 * The constraint is a "Life-Link." Without a central clearinghouse, these pairs 
 * are trapped in a biological dead-end. The algorithm creates a path out, but 
 * the rigid logic of compatibility acts as an immutable physical boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Reality Layer)
   ========================================================================== */

narrative_ontology:interval(kidney_exchange_market, 0, 10).
narrative_ontology:constraint_claim(kidney_exchange_market, mountain).

% Base extractiveness: Low (0.1). 
% There is no "profit" extraction; the system is designed for maximum life-saving.
domain_priors:base_extractiveness(kidney_exchange_market, 0.1).

% Suppression: High (0.9). 
% Selling kidneys is a felony (NOTA). The legal system suppresses the 
% "monetary alternative" almost entirely to prevent exploitation.
domain_priors:suppression_score(kidney_exchange_market, 0.9).

% Enforcement: Requires active enforcement (Institutional/Legal).
domain_priors:requires_active_enforcement(kidney_exchange_market).

% Beneficiaries: The Patient (receives life-saving organ).
constraint_beneficiary(kidney_exchange_market, organ_recipients).

% Victims: The "Hard-to-Match" (Patients with O-type blood or high antibodies 
% who wait longer due to the biological scarcity of compatible links).
constraint_victim(kidney_exchange_market, sensitized_patients).

% Metrics
narrative_ontology:constraint_metric(kidney_exchange_market, extractiveness, 0.1).
narrative_ontology:constraint_metric(kidney_exchange_market, suppression_requirement, 0.9).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SENSITIZED PATIENT - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: immediate (survival timeframe)
   WHERE: trapped
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For a patient with high antibodies, the matching logic is a "Noose." 
   Because they are biologically difficult to match, the system's focus 
   on "max matches" might skip over them repeatedly. The illegality of 
   monetary alternatives leaves them with no other exit options.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kidney_exchange_market,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(kidney_exchange_market, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ALTRUISTIC DONOR - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: biographical
   WHERE: mobile
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   To an altruistic donor, the exchange is a "Rope." It is a coordination 
   mechanism that allows their single act of generosity to trigger a 
   "chain," potentially saving 10+ people. They use the system as a 
   leverage tool for moral impact.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kidney_exchange_market,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    true.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BIOLOGIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   Blood types (A, B, AB, O) and HLA cross-matching are "Mountains." 
   They are immutable natural laws. No amount of social policy can 
   force an A-type kidney to work in a B-type patient without rejection.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kidney_exchange_market,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:emerges_naturally(kidney_exchange_market),
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(kidney_exchange_tests).

test(altruism_scaling) :-
    % Testing that altruistic 'mobile' agents perceive the coordination Rope.
    constraint_indexing:constraint_classification(kidney_exchange_market, rope, context(_, _, mobile, _)).

test(legal_suppression_effect) :-
    % High suppression of cash markets makes the institutional path a Noose 
    % for those it cannot help.
    constraint_indexing:constraint_classification(kidney_exchange_market, noose, context(individual_powerless, _, trapped, _)).

:- end_tests(kidney_exchange_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * I set extractiveness to 0.1 because, unlike corporate matching markets, 
 * there is no asymmetric "value capture" by the platform—all value is 
 * returned to the patients. However, the high suppression (0.9) of organ 
 * sales makes the "Noose" classification appropriate for those the 
 * system cannot find a match for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    immunosuppression_advancement,
    "Will new drugs eventually allow across-type transplants without matching?",
    resolution_mechanism("Clinical trial results for next-gen T-cell inhibitors"),
    impact("If Yes: The 'Mountain' of blood-type compatibility turns into a 'Rope' (optional coordination)."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Legalized Organ Markets: (Suppressed by NOTA; ethically contested).
 * 2. Desensitization Therapy: (Costly/Difficult; only works for some).
 * * CONCLUSION:
 * The Kidney Exchange is the ultimate "Rope" (coordination) built on 
 * the ultimate "Mountain" (biological compatibility).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
