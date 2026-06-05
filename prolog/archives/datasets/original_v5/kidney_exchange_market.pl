% ============================================================================
% CONSTRAINT STORY: kidney_exchange_market
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Alvin Roth / Market Design / Kidney Exchange Logic
% ============================================================================

:- module(constraint_kidney_exchange_market, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: kidney_exchange_market
 * human_readable: Kidney Exchange Cycles/Chains
 * domain: social/technological/biological
 * temporal_scope: Immediate (Critical Survival)
 * spatial_scope: Regional to National
 * 
 * SUMMARY:
 * Kidney exchange solves the "incompatible donor" problem. When a willing donor 
 * cannot give to their intended recipient due to blood/tissue type, they enter 
 * a swap. This creates a matching market where "Price" is illegal, necessitating
 * algorithmic coordination to save lives.
 * 
 * KEY AGENTS:
 * - The Sensitized Patient (Individual Powerless): Needs a kidney but is hard to match.
 * - The Algorithm / UNOS (Institutional): Computes optimal cycles and chains.
 * - The Altruistic Donor (Individual Moderate): Starts a chain by donating a kidney.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(kidney_exchange_market, 0, 10).
narrative_ontology:constraint_claim(kidney_exchange_market, rope).

% Base extractiveness: 0.1.
% There is no "profit" extraction; the system is designed for maximum life-saving.
domain_priors:base_extractiveness(kidney_exchange_market, 0.1).

% Suppression: 0.9.
% Selling kidneys is a felony (National Organ Transplant Act). The legal system
% suppresses the "monetary alternative" almost entirely to prevent exploitation.
domain_priors:suppression_score(kidney_exchange_market, 0.9).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(kidney_exchange_market, extractiveness, 0.1).
narrative_ontology:constraint_metric(kidney_exchange_market, suppression_requirement, 0.9).

% Enforcement: Requires active enforcement (Institutional/Legal).
domain_priors:requires_active_enforcement(kidney_exchange_market).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(kidney_exchange_market, organ_recipients).
constraint_victim(kidney_exchange_market, sensitized_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SENSITIZED PATIENT - Snare
   --------------------------------------------------------------------------
   WHO: powerless (High antibodies, hard to match)
   WHEN: immediate (Survival timeframe)
   WHERE: trapped (Biologically difficult to match)
   
   WHY THIS CLASSIFICATION:
   For a patient with high antibodies, the matching logic is a 'Snare'. 
   Because they are biologically difficult to match, the system's focus 
   on "max matches" might skip over them repeatedly. The illegality of 
   monetary alternatives leaves them with no other exit options.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kidney_exchange_market,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ALGORITHM / UNOS (INSTITUTIONAL) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The central clearinghouse for organ matching)
   WHEN: biographical (Long-term operation of the exchange program)
   WHERE: arbitrage (Optimizes matches across a complex network of donors/recipients)
   
   WHY THIS CLASSIFICATION:
   For the algorithm and the United Network for Organ Sharing (UNOS), the kidney
   exchange market is a 'Rope'. It is a powerful tool for optimizing matches and
   saving lives by coordinating complex swaps, effectively untying biological
   dead-ends into life-giving chains.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    kidney_exchange_market,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BIOLOGIST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observing the immutable laws of biology)
   WHEN: historical (From the discovery of blood types to HLA matching)
   WHERE: analytical (Universal laws of human compatibility)
   
   WHY THIS CLASSIFICATION:
   Blood types (A, B, AB, O) and HLA cross-matching are 'Mountains.' 
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
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(kidney_exchange_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(kidney_exchange_market, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kidney_exchange_market, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(kidney_exchange_market, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(kidney_exchange_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Algorithm / UNOS' as the institutional
 *    agent. This highlights how a complex algorithmic system can act as a powerful
 *    'Rope' for coordinating life-saving medical procedures.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Sensitized Patient (Snare): Trapped by biological incompatibility.
 *    - Algorithm/UNOS (Rope): A tool for optimizing matches and saving lives.
 *    - Biologist (Mountain): Immutable biological laws.
 * 
 * 3. CORE INSIGHT: The kidney exchange market is a triumph of market design,
 *    turning biological 'Mountains' (incompatibility) and legal 'Snares' (prohibition
 *    of sales) into a life-saving 'Rope' through intelligent coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the future role of technology in overcoming biological limits.
 */

omega_variable(
    immunosuppression_advancement,
    "Will new drugs or technologies eventually allow across-type transplants without needing perfect matching, or even allow for artificial organ generation?",
    resolution_mechanism("Clinical trial results for next-generation T-cell inhibitors; breakthroughs in xenotransplantation or bio-3D printing."),
    impact("If Yes: The 'Mountain' of blood-type compatibility could turn into a 'Rope' (an optional coordination). If No: It remains an enduring 'Mountain' for human biology."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Legalized Organ Markets
 *    Viability: Proposed by some economists to increase supply by offering monetary incentives.
 *    Suppression: Heavily suppressed by the National Organ Transplant Act (NOTA) and ethical concerns about exploitation and commodification of the human body.
 *
 * ALTERNATIVE 2: Desensitization Therapy
 *    Viability: Costly and difficult medical procedures to reduce antibody levels in highly sensitized patients.
 *    Suppression: Not actively suppressed, but the limited success rate and high cost make it a less viable option than exchange.
 *
 * CONCLUSION:
 * The kidney exchange market navigates a complex ethical and biological landscape.
 * By actively suppressing the monetary alternative ('Snare' of exploitation), it
 * creates a moral 'Rope' to save lives within existing biological 'Mountains'.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/kidney_exchange_market].
 * 2. Multi-perspective: ?- multi_index_report(kidney_exchange_market).
 * 3. Run tests: ?- run_tests(kidney_exchange_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */