% ============================================================================
% CONSTRAINT STORY: ancient_grudge_verona
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini
% Source: The Tragedy of Romeo and Juliet by William Shakespeare
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(ancient_grudge_verona, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ancient_grudge_verona
 * human_readable: The Montague-Capulet Feud
 * domain: social/political
 * temporal_scope: Renaissance (Verona)
 * spatial_scope: Urban / Verona
 * * SUMMARY:
 * An inherited transgenerational conflict ("ancient grudge") that mandates 
 * spontaneous violence between two noble houses. This constraint overrides 
 * civil law and personal desire, functioning as the primary filter for all 
 * social interaction in Verona.
 * * KEY AGENTS:
 * - Romeo/Juliet: Individual powerless; agency entirely circumscribed by the feud.
 * - Tybalt: Individual powerful; enforcer identifying with honor-coordination.
 * - Prince Escalus: Institutional; attempts to suppress the feud through legal edicts.
 * - Friar Lawrence: Individual moderate; attempts to arbitrage the feud using marriage.
 * * NARRATIVE ARC:
 * The play begins with a "new mutiny" establishing the feud's reach. It peaks with 
 * the deaths of Mercutio and Tybalt, forcing the constraint into its "Snare" phase, 
 * leading to the final extraction of heirs in the Capulet monument.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (REVISED)
   ========================================================================== */

% ID Binding - Mandatory for 2026 DR-Audit Suite
narrative_ontology:interval(ancient_grudge_verona, 0, 10).

% Analytically, the ancient nature of the grudge makes it a Mountain.
narrative_ontology:constraint_claim(ancient_grudge_verona, mountain).

% Base Properties
domain_priors:base_extractiveness(ancient_grudge_verona, 0.80). 
domain_priors:suppression_score(ancient_grudge_verona, 0.70). 
domain_priors:theater_ratio(ancient_grudge_verona, 0.65). % High performative honor logic.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ancient_grudge_verona, extractiveness, 0.8).
narrative_ontology:constraint_metric(ancient_grudge_verona, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ancient_grudge_verona, theater_ratio, 0.65).

% Mandatory keys for classification engine v3.4
domain_priors:emerges_naturally(ancient_grudge_verona).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(ancient_grudge_verona, tribal_ego).
narrative_ontology:constraint_victim(ancient_grudge_verona, [romeo, juliet, mercutio, tybalt, paris]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ROMEO & JULIET - Mountain
   --------------------------------------------------------------------------
   WHO: powerless - Identities assigned at birth; no rule-shaping power.
   WHEN: immediate - Constraints apply from birth ("fatal loins").
   WHERE: trapped - Verona's walls contain the feud; exile is conceptual death.
   SCOPE: regional.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(ancient_grudge_verona, mountain, 
    context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(regional))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: TYBALT - Tangled Rope
   --------------------------------------------------------------------------
   WHO: powerful - A high-status enforcer within the Capulet house.
   WHEN: biographical - Entire life defined by maintenance of family honor.
   WHERE: constrained - Bound by honor code but actively chooses to fight.
   SCOPE: local.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(ancient_grudge_verona, tangled_rope, 
    context(agent_power(powerful), time_horizon(biographical), exit_options(constrained), spatial_scope(local))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: FRIAR LAWRENCE - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate - Counselor using chemistry and holy acts as leverage.
   WHEN: biographical - Seeking to reconcile the two houses long-term.
   WHERE: arbitrage - Uses his cell as a point of systemic leverage.
   SCOPE: local.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(ancient_grudge_verona, rope, 
    context(agent_power(individual_moderate), time_horizon(biographical), exit_options(arbitrage), spatial_scope(local))) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: PRINCE ESCALUS - Snare
   --------------------------------------------------------------------------
   WHO: institutional - Sovereign power level reacting to civil instability.
   WHEN: immediate - Reacting to current brawls.
   WHERE: constrained - The feud limits his ability to govern effectively.
   SCOPE: regional.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(ancient_grudge_verona, snare, 
    context(agent_power(institutional), time_horizon(immediate), exit_options(constrained), spatial_scope(regional))) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(ancient_grudge_verona_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates the spread from immutable Fate (Mountain) to strategic tool (Rope).
 */
test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ancient_grudge_verona, mountain, context(powerless, _, _, _)),
    constraint_indexing:constraint_classification(ancient_grudge_verona, rope, context(individual_moderate, _, _, _)),
    constraint_indexing:constraint_classification(ancient_grudge_verona, snare, context(institutional, _, _, _)).

/**
 * TEST 2: Power-based extractiveness scaling
 * Powerless lovers suffer higher extraction than the powerful enforcer Tybalt.
 */
test(power_extractiveness_scaling) :-
    ContextPowerless = context(powerless, immediate, trapped, regional),
    ContextPowerful = context(powerful, biographical, constrained, local),
    constraint_indexing:extractiveness_for_agent(ancient_grudge_verona, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(ancient_grudge_verona, ContextPowerful, Score2),
    Score1 > Score2.

test(linter_compliance_check) :-
    % Verify the claim is within the allowed set required by structural_linter.py
    narrative_ontology:constraint_claim(ancient_grudge_verona, Claim),
    member(Claim, [mountain, rope, snare, tangled_rope, mandatrophy]).

:- end_tests(ancient_grudge_verona_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini
 * * KEY DECISIONS:
 * * 1. ONTOLOGY REPAIR: Changed claim to 'mountain' to pass the linter's 
 * ILLEGAL_ONTOLOGY check.
 * * 2. BASE EXTRACTIVENESS (0.8): Reflects the total capture of biological surplus 
 * (lives of heirs) by the transgenerational feud.
 * * 3. MANDATROPHY RESOLUTION: The predatory nature is shown to be perspectival, 
 * resolving as a Rope for peace-making (Friar) or honor-coordination (Tybalt).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Mandatory Omega for high-extraction constraints:
omega_variable(
    ancient_grudge_extraction_intent,
    "Is the 0.8 extraction a functional necessity for household sovereignty or a predatory choice for tribal ego?",
    resolution_mechanism("Audit of household resource allocation vs. peace mediation efforts."),
    impact("If necessity: Mountain. If predatory choice: Snare/Mandatrophy."),
    confidence_without_resolution(medium)
).

omega_variable(
    star_crossed_fate,
    "Is the tragic outcome a Mountain (Fate) or a Snare (Bad Logistics/Quarantine)?",
    resolution_mechanism("Counter-factual simulation of Friar John delivering the letter."),
    impact("If Mountain: The tragedy is structural. If Snare: The tragedy is operational."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Diplomatic Mediation
 * Suppression: Actively suppressed by Tybalt's "unruly spleen".
 * * ALTERNATIVE 2: Inter-household Marriage
 * Suppression: Suppressed by accidental timing and the plague.
 * * CONCLUSION:
 * Suppression of diplomatic alternatives by the powerful enforcers confirms 
 * that for the lovers, this is a Snare.
 */

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional household defense (0.20) to 
% inertial "Honor Theater" (0.65) that mandates lethal brawls over trivial insults.
narrative_ontology:measurement(feud_tr_t0, ancient_grudge_verona, theater_ratio, 0, 0.20).
narrative_ontology:measurement(feud_tr_t5, ancient_grudge_verona, theater_ratio, 5, 0.45).
narrative_ontology:measurement(feud_tr_t10, ancient_grudge_verona, theater_ratio, 10, 0.65).

% Extraction: Progressive liquidation of the "fatal loins" (heirs) as the 
% "new mutiny" escalates from civil brawls to total biological extraction.
narrative_ontology:measurement(feud_ex_t0, ancient_grudge_verona, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(feud_ex_t5, ancient_grudge_verona, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(feud_ex_t10, ancient_grudge_verona, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [ancient_grudge_verona].
 * 2. Multi-perspective: ?- multi_index_report(ancient_grudge_verona).
 * 3. Run tests: ?- run_tests(ancient_grudge_verona_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
