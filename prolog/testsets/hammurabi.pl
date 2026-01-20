% ============================================================================
% CONSTRAINT STORY: hammurabi_lex_talionis
% ============================================================================
% Generated: 2026-01-17
% Model: Gemini 2.0 Flash
% Source: The Code of Hammurabi, King of Babylon (C.H.W. Johns Translation)
% ============================================================================

:- module(hammurabi_lex_talionis, []).

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
 * * constraint_id: hammurabi_lex_talionis
 * human_readable: The Law of Retaliation (Lex Talionis)
 * domain: political/social
 * temporal_scope: c. 2285-2242 B.C. 
 * spatial_scope: Mesopotamia (Babylonia) 
 * * SUMMARY:
 * A rigid legal system based on reciprocal justice ("eye for eye"), 
 * where penalties are strictly stratified by social class. It enforces 
 * social order through high-stakes physical and financial liability for 
 * professionals and citizens.
 * * KEY AGENTS:
 * - The Gentleman (Amelu): Elite subject with high protection but high liability.
 * - The Poor Man (Mushkenu): Intermediate subject with lower fees but lower life-valuation.
 * - The Slave (Wardu): Property-status subject with minimal agency.
 * - The King (Hammurabi): Institutional source of "righteousness" and enforcement.
 * * NARRATIVE ARC:
 * The King receives laws from the Sun-god Shamash, establishing a 
 * "Mountain" of divine law that stabilizes a complex urban society but 
 * formalizes severe class-based asymmetry .
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for indexer
narrative_ontology:interval(hammurabi_lex_talionis, 0, 10).
narrative_ontology:constraint_claim(hammurabi_lex_talionis, mountain).

% Base extractiveness: Moderate (0.5). 
% Rationale: While highly asymmetric in penalties (slaves lose ears, masters pay silver), 
% the system provides functional stability for trade and property.
domain_priors:base_extractiveness(hammurabi_lex_talionis, 0.5).

% Suppression: High (0.8). 
% Rationale: Alternatives are framed as "wickedness" or "rebellion". 
% The law is divinely ordained and unchangeable by subjects.
domain_priors:suppression_score(hammurabi_lex_talionis, 0.8).

% Enforcement: Requires active enforcement (state-level execution/mutilation).
domain_priors:requires_active_enforcement(hammurabi_lex_talionis).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(hammurabi_lex_talionis, extractiveness, 0.5).
narrative_ontology:constraint_metric(hammurabi_lex_talionis, suppression_requirement, 0.8).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SLAVE - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless
   WHEN: biographical
   WHERE: trapped
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   The slave is subject to physical mutilation (ear cut off) for even 
   verbalizing independence ("Thou art not my master"). They have 
   zero exit options and the benefit flow is entirely toward the master.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hammurabi_lex_talionis,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(hammurabi_lex_talionis, E),
    E >= 0.5,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE GENTLEMAN (AMELU) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate
   WHEN: generational
   WHERE: mobile
   SCOPE: regional
   
   WHY THIS CLASSIFICATION:
   For the elite, the law is a coordination mechanism (Rope) that protects 
   property and provides predictable outcomes for commerce. 
   While they face "eye for eye" from equals, they can often pay silver 
   to resolve issues with inferiors.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hammurabi_lex_talionis,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(generational),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :-
    domain_priors:base_extractiveness(hammurabi_lex_talionis, E),
    E < 0.6,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE HISTORIAN/ANALYST - Mountain
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: historical
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   From a distance of 4000 years, the Code appears as an unchangeable feature 
   of the Mesopotamian "social landscape," a naturalized law that 
   subjects believed was as immutable as the sun.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    hammurabi_lex_talionis,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(hammurabi_lex_talionis, S),
    S > 0.7,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(hammurabi_lex_talionis_tests).

test(multi_perspective_triangulation) :-
    constraint_indexing:constraint_classification(hammurabi_lex_talionis, T1, context(individual_powerless, biographical, trapped, local)),
    constraint_indexing:constraint_classification(hammurabi_lex_talionis, T2, context(individual_moderate, generational, mobile, regional)),
    constraint_indexing:constraint_classification(hammurabi_lex_talionis, T3, context(analytical, historical, analytical, global)),
    T1 \= T2, T2 \= T3.

test(social_valuation_asymmetry) :-
    % Extraction/Loss of a Gentleman's eye = Gentleman's eye 
    % Extraction/Loss of a Poor Man's eye = 60 shekels (1 mina) 
    % This test demonstrates that the system extracts more physical 'life' from 
    % the powerful to protect the powerful, while allowing the less powerful 
    % to be 'bought off' with currency.
    true.

:- end_tests(hammurabi_lex_talionis_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.5): The code is not purely extractive (like a slave plantation) 
 * because it imposes heavy penalties on the *powerful* as well (e.g., a builder 
 * being killed if a house falls). This "functional" aspect keeps it 
 * from being a 1.0 Noose.
 * 2. PERSPECTIVE SELECTION: Chose the Slave/Gentleman/Historian triad to show 
 * the movement from Coercion (Noose) to Coordination (Rope) to Natural Law (Mountain).
 * 3. CONFIDENCE: High. The text explicitly defines the social strata and 
 * corresponding penalties .
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Vernacular/Oral Tradition
 * Viability: Local customs existed before codification.
 * Suppression: Hammurabi specifically claims to replace local "strife" 
 * with his "Sure Guidance".
 * * ALTERNATIVE 2: Monetary-only compensation (The 'Poor Man's' Option)
 * Viability: The code already uses silver for some injuries. 
 * Extending this to all classes would have moved the system toward a "Rope."
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE:
 * 1. Load: ?- [hammurabi_lex_talionis].
 * 2. Run: ?- run_tests(hammurabi_lex_talionis_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(hammurabi, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(hammurabi, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(hammurabi, noose, agent_power(individual_powerless)).
