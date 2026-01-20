% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: greenland_seizure_trade_war
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Financial Times (Jan 20, 2026), Atlantic Council, The Times
% ============================================================================

:- module(constraint_greenland_seizure_trade_war, []).

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
 * * constraint_id: greenland_seizure_trade_war
 * human_readable: The Greenland Seizure Threat and Transatlantic Strife
 * domain: geopolitical/economic
 * temporal_scope: January 2026
 * spatial_scope: Transatlantic (US, Greenland, EU)
 * * SUMMARY:
 * President Trump has refused to rule out the use of force or massive tariffs 
 * to secure U.S. control over Greenland, framing it as a "Modern Manifest 
 * Destiny" and a "Golden Dome" necessity. This has triggered 
 * a severe transatlantic rift, with the EU considering €93 billion in 
 * retaliatory tariffs and the first-ever use of its "Anti-Coercion Instrument" 
 * (ACI) against the United States.
 * * KEY AGENTS:
 * - Donald Trump (Institutional/Enforcer): Viewing Greenland through 19th-century 
 * expansionist instincts to secure 21st-century missile defense.
 * - EU Policymakers (Institutional): Deploying "nuclear" economic options (ACI) 
 * to preserve sovereignty and market shared rules.
 * - Greenlandic Population (Individual Powerless): Caught in a "Greenland crisis" 
 * as their territory becomes a flashpoint for superpower force.
 * * NARRATIVE ARC:
 * The story moves from diplomatic friction to "seizure threats" and "trade war 
 * fears," marking an inflection point where long-standing NATO cohesion is 
 * subordinated to unilateral strategic demands for territory and resource 
 * security.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(greenland_crisis_2026, 2026, 2027).
narrative_ontology:constraint_claim(greenland_seizure_trade_war, noose).

% Base extractiveness score: High (0.85)
% Rationale: The U.S. demand seeks total territorial extraction (sovereignty) 
% and mineral control, while the EU's response is a massive extraction of 
% capital through tariffs (€93bn).
domain_priors:base_extractiveness(greenland_seizure_trade_war, 0.85).

% Suppression score: High (0.75)
% Rationale: The threat of "force" suppresses diplomatic exit options, and 
% the Anti-Coercion Instrument actively limits foreign investment and 
% market access.
domain_priors:suppression_score(greenland_seizure_trade_war, 0.75).

% Enforcement: Requires active enforcement
% Rationale: The constraint is maintained via explicit threats of military 
% action, troop movements by Denmark, and "absolutist" policy stances.
domain_priors:requires_active_enforcement(greenland_seizure_trade_war).

% Beneficiaries and Victims
constraint_beneficiary(greenland_seizure_trade_war, [us_defense_sector, domestic_expansionists]).
constraint_victim(greenland_seizure_trade_war, [nato_cohesion, greenland_sovereignty, transatlantic_trade_networks]).

narrative_ontology:constraint_metric(greenland_seizure_trade_war, extractiveness, 0.85).
narrative_ontology:constraint_metric(greenland_seizure_trade_war, suppression_requirement, 0.75).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: GREENLANDIC CITIZEN - Noose
   --------------------------------------------------------------------------
   WHO: individual_powerless (56,000 residents vs a 19th-century mindset superpower)
   WHEN: immediate (Facing seizure threats and blockade risks)
   WHERE: trapped (Geography is non-negotiable)
   SCOPE: national (The survival of their autonomous status)
   
   WHY THIS CLASSIFICATION:
   For the Greenlandic people, this is a Noose. They have no individual 
   agency to stop the superpower collision and are the primary "extracted" 
   value in the seizure threat.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    greenland_seizure_trade_war,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: EU TRADE NEGOTIATOR - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Representing the EU market bloc)
   WHEN: biographical (The 2026 term of the second Trump administration)
   WHERE: mobile (Can deploy the ACI or negotiate for a "deal")
   SCOPE: global (Managing the transatlantic economic order)
   
   WHY THIS CLASSIFICATION:
   For the EU, this is a Rope. They view the dispute as a coordination problem 
   to be solved via "economic counterstrikes" and "Anti-Coercion 
   Instruments" to force a return to shared rules.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    greenland_seizure_trade_war,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: GLOBAL INVESTOR - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observing the "transatlantic rift")
   WHEN: historical (The long-term shift away from global interdependence)
   WHERE: analytical (Selling off U.S. assets due to "trade war fears")
   SCOPE: global (Market-wide consequences)
   
   WHY THIS CLASSIFICATION:
   For investors, the rift is a Mountain—a structural "inflection point" where 
   geopolitical risk is now a permanent, unchangeable feature of the 2026 
   landscape that requires rebalancing portfolios away from volatility.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    greenland_seizure_trade_war,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(greenland_trade_war_tests).

test(perspectival_gap_negotiation) :-
    % The US sees a Rope (territorial expansion), the EU sees a Rope (economic defense).
    % Both see mobility, suggesting a high-stakes "clash of ropes" before a Noose locks.
    constraint_indexing:constraint_classification(greenland_seizure_trade_war, rope, context(institutional, biographical, mobile, _)).

test(extraction_scale) :-
    % €93 billion and territorial seizure are high extractiveness.
    assertion(domain_priors:base_extractiveness(greenland_seizure_trade_war, 0.85)).

:- end_tests(greenland_trade_war_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. PERSPECTIVAL GAP:
 * The most striking feature is the "collision of eras" (19th, 20th, 21st). 
 * Institutional actors frame this as a Rope (a tool for security or trade 
 * leverage), while the Powerless (Greenlandic citizens) perceive it as a 
 * lethal Noose.
 * * 2. STRUCTURAL REALITY:
 * The FT's reporting on the Anti-Coercion Instrument (ACI) as a "nuclear 
 * option" justifies the high suppression and extractiveness scores. This is 
 * no longer a "trade skirmish" but a "trade war".
 * * 3. OMEGAS:
 * I have identified the "force vs. tariff" toggle as the primary Omega—it 
 * determines if the constraint remains in the economic domain (Rope/Noose) 
 * or transitions to pure military law (Mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_aci_threshold,
    "Will the U.S. continue the seizure threat if the EU triggers the Anti-Coercion Instrument?",
    resolution_mechanism("Monitor Davos 2026 outcomes and Brussels' trade announcements."),
    impact("If U.S. persists, the 'Rope' of economic leverage breaks, leaving a 'Noose' of blockade."),
    confidence_without_resolution(low)
).

omega_variable(
    greenland_mineral_accessibility,
    "Will the 'seizure' actually provide REE independence from China in the 2026-2027 timeframe?",
    resolution_mechanism("Verify mining infrastructure status vs. TFR83 goals."),
    impact("If extraction is unfeasible, the 'Mountain' of Greenland is purely a symbolic defense sink."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Diplomatic Purchase (The 2019-style offer)
 * Viability: Rejected by Denmark and Greenland as "not for sale".
 * Suppression: Actively suppressed by the U.S. pivot to "force or tariffs" 
 * rather than pure financial negotiation.
 * * ALTERNATIVE 2: NATO Co-Lease
 * Viability: Expanding existing lease agreements for the "Golden Dome."
 * Suppression: Rejected by the "absolutist" stance of the 19th-century 
 * expansionist mindset which requires "control" over "cooperation".
 * * CONCLUSION:
 * The active suppression of Alternative 2 (Cooperation) turns a potential 
 * "Rope" of alliance into a "Noose" of geopolitical coercion.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% ?- constraint_indexing:multi_index_report(greenland_seizure_trade_war).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
