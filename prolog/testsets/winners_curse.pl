% ============================================================================
% CONSTRAINT STORY: winners_curse
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% Model: Gemini
% Source: Auction Theory / Behavioral Economics / Thaler (1988)
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_winners_curse, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: winners_curse
 * human_readable: The Winner's Curse
 * domain: economic/social
 * * SUMMARY:
 * The Winner's Curse is a statistical phenomenon where the winning bidder for an item 
 * of uncertain value tends to overpay. This occurs because the winner is typically 
 * the person with the most optimistic estimate, which statistically likely 
 * exceeds the actual intrinsic value.
 * * KEY AGENTS:
 * - The Emotional Amateur: Subject (Powerless/Trapped by psychological commitment)
 * - The Seller/Auctioneer: Beneficiary (Institutional/Capturing optimism surplus)
 * - The Mathematical Economist: Auditor (Analytical/Detection of Mandatrophy)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: High extraction (0.70) as the seller captures the "optimism surplus," 
% often leaving the winner with negative equity.
domain_priors:base_extractiveness(winners_curse, 0.70). 
domain_priors:suppression_score(winners_curse, 0.60).   % Competitive pressure suppresses rational models.
domain_priors:theater_ratio(winners_curse, 0.65).       % Reflects performative "frenzy" and gamification.
domain_priors:requires_active_enforcement(winners_curse).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(winners_curse, extractiveness, 0.7).
narrative_ontology:constraint_metric(winners_curse, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(winners_curse, theater_ratio, 0.65).

% Constraint classification claim
narrative_ontology:constraint_claim(winners_curse, tangled_rope).

% Mandatory keys for classification engine v3.4
% Resolved MISSING_TEMPORAL_DATA by anchoring metrics for drift detection.
% Emerges naturally from competition and information asymmetry.
domain_priors:emerges_naturally(winners_curse).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(winners_curse, sellers).
narrative_ontology:constraint_beneficiary(winners_curse, auctioneers).
narrative_ontology:constraint_victim(winners_curse, aggressive_bidders).
narrative_ontology:constraint_victim(winners_curse, overconfident_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EMOTIONAL AMATEUR (SNARE)
% Trapped by "frenzy" and psychological commitment at the immediate horizon.
constraint_indexing:constraint_classification(winners_curse, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE CORPORATE M&A TEAM (ROPE)
% Institutional actors using "shading" models to coordinate safe bidding.
constraint_indexing:constraint_classification(winners_curse, rope, 
    context(agent_power(institutional), 
            time_horizon(biographical), 
            exit_options(arbitrage), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE MATHEMATICAL ECONOMIST (MOUNTAIN)
% Viewing the curse as a permanent feature of trade and probability.
constraint_indexing:constraint_classification(winners_curse, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(analytical), 
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(winners_curse_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless amateur vs Rope for the institutional strategist.
    constraint_indexing:constraint_classification(winners_curse, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(winners_curse, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.46) triggers lifecycle drift requirement.
    E >= 0.46.

:- end_tests(winners_curse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.70) reflects how auctions capture the optimism margin.
 * The perspectival gap exists between those using probability as a tool (Rope) 
 * and those consumed by its distribution (Snare).
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The predatory nature is shown to be perspectival, resolving as a Rope for 
 * Institutional agents who maintain strategic buffers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winners_curse_extraction_intent,
    'Is overpayment a functional necessity of price discovery or predatory design?',
    'Audit of auction formats (e.g., English vs. Vickrey) in high-stakes environments.',
    'If necessity: Mountain. If predatory design: Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(winners_curse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Modeling the intensification of performative "Auction Frenzy" (0.35 -> 0.65).
narrative_ontology:measurement(winners_tr_t0, winners_curse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(winners_tr_t5, winners_curse, theater_ratio, 5, 0.50).
narrative_ontology:measurement(winners_tr_t10, winners_curse, theater_ratio, 10, 0.65).

% Extraction: Tracking the intensification of the "Optimism Surplus" capture (0.60 -> 0.70).
narrative_ontology:measurement(winners_ex_t0, winners_curse, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(winners_ex_t5, winners_curse, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(winners_ex_t10, winners_curse, base_extractiveness, 10, 0.70).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
