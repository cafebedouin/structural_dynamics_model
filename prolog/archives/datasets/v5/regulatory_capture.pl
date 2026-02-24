% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: regulatory_capture
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Stigler, G. J. (1971) / Public Choice Theory
% ============================================================================

:- module(constraint_regulatory_capture, []).

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
 * * constraint_id: regulatory_capture
 * human_readable: Regulatory Capture
 * domain: economic/political
 * temporal_scope: Permanent (Modern Bureaucratic States)
 * spatial_scope: National (Regulated Industries)
 * * SUMMARY:
 * Regulatory capture occurs when a regulatory agency, created to act in the 
 * public interest, instead advances the commercial or political concerns of 
 * special interest groups that dominate the industry it is charged with 
 * regulating. It represents a corruption of the "referee" into a "player."
 * * KEY AGENTS:
 * - The Incumbent Firm: The beneficiary who uses regulations to bar new 
 * competitors and extract rents.
 * - The Regulator: Often a former or future employee of the industry, 
 * whose interests align with the regulated firms.
 * - The Public: The victim class who pays higher prices for lower-quality 
 * services due to suppressed competition.
 * * NARRATIVE ARC:
 * What starts as a public-interest "Rope" to coordinate a market safely
 * degrades over time through special-interest influence. It becomes a "Piton"—
 * an ossified, immovable liability that causes more harm than good but is
 * left in place out of inertia. For the incumbent, it is still a useful
 * "Rope" for market control, while for outsiders it is a "Snare".
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(capture_interval, 0, 10).
narrative_ontology:constraint_claim(regulatory_capture, piton).

% Base extractiveness: 0.8 (High)
% Rationale: It extracts consumer surplus and innovation energy, 
% redirecting it into monopoly profits for the captured industry.
domain_priors:base_extractiveness(regulatory_capture, 0.8).

% Suppression: 0.2 (Low)
% Rationale: Alternatives are not actively suppressed by force, but by the
% immense complexity, inertia, and "inside game" of the captured bureaucracy.
domain_priors:suppression_score(regulatory_capture, 0.2).

% Resistance: 0.6 (High)
% Rationale: The public and entrepreneurs actively protest and attempt to
% route around the captured regulations, indicating high friction.
domain_priors:resistance_score(regulatory_capture, 0.6).


% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(regulatory_capture, extractiveness, 0.8).
narrative_ontology:constraint_metric(regulatory_capture, suppression_requirement, 0.2).

% Make the constraint "evolve" by showing resistance increasing over time
narrative_ontology:constraint_metric(regulatory_capture, resistance, 0.1, 0).
narrative_ontology:constraint_metric(regulatory_capture, resistance, 0.6, 10).

% Formally model the viable alternative
narrative_ontology:intent_viable_alternative(capture_interval,
    'Decentralized Oversight (Blockchain/Public Ledger)',
    'Theoretically prevents opaque lobbying but is suppressed by the incumbent system.').

% BENEFICIARIES & VICTIMS
constraint_beneficiary(regulatory_capture, industry_monopolists).
constraint_beneficiary(regulatory_capture, bureaucratic_elites).
constraint_victim(regulatory_capture, innovative_startups).
constraint_victim(regulatory_capture, general_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PUBLIC CHOICE ECONOMIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of political incentives)
   WHEN: historical (Viewing the long-term trend of state growth)
   WHERE: analytical (Observing from the outside)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the economist, capture is a Mountain. It is an inevitable outcome of 
   human incentives: concentrated interests have a high motivation to lobby, 
   while the public (diffuse interests) remains rationally ignorant.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    regulatory_capture,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE FORTUNE 500 LOBBYIST - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (Agent with rule-shaping power)
   WHEN: biographical (Achieving corporate targets over a career)
   WHERE: arbitrage (Can move between government and private sectors)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the lobbyist, regulation is a Rope. It is a coordination mechanism to 
   "standardize" the industry in a way that just happens to favor their 
   firm's existing infrastructure, pulling up a wall against new entrants.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    regulatory_capture,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISRUPTIVE ENTREPRENEUR - SNARE
   --------------------------------------------------------------------------
   
   WHO: powerless (Relative to the regulatory state)
   WHEN: immediate (Trying to launch a product today)
   WHERE: constrained (Cannot afford the legal fees to comply or lobby)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the innovator, capture is a Snare. They have a better, cheaper 
   solution, but the "safety" regulations (written by their competitors) 
   make it illegal to operate. The harder they try to compete, the more 
   the legal trap tightens.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    regulatory_capture,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE INSTITUTIONAL ANALYST - PITON
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of institutional decay)
   WHEN: biographical (Seeing the agency's function degrade over a career)
   WHERE: analytical (Comparing stated goals vs. actual outcomes)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   The analyst sees the agency as a Piton. It was created as a Rope to ensure
   market safety, but has decayed into a liability that persists through
   inertia. It no longer serves its coordinating function effectively and now
   creates more friction than it resolves, but it's too embedded to easily remove.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    regulatory_capture,
    piton,
    context(
        agent_power(analytical),
        time_horizon(biographical),
        exit_options(analytical),
        spatial_scope(national)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(regulatory_capture_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(regulatory_capture, T1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture, T2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture, T3, context(agent_power(powerless), _, _, _)),
    T1 \= T2, T2 \= T3.

test(high_extraction_asymmetry) :-
    % Monopoly rents extract surplus from the public to the incumbent.
    domain_priors:base_extractiveness(regulatory_capture, E),
    E >= 0.7.

test(barrier_to_entry_snare) :-
    % Powerless agents with constrained exits should experience a Snare.
    constraint_indexing:constraint_classification(regulatory_capture, snare, context(powerless, _, constrained, _)).

:- end_tests(regulatory_capture_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.8): Ranked high because this is a direct transfer of 
 * wealth from the many (consumers) to the few (incumbents) via price 
 * distortions.
 * 2. SNARE VS ROPE: The distinction depends entirely on which side of the 
 * regulatory wall you sit. If you are the one holding the "compliance" 
 * standard, it's a Rope; if you are the one trying to climb it, it's a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_reversibility,
    "Can a captured agency ever be 're-set' to neutral through internal reform, or is total institutional collapse the only exit?",
    resolution_mechanism("Comparative history of anti-trust and regulatory 'house-cleanings'"),
    impact("If Mountain: We must build parallel systems. If Rope: Reform is viable."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Decentralized Oversight (Blockchain/Public Ledger)
 * Viability: Theoretically prevents opaque lobbying.
 * Suppression: Extreme. Incumbent agencies reject transparent auditing 
 * as a threat to "national security" or "privacy."
 * * CONCLUSION:
 * The existence of technological alternatives that are legally suppressed 
 * confirms that regulatory capture is a Snare, not a natural Mountain.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_regulatory_capture].
 * 2. Multi-perspective: ?- multi_index_report(regulatory_capture).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
