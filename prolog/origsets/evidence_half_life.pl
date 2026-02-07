% ============================================================================
% CONSTRAINT STORY: evidence_half_life
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(evidence_half_life, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: evidence_half_life
 * human_readable: The Epistemic Decay Constant
 * domain: technological/scientific/legal
 * * SUMMARY:
 * A scenario where the speed of information turnover and the ease of digital 
 * alteration cause the "half-life" of a piece of evidence to drop below 
 * the time required for judicial or scientific verification. This "Rope" for 
 * rapid information iteration becomes a "Snare" for the truth-seeker, 
 * liquidating the permanence of facts and trapping the subject in a 
 * "perpetual present" where proof is impossible to maintain.
 * * KEY AGENTS:
 * - Forensic Investigator: Subject (Powerless)
 * - Real-Time Information Platform: Beneficiary (Institutional)
 * - Epistemic Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) reflects the liquidation of the subject's ability 
% to build stable, grounded arguments over time.
domain_priors:base_extractiveness(evidence_half_life, 0.85). 
domain_priors:suppression_score(evidence_half_life, 0.72). % Alternatives (archival stability) are suppressed by the velocity of the medium.
domain_priors:theater_ratio(evidence_half_life, 0.81).    % High theater: "Timestamp" rituals that provide the optics of stability while the content drifts.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(evidence_half_life, extractiveness, 0.85).
narrative_ontology:constraint_metric(evidence_half_life, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(evidence_half_life, theater_ratio, 0.81).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The investigator is trapped: by the time a fact is verified, the 
% context has shifted or the data has decayed, liquidating their agency.
constraint_indexing:constraint_classification(evidence_half_life, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the decay as a Rope—the essential coordination 
% substrate for maintaining a high-velocity, low-friction information market.
constraint_indexing:constraint_classification(evidence_half_life, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.81) > 0.70 triggers Piton: the "Official Archive" 
% is an inertial spike; it performatively signals stability while siphoning 0.85 of agency.
constraint_indexing:constraint_classification(evidence_half_life, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.85) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(evidence_half_life, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(evidence_half_life, E), E >= 0.50,
    domain_priors:suppression_score(evidence_half_life, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evidence_half_life_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless investigator vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(evidence_half_life, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evidence_half_life, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.81) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(evidence_half_life, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(evidence_half_life, E),

    E > 0.70.

:- end_tests(evidence_half_life_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of real-time info is achieved by liquidating the 
 * long-term stability of the epistemic commons.
 * 
 * * PERSPECTIVAL GAP:
 * The Forensic Investigator feels a Snare because their work requires a 
 * persistence that the medium actively destroys. The Platform sees a Rope 
 * because the rapid turnover coordinates the attention of millions in a 
 * dynamic, low-latency environment.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Data Integrity" protocol is no longer functional (Theater 0.81); 
 * it is an inert spike siphoning 0.85 of the subject's investigative agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_verification_latency,
    'Can verification speed ever match decay speed, or is the "Snare" a physical law of networks (Snare vs Mountain)?',
    'Tracking the success rate of multi-year archival grounding against real-time drift.',
    'If archival grounding holds: Snare of current tech. If it fails: Mountain of Information Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(evidence_half_life, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
