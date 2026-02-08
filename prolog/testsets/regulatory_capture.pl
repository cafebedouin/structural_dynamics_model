% ============================================================================
% CONSTRAINT STORY: regulatory_capture
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_regulatory_capture, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: regulatory_capture
 * human_readable: Regulatory Capture
 * domain: economic/political
 * * SUMMARY:
 * Regulatory capture occurs when a regulatory agency, created to act in the
 * public interest, instead advances the commercial or political concerns of
 * special interest groups that dominate the industry it is charged with
 * regulating. It represents a corruption of the "referee" into a "player,"
 * creating barriers to entry and extracting consumer surplus.
 * * KEY AGENTS:
 * - Disruptive Entrepreneur: Subject (Powerless) who is blocked by regulations written by incumbents.
 * - Incumbent Firm / Lobbyist: Beneficiary (Institutional) who uses regulation as a tool for market control.
 * - Systems Analyst: Auditor (Analytical) who observes the structural decay and emergent properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(regulatory_capture, 0.80). % Extracts consumer surplus and innovation energy into monopoly profits.
domain_priors:suppression_score(regulatory_capture, 0.20).   % Alternatives are suppressed not by force, but by immense legal/bureaucratic complexity.
domain_priors:theater_ratio(regulatory_capture, 0.75).       % The agency performs public-interest duties theatrically, while its real function is gatekeeping.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(regulatory_capture, extractiveness, 0.80).
narrative_ontology:constraint_metric(regulatory_capture, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(regulatory_capture, theater_ratio, 0.75).

% Constraint self-claim (what does the constraint claim to be?)
% The agency claims to be an enforcement mechanism for the public good.
narrative_ontology:constraint_claim(regulatory_capture, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(regulatory_capture). % Required for Tangled Rope; regulations must be actively enforced to function as barriers.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(regulatory_capture, industry_monopolists).
narrative_ontology:constraint_beneficiary(regulatory_capture, bureaucratic_elites).
narrative_ontology:constraint_victim(regulatory_capture, innovative_startups).
narrative_ontology:constraint_victim(regulatory_capture, general_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE DISRUPTIVE ENTREPRENEUR (SNARE)
% For the innovator, capture is a Snare. They have a better, cheaper
% solution, but the "safety" regulations (written by their competitors)
% make it illegal to operate. The harder they try to compete, the more
% the legal trap tightens.
constraint_indexing:constraint_classification(regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE FORTUNE 500 LOBBYIST (ROPE)
% For the lobbyist, regulation is a Rope. It is a coordination mechanism to
% "standardize" the industry in a way that favors their firm's existing
% infrastructure, pulling up a wall against new entrants.
constraint_indexing:constraint_classification(regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PUBLIC CHOICE ECONOMIST (TANGLED ROPE)
% The analyst sees a system with a genuine coordination function (for incumbents)
% but also massive asymmetric extraction (from the public). It requires active
% enforcement to maintain this structure. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(regulatory_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INSTITUTIONAL ANALYST (PITON)
% This analyst sees the agency as a Piton. Created as a Rope to ensure
% market safety, it has decayed into a liability that persists through
% inertia. Its stated coordinating function is now mostly theater.
constraint_indexing:constraint_classification(regulatory_capture, piton,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))) :-
    domain_priors:theater_ratio(regulatory_capture, TR), TR > 0.70.


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_tests).

test(perspectival_gap) :-
    % Verify the core gap between the powerless (Snare) and institutional (Rope).
    constraint_indexing:constraint_classification(regulatory_capture, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % The default analytical view should resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(regulatory_capture, tangled_rope, context(agent_power(analytical), time_horizon(civilizational), _, _)).

test(piton_classification_requires_high_theater) :-
    % The Piton classification should only trigger if theater_ratio is high.
    domain_priors:theater_ratio(regulatory_capture, TR),
    ( TR > 0.70 ->
        constraint_indexing:constraint_classification(regulatory_capture, piton, context(agent_power(analytical), time_horizon(biographical), _, _))
    ;   \+ constraint_indexing:constraint_classification(regulatory_capture, piton, context(agent_power(analytical), time_horizon(biographical), _, _))
    ).

:- end_tests(regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that began with a coordination purpose but has since been co-opted for extraction.
 * - Extractiveness (0.80): High, representing the direct transfer of wealth from consumers and innovators to incumbents via price distortions and barriers to entry.
 * - Suppression (0.20): Low, because the system doesn't use overt force. It uses "boring" weapons: legal complexity, high compliance costs, and bureaucratic inertia.
 * - Theater Ratio (0.75): High, as the agency must maintain the performance of serving the public interest to retain its legitimacy, even as its primary function becomes gatekeeping. This justifies the Piton classification from a career-level analyst's perspective.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark: for the incumbent with institutional power, the regulations are a useful 'Rope' to coordinate the market and manage competition. For the startup with no power, the same regulations are an impassable 'Snare' designed to make them fail.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is crucial. It prevents the system from simplifying this to a pure Snare, which would ignore the genuine (if self-serving) coordination function the regulations provide for the incumbent players. It correctly identifies that the system has BOTH a coordination function AND an asymmetric extraction function, which is the core of sophisticated institutional failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_capture_reversibility,
    'Can a captured agency ever be reset to neutral through internal reform, or is total institutional collapse and replacement the only exit?',
    'Comparative historical analysis of anti-trust actions and successful regulatory "house-cleanings" vs. failed reforms.',
    'If irreversible (Mountain-like tendency), then reform efforts are wasted; parallel systems are the only solution. If reversible (Tangled Rope), reform is viable.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(regulatory_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the degradation of a public interest agency over time.
% It starts with low extraction and theater (a Rope) and drifts into a high
% extraction, high theater state (a Tangled Rope / Piton).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rc_tr_t0, regulatory_capture, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rc_tr_t5, regulatory_capture, theater_ratio, 5, 0.45).
narrative_ontology:measurement(rc_tr_t10, regulatory_capture, theater_ratio, 10, 0.75).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rc_ex_t0, regulatory_capture, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(rc_ex_t5, regulatory_capture, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(rc_ex_t10, regulatory_capture, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The agency's function is to enforce rules, making it an enforcement mechanism.
narrative_ontology:coordination_type(regulatory_capture, enforcement_mechanism).

% Network relationships (structural influence edges)
% Capture in one area often enables or reinforces capture in adjacent domains.
narrative_ontology:affects_constraint(regulatory_capture, media_ownership_rules).
narrative_ontology:affects_constraint(regulatory_capture, pharmaceutical_patents).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */