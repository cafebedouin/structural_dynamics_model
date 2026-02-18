% ============================================================================
% CONSTRAINT STORY: collective_stupidity_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_collective_stupidity_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: collective_stupidity_2026
 * human_readable: The Cipolla-Galloway Stupidity Snare
 * domain: social/behavioral
 * * SUMMARY:
 * Stupidity is defined as causing damage to others without deriving personal gain.
 * This creates a "Snare" because its lack of rational structure makes it
 * impossible to organize a defense against. It is independent of credentials,
 * affecting Ph.D.s and Presidents alike, leading to a "Sucking Black Hole" of
 * societal utility.
 * * KEY AGENTS:
 * - Rational Agents (The Non-Stupid): Subject (Powerless)
 * - Institutions (Affected Organizations): Subject (Institutional)
 * - Systems Theorists: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(collective_stupidity_2026, 0.88). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(collective_stupidity_2026, 0.75).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(collective_stupidity_2026, 0.92).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(collective_stupidity_2026, extractiveness, 0.88).
narrative_ontology:constraint_metric(collective_stupidity_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(collective_stupidity_2026, theater_ratio, 0.92).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
% It is framed as an immutable law of human nature, but its metrics reveal a Snare.
narrative_ontology:constraint_claim(collective_stupidity_2026, snare).
narrative_ontology:human_readable(collective_stupidity_2026, "The Cipolla-Galloway Stupidity Snare").
narrative_ontology:topic_domain(collective_stupidity_2026, "social/behavioral").

% Structural property derivation hooks:
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(collective_stupidity_2026, chaos_entropy).
narrative_ontology:constraint_victim(collective_stupidity_2026, rational_social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE RATIONAL AGENT (SNARE)
% For the non-stupid, stupidity is a Snare: a trap that lacks rational
% structure and cannot be negotiated with or mitigated by reasoning.
% χ = 0.88 * π(powerless:1.5) * σ(global:1.2) = 1.584. A clear Snare.
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE INSTITUTION (PITON)
% Organizations often treat stupidity as a Piton: an inertial, non-functional
% behavior that persists despite causing constant damage. The high theater_ratio
% (0.92) and negative effective extraction (χ = 0.88 * π(institutional:-0.2) = -0.176)
% confirm this view. It's a pure cost center.
constraint_indexing:constraint_classification(collective_stupidity_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The analytical observer sees the high base extraction (0.88) and high
% suppression (0.75) and must classify it as a Snare. The claim that it is a
% "natural law" is captured by constraint_claim/2, but the metrics do not
% support a Mountain classification (which requires ε <= 0.15).
constraint_indexing:constraint_classification(collective_stupidity_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_stupidity_2026_tests).

test(perspectival_gap_powerless_vs_institutional) :-
    % Verify the gap between the powerless agent (who sees a Snare)
    % and the institution (which sees a broken, inertial Piton).
    constraint_indexing:constraint_classification(collective_stupidity_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_stupidity_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == piton),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_snare) :-
    % Verify the analytical observer correctly identifies a Snare,
    % overriding the philosophical claim of it being a natural law.
    constraint_indexing:constraint_classification(collective_stupidity_2026, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == snare).

test(piton_threshold_validation) :-
    % Verify the theater ratio is high enough to justify the Piton classification.
    domain_priors:theater_ratio(collective_stupidity_2026, TR),
    assertion(TR >= 0.70).

:- end_tests(collective_stupidity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models stupidity as a pure deadweight loss, hence the extremely
 * high base_extractiveness (0.88). The key insight is the perspectival gap:
 * individuals see a malicious trap (Snare), while institutions see a dysfunctional,
 * inertial process (Piton), confirmed by the high theater_ratio (0.92).
 *
 * The original model's classification of the analytical view as a 'Mountain' was
 * a metric violation. This version corrects it to 'Snare', while moving the
 * "immutability" argument to `constraint_claim(natural_law)`. This allows the
 * system to correctly identify the constraint's function (a Snare) while
 * acknowledging its narrative framing (a natural law).
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.88) triggers Mandatrophy review. The resolution is
 * achieved by strictly adhering to the metric-based classification. The system
 * correctly identifies this as a Snare, not a Mountain, because its base
 * extraction far exceeds the Mountain threshold (ε <= 0.15). It is not a
 * Tangled Rope because it has no coordination function; the beneficiary is
 * abstractly defined as 'chaos_entropy', indicating a lack of a coherent
 * coordinating group. The system thus avoids being misled by the philosophical
 * framing of stupidity as a "law of nature."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_stupidity_duration,
    'Can the "duration and frequency" of stupidity be structurally reduced by system design?',
    'Analysis of feedback loops and incentive structures in decentralized autonomous organizations vs. traditional hierarchies.',
    'Success would lower suppression and extraction, potentially shifting the constraint toward a Piton or Rope; Failure confirms it as a permanent Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(collective_stupidity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio remains high as stupidity is performative by nature.
narrative_ontology:measurement(st_tr_t0, collective_stupidity_2026, theater_ratio, 0, 0.92).
narrative_ontology:measurement(st_tr_t5, collective_stupidity_2026, theater_ratio, 5, 0.92).
narrative_ontology:measurement(st_tr_t10, collective_stupidity_2026, theater_ratio, 10, 0.92).

% Extraction rises as the "Sucking Black Hole" of stupidity accumulates damage.
narrative_ontology:measurement(st_ex_t0, collective_stupidity_2026, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(st_ex_t5, collective_stupidity_2026, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(st_ex_t10, collective_stupidity_2026, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No coordination type is declared as stupidity is definitionally
% anti-coordination. No network relationships are modeled at this time.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */