% ============================================================================
% CONSTRAINT STORY: israel_electoral_threshold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_israel_electoral_threshold, []).

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
 * * constraint_id: israel_electoral_threshold
 * human_readable: The 3.25% Knesset Electoral Threshold
 * domain: political
 * * SUMMARY:
 * A statutory requirement that a political party must receive at least 3.25%
 * of the national vote to be allocated seats in the 120-member Knesset.
 * While intended to prevent extreme fragmentation and stabilize coalitions,
 * it creates a high-stakes barrier where "wasted votes" for parties narrowly
 * missing the mark can fundamentally shift the balance of power.
 * * KEY AGENTS:
 * - Small Minority Parties: Subject (Powerless) - Face binary outcome of representation or total exclusion.
 * - Major Party Blocs: Beneficiary (Institutional) - Benefit from the consolidation of votes and the elimination of small rivals.
 * - Electoral Auditor: Auditor (Analytical) - Evaluates the trade-off between governance stability and representational fidelity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% High extraction (wasted votes) in the 2022 context, where 289,000 votes for parties
% like Meretz (3.16%) and Balad (2.91%) were discarded.
domain_priors:base_extractiveness(israel_electoral_threshold, 0.48). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
% Suppression score is high because the law mandates a specific closed-list
% proportional representation system with no alternative mechanism for
% small parties to gain seats unless they form alliances.
domain_priors:suppression_score(israel_electoral_threshold, 0.65).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(israel_electoral_threshold, 0.15).       % Functional, not theatrical. Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(israel_electoral_threshold, extractiveness, 0.48).
narrative_ontology:constraint_metric(israel_electoral_threshold, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(israel_electoral_threshold, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% The stated purpose is to stabilize governance.
narrative_ontology:constraint_claim(israel_electoral_threshold, tangled_rope).
narrative_ontology:human_readable(israel_electoral_threshold, "The 3.25% Knesset Electoral Threshold").
narrative_ontology:topic_domain(israel_electoral_threshold, "political").

% Binary flags
domain_priors:requires_active_enforcement(israel_electoral_threshold). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(israel_electoral_threshold, major_party_blocs).
narrative_ontology:constraint_victim(israel_electoral_threshold, small_minority_parties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For small parties (powerless) over a biographical/election cycle horizon,
% the threshold acts as a Snare that captures votes without providing representation.
constraint_indexing:constraint_classification(israel_electoral_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the governing institutions, it is a Rope (coordination) intended to solve
% the "collective action problem" of hyper-fragmented, unstable coalitions.
constraint_indexing:constraint_classification(israel_electoral_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% From a historical view, it is a Tangled Rope: it provides coordination (stability)
% but does so through the asymmetric extraction of political voice from
% fringe or minority demographics.
constraint_indexing:constraint_classification(israel_electoral_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_electoral_threshold_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(israel_electoral_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_electoral_threshold, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies it as a Tangled Rope.
    constraint_indexing:constraint_classification(israel_electoral_threshold, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Confirms it is classified as a high-extraction constraint.
    narrative_ontology:constraint_metric(israel_electoral_threshold, extractiveness, E),
    E >= 0.46.

:- end_tests(israel_electoral_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 3.25% threshold represents a classic "Perspectival Gap." To the institutional
 * designer, it is an essential "Rope" for governance—without it, the 24th Knesset's
 * narrow 61-seat majority might have been even more precarious.
 * However, to parties like Meretz or Balad in 2022, it was a "Snare": they received
 * significant vote shares (3.16% and 2.91%) but were mathematically erased from
 * the legislature, leading to "wasted" votes that altered the national outcome.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is vital here. It prevents the system from
 * seeing the threshold as *purely* extractive (Snare). The "coordination"
 * dividend is the reduction of tiny, single-issue parties that can hold
 * entire governments hostage—a persistent issue in Israeli political history.
 * By requiring beneficiary/victim declarations, the system correctly identifies
 * both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_israel_electoral_threshold,
    'Does the 3.25% threshold actually increase coalition stability, or merely increase the frequency of snap elections by raising the stakes of defection?',
    'Comparative analysis of coalition longevity before and after the 2014 threshold hike.',
    'If it fails to stabilize, the Rope/Tangled Rope classifications collapse, and it may be better modeled as a Piton (inertial non-functional constraint) or pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. The interval represents the period of
% historical adjustments to the threshold (1% -> 1.5% -> 2% -> 3.25%).
narrative_ontology:interval(israel_electoral_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. The threshold has been raised
% several times, increasing its extractive potential. This models that drift.
% T=0: Early period (1% threshold)
% T=5: Intermediate period (2% threshold)
% T=10: Modern period (3.25% threshold)

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(israel_electoral_threshold_tr_t0, israel_electoral_threshold, theater_ratio, 0, 0.15).
narrative_ontology:measurement(israel_electoral_threshold_tr_t5, israel_electoral_threshold, theater_ratio, 5, 0.15).
narrative_ontology:measurement(israel_electoral_threshold_tr_t10, israel_electoral_threshold, theater_ratio, 10, 0.15).

% Extraction over time (increases as the threshold is raised):
narrative_ontology:measurement(israel_electoral_threshold_ex_t0, israel_electoral_threshold, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(israel_electoral_threshold_ex_t5, israel_electoral_threshold, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(israel_electoral_threshold_ex_t10, israel_electoral_threshold, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% An electoral threshold is a rule for allocating power.
narrative_ontology:coordination_type(israel_electoral_threshold, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(israel_electoral_threshold, 0.0).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(israel_electoral_threshold, [other_constraint_id]).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */