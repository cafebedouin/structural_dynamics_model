% ============================================================================
% CONSTRAINT STORY: gig_economy_algorithmic_management
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_gig_economy_algorithmic_management, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: gig_economy_algorithmic_management
 * human_readable: Algorithmic Management in the Gig Economy
 * domain: economic/technological
 * * SUMMARY:
 * The "Gig Economy" labor market is governed by algorithmic management, where platforms use
 * opaque, data-driven systems to assign tasks, set prices, and evaluate performance. This
 * creates a significant power and information asymmetry between the platform and its workers.
 * The constraint is the set of rules embedded in this algorithmic system.
 * * KEY AGENTS:
 * - Gig Worker: Subject (Powerless), subject to algorithmic dispatch and deactivation without traditional due process.
 * - Platform Operator: Beneficiary (Institutional), holds rule-making power and owns the market infrastructure.
 * - Systems Auditor: Observer (Analytical), analyzes the structural properties of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: High extraction (0.8) as platforms capture significant surplus value while shifting
% operational risks (vehicle maintenance, insurance, downtime) onto the individual worker.
domain_priors:base_extractiveness(gig_economy_algorithmic_management, 0.80).
% Rationale: High suppression (0.7) as the algorithm is a "black box." Workers cannot see how pay is
% calculated or tasks are assigned, suppressing their ability to negotiate or optimize their strategy.
domain_priors:suppression_score(gig_economy_algorithmic_management, 0.70).
% Rationale: Low theater; the system is brutally functional and not performative.
domain_priors:theater_ratio(gig_economy_algorithmic_management, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(gig_economy_algorithmic_management, extractiveness, 0.80).
narrative_ontology:constraint_metric(gig_economy_algorithmic_management, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(gig_economy_algorithmic_management, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The platform claims the algorithm is a neutral, efficient coordination tool.
narrative_ontology:constraint_claim(gig_economy_algorithmic_management, tangled_rope).
narrative_ontology:human_readable(gig_economy_algorithmic_management, "Algorithmic Management in the Gig Economy").
narrative_ontology:topic_domain(gig_economy_algorithmic_management, "economic/technological").

% Binary flags
% The system requires constant, automated enforcement (deactivations, warnings, incentive changes).
domain_priors:requires_active_enforcement(gig_economy_algorithmic_management).

% Structural property derivation hooks:
% Both beneficiaries and victims exist, enabling Tangled Rope classification.
narrative_ontology:constraint_beneficiary(gig_economy_algorithmic_management, platform_shareholders).
narrative_ontology:constraint_beneficiary(gig_economy_algorithmic_management, time_constrained_consumers).
narrative_ontology:constraint_victim(gig_economy_algorithmic_management, full_time_gig_workers).
narrative_ontology:constraint_victim(gig_economy_algorithmic_management, traditional_service_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE GIG WORKER (SNARE)
% High extraction felt as a predatory trap with no alternatives.
constraint_indexing:constraint_classification(gig_economy_algorithmic_management, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PLATFORM OPERATOR (ROPE)
% Viewed as essential coordination infrastructure for market efficiency.
constraint_indexing:constraint_classification(gig_economy_algorithmic_management, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes the dual nature: a genuine coordination function for consumers
% that is inextricably linked with asymmetric extraction from workers.
constraint_indexing:constraint_classification(gig_economy_algorithmic_management, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_economy_algorithmic_management_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the worker (powerless) and the platform (institutional).
    constraint_indexing:constraint_classification(gig_economy_algorithmic_management, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_economy_algorithmic_management, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict into a Tangled Rope.
    constraint_indexing:constraint_classification(gig_economy_algorithmic_management, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(gig_economy_algorithmic_management, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(gig_economy_algorithmic_management_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the perspectival gap. For the platform operator (institutional),
 * the algorithm is a pure Rope: a brilliant tool for coordinating supply (drivers) and demand
 * (riders) with minimal friction. They perceive the extraction as market-rate payment for this
 * service. For the full-time worker (powerless, trapped), the same system is a Snare. The
 * opacity of the algorithm, the constant risk of deactivation, and the shifting of all
 * operational costs onto them creates a highly extractive and coercive environment.
 * The analytical observer must classify this as a Tangled Rope because both functions are real
 * and inseparable: it has a genuine coordination function (beneficiaries exist) AND it has
 * asymmetric extraction (victims exist), and it requires active enforcement to maintain.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A worker might perceive the algorithm's logic as an unchangeable Mountain ("that's just how
 * the market is"). However, this is a constructed system. The objective function of the
 * algorithm (e.g., maximize platform revenue vs. maximize driver earnings) is a policy choice,
 * not a natural law. Because its parameters are tunable and serve specific beneficiaries at the
 * expense of victims, it cannot be a Mountain. The ambiguity is resolved in favor of a
 * constructed constraint (Tangled Rope/Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_gig_economy_algorithmic_management,
    "Would making the algorithm 100% transparent resolve the Snare, or is the extractiveness inherent in the payout ratios themselves?",
    "Comparative analysis of 'transparent' coop platforms vs. 'black-box' corporate platforms.",
    "If pay stabilizes with transparency, it was a suppression-based Snare. If pay stays low, it is a pure economic Snare based on labor surplus.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gig_economy_algorithmic_management, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (E=0.80 > 0.46).
% Models the platform's lifecycle: initial focus on growth (lower extraction)
% followed by a shift to profitability (higher extraction).
%
% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(gig_economy_tr_t0, gig_economy_algorithmic_management, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gig_economy_tr_t5, gig_economy_algorithmic_management, theater_ratio, 5, 0.08).
narrative_ontology:measurement(gig_economy_tr_t10, gig_economy_algorithmic_management, theater_ratio, 10, 0.10).

% Extraction over time (increases as platform matures and seeks profit):
narrative_ontology:measurement(gig_economy_ex_t0, gig_economy_algorithmic_management, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(gig_economy_ex_t5, gig_economy_algorithmic_management, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(gig_economy_ex_t10, gig_economy_algorithmic_management, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The algorithm's primary function is to allocate resources (drivers) to demand (riders).
narrative_ontology:coordination_type(gig_economy_algorithmic_management, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */