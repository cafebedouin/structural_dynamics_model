% ============================================================================
% CONSTRAINT STORY: iron_law_of_oligarchy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_iron_law_of_oligarchy, []).

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
 * * constraint_id: iron_law_of_oligarchy
 * human_readable: The Iron Law of Oligarchy
 * domain: political/social
 * * SUMMARY:
 * Proposed by Robert Michels, this principle states that all complex organizations,
 * regardless of how democratic at the start, eventually develop into oligarchies.
 * The necessity of bureaucracy and specialized knowledge creates a leadership class
 * that prioritizes its own survival over the group's original goals.
 * * KEY AGENTS:
 * - Rank-and-File Member: Subject (Powerless), the original source of power who becomes alienated.
 * - Leadership Elite: Beneficiary (Institutional), benefits from control of the administrative apparatus.
 * - Organizational Scientist: Auditor (Analytical), observes the shift from horizontal ideals to vertical hierarchies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(iron_law_of_oligarchy, 0.60). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(iron_law_of_oligarchy, 0.60).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(iron_law_of_oligarchy, 0.15).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(iron_law_of_oligarchy, extractiveness, 0.60).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(iron_law_of_oligarchy, tangled_rope).
narrative_ontology:human_readable(iron_law_of_oligarchy, "The Iron Law of Oligarchy").
narrative_ontology:topic_domain(iron_law_of_oligarchy, "political/social").

% Binary flags
domain_priors:requires_active_enforcement(iron_law_of_oligarchy). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(iron_law_of_oligarchy, leadership_elite).
narrative_ontology:constraint_victim(iron_law_of_oligarchy, rank_and_file_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE RANK-AND-FILE MEMBER (SNARE)
% The hierarchy that was meant to empower the collective now strangles dissent
% and extracts value (power, status, resources) for the leadership.
% χ = 0.60 * 1.5 (powerless) * 0.8 (local) = 0.72.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE LEADERSHIP ELITE (ROPE)
% For the leadership, the hierarchy is a necessary tool for coordination.
% It enables the organization to function effectively at scale.
% χ = 0.60 * -0.2 (institutional) * 1.0 (national) = -0.12.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the necessary coordination function (Rope) and the
% asymmetric extraction (Snare). The structure requires active enforcement to
% maintain the leadership's position, making it a canonical Tangled Rope.
% χ = 0.60 * 1.15 (analytical) * 1.2 (global) = 0.828.
constraint_indexing:constraint_classification(iron_law_of_oligarchy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iron_law_of_oligarchy_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify base metrics meet the criteria for a high-extraction constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(iron_law_of_oligarchy, ExtMetricName, E),
    E >= 0.46.

:- end_tests(iron_law_of_oligarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Iron Law of Oligarchy is a classic example of a constraint that appears
 * as a natural law (Mountain) but functions as a hybrid (Tangled Rope).
 * - Base Extractiveness (0.60): High. The leadership class extracts power, status,
 *   and control over resources from the rank-and-file.
 * - Suppression Score (0.60): High. Alternatives like pure horizontalism are
 *   suppressed by the argument of "technical necessity" and "efficiency" at scale.
 * - Perspectival Gap: The leadership experiences the hierarchy as a pure
 *   coordination tool (Rope), essential for achieving goals. The members, however,
 *   experience it as a Snare, where their power has been co-opted by an entrenched elite.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system resolves the mandatrophy of this constraint by classifying it as a
 * Tangled Rope from the analytical perspective. A simpler analysis might see only
 * the Snare (from the victim's view) or accept the constraint's claim to be a
 * Mountain/natural law. The Tangled Rope classification correctly identifies that
 * there is a *genuine coordination function* (the organization can do things)
 * that is *coupled with asymmetric extraction* (the leaders benefit disproportionately).
 * This prevents mislabeling a necessary, if flawed, structure as purely predatory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_iron_law_of_oligarchy,
    'Do modern communication technologies (e.g., DAOs, liquid democracy) fundamentally break the technical necessity of oligarchy, or do they just create new forms of elite capture?',
    'Longitudinal study of large-scale DAOs, tracking Gini coefficients of governance token holdings and decision-making power over a decade.',
    'If technology breaks the law, the constraint is a Scaffold of a low-bandwidth era. If not, it is a deep-seated Mountain of social physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(iron_law_of_oligarchy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the oligarchy hardening over time. Initially, extraction
% is lower as the organization is more aligned with its goals. As the leadership
% class solidifies, extraction and performative bureaucracy increase.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ilo_tr_t0, iron_law_of_oligarchy, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ilo_tr_t5, iron_law_of_oligarchy, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ilo_tr_t10, iron_law_of_oligarchy, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ilo_ex_t0, iron_law_of_oligarchy, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ilo_ex_t5, iron_law_of_oligarchy, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ilo_ex_t10, iron_law_of_oligarchy, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The oligarchy's primary function is to direct the organization's assets and efforts.
narrative_ontology:coordination_type(iron_law_of_oligarchy, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */