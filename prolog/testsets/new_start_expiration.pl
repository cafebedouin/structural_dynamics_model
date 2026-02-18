% ============================================================================
% CONSTRAINT STORY: new_start_expiration
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_new_start_expiration, []).

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
 * * constraint_id: new_start_expiration
 * human_readable: New START Treaty Expiration
 * domain: political
 * * SUMMARY:
 * The expiration of the New START treaty, which limited US and Russian nuclear arsenals, creates a new constraint landscape. The absence of formal limits and verification protocols establishes a dynamic conducive to a renewed arms race. This new reality diverts resources to military spending and suppresses diplomatic alternatives.
 * * KEY AGENTS:
 * - Global Public: Subject (Powerless), facing increased risk and resource diversion.
 * - Nuclear Arms Industry: Beneficiary (Institutional), profiting from increased defense spending.
 * - Arms Control Community: Auditor (Analytical), observing the degradation of international security norms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(new_start_expiration, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(new_start_expiration, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(new_start_expiration, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(new_start_expiration, extractiveness, 0.6).
narrative_ontology:constraint_metric(new_start_expiration, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(new_start_expiration, theater_ratio, 0.2).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
% The new dynamic is a constructed reality based on strategic decisions.
narrative_ontology:constraint_claim(new_start_expiration, tangled_rope).
narrative_ontology:human_readable(new_start_expiration, "New START Treaty Expiration").
narrative_ontology:topic_domain(new_start_expiration, "political").

% Binary flags
domain_priors:requires_active_enforcement(new_start_expiration). % Required for Tangled Rope. The arms race dynamic is self-enforcing through mutual suspicion.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(new_start_expiration, nuclear_arms_industry).
narrative_ontology:constraint_victim(new_start_expiration, global_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The global public is trapped in a dynamic that extracts public funds for
% private profit and increases existential risk.
% χ = 0.6 * 1.5 (powerless) * 1.2 (global) = 1.08 (Snare)
constraint_indexing:constraint_classification(new_start_expiration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The arms industry sees the removal of limits as a coordination mechanism
% for securing large, long-term government contracts.
% χ = 0.6 * -0.2 (institutional) * 1.0 (national) = -0.12 (Rope)
constraint_indexing:constraint_classification(new_start_expiration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees a system with a residual coordination function (mutual
% deterrence) but which is dominated by asymmetric extraction (public funds
% to private industry) and high suppression of alternatives.
% χ = 0.6 * 1.15 (analytical) * 1.2 (global) = 0.828 (Tangled Rope/Snare)
constraint_indexing:constraint_classification(new_start_expiration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(new_start_expiration_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(new_start_expiration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(new_start_expiration, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Ensures it's a high-extraction constraint.
    narrative_ontology:constraint_metric(new_start_expiration, extractiveness, E),
    E >= 0.46.

test(tangled_rope_properties_present) :-
    % Verify that all structural requirements for a Tangled Rope are declared.
    domain_priors:requires_active_enforcement(new_start_expiration),
    narrative_ontology:constraint_beneficiary(new_start_expiration, _),
    narrative_ontology:constraint_victim(new_start_expiration, _).

:- end_tests(new_start_expiration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The constraint is not the treaty itself, but the dynamic created by its expiration. This dynamic is an arms race, which is highly extractive (0.6) as it diverts public funds, and highly suppressive (0.7) as it marginalizes diplomatic solutions.
 * The perspectival gap is stark: for the global public (powerless), it's a Snare, trapping them in a high-risk, high-cost reality. For the arms industry (institutional), the lack of limits is a Rope, coordinating state behavior to guarantee profits. The analytical view recognizes both aspects, classifying it as a Tangled Rope: a system with a perverse coordination function (mutual deterrence) that enables massive, asymmetric extraction.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. A simpler model might see only the Snare aspect (the danger and cost to the public). However, that would miss the powerful coordination function that benefits the arms industry. By identifying the beneficiaries and the coordination logic that serves them, the Tangled Rope classification prevents mislabeling the system as pure, chaotic extraction. It correctly identifies it as a constructed, self-stabilizing system of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_new_start_expiration,
    'Is the post-treaty dynamic a rapid, uncontrolled arms race or a slower, more managed strategic competition?',
    'Monitoring deployed warhead counts, missile tests, and defense budget allocations over a decade.',
    'If rapid race (Snare dominates), global risk increases sharply. If managed competition (Tangled Rope is stable), extraction continues but existential risk is lower.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(new_start_expiration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the gradual emergence of the arms race dynamic after
% the treaty (the prior constraint) expires at T=0.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time: Remains low as the arms race is functional, not performative.
narrative_ontology:measurement(new_start_expiration_tr_t0, new_start_expiration, theater_ratio, 0, 0.1).
narrative_ontology:measurement(new_start_expiration_tr_t5, new_start_expiration, theater_ratio, 5, 0.15).
narrative_ontology:measurement(new_start_expiration_tr_t10, new_start_expiration, theater_ratio, 10, 0.2).

% Extraction over time: Ramps up as new weapons programs are funded and deployed.
narrative_ontology:measurement(new_start_expiration_ex_t0, new_start_expiration, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(new_start_expiration_ex_t5, new_start_expiration, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(new_start_expiration_ex_t10, new_start_expiration, base_extractiveness, 10, 0.6).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The residual coordination is the logic of mutual deterrence, a form of
% enforcement mechanism.
narrative_ontology:coordination_type(new_start_expiration, enforcement_mechanism).

% Network relationships (structural influence edges)
% The expiration of the treaty directly influences national defense budgets.
narrative_ontology:affects_constraint(new_start_expiration, global_defense_spending).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */