% ============================================================================
% CONSTRAINT STORY: lehman_repo_105
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_lehman_repo_105, []).

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
    narrative_ontology:omega_variable/5.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: lehman_repo_105
 * human_readable: Lehman Brothers Repo 105 Accounting Maneuver
 * domain: economic
 * * SUMMARY:
 * Repo 105 was an accounting maneuver used by Lehman Brothers to temporarily move
 * billions in liabilities off its balance sheet, primarily to hide leverage from
 * investors and rating agencies. While structured to be legally defensible as a
 * "sale" under specific accounting rules (its claim), it functioned as a tool for
 * systemic deception, extracting trust and capital from the market.
 * * KEY AGENTS:
 * - Counterparty Investors: The subjects, who were unaware of the true leverage.
 * - Lehman Senior Management: The beneficiaries, who used the tool to meet targets.
 * - Financial System Auditors: The analytical observers, assessing the structural impact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(lehman_repo_105, 0.90). % High extraction of trust and capital via deception.
domain_priors:suppression_score(lehman_repo_105, 0.85).   % High suppression of transparent accounting alternatives.
domain_priors:theater_ratio(lehman_repo_105, 0.10).       % Low theater; it was a functional, not performative, tool.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(lehman_repo_105, extractiveness, 0.90).
narrative_ontology:constraint_metric(lehman_repo_105, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(lehman_repo_105, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It was presented as a legitimate financial coordination tool.
narrative_ontology:constraint_claim(lehman_repo_105, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(lehman_repo_105). % Required legal opinions and internal processes to execute.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(lehman_repo_105, lehman_senior_management).
narrative_ontology:constraint_victim(lehman_repo_105, counterparty_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For an uninformed investor, this was a trap. Their capital was exposed to
% undisclosed risks, and their exit options were based on false information.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For Lehman's management, this was a coordination tool (a Rope) used to
% manage balance sheet perceptions and meet leverage targets.
constraint_indexing:constraint_classification(lehman_repo_105, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, Repo 105 is a Tangled Rope. It has a genuine coordination
% function (for the beneficiaries) but is coupled with severe asymmetric
% extraction from victims and requires active enforcement (legal opinions)
% to maintain its structure.
constraint_indexing:constraint_classification(lehman_repo_105, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lehman_repo_105_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(lehman_repo_105, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lehman_repo_105, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict into a Tangled Rope.
    constraint_indexing:constraint_classification(lehman_repo_105, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeAnalytical = tangled_rope.

test(threshold_validation_snare) :-
    % Verify that the base extraction meets the Snare/Tangled Rope threshold.
    domain_priors:base_extractiveness(lehman_repo_105, E),
    E >= 0.46.

:- end_tests(lehman_repo_105_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the findings of the Valukas Report. Base extractiveness is 0.90
 * because the maneuver was designed for maximum deception, extracting trust and
 * capital based on misleading financial statements. Suppression is 0.85 as it
 * actively prevented the use of standard, transparent accounting alternatives.
 *
 * The key insight is the perspectival gap. For executives (institutional), it was a
 * 'Rope'—a tool to coordinate financial reporting. For investors (powerless), it was
 * a 'Snare'—a hidden trap concealing massive risk. The analytical perspective
 * resolves this conflict by classifying it as a 'Tangled Rope'. This classification
 * is critical because it acknowledges both the coordination function (why it was
 * used internally) and the severe, asymmetric extraction (its external effect),
 * which a simple 'Snare' classification would miss.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The original analysis might have classified this as a
 * 'Mountain' from a legalistic viewpoint ("it was technically legal"). The
 * Tangled Rope classification prevents this error. It correctly identifies that
 * even if a constraint leverages a legal or regulatory framework (the 'Mountain'
 * of accounting rules), its function as a tool for coordination with asymmetric
 * extraction makes it a constructed, not natural, constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% This omega captures the core legal ambiguity that enabled the maneuver.
omega_variable(
    omega_lehman_repo_105,
    'Was the legal opinion that Repo 105 constituted a "true sale" a colorable claim or professional malpractice?',
    'A definitive court ruling on the fiduciary duty of the law firm (Linklaters) providing the opinion.',
    'If a colorable claim, the constraint leans towards a clever exploitation of a system Mountain. If malpractice, it confirms the Snare was constructed with full knowledge of its deceptive nature.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lehman_repo_105, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Repo 105 was a high-intensity, short-duration maneuver. The extraction and
% theater were consistent throughout its use leading up to the 2008 collapse.
% The data reflects a stable, high-extraction state rather than a gradual drift.

% Theater ratio over time (consistently low):
narrative_ontology:measurement(lehman_repo_105_tr_t0, lehman_repo_105, theater_ratio, 0, 0.10).
narrative_ontology:measurement(lehman_repo_105_tr_t5, lehman_repo_105, theater_ratio, 5, 0.10).
narrative_ontology:measurement(lehman_repo_105_tr_t10, lehman_repo_105, theater_ratio, 10, 0.10).

% Extraction over time (consistently high):
narrative_ontology:measurement(lehman_repo_105_ex_t0, lehman_repo_105, base_extractiveness, 0, 0.90).
narrative_ontology:measurement(lehman_repo_105_ex_t5, lehman_repo_105, base_extractiveness, 5, 0.90).
narrative_ontology:measurement(lehman_repo_105_ex_t10, lehman_repo_105, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: It was a mechanism to reallocate liabilities on a balance sheet.
narrative_ontology:coordination_type(lehman_repo_105, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */