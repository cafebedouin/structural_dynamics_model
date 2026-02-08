% ============================================================================
% CONSTRAINT STORY: germany_tennet_takeover
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_germany_tennet_takeover, []).

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
 * * constraint_id: germany_tennet_takeover
 * human_readable: German Government Stake in TenneT Germany
 * domain: economic/political
 * * SUMMARY:
 * The German government is buying a significant stake in TenneT Germany, a crucial electricity grid operator, to ensure energy security during the green energy transition and prevent foreign takeovers. This acquisition is framed as a vital infrastructure investment, funded by taxpayers.
 * * KEY AGENTS:
 * - German Citizens: Subject (Powerless)
 * - German Government & Industry: Beneficiary (Institutional)
 * - Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(germany_tennet_takeover, 0.35). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46. This is in the Tangled Rope range.
domain_priors:suppression_score(germany_tennet_takeover, 0.20).   % Structural property (raw, unscaled). Low suppression as there is a genuine public good.
domain_priors:theater_ratio(germany_tennet_takeover, 0.10).       % Piton detection (>= 0.70). Low theater; this is a functional, not performative, action.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(germany_tennet_takeover, extractiveness, 0.35).
narrative_ontology:constraint_metric(germany_tennet_takeover, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(germany_tennet_takeover, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(germany_tennet_takeover, coordination).

% Binary flags
domain_priors:requires_active_enforcement(germany_tennet_takeover). % Required for Tangled Rope. The state's legal/financial framework enforces the monopoly.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, german_state_and_industry).
narrative_ontology:constraint_victim(germany_tennet_takeover, german_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Taxpayers feel the cost directly. χ = 0.35 * 1.5 (powerless) * 1.0 (national) = 0.525.
% This high effective extraction, combined with being trapped, feels like a Snare.
constraint_indexing:constraint_classification(germany_tennet_takeover, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The state sees this as pure coordination. χ = 0.35 * -0.2 (institutional) * 1.0 (national) = -0.07.
% The negative extraction reflects a perceived net benefit and control gain.
constraint_indexing:constraint_classification(germany_tennet_takeover, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Acknowledges both the coordination function (beneficiary exists) and the asymmetric
% extraction (victim exists), enforced by the state. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% This is not a scaffold as there is no sunset clause.
% constraint_indexing:constraint_classification(germany_tennet_takeover, scaffold, ...).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% This is not a piton as the theater_ratio is 0.10, far below the 0.70 threshold.
% The constraint is highly functional.
% constraint_indexing:constraint_classification(germany_tennet_takeover, piton, ...).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germany_tennet_takeover_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the subject and beneficiary.
    constraint_indexing:constraint_classification(germany_tennet_takeover, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germany_tennet_takeover, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(germany_tennet_takeover_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The German government's stake in TenneT is a classic Tangled Rope. The base extractiveness (0.35) is moderate, reflecting the use of public funds for a project with a genuine, if debatable, public good component.
 * The PERSPECTIVAL GAP is stark:
 * - For taxpayers (powerless), the mandatory funding feels like a Snare (effective extraction χ=0.525).
 * - For the state (institutional), it's a Rope providing control and stability (effective extraction χ=-0.07).
 * - The Analytical observer synthesizes these views. It sees the valid coordination function (grid stability) and the asymmetric extraction (taxpayer funding), which, combined with state enforcement, defines a Tangled Rope.
 * The theater_ratio is low (0.10) because this is a direct, functional intervention, not a performative act. This correctly prevents a Piton classification.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is crucial. A simpler model might call this a Snare (focusing only on the taxpayer) or a Rope (focusing only on the state's intent). Tangled Rope correctly identifies that it is BOTH: a coordination mechanism funded by asymmetric extraction. This prevents the system from ignoring either the genuine benefit or the coercive cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_germany_tennet_takeover,
    'Will state ownership lead to greater efficiency and security, or will it introduce political inefficiencies that outweigh the benefits?',
    'Long-term comparative analysis of grid performance, costs, and innovation rates against privately-held grid operators in similar markets.',
    'If more efficient, it validates the Rope aspect. If less efficient, it strengthens the Snare aspect and increases the effective extraction over time.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(germany_tennet_takeover, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% While base_extractiveness (0.35) is below the 0.46 threshold requiring this
% data, it is included to model the initial state and potential for future drift.

% Theater ratio over time (starts low and stays low):
narrative_ontology:measurement(germany_tennet_takeover_tr_t0, germany_tennet_takeover, theater_ratio, 0, 0.05).
narrative_ontology:measurement(germany_tennet_takeover_tr_t5, germany_tennet_takeover, theater_ratio, 5, 0.10).
narrative_ontology:measurement(germany_tennet_takeover_tr_t10, germany_tennet_takeover, theater_ratio, 10, 0.10).

% Extraction over time (initial investment cost, then stable):
narrative_ontology:measurement(germany_tennet_takeover_ex_t0, germany_tennet_takeover, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(germany_tennet_takeover_ex_t5, germany_tennet_takeover, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(germany_tennet_takeover_ex_t10, germany_tennet_takeover, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a state intervention to secure a foundational utility.
narrative_ontology:coordination_type(germany_tennet_takeover, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */