% ============================================================================
% CONSTRAINT STORY: monetary_regime_transition
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_monetary_regime_transition, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: monetary_regime_transition
 * human_readable: Sovereign Fiat Currency Regime
 * domain: economic/political
 * * SUMMARY:
 * This constraint models the properties of a modern sovereign fiat currency system, which emerged after the transition from commodity-backed money (like the Gold Standard). The system functions as a coordination tool for the state to manage economic activity, but simultaneously enables value extraction from savers and wage-earners via inflation and monetary policy. The narrative arc is the shift from a perceived 'Mountain' (gold's scarcity) to a complex hybrid system.
 * * KEY AGENTS:
 * - Fixed-Income Saver: Subject (Powerless), experiences value erosion over time.
 * - Central Bank/Sovereign State: Beneficiary (Institutional), uses monetary policy as a flexible tool.
 * - Systems Auditor: Observer (Analytical), analyzes the dual coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(monetary_regime_transition, 0.55). % Extraction via seigniorage and inflation tax.
domain_priors:suppression_score(monetary_regime_transition, 0.40).   % Alternatives (gold, crypto, foreign currency) are regulated/suppressed.
domain_priors:theater_ratio(monetary_regime_transition, 0.10).       % Primarily functional, low theatricality.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(monetary_regime_transition, extractiveness, 0.55).
narrative_ontology:constraint_metric(monetary_regime_transition, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(monetary_regime_transition, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Fiat systems are presented as necessary tools for economic coordination.
narrative_ontology:constraint_claim(monetary_regime_transition, tangled_rope).
narrative_ontology:human_readable(monetary_regime_transition, "Sovereign Fiat Currency Regime").

% Binary flags
domain_priors:requires_active_enforcement(monetary_regime_transition). % Legal tender laws, capital controls.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(monetary_regime_transition, sovereign_debt_issuers).
narrative_ontology:constraint_victim(monetary_regime_transition, fixed_income_savers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a saver with fixed income, inflation is a direct, coercive extraction of
% purchasing power. Effective extraction χ = 0.55 * 1.5 (powerless) * 1.0 (national) = 0.825.
constraint_indexing:constraint_classification(monetary_regime_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the central bank, the system is a flexible coordination tool (a Rope)
% for managing economic cycles. Effective extraction is negative,
% χ = 0.55 * -0.2 (institutional) * 1.0 (national) = -0.11, implying a net benefit.
constraint_indexing:constraint_classification(monetary_regime_transition, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the coordination function (beneficiaries exist) and the
% asymmetric extraction (victims exist), supported by active enforcement. This
% combination defines a Tangled Rope.
constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_regime_transition_tests).

test(perspectival_gap) :-
    % Verify the saver (powerless) sees a Snare while the state (institutional) sees a Rope.
    constraint_indexing:constraint_classification(monetary_regime_transition, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_regime_transition, rope,
        context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify the necessary properties for a Tangled Rope classification are declared.
    narrative_ontology:constraint_beneficiary(monetary_regime_transition, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(monetary_regime_transition, _),     % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(monetary_regime_transition).

:- end_tests(monetary_regime_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the dual nature of fiat currency. The base extractiveness of 0.55 represents a persistent, moderate inflationary tax that compounds over a biographical timeline. The suppression score of 0.40 acknowledges that while alternatives exist, they face significant regulatory friction.
 * The Perspectival Gap is stark: for the institutional beneficiary (the state), it's a Rope for macroeconomic coordination. For the powerless subject (the saver), it's a Snare that erodes wealth.
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical here. It prevents the system from collapsing the analysis into a simplistic binary of "good coordination" (Rope) or "bad extraction" (Snare). It correctly identifies the constraint as a system that performs a genuine coordination function for one group while imposing coercive, asymmetric costs on another, sustained by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_monetary_regime_transition,
    'Is the inflationary extraction an unavoidable bug of fiat systems (a cost of flexibility) or a deliberate feature for wealth transfer and debt monetization?',
    'Analysis of central bank mandates, historical policy decisions, and political discourse surrounding inflation targets.',
    'If a bug, it might be fixable (Rope). If a feature, it is an intentional extraction mechanism (Snare component is primary).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(monetary_regime_transition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the period from the stable Bretton Woods system (T=0) through
% its breakdown and the subsequent era of discretionary monetary policy (T=10).
% Extraction accumulates as the constraints of gold backing are removed.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(mrt_tr_t0, monetary_regime_transition, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mrt_tr_t5, monetary_regime_transition, theater_ratio, 5, 0.08).
narrative_ontology:measurement(mrt_tr_t10, monetary_regime_transition, theater_ratio, 10, 0.10).

% Extraction over time (increases after link to gold is severed):
narrative_ontology:measurement(mrt_ex_t0, monetary_regime_transition, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(mrt_ex_t5, monetary_regime_transition, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mrt_ex_t10, monetary_regime_transition, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Fiat currency is a primary mechanism for allocating resources in an economy.
narrative_ontology:coordination_type(monetary_regime_transition, resource_allocation).

% Network relationships (structural influence edges)
% Monetary policy directly enables and influences the sustainability of national debt.
narrative_ontology:affects_constraint(monetary_regime_transition, national_debt_ceiling).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */