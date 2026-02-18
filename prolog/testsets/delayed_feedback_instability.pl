% ============================================================================
% CONSTRAINT STORY: delayed_feedback_instability
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_delayed_feedback_instability, []).

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
 * * constraint_id: delayed_feedback_instability
 * human_readable: The Oscillation Trap
 * domain: systems_engineering/economics/ecology
 * * SUMMARY:
 * This constraint occurs in systems where there is a significant temporal lag
 * between an action and its observable outcome. This delay causes agents to
 * over-correct (or under-correct), leading to violent oscillations and
 * systemic instability. It functions as a Snare for the individual trying to
 * maintain balance, while appearing as a Rope for institutions that profit
 * from the resulting volatility or use it to justify centralized control.
 * * KEY AGENTS:
 * - Reactive Agent: Subject (Powerless)
 * - Volatility Arbitrageur: Beneficiary (Institutional)
 * - Control Systems Engineer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.80) because the "blindness" caused by the delay allows
% the system to siphon the agent's resources through forced, inefficient
% over-corrections.
domain_priors:base_extractiveness(delayed_feedback_instability, 0.80).
domain_priors:suppression_score(delayed_feedback_instability, 0.65).
domain_priors:theater_ratio(delayed_feedback_instability, 0.45).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(delayed_feedback_instability, extractiveness, 0.80).
narrative_ontology:constraint_metric(delayed_feedback_instability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(delayed_feedback_instability, theater_ratio, 0.45).

% Constraint self-claim: The system presents its dynamics as an unavoidable
% consequence of complex interactions, a natural law of feedback.
narrative_ontology:constraint_claim(delayed_feedback_instability, tangled_rope).
narrative_ontology:human_readable(delayed_feedback_instability, "The Oscillation Trap").
narrative_ontology:topic_domain(delayed_feedback_instability, "systems_engineering/economics/ecology").

% Binary flags & Structural properties for Tangled Rope
domain_priors:requires_active_enforcement(delayed_feedback_instability).
narrative_ontology:constraint_beneficiary(delayed_feedback_instability, volatility_arbitrageurs).
narrative_ontology:constraint_victim(delayed_feedback_instability, reactive_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The agent is trapped: every effort to stabilize the system only increases
% the magnitude of the next swing because the data they act on is stale.
constraint_indexing:constraint_classification(delayed_feedback_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the instability as a Rope—a way to coordinate
% market shifts or justify "emergency" powers to manage the chaos they
% structurally ignore.
constraint_indexing:constraint_classification(delayed_feedback_instability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a mathematical perspective, the Nyquist-Shannon limit and phase
% shifts in feedback loops represent an irreducible Mountain of physics.
constraint_indexing:constraint_classification(delayed_feedback_instability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% The combination of a coordination function (for beneficiaries) and asymmetric
% extraction (from victims) under active enforcement makes this a Tangled Rope.
constraint_indexing:constraint_classification(delayed_feedback_instability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(delayed_feedback_instability_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless agent vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(delayed_feedback_instability, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(delayed_feedback_instability, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(delayed_feedback_instability, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify that all structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(delayed_feedback_instability, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(delayed_feedback_instability, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(delayed_feedback_instability).

:- end_tests(delayed_feedback_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.80) is high because the system's inherent delay
 * creates a structural information asymmetry that is exploited. Agents acting
 * on stale data are forced into inefficient over-corrections, and the
 * resulting energy/resource loss is captured by those who can arbitrage the
 * volatility. The suppression score (0.65) reflects the difficulty of
 * obtaining real-time feedback; alternatives are suppressed by physics and
 * information cost, not just policy.
 *
 * * PERSPECTIVAL GAP:
 * The Reactive Agent feels a Snare because they are punished for systemic
 * delays they cannot control. The Volatility Arbitrageur sees a Rope because
 * predictable cycles of over-correction allow for resource capture and market
 * coordination. The physicist sees a Mountain in the math of feedback loops.
 *
 * * [RESOLVED MANDATROPHY]:
 * The high extraction (0.80) creates a Mandatrophy state where a system that
 * claims to be a natural law (Mountain) is functioning as a highly extractive
 * mechanism (Snare). This is resolved by the Tangled Rope classification. It
 * correctly identifies the hybrid nature of the constraint: a genuine
 * coordination function (for arbitrageurs) is "tangled" with severe,
 * asymmetric extraction (from reactive agents), all enforced by the system's
 * physical time lag. This avoids misclassifying the entire system as pure
 * extraction or an immutable natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_lag_reduction,
    'Is the feedback delay an irreducible physical limit (Mountain) or a solvable technological/political problem (making this a Snare)?',
    'Auditing the delta between predictive AI accuracy and actual system lag; identifying if information is being deliberately withheld or is truly unobtainable.',
    'If AI/policy can stabilize the system, it was a constructed Snare. If instability persists despite perfect information, it is a Mountain of physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(delayed_feedback_instability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified as systems became more complex and interconnected,
% increasing both the potential for extraction and the need for performative
% "crisis management" narratives.
%
% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(dfi_tr_t0, delayed_feedback_instability, theater_ratio, 0, 0.20).
narrative_ontology:measurement(dfi_tr_t5, delayed_feedback_instability, theater_ratio, 5, 0.35).
narrative_ontology:measurement(dfi_tr_t10, delayed_feedback_instability, theater_ratio, 10, 0.45).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(dfi_ex_t0, delayed_feedback_instability, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(dfi_ex_t5, delayed_feedback_instability, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(dfi_ex_t10, delayed_feedback_instability, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The system's "coordination" function is the allocation of risk and reward
% based on the predictable oscillations.
narrative_ontology:coordination_type(delayed_feedback_instability, resource_allocation).

% This type of instability is a foundational dynamic that affects the stability
% of higher-level regulatory and economic systems.
narrative_ontology:affects_constraint(delayed_feedback_instability, financial_market_regulation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */