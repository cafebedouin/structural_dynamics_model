% ============================================================================
% CONSTRAINT STORY: structural_autonomy_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_autonomy_access, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_autonomy_access
 *   human_readable: Structural Autonomy Access Differential
 *   domain: philosophy_of_work/political_economy/ethics
 *
 * SUMMARY:
 *   The structural autonomy access differential describes the empirical
 *   observation that access to autonomous work (self-employment, independent
 *   practice, entrepreneurship) is strongly predicted by prior wealth
 *   position. This is not a claim about policy or institutional design — it
 *   is a claim about the mathematical structure of capital accumulation.
 *   Autonomous work requires upfront investment: tools, workspace, inventory,
 *   runway capital to survive the ramp-up period, reputation building, client
 *   acquisition. Access to investment capital is distributed according to
 *   prior asset position: those with wealth can self-finance or borrow at
 *   favorable rates; those without wealth face prohibitive interest rates or
 *   cannot borrow at all. Risk tolerance scales with asset position: those
 *   with safety nets can absorb entrepreneurial failure; those without safety
 *   nets cannot afford the risk. The constraint is classified as a mountain
 *   because these dynamics emerge from the mathematics of compound returns
 *   and risk aversion, not from any agent's active enforcement. However, the
 *   MAGNITUDE of the constraint — how severe the capital requirement is, how
 *   unequal the wealth distribution is, how accessible credit is — depends on
 *   policy choices (inheritance law, credit regulation, safety net design,
 *   intellectual property duration, land use policy). The mountain is the
 *   structural fact that autonomy requires capital and capital is unequally
 *   distributed. The height of the mountain is a policy variable.
 *
 * KEY AGENTS:
 *   - Asset-Poor Worker: Powerless/trapped — no inherited wealth, no collateral, no safety net; wage labor is the only accessible option
 *   - Professional Employee: Moderate/constrained — has skills and income but capital threshold for autonomy recedes as fast as savings accumulate
 *   - Inherited-Wealth Beneficiary: Institutional/arbitrage — autonomy is accessible due to prior generations' accumulation; benefits from asymmetry but did not create it
 *   - Worker Cooperative Movement: Organized/constrained — collective action can pool resources but cannot eliminate structural capital requirement
 *   - Analytical Observer: Analytical/analytical — recognizes constraint as mathematical consequence of capital dynamics, not policy choice (though magnitude is policy-dependent)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_autonomy_access, 0.18).
domain_priors:suppression_score(structural_autonomy_access, 0.03).
domain_priors:theater_ratio(structural_autonomy_access, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_autonomy_access, extractiveness, 0.18).
narrative_ontology:constraint_metric(structural_autonomy_access, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(structural_autonomy_access, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_autonomy_access, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(structural_autonomy_access, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_autonomy_access, mountain).
narrative_ontology:human_readable(structural_autonomy_access, "Structural Autonomy Access Differential").
narrative_ontology:topic_domain(structural_autonomy_access, "philosophy_of_work/political_economy/ethics").

domain_priors:emerges_naturally(structural_autonomy_access).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASSET-POOR WORKER (MOUNTAIN) — Experiences the capital requirement for autonomous work as an unchangeable barrier. No inherited wealth, no collateral for business loans, no safety net to absorb entrepreneurial risk. The constraint appears as a law of economic gravity: you need money to make money, and without initial capital, wage labor is the only accessible option. Biographical time horizon shows no path to accumulation sufficient for independence.
constraint_indexing:constraint_classification(structural_autonomy_access, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROFESSIONAL EMPLOYEE (MOUNTAIN) — Has skills and income but lacks the asset base for true autonomy. Could theoretically save toward independence, but the capital threshold (equipment, workspace, runway capital, client acquisition costs) recedes as fast as savings accumulate. The constraint appears as a structural feature of modern professional practice: even high-skill work increasingly requires institutional backing (credentials, liability insurance, client networks, infrastructure access) that independent practitioners cannot afford. Exit is constrained by the gap between salary and autonomy threshold.
constraint_indexing:constraint_classification(structural_autonomy_access, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INHERITED-WEALTH BENEFICIARY (MOUNTAIN) — Experiences the same structural constraint but from the opposite side: autonomy is accessible because prior generations accumulated the necessary capital. This agent does not experience extraction — they benefit from the constraint's asymmetry — but they still perceive it as a mountain: the capital requirement for autonomy is a fixed feature of economic reality, not something they created or maintain. Their arbitrage exit option (can choose wage labor or autonomy freely) does not change the constraint's immutability from their perspective.
constraint_indexing:constraint_classification(structural_autonomy_access, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Recognizes the constraint as a structural feature of capital accumulation dynamics: autonomous work requires upfront investment (tools, workspace, inventory, runway capital, reputation building), and access to investment capital is distributed according to prior wealth position. This is not a policy choice or institutional arrangement that could be otherwise — it is a mathematical consequence of how capital compounds and how risk tolerance scales with asset position. The constraint is a mountain because it emerges from the interaction of time preference, risk aversion, and compound returns, not from any agent's enforcement. However, the analytical observer must distinguish this structural feature from the policy choices that determine its severity (inheritance law, credit access, safety net design, intellectual property duration). The constraint itself — that autonomy requires capital and capital is unequally distributed — is a mountain. The magnitude of the inequality is not.
constraint_indexing:constraint_classification(structural_autonomy_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: ORGANIZED LABOR / COOPERATIVE MOVEMENT (MOUNTAIN) — Organized agents attempting to build alternative ownership structures (worker cooperatives, mutual aid funds, community land trusts) still face the underlying capital requirement. Collective action can pool resources and share risk, but it cannot eliminate the structural need for upfront investment. The cooperative movement experiences the constraint as a mountain with a lower summit: collective ownership reduces the per-person capital threshold, but the threshold remains. Generational time horizon reflects the long timeline required to build cooperative capital bases that compete with inherited wealth.
constraint_indexing:constraint_classification(structural_autonomy_access, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_autonomy_access_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(structural_autonomy_access, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_autonomy_access, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(structural_autonomy_access, ExtMetricName, E),
    domain_priors:suppression_score(structural_autonomy_access, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(structural_autonomy_access),
    narrative_ontology:constraint_metric(structural_autonomy_access, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(structural_autonomy_access, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(structural_autonomy_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint does create asymmetry — those with prior wealth have access to autonomy that those without wealth lack — but this asymmetry is not extraction in the DR sense. No agent is actively taking from another. The differential access emerges from the mathematics of capital accumulation: compound returns, risk aversion scaling with wealth, time preference differences. The low extractiveness reflects that the constraint is a structural feature of how capital works, not a mechanism by which one group extracts from another. The 0.18 value (rather than 0.05) reflects that the asymmetry does concentrate opportunity in ways that compound over generations, but this is closer to a coordination problem (how to enable autonomy for those without inherited capital) than to extraction. Suppression (0.03): Very low. The constraint does not suppress alternatives through active enforcement. Asset-poor workers are not prevented from attempting autonomous work — they face structural barriers (capital requirements, risk exposure) but not coercive suppression. The low suppression reflects that the constraint emerges naturally from capital dynamics rather than being maintained by force. Theater ratio (0.12): Very low. There is minimal performative content. The capital requirement for autonomous work is functional, not theatrical. The constraint does not persist through ritual or legitimation narratives — it persists because autonomous work genuinely requires upfront investment. Accessibility collapse (0.92): Very high. For agents without capital, the constraint is nearly impossible to circumvent through individual effort alone. The capital threshold is not a matter of skill or effort — it is a matter of asset position. Resistance (0.08): Very low. Attempts to change the constraint through individual action (saving toward autonomy, bootstrapping a business) face structural resistance from the mathematics of compound returns and risk exposure. The constraint is highly resistant to individual-level intervention.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this constraint as mountain, but they experience it from opposite sides of the capital asymmetry. The asset-poor worker sees an insurmountable barrier to autonomy. The inherited-wealth beneficiary sees the same structural feature but experiences it as an open door. The professional employee sees a receding threshold — the capital requirement grows as fast as savings accumulate. The cooperative movement sees a lower summit through collective action but still faces the underlying capital requirement. The analytical observer sees the mathematical structure: autonomous work requires upfront investment, investment capital is distributed by prior wealth, and this creates a structural differential in autonomy access. The gap is not in classification (all see mountain) but in experienced position: some are on the valley floor looking up, some are on the summit looking down, and the analytical observer sees the topography itself. This is a uniform-type constraint — mountain from all perspectives — but the uniformity reveals rather than conceals the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the structural sense because it is a mountain — it emerges naturally from capital dynamics rather than being maintained by any agent. The inherited-wealth beneficiary experiences the constraint's asymmetry in their favor, but they are not extracting from the asset-poor worker. The asymmetry is a consequence of prior accumulation, not active extraction. All perspectives classify as mountain because all agents perceive the constraint as an unchangeable structural feature, though they experience it from different sides of the asymmetry. The analytical observer recognizes that the constraint's magnitude (how severe the capital requirement is, how unequal the distribution is) depends on policy choices, but the constraint's existence — that autonomy requires capital and capital is unequally distributed — is a mathematical consequence of how capital compounds.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low extractiveness does not imply absence of structural asymmetry. The constraint is a mountain (not a snare) because it emerges from capital dynamics rather than active enforcement, but it still creates differential access to autonomy based on prior economic position. The mountain classification prevents mislabeling a structural feature of capital accumulation as active extraction, while the low-but-nonzero extractiveness (0.18) acknowledges that the asymmetry does concentrate opportunity in ways that compound over generations. The constraint is not 'fair' or 'just' — it is a structural fact. The policy question is not whether the constraint exists (it does, as a mathematical consequence of capital dynamics) but what magnitude of asymmetry is acceptable and what interventions (inheritance policy, credit access, safety nets, cooperative ownership structures) can reduce the height of the mountain without eliminating the structural need for capital.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_autonomy_access, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_autonomy_access, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is distinct from policy-level constraints about inheritance law, credit access, or safety net design. Those constraints determine the MAGNITUDE of the autonomy access differential, but this constraint describes the structural fact that the differential exists as a consequence of capital dynamics. Decomposition: structural_autonomy_access (this story, ε=0.18, mountain) is the base-level mathematical structure. Downstream constraints about specific policy interventions (inheritance_tax_policy, credit_access_regulation, safety_net_design) would have higher ε values and different classifications depending on their extractive vs coordinative balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
