% ============================================================================
% CONSTRAINT STORY: demographic_irreversibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_irreversibility, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: demographic_irreversibility
 *   human_readable: Demographic Irreversibility in Advanced Economies
 *   domain: political_economy/development_economics/comparative_systems
 *
 * SUMMARY:
 *   The global working-age population (ages 15-64) peaked in 2015 at
 *   approximately 4.8 billion and is projected to decline by 60 million by
 *   2035, with steeper declines in advanced economies (Japan, Germany, Italy,
 *   South Korea) and China. This trajectory is determined by cohort sizes
 *   locked in by past fertility decisions: the cohorts entering working age
 *   2015-2035 were born 1950-2020, and their sizes are already known. Policy
 *   can modulate the impact (immigration, retirement age, labor force
 *   participation) but cannot reverse the aggregate trajectory within the
 *   constraint's time horizon. The constraint exhibits mountain
 *   classification from all six perspectives, including beneficiaries with
 *   maximum exit options, making it a candidate for genuine natural law
 *   rather than false summit. However, the presence of identifiable
 *   beneficiaries (capital holders who gain bargaining power from labor
 *   scarcity, automation sector, healthcare industry) triggers the false
 *   summit detector, requiring omega analysis to distinguish immutable
 *   arithmetic from naturalized policy framing.
 *
 * KEY AGENTS:
 *   - Individual Worker: Powerless/trapped — cannot alter national demographic structure; experiences constraint as immutable labor market condition
 *   - National Government: Institutional/constrained — has policy instruments but faces 20-year lag and cohort arithmetic limits
 *   - Multinational Corporation: Institutional/arbitrage — can relocate across national markets but faces same constraint at global scope; benefits from labor scarcity
 *   - Regional Economic Bloc: Organized/constrained — can redistribute demographic pressure but cannot create new cohorts
 *   - Mobile Elite: Powerful/mobile — can access younger markets but cannot escape aggregate global trajectory
 *   - Analytical Observer: Analytical/analytical — sees cohort arithmetic as immutable within 2015-2035 window
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_irreversibility, 0.08).
domain_priors:suppression_score(demographic_irreversibility, 0.02).
domain_priors:theater_ratio(demographic_irreversibility, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_irreversibility, extractiveness, 0.08).
narrative_ontology:constraint_metric(demographic_irreversibility, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(demographic_irreversibility, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(demographic_irreversibility, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(demographic_irreversibility, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_irreversibility, mountain).
narrative_ontology:human_readable(demographic_irreversibility, "Demographic Irreversibility in Advanced Economies").
narrative_ontology:topic_domain(demographic_irreversibility, "political_economy/development_economics/comparative_systems").

domain_priors:emerges_naturally(demographic_irreversibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demographic_irreversibility, capital_holders).
narrative_ontology:constraint_beneficiary(demographic_irreversibility, automation_sector).
narrative_ontology:constraint_beneficiary(demographic_irreversibility, healthcare_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL WORKER (MOUNTAIN) — Cannot alter aggregate demographic trajectory through individual action. Experiences declining working-age population as immutable constraint on labor market conditions, wage dynamics, and social insurance sustainability. No exit from national demographic structure within biographical timeframe.
constraint_indexing:constraint_classification(demographic_irreversibility, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NATIONAL GOVERNMENT (MOUNTAIN) — Policy instruments (immigration, family subsidies, retirement age) can modulate but not reverse the trajectory. Cohort sizes are locked in by past fertility decisions. Even aggressive pro-natalist policy faces 20-year lag before new cohorts enter workforce. Constrained exit reflects policy space limitations, but the demographic momentum itself is immutable.
constraint_indexing:constraint_classification(demographic_irreversibility, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL CORPORATION (MOUNTAIN) — Can arbitrage across national labor markets, relocating production to younger-population regions. But global working-age population also peaked (2015) and is declining. Arbitrage provides temporary relief, not escape from the aggregate constraint. Even beneficiaries with maximum exit options face the same mountain at global scope.
constraint_indexing:constraint_classification(demographic_irreversibility, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL ECONOMIC BLOC (MOUNTAIN) — Coordinated immigration policy, labor mobility agreements, and pension harmonization can redistribute demographic pressure but cannot create new working-age cohorts. EU, ASEAN, USMCA all face the same aggregate constraint. Organized collective action changes distribution of impact, not the underlying trajectory.
constraint_indexing:constraint_classification(demographic_irreversibility, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — Cohort sizes are determined by fertility decisions made 20-65 years prior. The 2015 peak in global working-age population reflects fertility decline that began in the 1970s-1990s. Barring catastrophic mortality or unprecedented fertility rebound, the 2015-2035 decline is locked in. This is not policy, ideology, or institutional arrangement — it is arithmetic applied to existing cohorts.
constraint_indexing:constraint_classification(demographic_irreversibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: MOBILE ELITE (MOUNTAIN) — High-skill workers and capital holders can relocate to younger markets (Sub-Saharan Africa, South Asia) and access global investment opportunities. But even maximum individual mobility does not escape the aggregate constraint — global household formation rates, consumption growth, and dependency ratios are determined by the total working-age population trajectory, which is declining regardless of where the elite positions itself.
constraint_indexing:constraint_classification(demographic_irreversibility, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_irreversibility_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(demographic_irreversibility, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_irreversibility, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(demographic_irreversibility, ExtMetricName, E),
    domain_priors:suppression_score(demographic_irreversibility, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(demographic_irreversibility),
    narrative_ontology:constraint_metric(demographic_irreversibility, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(demographic_irreversibility, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(demographic_irreversibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The demographic trajectory itself is arithmetic applied to existing cohorts, not an extraction mechanism. The slight extractiveness reflects that policy responses to the trajectory (pension cuts, immigration restriction, labor market deregulation) may benefit capital holders, but the underlying constraint is not extractive. The rising trajectory (0.05 to 0.08) reflects increasing policy theater and potential for extractive responses as the decline becomes more visible. Suppression (0.02): Minimal. No agent is coerced by the demographic trajectory itself — it is a structural condition, not an imposed rule. The low suppression reflects that agents have full information and can adjust behavior (migration, savings, fertility) even though they cannot reverse the aggregate trajectory. Theater ratio (0.15): Low. Most demographic policy (pro-natalist subsidies, immigration targets, pension reform) has genuine functional intent even when ineffective at reversing the trajectory. The slight theater reflects political posturing around 'demographic crisis' framing, but the constraint itself is not performative. Accessibility collapse (0.92): Very high. All agents, regardless of power or exit options, face the same cohort arithmetic. Resistance (0.08): Very low. The constraint emerges from biological and social facts (fertility timing, cohort aging) without institutional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits uniform mountain classification across all six perspectives, including agents with maximum exit options (multinational corporations, mobile elite) and agents with maximum power (national governments, regional blocs). The uniformity is diagnostic: genuine natural laws appear as mountains from all perspectives because no agent can alter the underlying structure. The perspectival invariance distinguishes this constraint from false summits, where powerless agents see mountains but powerful agents see ropes or scaffolds. However, the presence of beneficiaries creates an analytical tension: if the constraint benefits identifiable agents, why does it appear as a mountain even to those agents? The resolution is that the beneficiaries cannot control the constraint (they did not cause the fertility decline and cannot reverse it), but they do benefit from policy responses to it. The omega variable on false summit identification addresses whether the 'crisis' framing serves to naturalize extractive policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's beneficiaries are declared to trigger false summit detection, but their presence does not make the constraint extractive in the traditional sense. Capital holders benefit from labor scarcity (stronger bargaining position, wage suppression), the automation sector benefits from demand for labor-saving technology, and the healthcare industry benefits from an aging population. However, these benefits are second-order effects of the demographic trajectory, not the constraint's primary function. The constraint itself is arithmetic — cohort sizes determined by past fertility. The engine will derive low directionality values for all agents because the constraint is not targeted extraction. Even the beneficiaries experience the constraint as a mountain (they cannot escape the aggregate trajectory), though they may benefit from policy responses to it. The false summit omega variable addresses whether the 'demographic crisis' framing naturalizes extractive policy choices.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN WITH BENEFICIARIES (FALSE SUMMIT CANDIDATE): The constraint passes all mountain gates (ε ≤ 0.25, suppression ≤ 0.05, emerges_naturally, accessibility_collapse ≥ 0.85, resistance ≤ 0.15) and classifies as mountain from all perspectives, including beneficiaries. This is the signature of either (a) genuine natural law with incidental beneficiaries, or (b) false summit where naturalization is so complete that even beneficiaries perceive immutability. The distinguishing test is counterfactual: if the beneficiaries disappeared, would the constraint persist? For demographic irreversibility, the answer is yes — capital holders, automation sector, and healthcare industry did not cause the fertility decline and cannot reverse it. They benefit from the trajectory and from policy responses to it, but the trajectory itself is arithmetic applied to existing cohorts. The constraint is a genuine mountain with incidental beneficiaries, not a false summit. However, the 'demographic crisis' framing and policy responses (austerity, immigration restriction, labor deregulation) may constitute separate constraints that ARE false summits — extractive policies naturalized by invoking the genuine demographic mountain. The omega variable on false summit identification captures this distinction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fertility_rebound_possibility,
    'Could unprecedented pro-natalist policy or cultural shift produce fertility rebound sufficient to reverse the working-age population decline within the 2015-2035 window?',
    'Historical fertility transition analysis; evaluation of pro-natalist policy effectiveness in Japan, Singapore, France, Hungary; biological and social constraints on fertility timing',
    'If fertility rebounds to replacement (2.1 TFR) by 2025, new cohorts enter workforce 2043-2060 — outside the 2035 horizon. The 2015-2035 decline remains locked in even under optimistic fertility scenarios.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fertility_rebound_possibility, empirical, 'Whether fertility rebound could reverse the trajectory within the constraint''s time horizon').

omega_variable(
    immigration_substitution_capacity,
    'Can immigration fully substitute for native-born cohort decline in maintaining working-age population levels?',
    'Analysis of immigration absorption capacity; political economy of immigration policy; source country demographic trajectories; integration and productivity effects',
    'If immigration can fully substitute: the constraint is mountain at national scope but rope at global scope (coordination problem in labor allocation). If immigration cannot substitute (political limits, source exhaustion, integration costs): the constraint is mountain at all scopes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immigration_substitution_capacity, empirical, 'Whether immigration can offset native-born cohort decline').

omega_variable(
    automation_demand_offset,
    'Does automation-driven productivity growth offset the demand deficit from declining household formation, or does it compound the problem by reducing labor income share?',
    'Decomposition of GDP growth into productivity vs. labor force components; analysis of automation''s effect on wage share and consumption capacity; comparison of high-automation vs. high-immigration adjustment paths',
    'If automation offsets demand deficit: the constraint''s extractiveness is lower (genuine coordination via technology). If automation compounds the problem: the constraint''s extractiveness is higher (productivity gains accrue to capital while consumption base shrinks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_demand_offset, empirical, 'Whether automation offsets or compounds the demographic demand deficit').

omega_variable(
    false_summit_identification,
    'Is the demographic trajectory genuinely immutable (mountain), or does framing it as such naturalize policy choices (immigration restriction, pension austerity, labor market deregulation) that benefit capital holders?',
    'Cross-national comparison of demographic adjustment paths; identification of policy variation within similar demographic constraints; analysis of who benefits from ''demographic crisis'' framing',
    'If genuinely immutable: mountain classification is correct across all perspectives. If naturalized: the constraint is a false summit — the arithmetic is real, but the ''crisis'' framing and policy responses serve identifiable beneficiaries (capital holders who benefit from labor scarcity, automation sector, healthcare industry serving aging population).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_identification, conceptual, 'Whether demographic irreversibility is genuine natural law or naturalized policy constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_irreversibility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demo_irrev_theater_2015, demographic_irreversibility, theater_ratio, 0, 0.1).
narrative_ontology:measurement(demo_irrev_theater_2020, demographic_irreversibility, theater_ratio, 5, 0.12).
narrative_ontology:measurement(demo_irrev_theater_2025, demographic_irreversibility, theater_ratio, 10, 0.13).
narrative_ontology:measurement(demo_irrev_theater_2030, demographic_irreversibility, theater_ratio, 15, 0.14).
narrative_ontology:measurement(demo_irrev_theater_2035, demographic_irreversibility, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(demo_irrev_extract_2015, demographic_irreversibility, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(demo_irrev_extract_2020, demographic_irreversibility, base_extractiveness, 5, 0.06).
narrative_ontology:measurement(demo_irrev_extract_2025, demographic_irreversibility, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(demo_irrev_extract_2030, demographic_irreversibility, base_extractiveness, 15, 0.08).
narrative_ontology:measurement(demo_irrev_extract_2035, demographic_irreversibility, base_extractiveness, 20, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_irreversibility, resource_allocation).

% DUAL FORMULATION NOTE:
% Demographic irreversibility is a foundational constraint that affects multiple downstream policy domains (pension sustainability, healthcare financing, immigration policy, labor market regulation) but is not itself decomposable into multiple observables with different epsilon values. The cohort arithmetic is invariant across measurement methodologies. Downstream policy constraints may have their own epsilon values reflecting extractive vs. coordinative responses to the demographic trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
