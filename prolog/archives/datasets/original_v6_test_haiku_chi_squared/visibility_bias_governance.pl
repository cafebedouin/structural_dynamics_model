% ============================================================================
% CONSTRAINT STORY: visibility_bias_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_visibility_bias_governance, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: visibility_bias_governance
 *   human_readable: The Dashboard Delusion
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The Dashboard Delusion describes a structural constraint where governance
 *   systems become optimized for high-visibility metrics (stock indices,
 *   unemployment rates, surface-level crime statistics, quarterly GDP) while
 *   systematically ignoring low-visibility systemic decay (infrastructure
 *   entropy, trust erosion, institutional capacity degradation, environmental
 *   tipping points). The constraint is not that leaders are blind to decay —
 *   many are aware — but that the political and economic institutions
 *   rewarding short-term metric improvement create strong incentives to defer
 *   maintenance and systemic repair costs. This transforms what could be a
 *   neutral measurement problem into an extraction mechanism: political
 *   incumbents and capital markets extract career/financial benefits from
 *   metric improvement while future generations and infrastructure-dependent
 *   populations bear the deferred costs. The constraint exhibits all six DR
 *   types depending on observer position, with a manifest increase in
 *   theater_ratio (0.55 → 0.78 over 30 years) reflecting that dashboard
 *   metrics have become increasingly disconnected from systemic health as
 *   measurement sophistication has concentrated on visible signals and away
 *   from early indicators of decay.
 *
 * KEY AGENTS:
 *   - Political Incumbents: Primary beneficiary (institutional/arbitrage) — extract re-election, budget approval, capital market confidence from improved visible metrics while deferring infrastructure/systemic costs
 *   - Short-term Capital Interests: Primary beneficiary (institutional/arbitrage) — extract financial gains from stock market improvement, quarterly earnings, leveraged positions enabled by borrowed time on deferred decay
 *   - Future Generations: Primary victim (powerless/trapped) — bear cost of accumulated infrastructure and institutional decay without ability to influence policy during accumulation phase
 *   - Diffuse Public Welfare: Primary victim (moderate/trapped) — long-term wellbeing (clean water, functional institutions, social trust) degraded through deferred maintenance
 *   - Infrastructure Maintenance Capacity: Primary victim (moderate/constrained) — municipal systems, public agencies chronically underfunded relative to decay rates
 *   - Institutional Reform Coalitions: Organized actors (organized/constrained) — infrastructure advocates, environmental groups, long-term risk councils building alternative measurement frameworks (sunset mechanism)
 *   - Measurement Systems Infrastructure: Institutional actor (institutional/arbitrage) — GDP, stock indices, official statistics maintain themselves through path-dependence and institutional coordination
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing temporal asymmetry (visible metrics are easier to measure) as an immutable law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(visibility_bias_governance, 0.52).
domain_priors:suppression_score(visibility_bias_governance, 0.68).
domain_priors:theater_ratio(visibility_bias_governance, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(visibility_bias_governance, extractiveness, 0.52).
narrative_ontology:constraint_metric(visibility_bias_governance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(visibility_bias_governance, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(visibility_bias_governance, tangled_rope).
narrative_ontology:human_readable(visibility_bias_governance, "The Dashboard Delusion").
narrative_ontology:topic_domain(visibility_bias_governance, "political/institutional").

domain_priors:requires_active_enforcement(visibility_bias_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(visibility_bias_governance, political_incumbents).
narrative_ontology:constraint_beneficiary(visibility_bias_governance, short_term_capital_interests).
narrative_ontology:constraint_victim(visibility_bias_governance, long_term_systemic_health).
narrative_ontology:constraint_victim(visibility_bias_governance, diffuse_public_welfare).
narrative_ontology:constraint_victim(visibility_bias_governance, infrastructure_maintenance_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Bear deferred costs of systemic decay (crumbling bridges, aquifer depletion, institutional erosion) with no exit. Trapped by temporal asymmetry: cannot influence policy during accumulation phase. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(visibility_bias_governance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MUNICIPAL INFRASTRUCTURE MANAGERS (TANGLED ROPE) — Constrained by budget cycles tied to visible metrics. Also benefit from coordination: clear reporting frameworks, benchmarking systems. But the constraint forces deferred maintenance to sustain appearance metrics. d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(visibility_bias_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POLITICAL LEADERSHIP (ROPE) — Benefits from visible metric improvement (re-election, stock index gains, fund inflows). Experiences the constraint as coordination: dashboard metrics enable clear communication to voters and investors. The constraint solves their coordination problem (how to signal competence) at others' expense. d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(visibility_bias_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REFORM COALITIONS (SCAFFOLD) — Organized groups (infrastructure advocacy, long-term risk councils, systems auditors) recognize the constraint as temporary mismatch between measurement and reality. Building alternative metrics (infrastructure condition indices, anticipatory decay modeling) that could sunset the dashboard-driven policy paradigm. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.25. Low effective extraction because reformers have agency and see structural fixes.
constraint_indexing:constraint_classification(visibility_bias_governance, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEASUREMENT SYSTEMS (PITON) — Dashboard infrastructure (GDP tracking, stock indices, official statistics) is largely theatrical: high visibility, low functional connection to actual systemic health. Persists through institutional inertia and path-dependence. theater_ratio=0.78 (≥0.70 gate satisfied). The infrastructure maintains itself because alternatives haven't fully displaced it, not because it produces accurate policy signals.
constraint_indexing:constraint_classification(visibility_bias_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some measurement lag is inherent to complex systems: visible metrics are always easier to measure than diffuse systemic decay. This perspective risks naturalizing a contingent institutional choice (optimizing for visible metrics) as an immutable feature of governance. However, base properties (ε=0.52, suppression=0.68) contradict mountain classification — the engine detects this as a false summit revealing that temporal asymmetry is institutionally produced, not naturally inevitable.
constraint_indexing:constraint_classification(visibility_bias_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(visibility_bias_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(visibility_bias_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(visibility_bias_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(visibility_bias_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(visibility_bias_governance, TR),
    TR >= 0.70.

:- end_tests(visibility_bias_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts substantial benefits for short-term actors (political cycles, capital gains) and substantial costs for long-term actors (future generations, infrastructure condition). The extraction is not maximal (0.66+) because decay eventually becomes visible and imposes costs on extractors too; this delays but does not prevent extraction. The 30-year measurement trajectory (0.32 → 0.52) reflects increasing extraction as political cycles optimize harder for visible metrics and as capital markets develop more sophisticated mechanisms for monetizing borrowed time. Suppression (0.68): High. Significant barriers exist to shifting measurement systems: incumbent vested interests in current metrics, technical difficulty of measuring diffuse decay, institutional path-dependence, and the temporal mismatch between political cycles (2-6 years) and infrastructure decay timescales (20-50 years). Decentralized alternatives (local infrastructure monitoring, community trust surveys) face coordination problems and funding constraints. Theater ratio (0.78): Very high. Dashboard metrics have become substantially performative: stock market movements reflect sentiment and leverage far more than underlying economic fundamentals; unemployment rates miss underemployment and precarity; crime statistics miss types of crime that don't trigger reports; GDP misses wellbeing and environmental accounting. The theater has increased over time as measurement sophistication has concentrated on visible signals (quarterly earnings, daily stock movements) rather than early indicators of systemic stress (trust decay, infrastructure age distribution, anticipatory maintenance deficits).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classification across the full range. Political leadership sees coordination (Rope) — dashboards enable efficient communication of competence signals to voters and markets. Infrastructure reformers see a temporary problem with a sunset (Scaffold) — alternative metrics (infrastructure condition indices, anticipatory decay models) could shift incentives. Measurement infrastructure sees its own degraded ritual (Piton) — dashboards persist through inertia despite low predictive value for actual systemic health. Infrastructure managers see mixed coordination and extraction (Tangled Rope) — the system both enables and constrains their work, forcing them to defer maintenance to maintain appearance metrics. Future generations see pure extraction (Snare) — they pay costs for deferred choices with no voice in their creation. The civilizational analytical observer risks seeing a natural law (Mountain) — measurement lag is inevitable in complex systems — but the data reveals this as a false summit: the temporal asymmetry is institutionally produced through electoral cycles and capital market structures that reward short-term performance, not a timeless feature of governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Political incumbents: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary during their tenure. Short-term capital: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. Future generations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — no exit from temporal position. Infrastructure managers: Victim + constrained → d≈0.70, f(d)≈1.05. High extraction but with some agency (can lobby, advocate). Institutional reformers: Organized + constrained → d≈0.45, f(d)≈0.48. Low effective extraction; organized agents with pathway forward. Measurement infrastructure: Institutional + arbitrage → d≈0.08, f(d)≈-0.09. Piton classification from theater gate, not high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here resolves through recognizing that the constraint is a hybrid: genuinely partially coordination (dashboards do enable communication) + genuinely partially extraction (they concentrate benefits and defer costs). The tangled rope classification captures both. However, the false summit risk is acute: one can argue that visibility bias is an immutable natural law ('measurement lag is unavoidable in complex systems'). The engine detects this as false because: (1) some measurement systems (GNH, wellbeing frameworks) have successfully replaced dashboard optimization with long-term indicators; (2) the increasing theater_ratio suggests the problem is institutional choice, not natural limits; (3) the perspectival structure (beneficiaries see rope, victims see snare, reformers see scaffold) reveals institutional distribution of benefits/costs, not natural inevitability. The false summit would be committed if this constraint classified as mountain — but ε=0.52 and suppression=0.68 contradict mountain thresholds, preventing that error. The ambiguity remaining is empirical: can measurement reform (omega: substitution vs complement) actually break the constraint? This is the live uncertainty the omegas track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_lag_threshold,
    'What threshold of invisible decay accumulation triggers crisis-level visible metrics collapse?',
    'Historical analysis of infrastructure/institutional systems: correlation between early-stage diffuse decay and eventual visible metric collapse (bridge failures, banking crises, state capacity collapse)',
    'If threshold < 5 years: policy cycle must prioritize long-term metrics to avoid crises. If threshold > 20 years: short-term dashboard optimization appears rational. The answer determines whether dashboard-driven policy is unavoidably extractive or remediable through better forecasting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_lag_threshold, empirical, 'Timeline threshold at which invisible decay triggers visible crisis').

omega_variable(
    measurement_system_entrenchment,
    'Are current dashboard metrics (GDP, stock indices, surface crime stats) entrenched due to genuine functional accuracy or purely due to institutional path-dependence?',
    'Comparative analysis: countries/institutions that abandoned standard dashboards (e.g., Bhutan GNH, New Zealand wellbeing framework) vs standard metric users; measurement of policy accuracy under both systems',
    'If purely path-dependent: relatively low-cost reform of measurement systems could break the constraint. If functionally accurate: dashboard metrics reflect genuine difficulty of measuring decay, and constraint is closer to natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_system_entrenchment, empirical, 'Whether dashboard metrics are entrenched by function or by path-dependence').

omega_variable(
    political_incentive_structure,
    'Can election cycles be extended or electoral incentives restructured to make long-term decay visible without requiring abandonment of democratic accountability?',
    'Policy experiments: longer budget cycles, cross-administration infrastructure audits, intergenerational representation mechanisms; measurement of whether extended timescales change investment patterns',
    'If achievable: scaffold perspective is correct — sunset through electoral reform. If not: structural incompatibility between democratic cycles and long-term systemic thinking suggests constraint is closer to tangled rope without clear exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_incentive_structure, preference, 'Whether electoral cycles can be reformed to align with long-term systemic health').

omega_variable(
    substitution_vs_complement,
    'Do alternative long-term metrics (infrastructure condition indices, institutional trust surveys, anticipatory decay modeling) substitute for dashboard metrics or merely complement them, allowing dashboard-driven policies to persist?',
    'Institutional adoption analysis: cases where long-term metrics were introduced; whether they changed budget allocation or merely added reporting overhead',
    'If true substitution: scaffold perspective (sunset through better measurement). If complement only: constraint persists because visibility bias is structural to how political actors communicate, not remediable by adding metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_complement, empirical, 'Whether long-term metrics substitute for or complement short-term dashboards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(visibility_bias_governance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visbias_tr_t0, visibility_bias_governance, theater_ratio, 0, 0.55).
narrative_ontology:measurement(visbias_tr_t15, visibility_bias_governance, theater_ratio, 15, 0.68).
narrative_ontology:measurement(visbias_tr_t30, visibility_bias_governance, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(visbias_be_t0, visibility_bias_governance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(visbias_be_t15, visibility_bias_governance, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(visbias_be_t30, visibility_bias_governance, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(visibility_bias_governance, information_standard).
narrative_ontology:affects_constraint(visibility_bias_governance, electoral_cycle_myopia).
narrative_ontology:affects_constraint(visibility_bias_governance, capital_market_temporality_mismatch).
narrative_ontology:affects_constraint(visibility_bias_governance, infrastructure_maintenance_deferral).

% DUAL FORMULATION NOTE:
% The Dashboard Delusion is downstream of electoral cycle structures and capital market incentives but represents a distinct structural constraint operating at the governance measurement level. Upstream constraints (electoral myopia, capital structure) produce the temporal asymmetry; the dashboard constraint mechanizes that asymmetry into policy. Decomposition: electoral_cycle_myopia (ε≈0.35, coordination problem) feeds visibility_bias_governance (ε=0.52, mixed coordination-extraction); the measurement system constraint is more extractive than its temporal root cause because it provides a coordination solution that entrenches the myopia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(visibility_bias_governance, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
