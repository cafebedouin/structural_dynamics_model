% ============================================================================
% CONSTRAINT STORY: unilateral_condition_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unilateral_condition_control, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unilateral_condition_control
 *   human_readable: Unilateral Condition Control in Performance-Based Systems
 *   domain: political_economy/labor_relations/debt_systems
 *
 * SUMMARY:
 *   Unilateral condition control appears across multiple domains: gig economy
 *   platforms setting delivery quotas while controlling route assignments and
 *   surge pricing; agricultural lenders setting repayment schedules while
 *   controlling input prices and market access; factory managers setting
 *   production targets while controlling machine maintenance and material
 *   quality; colonial administrations setting tax quotas while controlling
 *   market access and crop prices. The structural pattern is identical: one
 *   party has authority over both the performance metric and the
 *   environmental conditions that determine whether the metric is achievable.
 *   This creates a ratchet mechanism — when conditions deteriorate (whether
 *   through deliberate manipulation, structural forces, or stochastic
 *   variation), the authority can maintain or increase quotas, extracting
 *   more from subjects who have no recourse. The constraint exhibits genuine
 *   coordination function (performance systems do solve principal-agent
 *   problems and align incentives) alongside asymmetric extraction
 *   (unilateral authority enables rent-seeking). The theater_ratio (0.55)
 *   reflects increasing divergence between stated performance goals and
 *   actual outcomes as subjects game the metrics or as metrics become proxy
 *   goals (Goodhart drift). The constraint's extractiveness has increased
 *   over the interval (0.42 → 0.58) as authorities have learned to exploit
 *   the asymmetry more effectively.
 *
 * KEY AGENTS:
 *   - Metric-Setting Authority: Primary beneficiary (institutional/arbitrage) — controls both targets and conditions; captures productivity gains and shifts risk to subjects
 *   - Metric Subject (Trapped): Primary victim (powerless/trapped) — cannot exit system; bears full cost of condition deterioration and quota increases; no coordination benefit visible
 *   - Metric Subject (Constrained): Secondary victim (moderate/constrained) — has exit options at significant cost; experiences mixed coordination and extraction
 *   - Labor Organizing Coalition: Organized agents (organized/mobile) — building alternative governance structures (co-determination, collective bargaining) with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination function and extraction mechanism; classification matches claimed type (tangled_rope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unilateral_condition_control, 0.58).
domain_priors:suppression_score(unilateral_condition_control, 0.68).
domain_priors:theater_ratio(unilateral_condition_control, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unilateral_condition_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(unilateral_condition_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unilateral_condition_control, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unilateral_condition_control, tangled_rope).
narrative_ontology:human_readable(unilateral_condition_control, "Unilateral Condition Control in Performance-Based Systems").
narrative_ontology:topic_domain(unilateral_condition_control, "political_economy/labor_relations/debt_systems").

domain_priors:requires_active_enforcement(unilateral_condition_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unilateral_condition_control, metric_setting_authority).
narrative_ontology:constraint_victim(unilateral_condition_control, metric_subject).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED METRIC SUBJECT (SNARE) — Cannot exit the performance system; authority controls both the targets and the conditions that determine achievability. Experiences pure extraction: quotas rise when conditions worsen, creating impossible binds. No coordination function visible from this position — only asymmetric power.
constraint_indexing:constraint_classification(unilateral_condition_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED PERFORMER (TANGLED ROPE) — Has some exit capacity (can change employers, renegotiate contracts, or organize) but faces significant costs. Sees both coordination (performance metrics do align some incentives) and extraction (authority manipulates conditions to extract more). Mixed experience: the system sometimes works as intended, sometimes becomes a trap.
constraint_indexing:constraint_classification(unilateral_condition_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: METRIC-SETTING AUTHORITY (ROPE) — Experiences the constraint as coordination: setting performance targets and adjusting conditions is how they manage productivity and risk. Net beneficiary — the asymmetry runs in their favor. From this position, the system is a legitimate management tool, not extraction.
constraint_indexing:constraint_classification(unilateral_condition_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR ORGANIZING COALITION (SCAFFOLD) — Organized agents (unions, worker cooperatives, regulatory advocates) see unilateral condition control as a temporary problem being solved through collective bargaining rights, co-determination laws, and transparent performance systems. These mechanisms create joint authority over metrics and conditions, with a sunset logic: as worker power increases, unilateral control becomes structurally impossible.
constraint_indexing:constraint_classification(unilateral_condition_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, unilateral condition control exhibits both genuine coordination (performance systems do solve principal-agent problems) and structural extraction (asymmetric authority enables rent-seeking). The constraint is not reducible to either pure coordination or pure extraction — it is a hybrid that persists because it serves both functions simultaneously. The analytical classification matches the claimed type.
constraint_indexing:constraint_classification(unilateral_condition_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unilateral_condition_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unilateral_condition_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unilateral_condition_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unilateral_condition_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unilateral_condition_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The authority captures significant value through the asymmetry — they can adjust quotas upward when conditions worsen, extracting more labor or payment from subjects who have no corresponding adjustment mechanism. However, the extraction is not maximal because the system does provide some coordination function (performance metrics do align incentives when conditions are stable) and some subjects have exit options. The value reflects that roughly 60% of the system's function is extractive overhead rather than coordination benefit. Suppression (0.68): High. Subjects face significant barriers to exit or resistance: economic dependency (debt, income necessity), geographic constraints (rural isolation, urban lock-in), skill specificity (training investments that don't transfer), reputational risk (negative references, blacklisting), and in some cases identity fusion (professional identity tied to the role). Suppression is not total — some subjects can and do exit, and collective action is possible — but barriers are substantial. Theater ratio (0.55): Moderate-high and increasing. Performance metrics increasingly measure proxy goals rather than real outcomes: delivery drivers optimize for quota achievement rather than customer satisfaction; farmers prioritize repayment over soil health; factory workers meet production targets by sacrificing quality. The theater has increased over the interval as subjects have learned to game the metrics and as authorities have responded by adding more metrics (which increases complexity without improving measurement validity).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full indexical range from snare (trapped subjects) through tangled_rope (constrained subjects and analytical observer) to rope (authority) and scaffold (organized coalition). The authority sees pure coordination — they are solving the legitimate problem of managing productivity and aligning incentives. The organized coalition sees a temporary problem with a sunset — co-determination and collective bargaining are building joint authority over metrics and conditions. Constrained subjects see mixed coordination and extraction — the system sometimes works as intended, sometimes becomes a trap. Trapped subjects see pure extraction — quotas rise when conditions worsen, creating impossible binds with no coordination benefit visible. The analytical observer sees the hybrid structure — genuine coordination function coexisting with asymmetric extraction. The perspectival gap is not a disagreement about facts but a difference in structural position: who controls the metrics, who bears the risk, and who has exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The metric-setting authority is the primary beneficiary — they control both the performance targets and the environmental conditions, allowing them to extract value by adjusting quotas when conditions deteriorate. Their directionality is low (d ≈ 0.10-0.15), producing low or negative effective extraction (they experience the constraint as coordination). Trapped metric subjects are primary victims with no exit options — they bear the full cost of the asymmetry. Their directionality is high (d ≈ 0.95), producing maximum effective extraction (they experience the constraint as a snare). Constrained metric subjects have exit options at significant cost — they experience mixed extraction and coordination. Their directionality is moderate-high (d ≈ 0.60-0.70), producing moderate effective extraction (they experience the constraint as tangled_rope). Organized agents building alternative governance structures have mobile exit options and see a sunset path — their directionality is moderate (d ≈ 0.45-0.55), producing low-moderate effective extraction (they experience the constraint as scaffold). The analytical observer sees both functions simultaneously — their directionality is moderate (d ≈ 0.50), producing moderate effective extraction (they experience the constraint as tangled_rope, matching the claimed type).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that tangled_rope is the correct analytical classification when a system exhibits BOTH genuine coordination function AND asymmetric extraction. The authority's rope perspective is their genuine experience (they are net beneficiaries). The trapped subject's snare perspective is their genuine experience (they are net victims with no exit). The constrained subject's tangled_rope perspective is their genuine experience (they see both functions). The organized coalition's scaffold perspective is their genuine structural position (they are building an exit path with sunset logic). The analytical observer's tangled_rope classification synthesizes these perspectives: the constraint coordinates (performance systems do solve principal-agent problems) AND extracts (unilateral authority enables rent-seeking). Neither function is reducible to the other. The mandatrophy is resolved by recognizing that 'coordination vs extraction' is a false binary — many real-world constraints do both simultaneously, and tangled_rope is the type that captures this hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    condition_manipulation_intentionality,
    'Is condition deterioration that coincides with quota increases the result of deliberate manipulation, structural inevitability, or stochastic variation?',
    'Statistical analysis of condition changes vs quota adjustments across multiple instances; comparison of adjustment patterns in systems with vs without unilateral authority; identification of predictive signals (e.g., quota increases preceding predictable condition changes)',
    'If deliberate: extraction is higher than measured (authority is gaming the system). If structural: some apparent extraction is coordination cost (authority responding to genuine constraints). If stochastic: measured extraction includes noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(condition_manipulation_intentionality, empirical, 'Whether condition deterioration is manipulated or structural').

omega_variable(
    performance_metric_validity,
    'Do the performance metrics actually measure the intended outcome, or have they become proxy goals that diverge from real productivity?',
    'Longitudinal tracking of metric achievement vs actual outcomes (e.g., sales quotas vs customer retention, production targets vs quality, debt repayment schedules vs borrower solvency); Goodhart''s Law analysis',
    'If metrics are valid: coordination function is real. If metrics are proxies: theater_ratio should be higher — the system is performative rather than functional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_metric_validity, empirical, 'Whether performance metrics measure real outcomes or proxy goals').

omega_variable(
    exit_option_asymmetry,
    'What proportion of metric subjects have genuine exit options (can leave the system without catastrophic cost) vs those who are structurally trapped?',
    'Survey data on exit costs; analysis of turnover rates and reasons; comparison of outcomes for those who exit vs those who remain; identification of lock-in mechanisms (debt, geographic isolation, skill specificity, identity fusion)',
    'If most subjects can exit: suppression is lower than measured, and the constraint is closer to rope from more perspectives. If most are trapped: suppression is accurate or understated, and snare classification applies to more agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_asymmetry, empirical, 'Distribution of genuine exit capacity among metric subjects').

omega_variable(
    collective_action_threshold,
    'At what concentration of metric subjects does collective action become feasible, shifting power from powerless to organized?',
    'Historical analysis of successful organizing campaigns; identification of critical mass thresholds; comparison of organizing success rates across different subject concentrations and communication infrastructures',
    'If threshold is low: many powerless agents should be reclassified as organized, reducing experienced extraction. If threshold is high: powerless classification is accurate for most subjects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Critical mass threshold for collective action among subjects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unilateral_condition_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ucc_tr_t0, unilateral_condition_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ucc_tr_t3, unilateral_condition_control, theater_ratio, 3, 0.42).
narrative_ontology:measurement(ucc_tr_t6, unilateral_condition_control, theater_ratio, 6, 0.48).
narrative_ontology:measurement(ucc_tr_t10, unilateral_condition_control, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(ucc_be_t0, unilateral_condition_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ucc_be_t3, unilateral_condition_control, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ucc_be_t6, unilateral_condition_control, base_extractiveness, 6, 0.53).
narrative_ontology:measurement(ucc_be_t10, unilateral_condition_control, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unilateral_condition_control, resource_allocation).

% DUAL FORMULATION NOTE:
% Unilateral condition control is a structural pattern that appears across multiple domains (gig economy, agricultural lending, factory production, colonial taxation). Each domain instantiation could be modeled as a separate constraint story with domain-specific metrics, but they share the same structural signature: asymmetric authority over both performance targets and environmental conditions. This story models the abstract pattern; domain-specific stories would link here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
