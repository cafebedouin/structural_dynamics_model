% ============================================================================
% CONSTRAINT STORY: quota_ratchet_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quota_ratchet_asymmetry, []).

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
 *   constraint_id: quota_ratchet_asymmetry
 *   human_readable: Quota Ratchet Asymmetry in Performance Management Systems
 *   domain: organizational_systems/labor_economics/institutional_extraction
 *
 * SUMMARY:
 *   The quota ratchet asymmetry is a structural feature of performance
 *   management systems across manufacturing, logistics, customer service, and
 *   gig economy platforms. When workers or teams exceed targets—whether
 *   through genuine efficiency improvements (better tools, optimized
 *   processes) or unsustainable effort intensification—the system interprets
 *   the higher output as new baseline capacity and adjusts quotas upward.
 *   When capacity declines—through worker injury, equipment degradation, or
 *   external shocks—the system does not adjust quotas downward, treating the
 *   decline as temporary underperformance rather than a signal requiring
 *   recalibration. This asymmetry creates a structural transfer: efficiency
 *   gains are captured by the institution as permanent productivity
 *   increases, while capacity losses are borne by workers as increased strain
 *   to meet now-unsustainable targets. The constraint exhibits genuine
 *   coordination function (organizations do need mechanisms to translate
 *   capacity into output expectations and to respond to efficiency
 *   improvements) alongside asymmetric extraction (the unidirectional
 *   adjustment systematically transfers risk and health costs from
 *   institution to worker). The theater_ratio (0.52) reflects that much of
 *   the 'data-driven' performance management apparatus is performative:
 *   time-motion studies, efficiency consulting, and quota calibration rituals
 *   generate institutional legitimacy but often ignore worker health data,
 *   injury rates, and capacity decline signals that would require downward
 *   adjustments.
 *
 * KEY AGENTS:
 *   - Front-Line Workers: Primary victims (powerless/trapped) — bear full health and economic costs of unsustainable quotas; cannot exit without losing specialized skills, seniority, or benefits
 *   - Middle Management: Secondary victims and minor beneficiaries (moderate/constrained) — face career pressure to meet targets and moral injury from enforcing unsustainable quotas, but also receive bonuses and promotion prospects from aggregate performance
 *   - Institutional Budget Managers: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains as permanent productivity increases; can exit to similar roles across industries
 *   - Senior Management: Secondary beneficiaries (powerful/mobile) — benefit from improved efficiency metrics and shareholder value; insulated from operational consequences
 *   - Efficiency Consultants: Institutional actors maintaining performative ritual (institutional/mobile) — generate billable hours through quota calibration studies that ignore capacity decline signals
 *   - Labor Union Coalition: Organized agents building alternative frameworks (organized/constrained) — negotiating bidirectional adjustment clauses and health-indexed quotas
 *   - Worker Health Outcomes: Abstract collective victim (powerless/trapped) — long-term health costs (repetitive strain injuries, burnout, disability) that cannot be priced into the quota system
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination function and asymmetric extraction; recognizes the asymmetry as a design choice rather than structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quota_ratchet_asymmetry, 0.58).
domain_priors:suppression_score(quota_ratchet_asymmetry, 0.68).
domain_priors:theater_ratio(quota_ratchet_asymmetry, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quota_ratchet_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(quota_ratchet_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(quota_ratchet_asymmetry, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quota_ratchet_asymmetry, tangled_rope).
narrative_ontology:human_readable(quota_ratchet_asymmetry, "Quota Ratchet Asymmetry in Performance Management Systems").
narrative_ontology:topic_domain(quota_ratchet_asymmetry, "organizational_systems/labor_economics/institutional_extraction").

domain_priors:requires_active_enforcement(quota_ratchet_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quota_ratchet_asymmetry, institutional_budget_managers).
narrative_ontology:constraint_beneficiary(quota_ratchet_asymmetry, efficiency_consultants).
narrative_ontology:constraint_beneficiary(quota_ratchet_asymmetry, senior_management).
narrative_ontology:constraint_victim(quota_ratchet_asymmetry, front_line_workers).
narrative_ontology:constraint_victim(quota_ratchet_asymmetry, middle_management).
narrative_ontology:constraint_victim(quota_ratchet_asymmetry, worker_health_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONT-LINE WORKER (SNARE) — Trapped by economic necessity and skill specificity. Experiences pure extraction: every efficiency gain becomes a permanent baseline, every capacity decline is ignored. Cannot exit without losing specialized skills, seniority, or health insurance. The ratchet only turns one direction.
constraint_indexing:constraint_classification(quota_ratchet_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Constrained by career path and organizational loyalty. Benefits from meeting aggregate targets (bonus eligibility, promotion prospects) but also bears extraction through stress, turnover management costs, and moral injury from enforcing unsustainable quotas. Genuine coordination function exists (translating organizational goals into operational targets) alongside asymmetric extraction.
constraint_indexing:constraint_classification(quota_ratchet_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUDGET MANAGER (ROPE) — Primary beneficiary with arbitrage exit options. Experiences the ratchet as legitimate coordination: efficiency gains should translate to higher output expectations, and the system 'works' by continuously improving productivity metrics. Can exit to similar roles across industries. Extraction flows toward this position.
constraint_indexing:constraint_classification(quota_ratchet_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNION COALITION (SCAFFOLD) — Organized agents building alternative performance management frameworks with bidirectional adjustment clauses, health-indexed quotas, and worker voice in target-setting. Sees the asymmetry as a temporary coordination failure with a sunset: collective bargaining and regulatory intervention (OSHA ergonomic standards, right-to-refuse-unsafe-work protections) are creating pathways to symmetric adjustment mechanisms. Estimated sunset: 15-25 years as labor law reform and organizing density increase.
constraint_indexing:constraint_classification(quota_ratchet_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EFFICIENCY CONSULTANT (PITON) — Sees the performance management ritual as degraded. The original function (aligning worker capacity with organizational needs) has atrophied into theater: consultants run time-motion studies and recommend quota increases knowing the targets are already unsustainable, but the ritual persists because it generates billable hours and satisfies institutional demand for 'data-driven management.' The system is maintained through inertia and professional incentives, not because it optimizes productivity.
constraint_indexing:constraint_classification(quota_ratchet_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the ratchet exhibits both genuine coordination (organizations do need mechanisms to translate capacity improvements into output expectations) and asymmetric extraction (the unidirectional adjustment creates a structural transfer from worker health to institutional efficiency metrics). The asymmetry is not inherent to performance management but is a contingent design choice that could be corrected through bidirectional adjustment protocols.
constraint_indexing:constraint_classification(quota_ratchet_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quota_ratchet_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quota_ratchet_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quota_ratchet_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quota_ratchet_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quota_ratchet_asymmetry, TR),
    TR >= 0.70.

:- end_tests(quota_ratchet_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The ratchet captures efficiency gains as permanent institutional property while externalizing capacity decline costs to workers. The extraction is substantial but not maximal—some workers do benefit from efficiency improvements (less physical strain per unit output when tools improve), and the coordination function is genuine (organizations need output predictability). The value reflects that the asymmetry creates a structural transfer but not total extraction. Suppression (0.68): High. Workers face significant barriers to resisting quota increases: economic dependency (need income and benefits), skill specificity (specialized training not transferable), seniority loss (starting over elsewhere means losing accumulated advantages), and collective action problems (individual resistance is punished; coordinated resistance requires union density). Exit options exist but are costly. Theater ratio (0.52): Moderate. Performance management systems generate substantial performative content—time-motion studies that ignore ergonomic limits, efficiency consulting that recommends increases regardless of worker health data, data dashboards that track output but not injury rates—but the core function (translating capacity into targets) is not purely theatrical. The theater has increased over the interval as the gap between measured metrics (output) and ignored metrics (health costs) has widened.
 *
 * PERSPECTIVAL GAP:
 *   The front-line worker experiences pure extraction (Snare)—every efficiency gain becomes a trap, every capacity decline is ignored, and exit is prohibitively costly. The budget manager experiences coordination (Rope)—the system 'works' by translating improvements into higher expectations, and the manager benefits from the resulting efficiency metrics. The middle manager experiences mixed coordination and extraction (Tangled Rope)—the system enables their role (translating goals into operations) but also extracts through stress and moral injury. The labor union coalition sees a temporary problem with a sunset (Scaffold)—collective bargaining is building alternative frameworks with symmetric adjustment. The efficiency consultant sees a degraded ritual (Piton)—the performance management apparatus persists through professional incentives and institutional inertia, not because it optimizes outcomes. The analytical observer sees the full structure (Tangled Rope)—genuine coordination function with asymmetric extraction that is a contingent design choice, not a natural law. The perspectival gap is diagnostic: the same structural phenomenon appears as natural and efficient from the beneficiary position, as a solvable coordination problem from the organized position, and as inescapable extraction from the trapped position.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the directionality computation. Institutional budget managers are primary beneficiaries with arbitrage exit options—they experience low or negative effective extraction because the ratchet transfers value toward their position. Front-line workers are primary victims with trapped exit options—they experience maximum extraction because the ratchet transfers health and economic costs to their position and they cannot escape without severe penalty. Middle managers occupy a hybrid position: they are both minor beneficiaries (career advancement from meeting targets) and secondary victims (stress and moral injury from enforcing unsustainable quotas), with constrained exit options (can leave but at significant career cost). The analytical observer sees the structural asymmetry clearly: the coordination function is real (organizations need performance management), but the unidirectional adjustment is a design choice that creates extraction, not an inherent feature of coordination systems. The labor union coalition sees a sunset: bidirectional adjustment protocols and health-indexed quotas are being built through collective bargaining, creating an exit path from the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The quota ratchet asymmetry resolves the mandatrophy by demonstrating that the classification depends on the observer's structural position. The constraint is NOT purely extractive (it has genuine coordination function—organizations need mechanisms to set and adjust performance targets), and it is NOT purely coordinative (the asymmetry creates systematic transfer of health and economic costs from institution to worker). The tangled_rope classification at the analytical level captures this hybrid structure: the system coordinates (translates capacity into expectations) AND extracts (unidirectional adjustment externalizes decline costs). The snare classification from the trapped worker perspective is their lived reality—they experience the ratchet as pure extraction because they bear all costs and capture none of the efficiency gains. The rope classification from the institutional beneficiary perspective is also accurate—they experience the system as working coordination because extraction flows toward them. The scaffold classification from the organized labor perspective reflects a real structural feature—alternative frameworks with bidirectional adjustment are being built and have a plausible sunset timeline. No single type is 'the' answer; the presheaf over observation sites IS the answer. The mandatrophy is resolved by recognizing that 'Is this extraction or coordination?' is an indexical question, not an absolute one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_ambiguity,
    'What constitutes ''capacity decline'' sufficient to trigger quota reduction? Is it individual hand failure rates, aggregate team performance, or systemic factors like equipment degradation?',
    'Longitudinal tracking of quota adjustments correlated with worker injury reports, equipment maintenance logs, and aggregate output variance. Establish whether organizations recognize any decline signal as legitimate input to quota-setting.',
    'If organizations recognize decline signals but ignore them: pure extraction (Snare from more perspectives). If organizations genuinely cannot measure decline: coordination failure (Rope from more perspectives). If decline measurement exists but is suppressed: confirms asymmetric extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_measurement_ambiguity, empirical, 'What decline signals organizations recognize as legitimate quota adjustment triggers').

omega_variable(
    efficiency_attribution_problem,
    'When productivity increases, how much is attributable to worker effort intensification vs. genuine efficiency improvements (better tools, process optimization)?',
    'Decomposition analysis: control for tool/process changes vs. output increases. Compare quota adjustments in periods with documented tool improvements vs. periods with no process changes. If quotas increase equally in both cases, the ratchet extracts effort intensification regardless of efficiency source.',
    'If increases are effort-driven: the ratchet is extracting unsustainable labor intensification (higher extractiveness). If increases are tool-driven: the ratchet is capturing legitimate efficiency gains (lower extractiveness, more coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_attribution_problem, empirical, 'Whether quota increases capture efficiency gains or extract effort intensification').

omega_variable(
    exit_option_variance,
    'How much does worker exit capacity vary by industry, skill level, geographic region, and labor market tightness?',
    'Cross-industry comparison of turnover rates, wage premiums for job-switching, and geographic mobility patterns among workers subject to quota systems. Measure correlation between labor market tightness and quota adjustment asymmetry.',
    'If exit options are uniformly low: trapped classification is accurate across contexts. If exit options vary significantly: the constraint''s extractiveness is modulated by labor market conditions, and the classification should reflect regional/sectoral variation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_variance, empirical, 'How worker exit capacity varies across contexts').

omega_variable(
    bidirectional_adjustment_feasibility,
    'Are there organizational contexts where bidirectional quota adjustment (increases AND decreases) is structurally feasible without catastrophic coordination failure?',
    'Case studies of organizations that have implemented symmetric adjustment protocols. Measure whether bidirectional systems maintain coordination function (output predictability, resource allocation) while reducing extraction (injury rates, turnover, worker health outcomes).',
    'If bidirectional systems exist and function: the asymmetry is a design choice, not a structural necessity (confirms Tangled Rope, refutes Mountain). If bidirectional systems fail coordination tests: the asymmetry may be inherent to performance management under uncertainty (supports Mountain from some perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bidirectional_adjustment_feasibility, empirical, 'Whether symmetric adjustment protocols can maintain coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quota_ratchet_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quota_ratchet_tr_t0, quota_ratchet_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(quota_ratchet_tr_t3, quota_ratchet_asymmetry, theater_ratio, 3, 0.42).
narrative_ontology:measurement(quota_ratchet_tr_t6, quota_ratchet_asymmetry, theater_ratio, 6, 0.48).
narrative_ontology:measurement(quota_ratchet_tr_t10, quota_ratchet_asymmetry, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(quota_ratchet_be_t0, quota_ratchet_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(quota_ratchet_be_t3, quota_ratchet_asymmetry, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(quota_ratchet_be_t6, quota_ratchet_asymmetry, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(quota_ratchet_be_t10, quota_ratchet_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quota_ratchet_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(quota_ratchet_asymmetry, platform_algorithmic_management).
narrative_ontology:affects_constraint(quota_ratchet_asymmetry, warehouse_productivity_tracking).
narrative_ontology:affects_constraint(quota_ratchet_asymmetry, gig_economy_rating_systems).

% DUAL FORMULATION NOTE:
% The quota ratchet asymmetry is a structural pattern that appears across multiple organizational contexts. Each instantiation (manufacturing quotas, warehouse pick rates, gig platform acceptance rates, customer service handle times) is a distinct constraint with its own extractiveness value reflecting the specific industry's labor market conditions, regulatory environment, and worker organization density. This story models the general pattern; specific instantiations should be written as separate stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quota_ratchet_asymmetry, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
