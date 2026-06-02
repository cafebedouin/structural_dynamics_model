% ============================================================================
% CONSTRAINT STORY: institutional_metric_gaming
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_metric_gaming, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: institutional_metric_gaming
 *   human_readable: Institutional Metric Gaming and Goodhart Decay
 *   domain: organizational_governance/institutional_dysfunction
 *
 * SUMMARY:
 *   Institutional metric gaming occurs when organizations establish
 *   measurable performance targets to solve coordination and accountability
 *   problems, but the targets become misaligned from the actual outcomes they
 *   were designed to measure. As agents at all levels optimize for the metric
 *   rather than the underlying goal, the metric's informational value decays
 *   — Goodhart's Law manifests as institutional dysfunction. The constraint
 *   exhibits tangled rope structure: genuine coordination function (metrics
 *   enable resource allocation, accountability, performance visibility)
 *   coexists with asymmetric extraction (benefits accrue to leadership and
 *   metric-gaming specialists, while frontline workers and actual outcomes
 *   bear the cost). The theater ratio increases over time (0.35 → 0.72) as
 *   metric gaming becomes more sophisticated and institutionalized,
 *   illustrating the lifecycle drift toward performative compliance. The
 *   extractiveness value (0.58) reflects that this is neither pure
 *   coordination nor pure extraction — the metrics did solve real
 *   coordination problems, but those benefits are now obscured by gaming
 *   incentives. Different institutional observers experience this constraint
 *   radically differently: leadership sees pure coordination (Rope), the
 *   reform movement sees a solvable problem (Scaffold), the measurement
 *   apparatus sees its own degradation (Piton), powerless frontline actors
 *   see pure extraction (Snare), and the civilizational observer risks
 *   naturalizing institutional dysfunction as a law of nature (false
 *   Mountain).
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — captures appearance of performance improvement, controls metric definition and target setting, low accountability for actual outcomes
 *   - Actual Outcome Bearer: Primary victim (powerless/trapped) — frontline workers, service recipients, students constrained by gaming-optimized institutional rules that degrade real outcomes
 *   - Mid-Level Manager: Secondary victim (moderate/constrained) — caught between leadership targets and actual outcome responsibility; blamed for metric failures, pressured to participate in gaming
 *   - Field Integrity: Systemic victim (powerless/trapped) — abstract institutional mission and professional field norms degraded as agents optimize for metrics rather than substantive performance
 *   - Metrics Reform Movement: Organized agents (organized/constrained) — balanced scorecard, OKR, and participatory goal-setting advocates building alternative accountability structures
 *   - Measurement Ritual Apparatus: Institutional actor (institutional/arbitrage) — HR, compliance, data collection infrastructure that maintains metrics systems despite knowing their gaming vulnerabilities
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating metric gaming as an immutable law rather than a solvable institutional design problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_metric_gaming, 0.58).
domain_priors:suppression_score(institutional_metric_gaming, 0.65).
domain_priors:theater_ratio(institutional_metric_gaming, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_metric_gaming, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_metric_gaming, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_metric_gaming, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_metric_gaming, tangled_rope).
narrative_ontology:human_readable(institutional_metric_gaming, "Institutional Metric Gaming and Goodhart Decay").
narrative_ontology:topic_domain(institutional_metric_gaming, "organizational_governance/institutional_dysfunction").

domain_priors:requires_active_enforcement(institutional_metric_gaming).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_metric_gaming, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_metric_gaming, performance_gaming_agents).
narrative_ontology:constraint_victim(institutional_metric_gaming, actual_outcomes).
narrative_ontology:constraint_victim(institutional_metric_gaming, field_integrity).
narrative_ontology:constraint_victim(institutional_metric_gaming, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACTUAL OUTCOME BEARER (SNARE) — Frontend service workers, students in gaming-optimized schools, patients in metrics-driven healthcare systems. Trapped by institutional hierarchy; cannot exit the system without abandoning their role. Bears full extraction cost as institutional focus shifts from actual outcomes to metric gaming. Zero agency over the constraint.
constraint_indexing:constraint_classification(institutional_metric_gaming, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Department heads, middle administrators who must meet targets. Experiences genuine coordination function (metrics enable resource allocation, performance visibility) alongside asymmetric extraction (blamed when metrics decline, pressured to game when needed). Exit is constrained by career path dependence — cannot easily leave without reputation damage, yet has limited power to change target structure.
constraint_indexing:constraint_classification(institutional_metric_gaming, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Board, C-suite, strategic planners. Experiences metrics as pure coordination mechanism: objective performance assessment, resource optimization, accountability alignment. Leadership benefits from metrics without bearing downside of gaming distortions. High exit optionality — can redefine metrics, change targets, or shift focus with minimal personal cost. Net beneficiary.
constraint_indexing:constraint_classification(institutional_metric_gaming, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: METRICS REFORM MOVEMENT (SCAFFOLD) — Organized agents (balanced scorecard advocates, OKR frameworks, participatory goal-setting movements) see metric gaming as a solvable institutional problem with a generational sunset. New frameworks (outcome hierarchies, leading indicators, intrinsic motivation alignment) are gradually building alternative accountability structures. Organized agents have agency and see a pathway to exit — not high extraction because the movement perceives the problem as temporary and solvable.
constraint_indexing:constraint_classification(institutional_metric_gaming, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEASUREMENT RITUAL APPARATUS (PITON) — The institutional machinery of metrics reporting: HR dashboards, performance review cycles, compliance audits, data collection infrastructure. These persist through institutional inertia despite their known gaming vulnerabilities. The measurement apparatus sees itself as degraded — necessary for accountability but recognized as gameable. Theater ratio high (0.68) because compliance metrics often measure performative reporting rather than actual performance.
constraint_indexing:constraint_classification(institutional_metric_gaming, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GOODHART'S LAW VIEW (MOUNTAIN) — From a civilizational perspective, metric gaming appears as an immutable law: 'When a measure becomes a target, it ceases to be a good measure' (Campbell's Law / Goodhart's Law). The observer risks naturalizing what is actually a contingent institutional failure mode. The constraint is not a law of measurement per se but an outcome of misaligned incentives and enforcement structures. The engine's false summit detector identifies this as naturalization of institutional dysfunction as natural law.
constraint_indexing:constraint_classification(institutional_metric_gaming, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_metric_gaming_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_metric_gaming, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_metric_gaming, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_metric_gaming, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_metric_gaming, TR),
    TR >= 0.70.

:- end_tests(institutional_metric_gaming_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The metric gaming constraint extracts real value from actual performance — resources are diverted from substantive outcomes to metric optimization, and leadership captures career and financial benefits while frontline workers absorb the degradation. However, extraction is not as severe as a pure Snare (ε ≥ 0.66) because (1) the metrics do solve legitimate coordination problems in large institutions, (2) some gaming is unavoidable and not malicious, and (3) the constraint remains somewhat visible and contestable. The increase from 0.28 to 0.62 over the interval reflects institutionalization of gaming strategies. Suppression (0.65): High. Barriers to exiting metric gaming include: structural power imbalance (leadership controls metric definition), career path dependence (agents must perform well on metrics to advance), information asymmetry (actual outcomes harder to measure than metrics), resource scarcity (frontline workers under time pressure cannot both optimize metrics and maintain outcome quality), and institutional invisibility (gaming is often rationalized as 'working the system' rather than recognized as coordinated extraction). Theater ratio (0.68, rising to 0.72): High and increasing. Metric compliance rituals are increasingly performative — reporting cycles focus on target achievement rather than outcome verification, gaming strategies are sophisticated enough to pass audits, and leadership increasingly accepts metric achievement as proof of institutional success despite evidence of actual outcome degradation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by position in the extraction flow. Leadership sees coordination (Rope): metrics enable objective performance assessment and resource optimization — from their vantage point, the system is solving a real problem efficiently. The reform movement sees a solvable institutional problem with a sunset (Scaffold): new metrics frameworks and participatory goal-setting methods are building better alignment between measures and outcomes. Mid-level managers see mixed coordination and extraction (Tangled Rope): they benefit from objective performance visibility (coordination) but are squeezed by impossible targets and blamed for failures (extraction). Frontline workers see pure extraction (Snare): their work is optimized for metric appearance rather than actual quality, they cannot exit without career damage, and the extraction mechanism is enforced by structural power (leadership control of targets and resources). The field's institutional integrity sees extraction (Snare): the professional mission is degraded as agents abandon substantive excellence for metric achievement. The civilizational observer risks a false summit — treating Goodhart decay as an immutable law of measurement — when the constraint is actually a solvable institutional design failure caused by misaligned incentives and enforcement structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position in the extraction flow. Leadership benefits from metrics without bearing downside of gaming distortions: they capture improved-appearing performance metrics, control target setting, and face minimal accountability for actual outcome divergence. d ≈ 0.05-0.15 (beneficiary with arbitrage exit). Mid-level managers are structurally squeezed: they experience both coordination (metrics enable visibility) and extraction (impossible targets, blame for failures, pressure to participate in gaming). d ≈ 0.50-0.55 (symmetric position with constrained exit). Frontline workers and actual outcomes bear the full cost of metric optimization: they cannot influence target setting, face time pressure to optimize metrics at outcome expense, and have limited exit options without career damage. d ≈ 0.85-0.95 (victims with trapped or heavily constrained exit). The organized reform movement has moderate directionality: they perceive the constraint as a problem to solve rather than an extraction mechanism, and have institutional agency to push for metric redesign and alternative frameworks. d ≈ 0.35-0.45 (moderate position with constrained-to-mobile exit). The measurement apparatus is institutionally captured by the beneficiaries (leadership) and maintains gaming-vulnerable systems through inertia. d ≈ 0.20-0.30 (partial beneficiary or captured neutral).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing how institutional position determines whether metric gaming appears as coordination or extraction. Leadership's Rope classification is genuine — they experience the constraint as solving real coordination problems because they benefit from metric improvement and face minimal downside. The tangled rope classification at the mid-level and institutional level is genuine — the metrics do coordinate institutional resource allocation, but that coordination function coexists with asymmetric extraction that benefits leadership. The Snare classification from the powerless frontline perspective is genuine — those agents bear the cost of metric gaming with no ability to influence the constraint. The scaffold classification from the reform movement perspective is genuine — organized agents perceive a sunset pathway through alternative metrics frameworks. The Piton classification for the measurement apparatus is genuine — the machinery is institutionally degraded (known to be gaming-vulnerable) but persists through inertia. The mountain classification risks naturalizing institutional dysfunction as an immutable law, and the engine's false summit detector should flag this as a perspective error — Goodhart decay is real but not inevitable; it reflects institutional design choices, not laws of nature. No single classification is 'correct' — the presheaf over all perspectives reveals the constraint as a tangled institutional arrangement where coordination and extraction are genuinely mixed, and institutional position determines which dominates the observer's experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_vs_outcome_equivalence,
    'Can metric targets ever perfectly capture institutional goals, or is some Goodhart decay inherent to measurement?',
    'Historical analysis of metric systems: tracking divergence between target and outcome over 5+ year periods across institutional domains (healthcare, education, finance, services). Identify threshold where gaming dominates.',
    'If perfect capture possible: constraint is Rope (pure coordination). If decay inevitable: constraint is structural Snare (extraction mechanism inherent to measurement). Classification shifts from Rope → Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_vs_outcome_equivalence, empirical, 'Whether metric targets can achieve outcome equivalence without Goodhart decay').

omega_variable(
    gaming_visibility_window,
    'How long does metric gaming remain hidden before leadership detects divergence between target performance and actual outcomes?',
    'Longitudinal institutional audits: time from metric optimization to detection of gaming, correlated with outcome measurement lag and feedback loop delays.',
    'If detection < 1 year: leadership can course-correct continuously, reducing extraction. If detection > 3 years: gaming becomes embedded in institutional identity and resource allocation, increasing extraction and moving toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gaming_visibility_window, empirical, 'Detection delay between metric gaming and outcome divergence').

omega_variable(
    agency_capture_mechanism,
    'Does institutional focus on gaming metrics corrupt the intrinsic motivation and professional identity of frontline agents, or are barriers to ethical practice purely structural (resource scarcity, time pressure)?',
    'Post-exit surveys and identity interviews with agents who leave metric-gaming institutions: does their sense of professional purpose recover quickly (structural barrier) or persist in damage (identity capture)?',
    'If identity capture: constraint is Snare with internalized suppression — harder to reverse even after metric targets change. If purely structural: constraint remains Tangled Rope — solvable through metric redesign and resource reallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_capture_mechanism, empirical, 'Whether metric gaming produces intrinsic motivation corruption or purely structural barriers').

omega_variable(
    leadership_incentive_alignment,
    'Are leadership incentives genuinely aligned with actual institutional outcomes, or do leaders benefit from apparent metric improvement regardless of real performance?',
    'Compensation structure analysis: correlation between leadership bonuses/promotions and (a) metric targets hit vs (b) actual outcome metrics. Identify whether leaders have personal financial/career stake in real outcomes.',
    'If leadership incentives aligned with reality: leadership position could be rope or institutional perspective as partial victim. If incentives favor metrics over outcomes: leadership is pure beneficiary, constraint is Snare from leadership perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(leadership_incentive_alignment, empirical, 'Whether leadership compensation aligns with actual or apparent metrics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_metric_gaming, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metricsys_tr_t0, institutional_metric_gaming, theater_ratio, 0, 0.35).
narrative_ontology:measurement(metricsys_tr_t3, institutional_metric_gaming, theater_ratio, 3, 0.52).
narrative_ontology:measurement(metricsys_tr_t6, institutional_metric_gaming, theater_ratio, 6, 0.68).
narrative_ontology:measurement(metricsys_tr_t9, institutional_metric_gaming, theater_ratio, 9, 0.72).

% Extraction over time
narrative_ontology:measurement(metricsys_be_t0, institutional_metric_gaming, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(metricsys_be_t3, institutional_metric_gaming, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(metricsys_be_t6, institutional_metric_gaming, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(metricsys_be_t9, institutional_metric_gaming, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_metric_gaming, resource_allocation).
narrative_ontology:boltzmann_floor_override(institutional_metric_gaming, 0.12).
narrative_ontology:affects_constraint(institutional_metric_gaming, goal_displacement).
narrative_ontology:affects_constraint(institutional_metric_gaming, principal_agent_misalignment).
narrative_ontology:affects_constraint(institutional_metric_gaming, institutional_theater).

% DUAL FORMULATION NOTE:
% Metric gaming is downstream of principal-agent misalignment (upstream constraint: leadership and workers have divergent incentives) and feeds into institutional theater (downstream constraint: measurement apparatus becomes performative). The three constraints form a causal cascade: misaligned incentives enable metric gaming, which accumulates into performative institutional routines. Each story has its own extractiveness and perspectives, but they are causally linked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_metric_gaming, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
