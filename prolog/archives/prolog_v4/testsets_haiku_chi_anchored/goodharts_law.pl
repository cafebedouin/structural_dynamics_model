% ============================================================================
% CONSTRAINT STORY: goodharts_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodharts_law, []).

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
 *   constraint_id: goodharts_law
 *   human_readable: Goodhart's Law: Metric Gaming and Systemic Distortion
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Goodhart's Law describes a structural constraint that emerges whenever a
 *   metric becomes the target of institutional management. The constraint
 *   operates across every domain where measurement and incentive alignment
 *   are attempted: education (test scores), healthcare (readmission rates,
 *   surgical outcomes), law enforcement (arrest counts, response times),
 *   lending (credit scores, approval rates), manufacturing (quality metrics,
 *   efficiency ratios), and governance (KPIs, performance targets). The law
 *   manifests as a feedback loop: agents optimize for the metric rather than
 *   the underlying outcome, the metric becomes decoupled from the outcome it
 *   was meant to measure, the metric loses informational value, and the
 *   institution must either redesign the measurement system or accept
 *   systematic distortion. This constraint exhibits characteristics of
 *   tangled rope (coordination function + asymmetric extraction), piton
 *   (performative monitoring), snare (end users trapped by metric collapse),
 *   and scaffold (reform cycles attempting to replace gamed metrics). The
 *   theater_ratio (0.68) reflects that institutional measurement and
 *   verification become increasingly performative over time: audits,
 *   compliance certifications, and metric reporting persist even as the
 *   metrics lose connection to actual outcomes.
 *
 * KEY AGENTS:
 *   - Principal Institutions: Institutional/arbitrage — government agencies, corporations, boards using metrics to coordinate behavior; net beneficiaries during the optimization phase
 *   - Metric Optimizers: Moderate/constrained — teachers, doctors, police, loan officers, workers facing career incentives to hit targets; experience dual pressure (coordination + coercion)
 *   - End Users: Powerless/trapped — students, patients, loan applicants, citizens served by optimized systems; bear the cost of metric collapse; cannot exit
 *   - Measurement Apparatus: Institutional/constrained — auditors, inspectors, regulators performing verification rituals; theatrical monitoring; maintain degraded function through inertia
 *   - Reform Coalitions: Organized/mobile — practitioners pushing for outcome evaluation, balanced scorecards, stakeholder feedback; see metrics as temporary rather than permanent
 *   - Analytical Observer: Analytical/analytical — risks naturalizing metric gaming as inevitable law rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodharts_law, 0.52).
domain_priors:suppression_score(goodharts_law, 0.65).
domain_priors:theater_ratio(goodharts_law, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodharts_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(goodharts_law, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(goodharts_law, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodharts_law, tangled_rope).
narrative_ontology:human_readable(goodharts_law, "Goodhart's Law: Metric Gaming and Systemic Distortion").
narrative_ontology:topic_domain(goodharts_law, "economic/social/technological").

domain_priors:requires_active_enforcement(goodharts_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodharts_law, metric_optimizers).
narrative_ontology:constraint_beneficiary(goodharts_law, principal_institutions).
narrative_ontology:constraint_victim(goodharts_law, end_users_served).
narrative_ontology:constraint_victim(goodharts_law, aggregate_system_health).
narrative_ontology:constraint_victim(goodharts_law, information_ecology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USERS (SNARE) — Cannot exit the system; bear the cost of optimized-away quality. Teachers' students suffer dumbed-down curriculum. Hospital patients receive faster-to-treat care regardless of outcomes. Loan applicants face inflated credit scores. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.93. The constraint extracts via forced exposure to degraded service.
constraint_indexing:constraint_classification(goodharts_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: METRIC OPTIMIZERS (TANGLED ROPE) — Teachers, doctors, loan officers, police: face dual incentives. Metric targets enable coordination (know what's expected, plan resources) but create extraction (gaming is career-mandatory; real outcomes degrade; gaming becomes necessary for survival). d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.52. Mixed: genuine coordination function + coercive pressure.
constraint_indexing:constraint_classification(goodharts_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRINCIPAL INSTITUTIONS (ROPE) — Government, corporations, boards use metrics to coordinate agency behavior across distributed actors. Metrics solve a real problem: how to align incentives at scale without direct supervision. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.03. Beneficiary. The institution experiences Goodhart's Law as a coordination mechanism they designed, unaware it's degenerating into gaming.
constraint_indexing:constraint_classification(goodharts_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEASUREMENT SYSTEM (PITON) — Once metrics are set and gamed, the measurement apparatus itself becomes theater. Auditors, inspectors, regulators perform verification rituals that no longer detect real problems. Goodhart's Law hollows out the monitoring function. theater_ratio=0.68 → piton gate satisfied. The measurement system persists through regulatory inertia despite low functional value.
constraint_indexing:constraint_classification(goodharts_law, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITIONS (SCAFFOLD) — Qualitative evaluation, outcome measurement, balanced scorecards, stakeholder feedback represent temporary coordination solutions with sunset logic. As gaming evolves, metrics are periodically abandoned and replaced. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.16. Low effective extraction because reform agents have exit (can switch methodologies) and see a path forward.
constraint_indexing:constraint_classification(goodharts_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN?) — From a universal/civilizational view, Goodhart's Law appears as an immutable principle of information theory: any metric used for control becomes distorted as agents optimize it. However, base properties (ε=0.52, suppression=0.65, theater=0.68) contradict mountain classification. Engine will flag false summit: Goodhart's Law is contingent on institutional measurement choice, not a law of nature.
constraint_indexing:constraint_classification(goodharts_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodharts_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodharts_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodharts_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodharts_law, TR),
    TR >= 0.70.

:- end_tests(goodharts_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value from end users (who receive degraded service as metrics displace outcomes) and from metric optimizers (who must game to survive). The extraction is not as severe as a full snare (0.70+) because: (a) principals could redesign metrics but choose not to because single metrics are cheaper to monitor; (b) reformers periodically replace gamed metrics, creating temporary relief; (c) some agents (institutional reformers) can exit to different measurement regimes. Suppression (0.65): Moderate-high. Significant barriers to escaping metric gaming include: institutional scale (millions of teachers/doctors cannot coordinate to refuse targets), career risk (refusing to optimize damages advancement), regulatory binding (metrics are often mandated by law), and lack of alternatives (what metric could replace test scores for education?). But suppression is not total because alternative methodologies exist and are deployed episodically. Theater_ratio (0.68): High. Institutional measurement becomes increasingly performative as gaming advances. Compliance audits detect whether metrics are being reported, not whether outcomes are improving. Inspections certify that measurement procedures are followed, not that measured phenomena are real. The measurement system persists through regulatory inertia even as its informational value collapses.
 *
 * PERSPECTIVAL GAP:
 *   The principal institution sees Goodhart's Law as a coordination mechanism they designed (Rope perspective) — metrics solve the problem of aligning thousands of distributed agents without direct supervision. The metric optimizer sees it as a coercive dual-pressure system (Tangled Rope) — they must optimize because the metric is their performance evaluation, but optimization damages the outcomes they actually care about. The end user sees pure extraction (Snare) — their experience systematically degrades as service providers optimize for metrics rather than quality. The measurement system (auditors, inspectors) sees a degraded ritual (Piton) — they perform verification that no longer detects problems. The reform coalition sees a temporary problem with cyclical solutions (Scaffold) — metrics get replaced on generational timescales. The civilizational analytical observer risks seeing Goodhart's Law as an immutable principle of information theory (false Mountain) — in reality, it reflects the choice to use single-metric incentives rather than outcome-based evaluation.
 *
 * DIRECTIONALITY LOGIC:
 *   Principal Institutions: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary. They benefit from the low cost of single-metric coordination and only face consequences if gaming becomes visible. Metric Optimizers: Victim + constrained → d≈0.65, f(d)≈1.00. Moderate extraction. They are forced to game because the metric is their evaluation but cannot organize collectively because they are geographically distributed (teachers in thousands of schools). End Users: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. They bear the cost of optimized-away quality and have no exit option short of leaving the system entirely (withdrawing from school, changing hospitals, switching lenders). Measurement Apparatus: Institutional + constrained → d≈0.40, f(d)≈0.40. Moderate extraction. Auditors and inspectors perform theater because the institution defines what counts as 'passing verification.' Reform Coalitions: Organized + mobile → d≈0.35, f(d)≈0.30. Low effective extraction. They can exit to alternative measurement regimes and see a sunset path (new metrics will replace old ones).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gaming_vs_legitimate_optimization,
    'Where is the boundary between legitimate performance improvement and destructive metric gaming?',
    'Outcome audit: track whether metric improvement correlates with improvement in the actual outcome the metric was meant to capture. If correlation persists, it''s optimization; if it reverses or plateaus, it''s gaming.',
    'If boundary is clear: gaming is identifiable and can be penalized. If boundary is fuzzy: all optimization appears as potential gaming, paralyzing agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gaming_vs_legitimate_optimization, empirical, 'Distinguishing legitimate performance from metric gaming').

omega_variable(
    metric_half_life,
    'What is the typical half-life of a metric before gaming reduces its informational value below usefulness?',
    'Longitudinal analysis across domains: track metric validity, correlation with outcomes, and gaming rate over time for education (test scores), healthcare (readmission rates), law enforcement (arrest counts), lending (credit scores).',
    'If half-life < 3 years: metrics are effectively theater; institutions should adopt shorter cycles or qualitative evaluation. If half-life > 10 years: metrics retain value; current optimization assumptions are justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_half_life, empirical, 'Typical duration before metric gaming undermines informational value').

omega_variable(
    principal_awareness_and_intent,
    'Do principal institutions deliberately choose metric targets knowing gaming will occur, extracting value anyway, or do they genuinely believe in metric informativeness?',
    'Institutional behavior analysis: track whether principals replace gamed metrics proactively or only after public crisis. Interview principals about their assumptions. Compare replacement rate to the empirical half-life of metrics.',
    'If deliberate: Goodhart''s Law is intentional extraction (higher snare classification for metric-setters). If unaware: principals are trapped by their own constraint (piton classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principal_awareness_and_intent, conceptual, 'Whether principals understand and exploit metric gaming').

omega_variable(
    multi_metric_stabilization,
    'Does deploying multiple independent metrics (balanced scorecards) prevent gaming, or does gaming simply diversify across multiple targets?',
    'Comparative case study: organizations using single metrics vs balanced scorecards vs qualitative evaluation. Track outcome quality, agent behavior, and time to metric collapse.',
    'If stabilizes: multiple metrics offer structural escape. If gaming diversifies: adding metrics increases complexity but doesn''t prevent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_metric_stabilization, empirical, 'Whether multiple metrics prevent or merely redistribute gaming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodharts_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodharts_law, theater_ratio, 0, 0.35).
narrative_ontology:measurement(goodhart_tr_t5, goodharts_law, theater_ratio, 5, 0.52).
narrative_ontology:measurement(goodhart_tr_t10, goodharts_law, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodharts_law, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(goodhart_be_t5, goodharts_law, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(goodhart_be_t10, goodharts_law, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodharts_law, enforcement_mechanism).
narrative_ontology:affects_constraint(goodharts_law, teaching_to_the_test).
narrative_ontology:affects_constraint(goodharts_law, financial_metrics_decoupling).
narrative_ontology:affects_constraint(goodharts_law, healthcare_outcome_compression).
narrative_ontology:affects_constraint(goodharts_law, policing_arrest_rate_substitution).

% DUAL FORMULATION NOTE:
% Goodhart's Law is a meta-constraint that affects the structure of many domain-specific constraints. The law describes how any institutional measurement system degrades when used for control. Domain-specific instantiations (teaching-to-the-test, financial reporting manipulation, healthcare gaming) have their own ε values reflecting domain-specific resistance and institutional dynamics, but all are downstream of the Goodhart structural principle. The general law (this story) has ε=0.52 reflecting the coordination-extraction hybrid. Domain instances may have lower ε (if domain-specific alternatives to metrics exist) or higher ε (if metrics are uniquely powerful in that domain).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodharts_law, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
