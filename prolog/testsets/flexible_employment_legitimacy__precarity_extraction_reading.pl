% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Precarity Extraction (Algorithmic Labor Discipline Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'flexible employment legitimacy.' The kernel is a set of arrangements
 *   claiming justification as flexible-labor markets; this reading interprets
 *   the same arrangements as a mechanism for platform extraction of surplus
 *   value through wage-risk externalization and algorithmic labor discipline.
 *   The sibling readings (market_efficiency_reading,
 *   developmental_state_reading) frame the same kernel differently:
 *   efficiency-based legitimacy vs. transitional-state management. This story
 *   focuses on the extraction mechanism, making visible how nominal
 *   flexibility functions as coercive precarity at the point where
 *   identity-lock (economic necessity + internalized entrepreneurship
 *   narratives) prevents exit despite algorithmic assignment and unilateral
 *   rate-setting. Claim and metrics are intentionally independent: the
 *   reading CLAIMS snare (coercive extraction), and the metrics (high
 *   extractiveness, high suppression, rising theater ratio) support that
 *   claim — but the claim stands as the reading's assertion, not as a
 *   prediction of the engine's output.
 *
 * KEY AGENTS:
 *   - platform_operators: institutional agenda-setters with global scope, arbitrage-grade exit options, and direct revenue capture from the wage spread
 *   - flexible_workers: powerless, identity-locked targets who bear all income volatility and unemployment risk while accepting algorithmic assignment at algorithmic rates
 *   - service_provision_communities: powerless, trapped payers bearing costs of safety-net gaps and worker insufficient contributions
 *   - traditional_employers: excluded powerful actors who would compete for labor but are structurally locked out of algorithmic allocation
 *   - labor_regulators: analytical observers who can reclassify workers and impose benefits/transparency requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.81).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.76).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, snare).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Precarity Extraction (Algorithmic Labor Discipline Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '5f730cd4-3ef8-4fdc-978b-8fe86482384d').
narrative_ontology:cs_kernel_codification('5f730cd4-3ef8-4fdc-978b-8fe86482384d', distributed).
narrative_ontology:cs_authority_grounding('5f730cd4-3ef8-4fdc-978b-8fe86482384d', extraction).
narrative_ontology:cs_reading_relation('5f730cd4-3ef8-4fdc-978b-8fe86482384d', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f730cd4-3ef8-4fdc-978b-8fe86482384d', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('5f730cd4-3ef8-4fdc-978b-8fe86482384d', foundational, wage_compression_and_risk_externalization_enable_extraction).
narrative_ontology:cs_axiom_status(wage_compression_and_risk_externalization_enable_extraction, holdable).
narrative_ontology:cs_axiom_grounding('5f730cd4-3ef8-4fdc-978b-8fe86482384d', wage_compression_and_risk_externalization_enable_extraction, empirically_contingent).
narrative_ontology:cs_axiom('5f730cd4-3ef8-4fdc-978b-8fe86482384d', foundational, algorithmic_assignment_and_unilateral_rate_setting_constitute_coercion).
narrative_ontology:cs_axiom_status(algorithmic_assignment_and_unilateral_rate_setting_constitute_coercion, holdable).
narrative_ontology:cs_axiom_grounding('5f730cd4-3ef8-4fdc-978b-8fe86482384d', algorithmic_assignment_and_unilateral_rate_setting_constitute_coercion, deontological).
narrative_ontology:cs_reference_frame('5f730cd4-3ef8-4fdc-978b-8fe86482384d', employment_with_comprehensive_risk_bearing).
narrative_ontology:cs_drift_state('5f730cd4-3ef8-4fdc-978b-8fe86482384d', contemporary_algorithmic_platform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5f730cd4-3ef8-4fdc-978b-8fe86482384d', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, flexible_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, service_provision_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, flexible_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, consumer_facing_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce algorithmic assignment of work, set piece rates, control quality metrics, and terminate workers unilaterally. Frame flexibility as worker choice and opportunity. Externalize wage risk, unemployment insurance obligations, and benefits administration to workers and public systems. Capture wage spread between worker payment and customer billing. Technological lock-in (proprietary ratings, algorithm opacity) makes platform switching prohibitively costly for workers despite nominal freedom to exit.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Accept algorithmic work assignment at algorithmically-determined rates, work under algorithmic performance monitoring and discipline, absorb all income volatility and unemployment risk, bear full cost of benefits, training, equipment, and tax compliance. Nominally free to exit but face reputational degradation (low rating), loss of algorithmic priority, and near-total loss of income on departure. Identity-locked through economic necessity (precarity makes staying despite exploitation the rational choice) and through internalized narratives of entrepreneurship and flexibility as personal liberation. Derive some genuine benefits from schedule flexibility and immediate income access, but at cost of perpetual precarity.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, flexible_workers, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, flexible_workers, beneficiary).

% Bear costs of social safety net gaps and workers' insufficient contributions: unemployment insurance systems absorb workers unable to afford contributions, public health systems treat occupational injuries, child care subsidies compensate for low irregular incomes, housing assistance absorbs cost volatility. Absorb externalized risk as tax burden and uncompensated public provision. Geographic overlap with high flexible-work concentration amplifies concentrated cost burden on place-based communities.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, service_provision_communities, payer,
    powerless, generational, trapped, national).

% Face wage pressure and recruitment difficulty as workers are allocated to platform work; compete for workers against the platform's guaranteed income access and lack of employment obligations. Would argue for labor-supply restoration and worker cost-shifting internalization but are structurally excluded from the algorithmic assignment system that allocates labor to the platform. Their exclusion from the work-allocation mechanism is what enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, traditional_employers, excluded,
    powerful, biographical, constrained, national).

% Capture value from the wage spread between worker payment and customer billing. Pass through lower labor costs to customers, reinforcing network effects and competitive advantage. Benefit from algorithmic labor discipline without bearing risk or compliance cost. Participate in the ratcheting of precarity through cross-platform competition on labor cost and service speed.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, consumer_facing_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Investigate whether flexible work structures satisfy labor-law thresholds for worker protection, benefit contribution, and termination notice. Encounter difficulty measuring extraction under the framework that treats workers as independent contractors. Can impose reclassification, mandated benefits, or algorithmic transparency requirements that would alter the constraint's enforcement.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% Organize workers and demand wage floors, benefit access, and algorithmic transparency; are excluded from platform governance and compensation decisions. Would testify that precarity is coercive mechanism, not worker choice, and that identity-lock (low rating making exit costly) violates freedom of association. Structurally blocked from collective bargaining by independent-contractor classification.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, worker_advocacy_organizations, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches labor supply with service demand in real time across geographic regions and customer demand variation, eliminating scheduling friction and allowing customers to access services at moments of need without platform commitment to always-available workforce.
% TRANSFER_FUNCTION: Moves wage surplus (the spread between what workers are paid per unit of service and what customers pay) to platform operators; transfers unemployment, disability, and income-volatility risk from platforms to workers and public safety nets; transfers benefits administration and employment-law compliance cost from platforms to workers, governments, and informal family structures.
% ABSENT_VOICES: Traditional labor unions and collective-bargaining organizations are excluded from platform governance; worker-advocacy organizations cannot participate in algorithmic design or rate-setting; social safety-net systems bear costs but have no voice in labor classification decisions that externalize risk to them.
% DISAPPEARANCE_RATIONALE: If the precarity structure and its enforcement machinery disappeared overnight, labor would shift back to traditional employment forms with sunk benefits and termination costs, customer service prices would rise to reflect true labor costs, platform margins would compress, public safety nets would see reduced caseloads (workers with stable employment contribute more and draw less), and wage volatility for service workers would decline measurably — the entire coordination architecture would reorganize around formal employment or cooperative labor structures.
% FOUNDING_PROBLEM: Traditional employment created costly overhead for platforms and inefficient labor allocation: workers had fixed schedules and termination costs, platforms bore benefits and training expenses, and supply could not scale instantly with demand spikes.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators attest the founding problem is live and solved by flexibility. Labor economists and worker-advocacy organizations corroborate that efficient allocation exists but attest the founding problem has been substantially transformed into a mechanism for cost-shifting; independent research on gig-work earnings volatility and public-benefit uptake supports the precarity reading. Regulatory testimony from labor departments confirms workers cannot negotiate or understand terms they accept, contradicting the autonomy-and-efficiency narrative.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.62→0.81 over interval) because the wage spread (difference between customer price and worker payment) is decoupled from platform marginal cost and grows as competition ratchets down labor cost. Suppression is high (0.76) and rising because the constraint's persistence depends on actively preventing worker exit through algorithmic priority/rating systems and on preventing traditional-employer competition by capturing labor supply. Theater ratio is moderate-to-high (0.38→0.58) because the narrative of 'worker flexibility' and 'entrepreneurship opportunity' performs the work of justifying precarity, and algorithmic speed/responsiveness documentation serves as cover for algorithmic labor discipline. Accessibility of alternatives collapses (0.62) because once inside the rating system, workers face reputation cost that makes exit rational only under severe duress. Resistance is moderate (0.54) because worker advocacy is real but organizationally constrained by independent-contractor classification and algorithmic atomization. Measurements share one time grid across all metrics at each point; the rising extractiveness and theater ratio over the interval track the compression of wage floors and the sophistication of algorithmic performance management (Goodhart drift: platform optimizes for metrics the algorithm can see, not for worker welfare).
 *
 * PERSPECTIVAL GAP:
 *   Platform operators perceive a genuine coordination solution they built and maintain at positive cost (infrastructure, fraud prevention, customer-facing reliability). Workers perceive algorithmic coercion with nominal freedom: they can exit but face reputational and financial catastrophe, which makes staying rational despite extraction. Labor regulators perceive a classification problem: the worker is formally independent, but the terms of work (assignment, rate, termination) are unilaterally set and enforced algorithmically — the structural attributes of employment without the legal protections. The engine computes these seats differently: platform as beneficiary (d→0, low extraction on their side), worker as target (d→1, high extraction), regulator as observer (d=0.5). The divergence is the point: the same structure experiences completely different extraction depending on where you sit.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators: institutional power, arbitrage exit (can switch to other revenue streams, geographic markets, or labor populations), declared as agenda-setter and beneficiary — derived d approaches 0.0 (subsidy/benefit) because they capture the extraction. Flexible workers: powerless, identity-locked (staying is rational despite extraction because leaving means rating death and income collapse; internalized narratives of entrepreneurship fuse identity with precarity), declared as payer — derived d approaches 1.0 (full target) because they absorb the extraction's weight without exit. Service communities: powerless, trapped (geographic concentration means they cannot exit the region, and cost-shifting happens at regional budget level), declared as payer — derived d approaches 1.0. Labor regulators: institutional, analytical (can observe but not directly alter without legislative action; exit is analytical distance, not concrete), declared as observer — derived d = 0.5 (symmetric position). The identity-locked exit for workers is the key structural difference from trapped-exit: trapped agents face external barriers; identity-locked agents have internalized the barrier, making exit feel like identity death rather than economic necessity. This distinction should modulate suppression upward for identity-locked agents — they suppress themselves more efficiently than external force could achieve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (scheduling friction, labor-cost overhead) was real and the flexible-employment coordination does solve it. But the founding problem's status has shifted: it is now contested whether the founding problem persists or whether what persists is a solutions-cum-extraction hybrid. This reading asserts the hybrid: the coordination function persists but has been layered with extraction (wage compression, risk externalization) such that the bundle no longer solves the founding problem in the way the name suggests. The theater ratio (0.58) indicates significant performative content: the 'flexibility,' 'opportunity,' and 'autonomous' narratives perform the work of justifying precarity while the algorithmic machinery performs labor discipline. A snare classified as mandatrophic would show theater_ratio > 0.65 and would derive at least one of its persistence mechanisms from inertia rather than active extraction. This constraint shows active extraction (wage spread growth, algorithmic rate compression) driving persistence, so mandatrophy is not the right frame — it is an active snare with theatrical cover, not a degraded coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the measured suppression (0.76) primarily structural (external algorithmic barriers, rating mechanisms that impose financial cost on exit) or internalized (workers have accepted precarity as legitimate, fused their identity with entrepreneurship-framing, and suppress themselves)?',
    'Comparative analysis of worker exit trajectories post-deactivation: do workers maintain suppression-level beliefs and constraints after platform removal (internalized), or do beliefs and constraints rapidly dissolve (structural)? Qualitative interviews about workers'' self-perception of choice and autonomy before vs. after exit.',
    'If primarily internalized, the constraint''s effective suppression is higher than structural measurement suggests — workers carry suppression with them post-exit, enabling rapid re-platforming. If primarily structural, exit would show rapid norm-shift, suggesting suppression is artifact of the mechanism. The distinction affects remediation strategy: structural suppression can be relieved by removing barriers; internalized suppression requires identity re-fusion work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Structural vs. internalized suppression in algorithmic labor discipline').

omega_variable(
    wage_spread_decoupling_from_cost,
    'What is the platform''s actual marginal cost to match workers with customers, verify availability, process payment, and manage disputes, relative to the wage spread it extracts (difference between customer price and worker payment)?',
    'Regulatory discovery compelling cost structure and transaction-volume disclosure; independent economic modeling of comparable matching and payment services; comparative analysis of worker earnings on competing platforms with different margin structures.',
    'A wide cost-to-spread gap (e.g., 15% cost for 40% spread) establishes the wage spread as monopoly rent and supports mandated rate transparency or algorithmic auditing. A narrow gap would suggest the spread reflects genuine platform cost. A negative gap (cost exceeds spread) would be prima facie evidence of predatory pricing or loss-leader extraction from downstream service providers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_spread_decoupling_from_cost, empirical, 'Whether wage spread tracks platform marginal cost or market power').

omega_variable(
    coordination_extraction_separability,
    'Is algorithmic labor matching (the coordination function) structurally inseparable from worker precarity (identity-lock, wage compression, risk externalization), or could matching occur with worker protections in place (guaranteed minimum hours, benefits, transparent algorithms)?',
    'Natural experiment from jurisdictions that mandate worker protections, algorithmic transparency, or benefits access while preserving platform matching: do coordination benefits persist, degrade, or require cost pass-through to customers? Cross-platform comparison of matching efficiency under different labor-protection regimes.',
    'If separable, worker protections can be mandated without sacrificing matching efficiency, making the precarity component pure extraction. If inseparable, some tradeoff exists between matching speed and worker stability — but the tradeoff point is an empirical question, not a natural law. Most likely: separability at reduced margin (slower matching, higher prices) — which means the current precarity level exceeds what coordination requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether coordination and precarity are structurally separable or require tradeoff').

omega_variable(
    kernel_reading_contest_boundaries,
    'Are the three readings of the flexible_employment_legitimacy kernel genuinely coexistent, or does empirical evidence about extraction levels and worker-benefit gaps functionally foreclose the market_efficiency_reading?',
    'Systematic comparison of earnings volatility, benefit access, and risk externalization against traditional employment baselines; analysis of whether platform labor outcomes match predicted efficiency distributions or diverge significantly; assessment of whether platforms'' efficiency gains have been passed to customers or captured as operator margin.',
    'If market_efficiency reading is empirically foreclosed (worker outcomes are worse, platforms pocket efficiency gains, risk is externalized), the contest narrows to developmental_state vs. precarity_extraction — a more tractable policy dispute. If efficiency gains do materialize in customer prices and worker earnings, the reading remains live. This distinction affects the status field in cs_structure.axioms: market_efficiency''s empirical grounding is testable and potentially overridable by data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_boundaries, empirical, 'Whether the market_efficiency reading remains live given empirical evidence of wage compression and externalization').

omega_variable(
    precarity_reading_axiom_status_hold,
    'The foundational axiom of this reading (wage_compression_and_risk_externalization_enable_extraction) depends on the empirical claim that platforms are compressing wages below what competitive equilibrium would permit and externalizing risks that traditional employers would bear. If that empirical claim is overridden by evidence of genuine competitive wage floors and risk-sharing arrangements, does the axiom remain holdable?',
    'Longitudinal wage analysis comparing gig workers to traditional employees in same-skill occupations, controlling for effort and hours; benefit-participation rates and subsidy uptake by gig workers; employer bearing of occupational-injury costs and unemployment contributions across sectors.',
    'If evidence shows platforms ARE compressing wages and externalizing risk, the axiom remains holdable and the precarity_extraction reading stands. If evidence shows competitive wage-setting and risk-bearing equivalent to traditional employment, the axiom becomes overridden within this reading''s own tradition — the reading would have to switch to a different distinguishing claim or collapse into the market_efficiency reading. This is an internal consistency test, not an external truth test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precarity_reading_axiom_status_hold, empirical, 'Whether the precarity reading''s foundational axiom about wage compression remains empirically holdable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(flex_tr_t0, observed).
narrative_ontology:measurement(flex_tr_t3, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement_basis(flex_tr_t3, observed).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement_basis(flex_tr_t8, observed).
narrative_ontology:measurement(flex_tr_t13, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 13, 0.53).
narrative_ontology:measurement_basis(flex_tr_t13, observed).
narrative_ontology:measurement(flex_tr_t18, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 18, 0.56).
narrative_ontology:measurement_basis(flex_tr_t18, observed).
narrative_ontology:measurement(flex_tr_t25, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(flex_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(flex_be_t0, observed).
narrative_ontology:measurement(flex_be_t3, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 3, 0.67).
narrative_ontology:measurement_basis(flex_be_t3, observed).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 8, 0.74).
narrative_ontology:measurement_basis(flex_be_t8, observed).
narrative_ontology:measurement(flex_be_t13, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 13, 0.77).
narrative_ontology:measurement_basis(flex_be_t13, observed).
narrative_ontology:measurement(flex_be_t18, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 18, 0.79).
narrative_ontology:measurement_basis(flex_be_t18, observed).
narrative_ontology:measurement(flex_be_t25, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(flex_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(flex_su_t0, observed).
narrative_ontology:measurement(flex_su_t3, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 3, 0.68).
narrative_ontology:measurement_basis(flex_su_t3, observed).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement_basis(flex_su_t8, observed).
narrative_ontology:measurement(flex_su_t13, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 13, 0.74).
narrative_ontology:measurement_basis(flex_su_t13, observed).
narrative_ontology:measurement(flex_su_t18, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 18, 0.75).
narrative_ontology:measurement_basis(flex_su_t18, observed).
narrative_ontology:measurement(flex_su_t25, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(flex_su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(flex_grid_01, flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(flex_grid_02, flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse(class), 25, 0.63).
narrative_ontology:measurement(flex_grid_03, flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse(individual), 0, 0.71).
narrative_ontology:measurement(flex_grid_04, flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse(individual), 25, 0.79).
narrative_ontology:measurement(flex_grid_05, flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse(organizational), 0, 0.38).
narrative_ontology:measurement(flex_grid_06, flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse(organizational), 25, 0.44).
narrative_ontology:measurement(flex_grid_07, flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse(structural), 0, 0.42).
narrative_ontology:measurement(flex_grid_08, flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse(structural), 25, 0.51).
narrative_ontology:measurement(flex_grid_09, flexible_employment_legitimacy__precarity_extraction_reading, resistance(class), 0, 0.43).
narrative_ontology:measurement(flex_grid_10, flexible_employment_legitimacy__precarity_extraction_reading, resistance(class), 25, 0.57).
narrative_ontology:measurement(flex_grid_11, flexible_employment_legitimacy__precarity_extraction_reading, resistance(individual), 0, 0.28).
narrative_ontology:measurement(flex_grid_12, flexible_employment_legitimacy__precarity_extraction_reading, resistance(individual), 25, 0.31).
narrative_ontology:measurement(flex_grid_13, flexible_employment_legitimacy__precarity_extraction_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(flex_grid_14, flexible_employment_legitimacy__precarity_extraction_reading, resistance(organizational), 25, 0.71).
narrative_ontology:measurement(flex_grid_15, flexible_employment_legitimacy__precarity_extraction_reading, resistance(structural), 0, 0.35).
narrative_ontology:measurement(flex_grid_16, flexible_employment_legitimacy__precarity_extraction_reading, resistance(structural), 25, 0.52).
narrative_ontology:measurement(flex_grid_17, flexible_employment_legitimacy__precarity_extraction_reading, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(flex_grid_18, flexible_employment_legitimacy__precarity_extraction_reading, stakes_inflation(class), 25, 0.68).
narrative_ontology:measurement(flex_grid_19, flexible_employment_legitimacy__precarity_extraction_reading, stakes_inflation(individual), 0, 0.68).
narrative_ontology:measurement(flex_grid_20, flexible_employment_legitimacy__precarity_extraction_reading, stakes_inflation(individual), 25, 0.81).
narrative_ontology:measurement(flex_grid_21, flexible_employment_legitimacy__precarity_extraction_reading, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(flex_grid_22, flexible_employment_legitimacy__precarity_extraction_reading, stakes_inflation(organizational), 25, 0.42).
narrative_ontology:measurement(flex_grid_23, flexible_employment_legitimacy__precarity_extraction_reading, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(flex_grid_24, flexible_employment_legitimacy__precarity_extraction_reading, stakes_inflation(structural), 25, 0.54).
narrative_ontology:measurement(flex_grid_25, flexible_employment_legitimacy__precarity_extraction_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(flex_grid_26, flexible_employment_legitimacy__precarity_extraction_reading, suppression(class), 25, 0.71).
narrative_ontology:measurement(flex_grid_27, flexible_employment_legitimacy__precarity_extraction_reading, suppression(individual), 0, 0.72).
narrative_ontology:measurement(flex_grid_28, flexible_employment_legitimacy__precarity_extraction_reading, suppression(individual), 25, 0.83).
narrative_ontology:measurement(flex_grid_29, flexible_employment_legitimacy__precarity_extraction_reading, suppression(organizational), 0, 0.41).
narrative_ontology:measurement(flex_grid_30, flexible_employment_legitimacy__precarity_extraction_reading, suppression(organizational), 25, 0.48).
narrative_ontology:measurement(flex_grid_31, flexible_employment_legitimacy__precarity_extraction_reading, suppression(structural), 0, 0.44).
narrative_ontology:measurement(flex_grid_32, flexible_employment_legitimacy__precarity_extraction_reading, suppression(structural), 25, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__precarity_extraction_reading, 0.18).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, algorithmic_labor_discipline_gate).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, wage_externalization_mechanism).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, public_safety_net_cost_shifting).

% DUAL FORMULATION NOTE:
% The flexible_employment_legitimacy kernel has three readings: market_efficiency_reading (flexibility as legitimate market mechanism), developmental_state_reading (flexibility as transition toward formalization), precarity_extraction_reading (this story — flexibility as coercive precarity mechanism). Each reading instantiates the same algorithmic labor platform arrangements differently, with different beneficiary/victim structures and different ε values. The readings coexist as live positions held by different parties (platforms, regulators, workers, economists) with no single framework resolving them. Empirical evidence about wage compression, risk externalization, and worker-benefit gaps may eventually foreclose the market_efficiency reading or override the precarity reading's axioms, but as of composition both remain live. The three-reading decomposition respects the ε-invariance principle: each reading has a distinct, internally consistent ε (extractiveness that follows from its structural assumptions) rather than one constraint measured via different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
