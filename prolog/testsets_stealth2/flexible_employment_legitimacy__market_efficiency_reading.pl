% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Legitimate Market-Clearing Mechanism
 *   domain: economic/labor/social_policy
 *
 * SUMMARY:
 *   This story authors flexible employment as its market-efficiency reading
 *   holds it: a legitimate clearing mechanism in which platform-mediated
 *   matching, temp-agency rosters, and freelance marketplaces allocate labor
 *   supply to demand at mutually accepted terms. Wage convergence among
 *   flexible workers is read as a scarcity signal, dispatch algorithms as
 *   neutral coordination instruments, and worker autonomy as maximized
 *   relative to scheduled employment. The claim/metric gap is deliberate and
 *   bounded: the reading CLAIMS rope (genuine coordination, net-benefiting
 *   participants, no suppressed alternatives) while the authored metrics
 *   describe low-but-nonzero extraction, modest soft-control growth, and a
 *   real, observable resistance record that the reading attributes to
 *   transition frictions and classification disputes rather than to
 *   extraction. The engine measures that divergence; the claim is not
 *   reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - - platform_operators: Agenda-setting intermediary (institutional/arbitrage) — sets terms, runs matching, collects per-engagement fees
 *   - - on_demand_workers: Participating labor supplier (moderate/constrained) — sells task labor in chosen increments, bears own tooling and downtime
 *   - - skilled_freelancers: Participating labor supplier (moderate/mobile) — sells professional services project-by-project across concurrent clients
 *   - - on_demand_service_consumers: Demand-side beneficiary (organized/constrained) — buys availability and speed at posted prices
 *   - - businesses_with_volatile_demand: Demand-side beneficiary (powerful/mobile) — converts fixed payroll into variable staffing cost
 *   - - staffing_intermediaries: Secondary intermediary (organized/mobile) — recruits, sets assignment rates, collects the bill-rate/pay-rate spread
 *   - - labor_standards_advocates: Excluded critic seat (organized/constrained) — contests terms from outside the arrangement's governance
 *   - - labor_regulators: Analytical observer (institutional/analytical) — adjudicates classification and enforces sectoral rules from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.3).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.18).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Legitimate Market-Clearing Mechanism").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "economic/labor/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, '740ab715-6a21-4ec6-b888-c07674a2207e').
narrative_ontology:cs_kernel_codification('740ab715-6a21-4ec6-b888-c07674a2207e', formalized).
narrative_ontology:cs_authority_grounding('740ab715-6a21-4ec6-b888-c07674a2207e', practice).
narrative_ontology:cs_interpretation_layer_present('740ab715-6a21-4ec6-b888-c07674a2207e').
narrative_ontology:cs_reading_relation('740ab715-6a21-4ec6-b888-c07674a2207e', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('740ab715-6a21-4ec6-b888-c07674a2207e', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('740ab715-6a21-4ec6-b888-c07674a2207e', foundational, voluntary_exchange_confers_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_exchange_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('740ab715-6a21-4ec6-b888-c07674a2207e', voluntary_exchange_confers_legitimacy, deontological).
narrative_ontology:cs_axiom('740ab715-6a21-4ec6-b888-c07674a2207e', foundational, wage_convergence_is_scarcity_signal).
narrative_ontology:cs_axiom_status(wage_convergence_is_scarcity_signal, holdable).
narrative_ontology:cs_axiom_grounding('740ab715-6a21-4ec6-b888-c07674a2207e', wage_convergence_is_scarcity_signal, empirically_contingent).
narrative_ontology:cs_reference_frame('740ab715-6a21-4ec6-b888-c07674a2207e', voluntary_exchange_clearing_norm).
narrative_ontology:cs_drift_state('740ab715-6a21-4ec6-b888-c07674a2207e', contemporary_algorithmic_management_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('740ab715-6a21-4ec6-b888-c07674a2207e', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, on_demand_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, skilled_freelancers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, on_demand_service_consumers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, businesses_with_volatile_demand).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, staffing_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the digital marketplaces and dispatch systems through which flexible work is offered: they set participation terms, publish pay formulas, run matching algorithms, and collect a percentage fee on each completed engagement. Their revenue scales with matched volume. Exit for them means reallocating capital to other lines of business or jurisdictions; nothing binds them to this particular market.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Supply driving, delivery, care, and task labor in increments they choose, paid per trip or task after a platform fee. They provide their own tools and cover their own downtime, taxes, and insurance. They can switch apps, work several at once, or return to scheduled employment, though switching involves re-onboarding and income gaps.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, on_demand_workers, beneficiary,
    moderate, immediate, constrained, national).

% Sell professional services such as design, writing, software, and consulting project by project through freelance marketplaces and direct client relationships. They set rates within marketplace ranges, maintain profiles and ratings, and typically serve several clients concurrently. Reputational capital travels with them and direct contracting off-platform is common.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, skilled_freelancers, beneficiary,
    moderate, biographical, mobile, global).

% Purchase rides, meals, errands, and services on demand at posted prices. They gain availability and speed without scheduling lead times. Their alternative is doing without, waiting, or using traditional providers at higher prices or lower convenience.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, on_demand_service_consumers, beneficiary,
    organized, biographical, constrained, global).

% Retailers, logistics firms, restaurants, and event companies staff peaks with temp agency rosters and on-demand platforms instead of carrying idle payroll between rushes. They convert fixed labor cost into variable cost. Exiting means rebuilding scheduled staffing and accepting slack capacity during troughs.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, businesses_with_volatile_demand, beneficiary,
    powerful, biographical, mobile, national).

% Run local temp and placement offices: recruit workers, set assignment pay rates, invoice client firms at a markup, and handle payroll compliance. They collect the spread between bill rate and pay rate. Their books of clients and workers are segment-specific, so exit means leaving the staffing line.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, staffing_intermediaries, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, staffing_intermediaries, agenda_setter).

% Unions, worker centers, and policy groups that argue for employment-status protections, minimum earnings floors, and collective bargaining rights for flexible workers. They litigate, campaign, and lobby but hold no seat in the arrangement's day-to-day governance, which runs through platform terms of service and bilateral market participation.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_standards_advocates, excluded,
    organized, generational, constrained, national).

% Labor departments, tax authorities, and courts that adjudicate whether flexible engagements fall inside or outside employment protections, enforce sectoral rules, and occasionally mandate pay floors or benefit contributions. They observe the arrangement from outside it and act through rulemaking and enforcement.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches heterogeneous, fluctuating demand for work (rides, deliveries, projects, seasonal shifts) with heterogeneous labor supply in real time, cutting search costs for both sides, letting firms scale staffing to demand, and letting individuals sell time and skills in small increments.
% TRANSFER_FUNCTION: Moves payment for each engagement from client firms and consumers to workers, minus a percentage fee retained by platforms and staffing intermediaries; moves scheduling risk, equipment costs, and benefits provision from firms to workers; moves price, rating, and demand information across the pooled market.
% ABSENT_VOICES: Labor unions, worker centers, and employment-rights advocates would contest the voluntariness framing and press for employee-status protections and bargaining rights; they sit outside the arrangement's governance, which is conducted through platform terms of service and individual market participation rather than tripartite negotiation.
% DISAPPEARANCE_RATIONALE: Firms would rebuild scheduled rosters and buffer capacity, workers would queue for standard employment or informal arrangements, response times and prices for on-demand services would shift, and the matching infrastructure's fee streams would disappear; staffing patterns across retail, logistics, and hospitality would reorganize within months.
% FOUNDING_PROBLEM: Standard employment tied labor cost to headcount: firms carried idle payroll between demand peaks, and workers with irregular availability, care responsibilities, or niche skills lacked channels to sell labor in small increments. Search costs were high on both sides and demand volatility went unbuffered.
% FOUNDING_PROBLEM_CORROBORATION: Statistical agencies document persistent demand volatility and growth in alternative work arrangements; academic labor economists across camps concede the matching problem is real while disputing whether current arrangements solve it on fair terms; operations research on peak-load staffing predates and is independent of the platform industry. Attestation does not rest on the beneficiary set alone.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).
:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.30 at interval end) because this reading prices the platform fee and the staffing spread as compensation for matching, trust, and compliance services, with residual extraction limited to margins above competitive coordination cost and information asymmetries in pay transparency. Suppression is low (0.18) because exit is real: multi-homing across apps, off-platform contracting, and return to scheduled employment are all exercised paths, and no legal compulsion binds anyone into the arrangement. Theater is low-moderate (0.20): autonomy branding and fairness dashboards exist, but the matching function they decorate is operative. Accessibility collapse is moderate (0.40): standard employment, traditional providers, and direct hiring remain reachable, so alternatives persist under friction rather than collapsing. Resistance is the highest metric (0.55) and is authored as a descriptive fact the reading cannot wish away: classification litigation, unionization drives, and minimum-pay ordinances are observable and ongoing; the reading interprets them as disputes over transition costs and legal categories, not as evidence of extraction. The temporal series run on one shared six-point grid so every tracked metric is authored at every examined time point; the gentle rise in suppression_requirement tracks the maturation of platform management infrastructure (background-check regimes, deaction systems, pay-formula administration) as the sector scaled, not any ratcheting of legal coercion.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently from identical structural inputs. From the platform_operator seat the arrangement is coordination it built and prices; from the on_demand_worker seat it is income access purchased with volatility and self-funded tooling; from the consumer seat it is cheap availability; from the excluded advocate seat the same fee streams and constrained exits would compute as enforced extraction. Among same-level worker seats, exit options differentiate experience: the skilled freelancer's mobile exit (portable reputation, concurrent clients) yields a materially different constraint than the on-demand worker's constrained exit (asset specificity, re-onboarding gaps), despite nominally equal standing. The engine computes these per-seat classifications from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared position in this reading is a beneficiary, so derived directionality sits near the beneficiary pole across seats and no victims are declared — the reading's premises support no victim set, and inventing one would import a sibling reading's structure into this constraint. Residual differences in effective extraction arise from exit options and scope: platform_operators combine beneficiary position with arbitrage-grade exit and agenda control (lowest effective extraction), while constrained-exit seats (on_demand_workers, consumers) carry somewhat higher effective extraction than mobile seats despite identical beneficiary declarations. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — matching volatile demand to heterogeneous supply at low search cost — remains live, so no mandate decay is declared and the arrangement is not drifting toward piton on this reading's account. The theater_ratio series is the early-warning instrument: if autonomy rhetoric decouples from practice (Goodhart drift), theater rises before extraction does, and the reading's rope claim becomes inspectable rather than assumed. The classification discipline cuts both ways: by refusing victim declarations its premises do not support, this reading prevents coordination from being mislabeled as pure extraction; by carrying omegas on algorithmic neutrality, wage-signal interpretation, and the source of participation volition, it preserves exactly the questions whose resolution could flip the computed type toward the sibling readings' profiles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the market_efficiency_reading of the flexible_employment_legitimacy kernel; do the precarity_extraction_reading and developmental_state_reading instantiate structurally different constraints over the same arrangement?',
    'Compile the sibling stories and compare: the precarity reading is expected to author high epsilon with flexible workers declared as victims; the developmental reading is expected to author a sunset-bound transitional form. Divergence in epsilon, victim sets, and computed types across readings locates the kernel''s indexicality.',
    'Sharp cross-reading divergence confirms the kernel is genuinely indexical: per-seat classifications must be read per reading and never averaged; convergence would suggest the readings collapse into one constraint and the family decomposition is spurious.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one kernel, three readings, potentially three distinct constraints.').

omega_variable(
    wage_convergence_signal_or_monopsony,
    'Does blue-collar wage convergence reflect genuine scarcity pricing arrived at through market clearing, or monopsony wage-setting against a queue of surplus labor?',
    'Labor-supply elasticity estimates at the platform level, vacancy-to-applicant ratios in flexible-work segments, and quasi-experiments from platform entry, exit, and pay-formula changes.',
    'Monopsony findings would convert the reading''s central signal interpretation into evidence of wage suppression, raising epsilon above the authored value and stripping the voluntary-clearing axiom of its empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_convergence_signal_or_monopsony, empirical, 'Whether observed wage levels are market signals or exercises of buyer power.').

omega_variable(
    algorithmic_dispatch_neutrality,
    'Are dispatch and pay algorithms neutral matching instruments, or do they embed pay experimentation, surge opacity, and deactivation leverage that tilt terms toward the operator?',
    'Algorithmic audits, pay-variation studies across equivalent trips and tasks, and analysis of deactivation and appeal outcomes.',
    'Confirmed manipulation would raise both extractiveness and suppression above the authored values and shift computed seat types toward enforced-extraction profiles, weakening the neutral-coordination premise this reading stands on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_dispatch_neutrality, empirical, 'Whether the coordination layer is neutral or carries calibrated operator leverage.').

omega_variable(
    participation_volition_source,
    'Is worker participation revealing a preference for flexibility, or the absence of acceptable alternatives — and does the revealed preference persist once local alternatives improve?',
    'Panel data on entry and exit motives, post-exit trajectories of workers who return to standard employment, and natural experiments where local labor markets tighten.',
    'If participation is alternative-driven, the low authored suppression understates effective lock-in, the voluntariness foundation of the coordination reading erodes, and the sibling readings'' classifications gain force; if preference-driven, the low suppression value is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_volition_source, empirical, 'Structural versus preference-based basis of participation; doubles as the suppression-mechanism ambiguity for this arrangement.').

omega_variable(
    steady_state_vs_transitional_status,
    'Is flexible employment a durable steady-state arrangement, or a transitional form that formalization pressure will eventually absorb into standard employment?',
    'Longitudinal shares of alternative work in total employment; whether platform-originated workers accumulate into standard employment over careers or remain in flexible segments.',
    'A transitional finding supports sunset-style treatment and strengthens the developmental sibling reading; a steady-state finding stabilizes this reading''s rope-type claim and its no-sunset structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(steady_state_vs_transitional_status, conceptual, 'Whether the arrangement is an endpoint or a way-station in the organization of work.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(flex_tr_t6, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(flex_tr_t18, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(flex_tr_t30, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(flex_be_t6, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 6, 0.24).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(flex_be_t18, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 18, 0.27).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 24, 0.29).
narrative_ontology:measurement(flex_be_t30, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 30, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(flex_su_t6, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 6, 0.1).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 12, 0.12).
narrative_ontology:measurement(flex_su_t18, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 18, 0.14).
narrative_ontology:measurement(flex_su_t24, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 24, 0.16).
narrative_ontology:measurement(flex_su_t30, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, developmental_state_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'flexible employment legitimacy' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per declared reading of the flexible_employment_legitimacy kernel. Each reading authors its own epsilon, beneficiary/victim structure, and claimed type over the same standing arrangement; this market-efficiency story is the low-epsilon, no-victims, rope-claim reference case. The sibling stories link back through their own affects_constraints arrays; cross-reading divergence in computed types is the corpus's indexicality measurement, not an inconsistency to be reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
