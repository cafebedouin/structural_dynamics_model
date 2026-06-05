% ============================================================================
% CONSTRAINT STORY: parkinsons_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parkinsons_law, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: parkinsons_law
 *   human_readable: Parkinson's Law: Work Expansion and Bureaucratic Growth
 *   domain: organizational/economic
 *
 * SUMMARY:
 *   Parkinson's Law describes a structural tendency within organizations for
 *   work and personnel to expand regardless of actual productive output
 *   requirements. The constraint operates through a combination of
 *   psychological factors (workers padding timelines, appearing busy) and
 *   structural incentives (administrative personnel justified through
 *   headcount, budgets tied to departmental size, hierarchical advancement
 *   tied to supervisory roles). The constraint exhibits properties of both
 *   pure coordination (legitimate hierarchical administration) and extraction
 *   (generating busywork to justify budgets). Theater ratio (0.78) reflects
 *   that much administrative activity is performative: status meetings,
 *   approval chains, documentation, and audit procedures that consume time
 *   without proportional productive output. The constraint is most visible in
 *   government bureaucracies, large corporations, and organizations insulated
 *   from competitive market pressure, where budget autonomy and hierarchical
 *   advancement incentives dominate. The constraint is least visible in
 *   startups and highly competitive firms where survival pressure forces
 *   alignment between headcount/budget and actual output. This distribution
 *   pattern suggests Parkinson's Law is not a natural law but a contingent
 *   institutional arrangement enabled by specific incentive structures.
 *
 * KEY AGENTS:
 *   - Individual Worker: Primary victim (powerless/trapped) — faces expanding workload and manufactured tasks; cannot exit or refuse work
 *   - Administrative Personnel and Middle Management: Primary beneficiary (institutional/arbitrage) — benefits from expanded budgets, headcount, and advancement opportunities tied to departmental size
 *   - Senior Executive: Dual perspective (powerful/mobile) — benefits from organizational prestige and size; bears costs from inefficiency and decision slowness; can exit to leaner organizations
 *   - Labor Union / Employee Coalition: Organized secondary beneficiary (organized/constrained) — benefits from job creation and negotiating leverage; bears costs from organizational inefficiency
 *   - Bureaucratic Apparatus: Mechanism (institutional/arbitrage) — administrative procedures, approval chains, and documentation systems that generate performative work
 *   - Lean Management / Reform Coalition: Organized reformers (organized/constrained) — see temporary problem with identifiable interventions; building alternative pathways (time-based budgeting, agile, outcome metrics)
 *   - Competitive Market Discipline: Structural alternative (analytical) — organizations subject to profit/loss discipline show weaker Parkinson's effects, suggesting constraint is policy-enabled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parkinsons_law, 0.52).
domain_priors:suppression_score(parkinsons_law, 0.65).
domain_priors:theater_ratio(parkinsons_law, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parkinsons_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(parkinsons_law, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(parkinsons_law, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parkinsons_law, tangled_rope).
narrative_ontology:human_readable(parkinsons_law, "Parkinson's Law: Work Expansion and Bureaucratic Growth").
narrative_ontology:topic_domain(parkinsons_law, "organizational/economic").

domain_priors:requires_active_enforcement(parkinsons_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parkinsons_law, administrative_personnel).
narrative_ontology:constraint_beneficiary(parkinsons_law, middle_management).
narrative_ontology:constraint_victim(parkinsons_law, productive_output_efficiency).
narrative_ontology:constraint_victim(parkinsons_law, resource_allocation_optimization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE WORKER (SNARE) — Individual employees cannot exit the constraint; they face manufactured work, elongated timelines, and task proliferation that absorbs their time regardless of actual productivity needs. Trapped in organizational hierarchy with no ability to refuse work expansion. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(parkinsons_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADMINISTRATIVE PERSONNEL AND MIDDLE MANAGEMENT (ROPE) — Benefit from work expansion through budget growth, headcount increases, and career advancement tied to departmental size. Experience the constraint as coordination: justifying larger budgets through demonstrated busy-ness. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary; sees constraint as functional.
constraint_indexing:constraint_classification(parkinsons_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SENIOR EXECUTIVE (TANGLED ROPE) — Both benefits (organizational size correlates with prestige, compensation, and influence) and bears costs (inefficiency, slower decision-making, reduced actual output). Mobile exit option (can move to leaner organization) but substantial switching costs. Experiences constraint as hybrid: coordination infrastructure (necessary hierarchy) layered with extractive waste. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(parkinsons_law, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNION / EMPLOYEE COALITION (TANGLED ROPE) — Organized agents benefit from expanded hiring (job security, membership growth, negotiating leverage) but also bear costs from inefficiency (organizational rigidity, reduced competitiveness, job precarity if organization fails). Constrained exit (collective action required, switching costs high). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(parkinsons_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE BUREAUCRATIC APPARATUS (PITON) — The constraint's primary mechanism: administrative procedures, status reports, approval chains, and personnel justification. Theater ratio (0.78) reflects that much administrative activity is performative (meetings about meetings, reports no one reads, approval rituals). The system persists through institutional inertia: these procedures were once necessary coordination mechanisms but now operate largely as theater. theater_ratio=0.78 satisfies piton gate (≥0.70).
constraint_indexing:constraint_classification(parkinsons_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEAN MANAGEMENT / ORGANIZATIONAL REFORM (SCAFFOLD) — Organized agents (consulting firms, process improvement teams, efficiency advocates) see Parkinson's Law as a temporary organizational pathology with identifiable sunset mechanisms: time-based budgeting, zero-based budgeting, performance metrics tied to output rather than inputs, agile methodologies, and decentralized decision-making. Constrained by organizational resistance and incumbent interests, but with genuine methodology to replace the constraint. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.22. Low effective extraction because reform pathway exists.
constraint_indexing:constraint_classification(parkinsons_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk naturalization: viewing Parkinson's Law as an immutable organizational principle, like thermodynamic law. 'Bureaucracy always expands because people naturally seek easier lives.' This perspective treats the constraint as axiomatic. However, base_extractiveness=0.52 and suppression=0.65 contradict a mountain classification — these values indicate institutional arrangements, not natural law. The engine will classify this as false summit: Parkinson's Law appears natural but is contingent on incentive structures, not inevitable.
constraint_indexing:constraint_classification(parkinsons_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parkinsons_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parkinsons_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parkinsons_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parkinsons_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parkinsons_law, TR),
    TR >= 0.70.

:- end_tests(parkinsons_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint generates real extraction: administrative overhead that consumes resources without proportional productive output. However, extraction is not maximal (≤0.66 snare threshold) because: (1) legitimate hierarchical coordination is present, (2) some busywork serves compliance/risk-management functions, (3) workers are not in absolute desperation. Suppression (0.65): Moderate-high. Barriers to exit include: organizational hierarchy (cannot refuse work), labor market switching costs, regulatory requirements that mandate bureaucratic procedures, and collective action problems preventing coalitions. However, suppression is not total because competitive market discipline and lean management alternatives do exist. Theater ratio (0.78): High and rising. Administrative activity has become increasingly performative: documentation that no one reads, meetings about meetings, audit trails that don't prevent problems, approval chains that add coordination overhead without decision improvement. The theater increases over time as administrative procedures accumulate and become ritually maintained even after their original justification disappears (piton mechanism).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence. Individual workers see pure extraction (Snare): manufactured work, inflated timelines, inability to refuse. Beneficiary administrators see coordination (Rope): budgets, growth, advancement — legitimate organizational function. Lean reformers see temporary pathology with sunset (Scaffold): they have methodology to flatten hierarchies, tie budgets to output, and replace theater with metrics. The bureaucratic apparatus sees itself as degraded (Piton): it persists through institutional inertia, not function — procedures that were once necessary (audit trails, approval chains) now operate as ritual. Senior executives see mixed picture (Tangled Rope): real coordination benefits (hierarchy enables large-scale organization) but also real waste (inefficiency, slowness). The false naturalization (Mountain) risks treating a contingent incentive structure as an immutable law of human nature. Market-discipline evidence suggests the constraint is institutional, not natural: it shrinks dramatically when organizations face competitive pressure to align costs with output.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual worker: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit options. Administrative beneficiary: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can move freely within incentive structure. Senior executive: Both + mobile → d≈0.50, f(d)≈0.65. Symmetric position; both benefits and costs are real. Union coalition: Victim + organized (constrained) → d≈0.55, f(d)≈0.75. Organized but constrained by collective action and switching costs. Lean reformers: Organized + constrained → d≈0.42, f(d)≈0.42. Have methodology and organization but face institutional resistance. The engine derives these d values from beneficiary/victim declarations and exit options without override.
 *
 * MANDATROPHY ANALYSIS:
 *   Parkinson's Law resolves mandatrophy by distinguishing coordination from extraction within a single institutional mechanism. The hierarchical structure serves both functions: legitimate coordination (large-scale organization, delegation, accountability) and extraction (work expansion, busywork, budget justification). The constraint is Tangled Rope (not pure Rope) because suppression is high (0.65 ≥ 0.40 gate) — significant barriers prevent exit. The constraint is Tangled Rope (not pure Snare) because beneficiary-driven coordination is real; the mechanism would not function without genuine administrative coordination. Theater ratio (0.78) indicates that the coordination function has atrophied relative to the performative ritual — the system maintains procedures long after their justification changes (piton mechanism). Mandatrophy resolution: the same budget, headcount, and advancement incentives that drive coordination also drive extraction. These cannot be separated within the current institutional structure; any reform that reduces theater and extraction (lean management, zero-based budgeting, outcome metrics) necessarily reduces some legitimate coordination functions as well. The system is genuinely hybrid, not misclassified as one type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_expansion_mechanism,
    'Is work expansion driven by psychological factors (Parkinson''s original theory: desire to appear busy) or by structural incentives (budget growth tied to headcount, hierarchical justification)?',
    'Comparative analysis of organizations with different incentive structures: time-based vs output-based metrics, centralized vs decentralized, competitive vs monopolistic. If expansion persists in output-metric organizations, psychological factors dominate. If absent in competitive markets, structural incentives dominate.',
    'If psychological: constraint is deep human behavior pattern (mountain tendency). If structural: constraint is policy artifact (rope/snare/tangled_rope with clear intervention points).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_expansion_mechanism, empirical, 'Whether work expansion is driven by psychology or incentive structures').

omega_variable(
    productivity_measurement_problem,
    'Does Parkinson''s Law describe genuine productivity loss or measurement bias (appearance of inefficiency when true output is invisible)?',
    'Longitudinal productivity analysis controlling for output quality, innovation metrics, and knowledge work intangibles. Comparison of ''busy'' organizations with high research output vs demonstrably empty-work organizations.',
    'If measurement bias: Parkinson''s Law misclassifies productive complexity as waste (snare reclassifies as rope). If real productivity loss: extraction mechanism is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_measurement_problem, empirical, 'Whether apparent inefficiency reflects real productivity loss or measurement bias').

omega_variable(
    reform_sustainability,
    'Do lean management, zero-based budgeting, and agile methodologies create permanent exits from Parkinson''s Law or temporary suppressions that degrade under organizational pressure?',
    'Long-term tracking of reformed organizations: do they maintain reduced headcount and efficiency gains 10+ years post-reform, or do constraints re-expand as institutional memory fades?',
    'If permanent: scaffold sunset is real, constraint can be resolved. If temporary: constraint is more fundamental; scaffold is aspirational theater. Affects classification stability of reform perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_sustainability, empirical, 'Whether lean management reforms create durable exits from Parkinson''s Law').

omega_variable(
    market_discipline_exemption,
    'Why does Parkinson''s Law appear absent in highly competitive private firms and present in government and regulated monopolies?',
    'Cross-sectoral comparison: startup failure rates vs government agency persistence, private firm headcount volatility vs public sector headcount trends, competitive market dynamics vs regulatory protection. Test whether ''escape'' from constraint is possible or merely opportunity-determined.',
    'If absent in competitive markets: constraint is enabled by regulatory protection/budget autonomy (snare from worker perspective, rope from administrator). If present everywhere: constraint is deeper organizational tendency (mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_discipline_exemption, empirical, 'Why Parkinson''s Law is absent in competitive markets but present in protected organizations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parkinsons_law, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(park_tr_t0, parkinsons_law, theater_ratio, 0, 0.42).
narrative_ontology:measurement(park_tr_t15, parkinsons_law, theater_ratio, 15, 0.62).
narrative_ontology:measurement(park_tr_t30, parkinsons_law, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(park_be_t0, parkinsons_law, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(park_be_t15, parkinsons_law, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(park_be_t30, parkinsons_law, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parkinsons_law, resource_allocation).
narrative_ontology:affects_constraint(parkinsons_law, bureaucratic_expansion).
narrative_ontology:affects_constraint(parkinsons_law, administrative_overhead_accumulation).
narrative_ontology:affects_constraint(parkinsons_law, hierarchical_information_loss).

% DUAL FORMULATION NOTE:
% Parkinson's Law is upstream of several institutional constraints: it drives bureaucratic expansion (work creation), which drives administrative overhead accumulation (resource extraction), which drives hierarchical information loss (coordination failure). Each downstream constraint has its own ε and perspectives reflecting specific institutional mechanisms. Parkinson's Law is the driving mechanism; its psychological and structural components also affect decision-making constraints in organizations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
