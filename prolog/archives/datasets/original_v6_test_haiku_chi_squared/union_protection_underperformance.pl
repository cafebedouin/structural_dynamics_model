% ============================================================================
% CONSTRAINT STORY: union_protection_underperformance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_union_protection_underperformance, []).

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
 *   constraint_id: union_protection_underperformance
 *   human_readable: "Just Cause" Protection for Underperforming Union Employees
 *   domain: economic/labor_relations
 *
 * SUMMARY:
 *   Just-cause protection in union contracts creates a structural tension
 *   between two legitimate objectives: protecting workers from arbitrary
 *   dismissal and maintaining firm productivity and fair distribution of
 *   work. The constraint manifests differently depending on the observer's
 *   structural position within the labor system. Union leadership and worker
 *   advocates see a pure coordination mechanism solving a collective action
 *   problem. Employers see extraction — the inability to remove unproductive
 *   workers without exhaustive documentation and grievance costs.
 *   High-performing union peers see extraction — they absorb workload to
 *   compensate for protected underperformance. The underperforming employee
 *   themselves experience a hybrid: protection against dismissal (genuine
 *   benefit) plus reduced career consequences (perverse incentive). The
 *   labor-relations system sees its own process as substantially
 *   performative: performance improvement plans, grievance procedures, and
 *   arbitration consume significant labor-relations resources with limited
 *   success at actually improving performance. The constraint's
 *   extractiveness has increased over 40 years (0.32 → 0.52) as performance
 *   measurement has become more sophisticated, creating more opportunities
 *   for just-cause disputes and more elaborate remediation theater. Theater
 *   ratio has likewise increased (0.48 → 0.65) as formal grievance procedures
 *   have grown more complex relative to actual productivity outcomes.
 *
 * KEY AGENTS:
 *   - Union Leadership & Worker Advocacy: Primary beneficiary (organized/mobile) — negotiates just-cause terms, reduces arbitrary dismissal, maintains membership value
 *   - Unionized Workers (Average): Mixed beneficiary/victim (moderate/constrained) — protected from arbitrary dismissal but also constrained by peer compensation for underperformance
 *   - High-Performing Union Peers: Secondary victim (moderate/trapped) — forced to absorb workload from protected underperformers, receive no extra compensation
 *   - Underperforming Protected Employee: Hybrid beneficiary/victim (powerless to moderate/constrained) — protected from dismissal but also insulated from performance consequences, creating perverse incentive
 *   - Employers (Firm Management): Primary victim (powerful/arbitrage) — bears direct productivity loss, documentation burden, grievance costs; can arbitrage but at substantial cost
 *   - Industry Sector (e.g., Auto, Steel): Institutional victim (institutional/constrained) — collective workforce inflexibility reduces competitive positioning, though labor stability provides offsetting benefit
 *   - Labor Relations System: Institutional theater (institutional/constrained) — maintains performative grievance procedures that consume resources but rarely achieve productivity improvement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(union_protection_underperformance, 0.52).
domain_priors:suppression_score(union_protection_underperformance, 0.68).
domain_priors:theater_ratio(union_protection_underperformance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(union_protection_underperformance, extractiveness, 0.52).
narrative_ontology:constraint_metric(union_protection_underperformance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(union_protection_underperformance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(union_protection_underperformance, tangled_rope).
narrative_ontology:human_readable(union_protection_underperformance, "\"Just Cause\" Protection for Underperforming Union Employees").
narrative_ontology:topic_domain(union_protection_underperformance, "economic/labor_relations").

domain_priors:requires_active_enforcement(union_protection_underperformance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(union_protection_underperformance, unionized_workers).
narrative_ontology:constraint_beneficiary(union_protection_underperformance, union_leadership).
narrative_ontology:constraint_beneficiary(union_protection_underperformance, worker_advocacy_organizations).
narrative_ontology:constraint_victim(union_protection_underperformance, employer_productivity).
narrative_ontology:constraint_victim(union_protection_underperformance, high_performing_peers).
narrative_ontology:constraint_victim(union_protection_underperformance, consumer_service_quality).
narrative_ontology:constraint_victim(union_protection_underperformance, firm_competitive_position).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNION LEADERSHIP & COALITION (ROPE) — Organized beneficiary with mobile exit options (can negotiate different terms, shift to different industries). Experiences just-cause as pure coordination mechanism: solves collective action problem of protecting median worker from arbitrary dismissal. Union successfully advocates for worker interests through collective bargaining. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Negative effective extraction; net beneficiary.
constraint_indexing:constraint_classification(union_protection_underperformance, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDERPERFORMING EMPLOYEE TRAPPED VIEW (SNARE) — When an employee faces repeated performance issues but cannot be terminated, the constraint appears as pure extraction to coworkers who must compensate. From the trapped peer's view (same union, same workplace, high performer bearing extra load), just-cause protection extracts from them. d≈0.88, f(d)≈1.28, σ=0.8 → χ≈0.54. High extraction from the productive median.
constraint_indexing:constraint_classification(union_protection_underperformance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: UNDERPERFORMING EMPLOYEE MIXED VIEW (TANGLED ROPE) — The same employee protected by just-cause sees coordination AND extraction. Coordination: protection against arbitrary dismissal is genuinely valuable (solves fear problem). Extraction: protection also means minimal career consequence for chronic underperformance, creating perverse incentive. d≈0.60, f(d)≈0.80, σ=0.9 → χ≈0.37. Mixed experience reflects dual function.
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EMPLOYER (SNARE) — Powerful actor with arbitrage exit options (can restructure, relocate, contract out, automate). Experiences just-cause as pure extraction: unable to remove unproductive workers at will, forced to incur remediation, documentation, grievance costs. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72. High effective extraction. The employer bears direct productivity costs and cannot exit without substantial reorganization.
constraint_indexing:constraint_classification(union_protection_underperformance, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INDUSTRY SECTOR INSTITUTIONAL VIEW (TANGLED ROPE) — Unionized sector (auto, steel, public service) benefits from labor stability (reduces costly turnover, maintains institutional knowledge) but bears collective extraction through reduced flexibility to optimize workforce composition. d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.44. Mixed coordination (stability) and extraction (inflexibility).
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR RELATIONS INSTITUTIONAL THEATER (PITON) — The formal just-cause process is substantially performative: employers document performance issues, schedule remediation meetings, file grievances, wait for arbitration — all rituals that often fail to change outcomes. Many protected workers remain underperforming after arbitration. theater_ratio=0.65 reflects that grievance procedures consume substantial labor-relations resources with limited productivity improvement. Constraint persists through institutional inertia and legal structure, not functional necessity.
constraint_indexing:constraint_classification(union_protection_underperformance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global, long-term view, just-cause protection solves a genuine coordination problem (arbitrary dismissal creates worker precarity and efficiency loss) but introduces new extraction (protected underperformance reduces firm competitiveness and harms productive workers). The constraint is structurally hybrid: both protective and extractive. d≈0.52, f(d)≈0.75, σ=1.2 → χ≈0.47. Solidly tangled rope.
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(union_protection_underperformance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(union_protection_underperformance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(union_protection_underperformance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(union_protection_underperformance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(union_protection_underperformance, TR),
    TR >= 0.70.

:- end_tests(union_protection_underperformance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Just-cause protection extracts from employers and high-performing peers. Employers face real costs: documentation burden, grievance procedures, potential arbitration, inability to quickly replace unproductive workers. High performers absorb workload. However, the extraction is not as severe as pure dismissal prohibition would be (which would approach 0.75+) because employers can still terminate through arbitration if documentation is sufficient — the process is costly and uncertain, but not impossible. The recent increase from 0.32 to 0.52 reflects growing sophistication of grievance procedures and arbitration, making the extraction mechanism more visible and more costly in labor-relations resources. Suppression (0.68): High. Workers have significant barriers to opting out: collective bargaining agreement applies to entire bargaining unit, individual workers cannot negotiate away just-cause terms, switching to non-union employment typically requires changing employers or sectors. For employers, suppression is also high: they cannot easily terminate workers unilaterally, cannot contract around collective bargaining terms without union agreement, cannot reduce grievance rights without negotiation. Theater ratio (0.65): Moderate-high. The formal just-cause process includes performance evaluation, documentation, remediation meetings, performance improvement plans, formal grievance filing, union representation, potential arbitration — all rituals that are often decoupled from actual performance improvement. Many employees go through the full process and remain underperforming. The 0.65 value reflects that the theater has grown as procedures have become more formal while actual productivity gains from the procedures remain modest.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence despite a single structural mechanism. Union leadership and worker advocates classify it as Rope: pure coordination solving collective action problem. Employers classify it as Snare: pure extraction with no offsetting coordination benefit. High-performing peers classify it as Snare: they bear all extraction, no coordination benefit. The underperforming employee experiences Tangled Rope: coordination (protection from arbitrary dismissal) bundled with extraction (reduced career consequences). The analytical observer sees Tangled Rope: both coordination function (necessary to prevent arbitrary dismissal) and extraction function (protects underperformance) are real and inseparable. The labor relations system sees Piton: the formal process is substantially performative, maintained through institutional inertia and legal structure rather than functional productivity. The perspectival gap reflects fundamentally different structural positions: beneficiaries see coordination; victims see extraction; observers see both.
 *
 * DIRECTIONALITY LOGIC:
 *   Union leadership: Beneficiary + organized/mobile → d≈0.10, f(d)≈-0.05. Net beneficiary. Successfully advocates for worker interests. Underperforming protected worker: Victim + constrained (from job loss) but beneficiary (from dismissal protection) = mixed d≈0.55, f(d)≈0.75. Mixed extraction/benefit. High-performing peer: Victim (forced to compensate) + trapped (cannot exit union without changing job) → d≈0.88, f(d)≈1.28. High extraction from this agent. Employer: Victim (productivity loss, grievance costs) + arbitrage (can restructure/automate but at high cost) → d≈0.85, f(d)≈1.15. High extraction. Industry sector: Victim/beneficiary mix (benefits from labor stability, harmed by inflexibility) → d≈0.60, f(d)≈0.80. Mixed experience. Labor relations system: Institutional theater maintaining process → d≈0.50, f(d)≈0.65. The constraint benefits unions (who created it) and harms efficiency, neutral institutional position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that classification depends entirely on structural position. Union advocates experience genuine coordination: solving collective dismissal problem, establishing fairness norms, enabling worker dignity. This is legitimate Rope functionality. Simultaneously, employers and high performers experience genuine extraction: real productivity loss, real work burden. This is legitimate Snare reality. The constraint is NOT misclassified as tangled rope due to confusion between coordination and extraction at the same level — both are real structural features at different levels of the system. The extraction from employers enables coordination for workers. The constraint is tangled because it solves a coordination problem (worker security) through a mechanism that extracts from other agents (employer flexibility, peer workload). Rejecting mandatrophy would require either denying that just-cause solves a real coordination problem (false — worker arbitrary dismissal is real), or denying that it extracts real costs (also false — productivity loss and peer burden are real). The tangled rope classification stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_measurement_validity,
    'Are employer performance metrics valid and reliably executed, or are they systematically biased, politicized, or used as cover for discrimination?',
    'Comparative analysis of performance evaluation reliability across unionized vs non-unionized workforces; audit of evaluation calibration and consistency within firms; correlation between performance ratings and actual productivity measures',
    'If metrics valid: just-cause protection efficiently targets truly arbitrary dismissals (rope). If metrics unreliable: just-cause is necessary protection against discrimination or arbitrary targeting (snare for workers). Classification shifts from tangled_rope toward rope from worker perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_measurement_validity, empirical, 'Validity of employer performance measurement systems').

omega_variable(
    remediation_effectiveness,
    'Do formal remediation and performance improvement plans (PIPs) actually improve performance, or do they function as pre-termination theater masking predetermined outcomes?',
    'Longitudinal tracking of employees on PIPs: percentage actually improved vs terminated vs left voluntarily; comparison of performance gains (if any) across unionized vs non-unionized workforces with similar performance improvement protocols',
    'If remediation effective: just-cause serves coordination function (pipe → rope). If theater: just-cause protects bad employees without changing behavior (pure extraction for peers, snare for employers). Determines whether constraint is functional hybrid or performative extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_effectiveness, empirical, 'Whether performance improvement plans achieve actual remediation').

omega_variable(
    peer_burden_distribution,
    'How much additional work burden do high-performing union peers absorb due to underperforming protected coworkers? Is this burden proportional, concentrated, or systemically unequal?',
    'Time-use and workload analysis within unionized teams; surveys of peer burden perception; correlation between peer workload and team underperformance rate; comparison of burn-out rates for high performers in union vs non-union settings with similar underperformance rates',
    'If burden minimal/distributed: extraction from peers is low, classification shifts toward rope. If burden concentrated: some peers effectively subsidize underperformers (high snare signal). Determines whether just-cause is symmetric coordination or asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_burden_distribution, empirical, 'Work burden absorbed by high performers due to protected underperformance').

omega_variable(
    arbitration_bias_direction,
    'Do union arbitrators systematically favor worker retention (union bias) or do they split the difference, resulting in retention but with meaningful remediation (functional outcome)?',
    'Analysis of arbitration outcomes: percentage of cases resulting in reinstatement, demotion, or reduced hours; comparison of arbitrator decision patterns over time; surveys of employer and union satisfaction with arbitration outcomes',
    'If arbitrators consistently reinstate without consequence: extraction mechanism is unbiased but labor-intensive (piton theater). If arbitrators impose meaningful terms: constraint is more functional than it appears (rope with enforcement). Affects classification from employer perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitration_bias_direction, empirical, 'Direction and magnitude of arbitrator bias in just-cause disputes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(union_protection_underperformance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ujp_tr_t0, union_protection_underperformance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ujp_tr_t20, union_protection_underperformance, theater_ratio, 20, 0.58).
narrative_ontology:measurement(ujp_tr_t40, union_protection_underperformance, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(ujp_be_t0, union_protection_underperformance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ujp_be_t20, union_protection_underperformance, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(ujp_be_t40, union_protection_underperformance, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(union_protection_underperformance, enforcement_mechanism).
narrative_ontology:affects_constraint(union_protection_underperformance, union_wage_compression).
narrative_ontology:affects_constraint(union_protection_underperformance, seniority_based_advancement).
narrative_ontology:affects_constraint(union_protection_underperformance, employer_automation_incentive).

% DUAL FORMULATION NOTE:
% Just-cause protection is downstream of broader union bargaining power and upstream of specific labor market outcomes (wage compression, seniority advancement, automation). The wage compression constraint (ε≈0.15, Rope) shows the enabling coordination side; this story (ε≈0.52, Tangled Rope) shows the extraction side of the same institutional structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(union_protection_underperformance, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
