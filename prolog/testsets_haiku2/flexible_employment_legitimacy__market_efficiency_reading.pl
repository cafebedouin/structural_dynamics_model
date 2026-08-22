% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market Efficiency Mechanism
 *   domain: labor_economics/platform_economy
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'flexible_employment_legitimacy'. The market efficiency reading frames
 *   flexible employment as a legitimate market-clearing mechanism: wage
 *   signals attract and repel workers; algorithms coordinate supply and
 *   demand neutrally; worker autonomy is maximized by hourly opt-in. The
 *   constraint is CLAIMED as rope (genuine coordination without asymmetric
 *   extraction). The measurement series shows extractiveness rising from 0.28
 *   to 0.38 over the interval, with theater ratio climbing from 0.08 to 0.22
 *   — signal that the pure-coordination story is increasingly supplemented by
 *   legitimation work (performative framing of commission as algorithmic
 *   transparency, framing of wage compression as scarcity pricing). The
 *   reading's ε referent is the standing flexible-employment arrangement as
 *   instantiated by platforms, measured by this reading's own epistemic
 *   lights: wage matching efficiency, algorithmic neutrality claims, and
 *   worker autonomy framing. A precarity reading would author higher ε for
 *   the same arrangement; a developmental reading would author different
 *   beneficiary/victim boundaries. This is the market reading's own
 *   measurement of its own arrangement.
 *
 * KEY AGENTS:
 *   - Platform operators: institutional power, arbitrage exit, set pricing and commission structures, claim algorithmic neutrality
 *   - Blue-collar workers in high-demand sectors: moderate power, mobile exit, experience wage upward pressure, benefit from flexibility
 *   - Workers in low-demand sectors: powerless, constrained exit, experience wage compression, beneficiary/payer hybrid
 *   - Consumers: organized power, arbitrage exit, benefit from scarcity pricing and on-demand availability
 *   - Labor standards advocates: excluded from algorithm design, would contest the market-efficiency framing
 *   - Empirical labor economists: observer seats, measure wage and employment effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.38).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.31).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market Efficiency Mechanism").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "labor_economics/platform_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, '5acf7a0e-68ae-4856-b931-defbc65ec4f6').
narrative_ontology:cs_kernel_codification('5acf7a0e-68ae-4856-b931-defbc65ec4f6', distributed).
narrative_ontology:cs_authority_grounding('5acf7a0e-68ae-4856-b931-defbc65ec4f6', diffuse_epistemic).
narrative_ontology:cs_reading_relation('5acf7a0e-68ae-4856-b931-defbc65ec4f6', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5acf7a0e-68ae-4856-b931-defbc65ec4f6', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('5acf7a0e-68ae-4856-b931-defbc65ec4f6', foundational, labor_markets_clear_via_price_signals).
narrative_ontology:cs_axiom_status(labor_markets_clear_via_price_signals, holdable).
narrative_ontology:cs_axiom_grounding('5acf7a0e-68ae-4856-b931-defbc65ec4f6', labor_markets_clear_via_price_signals, empirically_contingent).
narrative_ontology:cs_axiom('5acf7a0e-68ae-4856-b931-defbc65ec4f6', foundational, worker_autonomy_maximized_through_flexibility).
narrative_ontology:cs_axiom_status(worker_autonomy_maximized_through_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('5acf7a0e-68ae-4856-b931-defbc65ec4f6', worker_autonomy_maximized_through_flexibility, instrumental).
narrative_ontology:cs_axiom('5acf7a0e-68ae-4856-b931-defbc65ec4f6', secondary, algorithmic_matching_neutral_and_efficient).
narrative_ontology:cs_axiom_status(algorithmic_matching_neutral_and_efficient, holdable).
narrative_ontology:cs_axiom_grounding('5acf7a0e-68ae-4856-b931-defbc65ec4f6', algorithmic_matching_neutral_and_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('5acf7a0e-68ae-4856-b931-defbc65ec4f6', efficient_labor_market_clearing_via_flexibility).
narrative_ontology:cs_drift_state('5acf7a0e-68ae-4856-b931-defbc65ec4f6', contemporary_platform_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5acf7a0e-68ae-4856-b931-defbc65ec4f6', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, marginal_consumers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, high_skill_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, blue_collar_workers_high_demand).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, workers_low_demand_sectors).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumer_beneficiaries).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, high_skill_knowledge_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, workers_low_demand_sectors).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, labor_market_clearing_hypothesis).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, algorithmic_neutrality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the algorithmic matching system that treats labor as a fungible, hourly resource matched to demand via real-time pricing. Set commission structures, define worker classification, and control the visibility and rank-ordering of tasks. Justify the system as enabling worker autonomy and demand-responsive pricing.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate in high-demand sectors (delivery, rideshare, skilled trades) where real-time pricing creates wage pressure upward. They experience the constraint as enabling: flexible hour entry, skill-based pricing, and rapid income response to market conditions. Exit options exist if rates decline — they can shift to competing platforms or traditional employment.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, blue_collar_workers_high_demand, beneficiary,
    moderate, biographical, mobile, national).

% Operate in low-demand, high-supply sectors (crowdsourced microtasks, data labeling, administrative support) where real-time pricing creates persistent wage pressure downward. They experience the market-efficiency framing as both enabling (flexible access to income streams) and extractive (wages compressed by supply abundance they cannot exit without income loss). Traditional employment alternatives often unavailable or lower-income.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, workers_low_demand_sectors, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, workers_low_demand_sectors, beneficiary).

% Receive on-demand services at prices determined by algorithmic clearing: lower service cost when supply exceeds demand, higher prices during scarcity. The market-efficiency framing legitimates price volatility as fair (scarcity pricing) rather than exploitative. Consumers benefit from the availability and pricing flexibility the system enables.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumer_beneficiaries, beneficiary,
    organized, immediate, arbitrage, global).

% Are rarely classified as flexible employment; they maintain salaried status and benefits. The market-efficiency framing applied to flexible work validates their non-flexible status as a premium earned through scarcity in their skill domain. They implicitly benefit from the constraint by contrast — the legitimacy of market-based wage differentiation protects their higher compensation.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, high_skill_knowledge_workers, beneficiary,
    powerful, generational, arbitrage, global).

% Would argue that the market-efficiency framing obscures structural asymmetries: workers lack collective bargaining power, information about pricing algorithms, and mobility out of the platform ecosystem. They are excluded from the algorithmic design process and agenda-setting; their position (precarity_extraction_reading framing) is not represented in this constraint's authority structure.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_standards_advocates, excluded,
    organized, generational, constrained, national).

% Measure wage convergence, employment stability, cross-platform mobility, and welfare effects. They observe both directions: wage gains in high-demand sectors, wage compression in low-demand sectors. Their role is to test the core empirical claim of the market-efficiency reading — whether the system actually clears labor markets efficiently or instead produces predictable asymmetries.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, empirical_labor_economists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches labor supply to real-time demand via algorithmic pricing and task allocation: workers enter when compensation is high, exit when rates decline; platforms adjust task availability and visibility to signal scarcity; consumers see prices reflect actual supply-demand balance. The system solves the classical labor market information problem — workers no longer need to hunt for available work or negotiate individually; platforms provide instantaneous demand signals via pricing.
% TRANSFER_FUNCTION: Moves labor effort from workers to platforms and consumers. Platforms capture a commission (typically 20-30%) on transaction value and gain first-mover advantage in algorithmic design. Consumers receive services at algorithmically-optimized prices. Workers receive compensation calculated as (market_clearing_wage - platform_commission).
% ABSENT_VOICES: Workers in sectors with persistent structural oversupply (data labeling, microtasks, basic administrative work) would argue the market-efficiency framing masks wage suppression. Labor standards advocates would argue that 'flexibility' is a reframing of eliminated employment protection. They are excluded from the algorithmic design and are not seated in the market-efficiency reading's authority structure (they inhabit the precarity_extraction_reading).
% DISAPPEARANCE_RATIONALE: If the constraint disappeared — if flexible employment were prohibited and all work reverted to full-time traditional employment — labor markets would reorganize significantly: tight labor markets (current high-demand sectors) would face worker shortages, consumers would see service scarcity and fixed pricing, and many workers currently in low-demand sectors would be excluded from that work altogether. The system's liquidity would vanish; excess supply would reappear as unemployment rather than low wages.
% FOUNDING_PROBLEM: Traditional employment created structural mismatches: full-time workers were retained even during low-demand periods, creating unemployment; workers seeking supplemental income or irregular work schedules had no accessible labor market; geographic and skill mismatches created persistent unemployment. Flexible employment was built to solve the rigidity problem.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and mainstream labor economics (papers on labor market matching and price discovery) attest the problem is still live and the mechanism is effective. Independent researchers (Rosenblat, Rahman, Chen) and labor advocates attest the founding problem was real but the solution transfers the mismatch to workers (wage volatility, benefits withdrawal, algorithmic control) rather than eliminating it. No external corroboration exists for the claim that the market-efficiency framing is the true source of the system's legitimacy; the corroboration asymmetry is itself part of the kernel contest.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38 at interval end) because the market-efficiency reading treats wage convergence as efficient price discovery, not extraction. Suppression is low-moderate (0.31) because the system requires minimal coercive enforcement — workers' exit is theoretically free (though practically constrained by income need and platform-dependence). Theater is rising (0.08 → 0.22) because platforms increasingly frame algorithmic decision-making (ranking, visibility, commission structures) as neutral market mechanisms, while the actual allocation power is concentrated and non-transparent. The measurement series run on a shared time grid aligned at each time point. The rising theater ratio tracks the increasing performative maintenance of the 'neutral algorithm' story as structural scrutiny intensifies — the coordination function remains real, but the narrative scaffolding around it becomes thicker.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (platform operators) should compute a much lower type (pure rope, minimal extraction) because they define efficiency as the algorithm's own outcome and control the definition. High-demand worker seats should compute rope or near-rope because they experience wage mobility and exit options. Low-demand worker seats should compute tangled_rope or snare because they experience wage compression and constrained exit despite the market-efficiency framing. The excluded seats (labor advocates) would compute snare if seated. The engine divergence is the measurement the corpus takes: a claimed rope with computed snare variance across seats signals that the market-efficiency reading's framing distributes legitimacy unevenly, concentrating it at the top.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are structural beneficiaries: they control the algorithm, capture commissions, and set the frame (d near 0.0). High-demand sector workers are near-symmetric: they benefit from wage signals and exit mobility, but are locked into platform ecosystem for market access (d near 0.5). Low-demand sector workers bear the cost of supply abundance without equivalent exit options; they are partly targets (d ~ 0.65-0.75). Consumers are beneficiaries of lower prices when supply exceeds demand (d ~ 0.2). High-skill workers are implicitly beneficiaries by contrast: market-efficiency framing legitimates wage differentiation that protects their premium compensation (d ~ 0.1). The divergence in per-seat directionality is key: the agenda-setter (platform) and the high-demand beneficiaries experience the constraint as genuine coordination; the low-demand workers experience partial extraction; labor advocates (excluded) would experience it as pure extraction if seated. This divergence should compute clearly from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not have a mandatrophy condition yet (founding problem still live, constraint still serves it). However, if empirical evidence accumulated showing that the algorithm does not in fact clear labor markets efficiently — if wage convergence tracks platform power rather than labor scarcity, if worker autonomy is constrained rather than enabled by algorithmic control, if platform margins exceed marginal service costs — then the founding problem would become 'contested' and a mandatrophy omega would fire. The constraint's legitimacy rides entirely on the descriptive claim that markets clear efficiently; if that claim fails, the constraint's rationale evaporates while the structure persists, producing a piton or dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_neutrality_assumption,
    'Do the algorithmic matching and pricing systems operate neutrally with respect to worker welfare, or do they embed structural biases that systematically advantage certain worker classes?',
    'Transparency audits of ranking algorithms and compensation formulas; comparison of wage trends across demographic groups and skill levels controlling for supply-demand elasticity; measurement of algorithm-driven task allocation patterns over time.',
    'If algorithms are neutral, the market-efficiency reading stands: observed wage convergence reflects true market clearing. If algorithms embed biases, extractiveness reclassifies upward and theater_ratio increases — the neutral framing becomes performative cover for algorithmic control. The constraint might shift from rope to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_neutrality_assumption, empirical, 'Whether algorithmic systems operate with structural neutrality or embed systematic biases.').

omega_variable(
    wage_convergence_as_scarcity_vs_power,
    'When wage convergence occurs in high-demand sectors, is it a market signal of labor scarcity, or a side effect of platform''s dynamic pricing capturing worker surplus during demand spikes?',
    'Decompose wage volatility into (a) baseline wage growth reflecting scarcity and (b) surge pricing capturing temporary demand peaks. Compare to labor-scarcity benchmarks (job openings, worker unemployment duration, outside-option quality). If surge pricing dominates baseline scarcity signal, the framing fails.',
    'If true scarcity signals, the market-efficiency reading holds and extractiveness stays moderate. If surge pricing dominates, the wage gains are transient and concentrically captured by platforms; extractiveness rises and the reading shifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_convergence_as_scarcity_vs_power, empirical, 'Whether wage convergence reflects labor-market scarcity or algorithmic rent extraction.').

omega_variable(
    exit_options_asymmetry,
    'Do workers classified as ''flexible'' retain genuine exit options (exit to competing platforms, traditional employment, or non-participation) or are exits increasingly constrained by platform ecosystem lock-in, algorithmic reputation systems, and income dependency?',
    'Longitudinal tracking of worker mobility: what fraction transitions to competing platforms, traditional employment, or exit the labor market entirely? What costs do they bear (reputation penalties, credential loss, income gaps)? How do exit costs vary by worker skill level?',
    'If exits remain mobile (high inter-platform mobility, low cost to traditional employment), the beneficiary/victim boundary stays as authored. If exits become constrained (low mobility, high cost), low-demand workers shift from ''constrained exit'' toward ''trapped'' or ''identity_locked'', and the constraint reclassifies from rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_options_asymmetry, empirical, 'Whether worker exit options remain genuinely mobile or are increasingly constrained by ecosystem lock-in.').

omega_variable(
    market_efficiency_reading_vs_precarity_reading_axiom_contest,
    'Does the core axiom of this reading — that flexible employment equilibrates labor markets through price signals and worker autonomy — remain holdable in light of persistent sectoral wage divergence, or has empirical accumulation overridden it?',
    'Systematic measurement of whether wage convergence produces stable equilibrium or cyclical volatility; whether worker autonomy measurably increases with flexibility or decreases with dependence; whether labor market matching efficiency improves or worsens relative to pre-platform baselines.',
    'If the axiom remains holdable, the reading persists as a live position. If empirical challenges mount (wage volatility increases, autonomy measures decline, matching efficiency stalls), the axiom drifts toward overridden status; the reading loses epistemic ground and the precarity_extraction_reading gains influence within organizational and policy spheres.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_efficiency_reading_vs_precarity_reading_axiom_contest, empirical, 'Whether the core market-efficiency axiom remains holdable given accumulating empirical evidence.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.31) primarily structural (external barriers: algorithmic exclusion, platform control of task visibility) or internalized (workers accept low wages as just market outcome, believe exit is unavailable even where technically possible)?',
    'Post-exclusion or exit surveys: do workers who leave platforms report changed beliefs about wage adequacy and labor-market fairness? Do longitudinal studies show workers internalizing platform norms (viewing compensation as ''what the market says'', algorithmic control as legitimate)?',
    'If suppression is structural, removing platform barriers could enable higher wages. If suppression is internalized, the constraint carries its enforcement with it beyond the platform boundary — effective suppression is higher than measured, and reclassification pressure increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression in the flexible-employment system is primarily structural or internalized.').

omega_variable(
    authority_grounding_empirical_vs_normative,
    'Is the market-efficiency reading''s authority grounded in empirical claims (that markets do clear efficiently) or in normative claims (that market clearing is good, regardless of efficiency outcomes)?',
    'Genealogical analysis: trace policy adoption and platform legitimation narratives to identify whether authority rests on demonstrated labor-market efficiency or on prior commitment to market-based allocation. Identify cases where efficiency claims were contested but market framing persisted.',
    'If grounding is empirical, falsification of efficiency would undermine authority and reclassify the reading. If grounding is normative (markets are good by principle), the reading remains stable even if efficiency claims fail — but becomes a normative assertion rather than an empirical one, shifting the reading''s epistemology and authority structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_empirical_vs_normative, conceptual, 'Whether the market-efficiency reading grounds its legitimacy in empirical efficiency or prior normative commitment to market allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(flex_tr_t0, observed).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(flex_tr_t5, observed).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(flex_tr_t10, observed).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(flex_tr_t15, observed).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(flex_tr_t20, projected).
narrative_ontology:measurement(flex_tr_t25, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(flex_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(flex_be_t0, observed).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(flex_be_t5, observed).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(flex_be_t10, observed).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(flex_be_t15, observed).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(flex_be_t20, projected).
narrative_ontology:measurement(flex_be_t25, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(flex_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(flex_su_t0, observed).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement_basis(flex_su_t5, observed).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement_basis(flex_su_t10, observed).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 15, 0.29).
narrative_ontology:measurement_basis(flex_su_t15, observed).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(flex_su_t20, projected).
narrative_ontology:measurement(flex_su_t25, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 25, 0.31).
narrative_ontology:measurement_basis(flex_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__market_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel flexible_employment_legitimacy. The market_efficiency_reading instantiates ε=0.38, beneficiaries=[platform_operators, high_demand_workers, consumers], victims=[]. The precarity_extraction_reading instantiates the same standing arrangement with ε=0.72, beneficiaries=[platform_operators], victims=[all_workers, consumers]. The developmental_state_reading instantiates it with ε=0.55, beneficiaries=[platforms, interim_state_capacity], victims=[formal_employment_workers], sunset_clause=true. All three readings reference identical institutional arrangements; they differ in ε-referent framing (efficiency vs. extraction vs. transition), beneficiary/victim boundaries, and claimed type. Sibling readings linked via network.affects_constraints. The decomposition is mandated by ε-invariance: a single constraint cannot authentically carry three structurally incoherent ε values. Each reading's ε is stable and unique; the contested kernel produces three constraints, three stories, three classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__market_efficiency_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
