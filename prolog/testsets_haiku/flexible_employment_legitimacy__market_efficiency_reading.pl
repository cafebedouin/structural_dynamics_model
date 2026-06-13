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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market-Clearing Mechanism (Efficiency Reading)
 *   domain: economic/labor/technological
 *
 * SUMMARY:
 *   This constraint instantiates the market-efficiency reading of flexible
 *   employment. The reading frames platform-mediated labor matching as a
 *   legitimate, welfare-enhancing coordination mechanism where wages adjust
 *   to clear supply and demand, workers retain autonomy to exit and
 *   reallocate, and algorithms serve as neutral technical infrastructure for
 *   price discovery. This is CLAIMED as rope — genuine market clearing with
 *   real coordination function. The authored metrics (moderate extractiveness
 *   0.42, low suppression 0.28) reflect the reading's assertion that the
 *   constraint operates primarily through price incentives rather than
 *   coercion, and that the commission reflects genuine transaction costs
 *   rather than monopoly rents. The rising theater_ratio over the interval
 *   suggests that justificatory narratives become more prominent over time
 *   relative to functional necessity — a signal the engine will evaluate.
 *   This reading coexists with the precarity_extraction_reading (which
 *   emphasizes worker vulnerability, algorithmic wage suppression, and
 *   asymmetric information) and the developmental_state_reading (which frames
 *   flexibility as transitional, requiring state-directed formalization). The
 *   three readings represent genuinely different structural accounts of the
 *   same kernel (flexible employment as a social form) — they are not merely
 *   different opinions about the same fact, but different constitutive claims
 *   about what the arrangement IS.
 *
 * KEY AGENTS:
 *   - platform_operators: institutional power, control the algorithmic matching rules and commission structure
 *   - flexible_workers: moderate power, gain access to work but experience wage volatility
 *   - consumers_of_flexible_services: organized power, benefit from price efficiency signals
 *   - workers_with_formal_alternatives: excluded voice, would contest the efficiency narrative
 *   - labor_market_economists: analytical observers, generate evidence that validates or contests the reading
 *   - regulatory_authorities: institutional power, can impose constraints that reshape the constraint's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.42).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.28).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "economic/labor/technological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, 'a9345ca4-0789-4395-9b94-0d187d78b7fd').
narrative_ontology:cs_kernel_codification('a9345ca4-0789-4395-9b94-0d187d78b7fd', distributed).
narrative_ontology:cs_authority_grounding('a9345ca4-0789-4395-9b94-0d187d78b7fd', distributed).
narrative_ontology:cs_reading_relation('a9345ca4-0789-4395-9b94-0d187d78b7fd', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9345ca4-0789-4395-9b94-0d187d78b7fd', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('a9345ca4-0789-4395-9b94-0d187d78b7fd', foundational, wage_flexibility_enables_scarcity_signaling).
narrative_ontology:cs_axiom_status(wage_flexibility_enables_scarcity_signaling, holdable).
narrative_ontology:cs_axiom_grounding('a9345ca4-0789-4395-9b94-0d187d78b7fd', wage_flexibility_enables_scarcity_signaling, empirically_contingent).
narrative_ontology:cs_axiom('a9345ca4-0789-4395-9b94-0d187d78b7fd', foundational, worker_exit_optionality_constitutes_autonomy).
narrative_ontology:cs_axiom_status(worker_exit_optionality_constitutes_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a9345ca4-0789-4395-9b94-0d187d78b7fd', worker_exit_optionality_constitutes_autonomy, instrumental).
narrative_ontology:cs_reference_frame('a9345ca4-0789-4395-9b94-0d187d78b7fd', labor_market_efficiency_framework).
narrative_ontology:cs_drift_state('a9345ca4-0789-4395-9b94-0d187d78b7fd', contemporary_algorithmic_regulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9345ca4-0789-4395-9b94-0d187d78b7fd', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_flexible_services).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, market_clearing_function).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, labor_market_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, price_discovery_through_flexibility).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, algorithmic_coordination_neutrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the algorithmic matching system that pairs workers with on-demand tasks. Set commission rates, algorithmic ranking, and task acceptance thresholds. Frame the system as enabling genuine market clearing where worker supply and demand converge through price signals (wage rates). Argue that flexibility maximizes worker autonomy and allows rapid response to labor market shifts.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Access work opportunities without long-term commitment, choose tasks matching their schedule, and exit costlessly. Under this reading, they benefit from wage rates that adjust quickly to labor scarcity signals — when demand for their labor is high, wages rise; when low, they can reallocate to other tasks or exit. They pay implicit costs in the form of volatility and task-switching friction, but the reading treats these as acceptable trade-offs for flexibility.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, flexible_workers, payer).

% Gain access to on-demand labor at prices that reflect real scarcity — the platform's matching efficiency means they pay market-clearing rates rather than monopoly premiums. Under this reading, price signals reward scarce skills and penalize abundant ones, allocating labor efficiently across the economy.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_flexible_services, beneficiary,
    organized, immediate, mobile, regional).

% Have credential-based or union-protected employment that insulates them from the flexible labor market. They would contest the efficiency framing by noting that platform flexibility only clears the market for the uncredentialed or under-employed, and masks structural unemployment in formal sectors. Their voice is structurally absent from the platform's operations.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, workers_with_formal_alternatives, excluded,
    organized, biographical, arbitrage, national).

% Analyze whether wage volatility on platforms reflects genuine scarcity signals or algorithmic manipulation; whether flexibility increases or decreases aggregate worker welfare; and whether the market-clearing framing captures the actual outcome structure. They produce empirical evidence that either validates or contests the efficiency narrative.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_market_economists, observer,
    analytical, biographical, analytical, global).

% Investigate whether algorithmic wage setting and task allocation constitute genuine market clearing or coordinated price suppression. They can impose constraints on commission rates, algorithmic transparency, or minimum earnings floors that would alter the constraint's operation and the reading's validity.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches labor supply to demand in real time via algorithmic task allocation and wage signals. Workers can rapidly adjust effort and hours to changing opportunity costs; consumers access labor at prices reflecting current scarcity. The reading treats this as solving a genuine market-clearing problem: without platforms, information asymmetry and search frictions prevent workers and task-givers from finding each other efficiently.
% TRANSFER_FUNCTION: Transfers a commission (typically 15-30% per transaction) from workers and consumers to the platform operator, in exchange for the matching service, reputation systems, and payment processing.
% ABSENT_VOICES: Formal-sector workers and union-protected labor are structurally absent — they would contest whether platform flexibility is genuine market clearing or a secondary labor market absorbing workers excluded from formal employment. Labor standards advocates would object that 'market clearing' treats floor wages and benefits as distortions rather than legitimate coordination costs. Their objections are not admitted into the platform's design discourse.
% DISAPPEARANCE_RATIONALE: If this constraint and its algorithmic enforcement vanished, the matching function would revert to slower, higher-friction processes (job boards, personal networks, union dispatch systems). Task-givers would face search costs and workers would lose instant access to marginal opportunities. The economy would reorganize around formal employment, union labor, or geographic-local matching — labor allocation would be less fluid and wages would diverge further across regions.
% FOUNDING_PROBLEM: Labor markets have search frictions and information asymmetry: workers don't know where work is available, task-givers don't know where workers are. Fixing this requires a coordination platform that can scale beyond geographic locality and match supply to demand in real time.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and market economists attest the search-friction problem persists — cities still have unemployment and unfilled tasks simultaneously. Independent labor economists (including some skeptics of the efficiency reading) acknowledge the matching function is real and valuable. Formal labor advocates dispute whether platforms solve it equitably, but do not contest that matching friction exists.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.42) under this reading because the commission is treated as a legitimate coordination cost (matching service, reputation infrastructure, payment clearing) rather than monopoly rent. Suppression is low (0.28) because the reading emphasizes worker autonomy — workers can exit at any time, choose tasks matching their schedule, and access competing platforms. Theater_ratio rises from 0.20 to 0.31 over the interval, suggesting that algorithmic 'neutrality' and 'market clearing' narratives increasingly substitute for evidence of actual matching efficiency — a pattern consistent with Goodhart drift where the justification becomes more rhetorically prominent as the underlying coordination function becomes harder to verify. The measurement grid is shared across all three metrics, authored at six time points spanning the interval. Suppression_requirement is low throughout because the reading does not posit coercive enforcement — market prices alone sustain the arrangement under this framing. If the precarity reading were instantiated (separate file), its suppression would be substantially higher because that reading emphasizes algorithmic wage capping, task rejection penalties, and the psychological pressure to maintain high acceptance rates — different mechanisms, different ε.
 *
 * PERSPECTIVAL GAP:
 *   The platform-operator seat and the flexible-worker seat should compute differently from the regulatory-observer seat. From the operator's position, the arrangement is genuine coordination it built and maintains — market clearing via algorithm and price signals. From a flexible worker's position constrained by need, the same structure may feel less like an offer and more like an ultimatum with wage variability. From the regulatory-observer position, the constraint is contestable: wage patterns may reflect genuine scarcity signals OR algorithmic optimization for platform profit rather than market efficiency. The engine computes these divergences from the structural data. The authored claim (rope) and the authored metrics (moderate extraction, low suppression) are independent; the divergence is what the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are the structural beneficiary (capture the commission, control the rules, set the frame — d near 0.0–0.1). Flexible workers sit near symmetric (d~0.5) under this reading: they gain genuine coordination benefit (task access, schedule autonomy) and pay real costs (volatility, overhead of switching tasks). Under the precarity reading, the same workers would sit closer to target (d~0.7–0.8) because algorithmic wage suppression and task-rejection penalties would be emphasized. Consumers of flexible services are beneficiaries (d~0.1–0.2). Workers with formal alternatives are excluded rather than coordinated — they are not even in the platform's market, so their d is not computed in this constraint's frame. The engine derives d from the beneficiary/victim declarations and the exit_options field; this reading declares no victims (only beneficiaries and neutral participants), which is structurally distinct from the precarity reading, which would declare workers as victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not declare mandatrophy because the founding problem (search friction in labor markets) remains live and the arrangement's function (matching supply to demand) is still operative. However, the rising theater_ratio (justificatory narrative becoming more salient) combined with the low underlying suppression could signal that rhetorical work is substituting for functional verification — a precursor to mandatrophy if wage-clearing evidence erodes. An omega variable captures this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_signal_validity,
    'Do wage variations on platforms reflect genuine labor scarcity signals (market clearing) or algorithmic optimization for platform profit (margin maximization)?',
    'Empirical analysis of wage-task-scarcity correlation: regress wage rates against task queue length, worker availability, and consumer demand. If correlation is strong and independent of platform profitability, the signal-validity hypothesis holds; if correlation with platform profit margins is stronger, the algorithmic optimization hypothesis holds.',
    'If signals are valid (market clearing), the efficiency reading is strengthened and extraction is genuinely coordination cost. If signals are algorithmic artifacts, the efficiency reading fails and extraction is reclassified as monopoly rent — a shift toward the precarity reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_signal_validity, empirical, 'Whether wage rates function as genuine scarcity signals or as algorithmic artifacts').

omega_variable(
    worker_autonomy_vs_necessity,
    'When workers report high task-acceptance rates and long working hours, is this voluntary response to high wages (autonomy) or constrained response to income necessity?',
    'Post-exit surveys and longitudinal tracking: interview workers who have left platforms and ask whether they felt they chose their hours or felt forced by need. Compare earnings volatility to cost-of-living data to establish whether flexibility accommodates genuine preference or masks income insecurity.',
    'If autonomy is high (workers actively choose hours as preferences shift), the efficiency reading holds and suppression is low. If necessity is dominant (workers work long hours despite preference for stability due to income pressure), the suppression metric should be higher and the constraint should shift toward snare or tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_autonomy_vs_necessity, empirical, 'Whether worker flexibility reflects genuine autonomy or constrained necessity').

omega_variable(
    algorithmic_neutrality_vs_strategic_design,
    'Are platform algorithms designed to maximize match quality (true market clearing) or to maximize worker effort extraction and consumer spending (strategic design for profit)?',
    'Regulatory discovery of algorithmic source code and design objectives. Comparison of actual match efficiency (wait times, task-worker mismatch rates) against theoretical optimums. Analysis of whether notification timing, task presentation order, and acceptance-penalty structures favor worker supply or platform revenue.',
    'If algorithms are neutral (designed for match quality), the reading stands. If algorithms are strategically designed for extraction, the efficiency framing is false — the algorithms serve extraction, not coordination. This would move the constraint toward the precarity reading''s framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_neutrality_vs_strategic_design, empirical, 'Whether platform algorithms serve neutral matching or strategic profit extraction').

omega_variable(
    reading_identity_fusion,
    'Is this reading (market efficiency) grounded in genuine structural analysis of how flexible labor markets operate, or is it an identity-fusion mechanism where market ideology becomes inseparable from individual advantage?',
    'Examine whether platform operators and efficiency advocates update their reading when empirical evidence shows wage suppression or algorithmic strategic design. If they defend the reading despite contradictory evidence, the reading becomes identity-fused and functions as justification rather than analysis.',
    'If identity-fused, this reading functions as a snare''s justification layer (false summit). The constraint would be reclassified as extraction dressed in coordination language. If not identity-fused, the reading remains an open empirical claim subject to evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_fusion, conceptual, 'Whether the efficiency reading is empirically grounded or identity-fused justification').

omega_variable(
    sibling_reading_foreclosure,
    'Does this reading''s core axiom (flexibility enables market clearing through wage signals) logically rule out the precarity_reading''s core axiom (flexibility enables extraction through information asymmetry), or do they remain coexistent claims held by different parties?',
    'If both readings can be simultaneously true (wage signals ARE valid AND information asymmetry enables suppression — they operate at different levels of analysis), they coexist. If accepting one reading requires rejecting the other''s premises, they foreclose.',
    'Coexistence indicates the kernel genuinely contests what flexibility IS structurally. Foreclosure indicates the contest is illusory — only one reading can be true and the others are false summits. This determines whether the family structure is one contested kernel (coexist) or multiple incompatible claims (foreclosure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading forecloses or coexists with the precarity and developmental readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(flex_tr_t0, observed).
narrative_ontology:measurement(flex_tr_t3, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 3, 0.23).
narrative_ontology:measurement_basis(flex_tr_t3, observed).
narrative_ontology:measurement(flex_tr_t6, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(flex_tr_t6, observed).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(flex_tr_t10, observed).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(flex_tr_t15, observed).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(flex_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(flex_be_t0, observed).
narrative_ontology:measurement(flex_be_t3, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement_basis(flex_be_t3, observed).
narrative_ontology:measurement(flex_be_t6, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement_basis(flex_be_t6, observed).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(flex_be_t10, observed).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(flex_be_t15, observed).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(flex_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(flex_su_t0, observed).
narrative_ontology:measurement(flex_su_t3, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 3, 0.2).
narrative_ontology:measurement_basis(flex_su_t3, observed).
narrative_ontology:measurement(flex_su_t6, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 6, 0.22).
narrative_ontology:measurement_basis(flex_su_t6, observed).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement_basis(flex_su_t10, observed).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 15, 0.27).
narrative_ontology:measurement_basis(flex_su_t15, observed).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(flex_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__market_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the flexible_employment_legitimacy kernel. All three readings account for the same empirical phenomenon (platform-mediated labor matching) but constitute it differently: market efficiency reading emphasizes price discovery and worker autonomy; precarity reading emphasizes algorithmic wage suppression and structural extraction; developmental reading emphasizes transitional policy toward formalization. The three constraints share the same kernel_id but have distinct constraint_ids, omegas, and structural claims. Each reading is ε-invariant and stable within its own frame. The three are linked via affects_constraints to enable constraint-family analysis. The contest among readings is routed through omega variables (axiom validity, mechanism empiricism, reading foreclosure/coexistence) rather than authored into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__market_efficiency_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
