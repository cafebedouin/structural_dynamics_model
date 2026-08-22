% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Flexible Employment as Legitimate Market-Clearing Mechanism (Market-Efficiency Reading)
 *   domain: economic/labor/social_policy
 *
 * SUMMARY:
 *   The standing arrangement under contest is platform-mediated flexible
 *   employment — ride-hail, delivery, freelance marketplace, and
 *   task-platform work as it actually operates. This file instantiates ONE
 *   reading of the flexible_employment_legitimacy kernel: the
 *   market_efficiency_reading, under which the arrangement is a legitimate
 *   market-clearing mechanism matching heterogeneous labor supply to
 *   fluctuating demand, wage convergence across gig and traditional
 *   blue-collar work is a market signal of scarcity rather than a
 *   distributive failure, platform algorithms are neutral coordinators, and
 *   worker autonomy is maximized by cheap entry and exit. Per the
 *   epsilon-referent rule for kernel readings, extractiveness is authored for
 *   the standing arrangement AS THIS READING SEES IT — hence the low authored
 *   epsilon; the sibling readings (precarity_extraction_reading,
 *   developmental_state_reading) share the same referent and author their own
 *   higher epsilon values in separate linked files. The claim and the metrics
 *   are independent authored facts: claimed_type is rope from this reading's
 *   seat, and the metrics describe the arrangement's operation as this
 *   reading assesses it.
 *
 * KEY AGENTS:
 *   - gig_platform_operators: agenda-setter and fee collector (institutional/arbitrage) — runs the matching machinery, sets terms, receives the commission flow
 *   - flexible_gig_workers: declared beneficiaries (powerless/mobile) — supply per-task labor, bear income volatility, retain cheap entry and exit
 *   - on_demand_service_consumers: beneficiaries (organized/mobile) — buy dispatchable services at posted prices, switch apps freely
 *   - firms_needing_elastic_labor: beneficiaries (powerful/arbitrage) — buy elastic capacity without fixed payroll obligations
 *   - traditional_sector_unions: excluded voice (organized/constrained) — hold negotiated standards the arrangement undercuts, no seat in platform governance
 *   - labor_economists: analytical observers — test the clearing-price and marginal-product claims econometrically
 *   - gig_classification_regulators: institutional observers — decide the legal category the arrangement occupies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.26).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.16).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.16).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Legitimate Market-Clearing Mechanism (Market-Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "economic/labor/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, '3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22').
narrative_ontology:cs_kernel_codification('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', distributed).
narrative_ontology:cs_authority_grounding('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', expertise).
narrative_ontology:cs_interpretation_layer_present('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22').
narrative_ontology:cs_reading_relation('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', flexible_employment_legitimacy__precarity_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', foundational, wage_convergence_signals_scarcity).
narrative_ontology:cs_axiom_status(wage_convergence_signals_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', wage_convergence_signals_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', foundational, algorithmic_coordination_maximizes_worker_autonomy).
narrative_ontology:cs_axiom_status(algorithmic_coordination_maximizes_worker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', algorithmic_coordination_maximizes_worker_autonomy, empirically_contingent).
narrative_ontology:cs_reference_frame('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', competitive_market_clearing_equilibrium).
narrative_ontology:cs_drift_state('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', post_monopsony_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f97c66c-34cf-41a2-a26b-4ad9fc5a7a22', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, gig_platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, flexible_gig_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, on_demand_service_consumers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, firms_needing_elastic_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and run the matching algorithms that connect workers to tasks, set commission rates and surge pricing parameters, and maintain the reputation systems both sides rely on. Collect a per-transaction commission on every completed job and can adjust terms unilaterally. Their exit is portfolio-wide: they operate across jurisdictions and can relocate legal domicile or shift which services they intermediate.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, gig_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__market_efficiency_reading, gig_platform_operators, beneficiary).

% Supply driving, delivery, care, or task labor per-job with no fixed schedule. They take income immediately, choose when to log on, and can sign up for several apps at once. Entry requires little more than a vehicle or a phone; leaving means simply stopping, though the income stops too. Many hold this work alongside other employment or study; some rely on it as primary income.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, flexible_gig_workers, beneficiary,
    powerless, immediate, mobile, national).

% Purchase rides, deliveries, and services on demand at transparent posted prices, often cheaper and faster than the dispatched alternatives that preceded the platforms. They bear none of the arrangement's risks and can switch apps whenever price or quality disappoints.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, on_demand_service_consumers, beneficiary,
    organized, biographical, mobile, global).

% Restaurants, retailers, logistics shippers, and project-based businesses scale their workforce up and down with demand without carrying fixed payroll, benefits, or severance obligations. They buy capacity per unit and offload scheduling risk to the platform layer.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, firms_needing_elastic_labor, beneficiary,
    powerful, generational, arbitrage, global).

% Represent workers in taxi, courier, and regulated transport sectors whose wages, benefits, and licensing protections were built up over decades. They are not part of platform governance and have no seat in how matching or pay algorithms are set; their recourse is legislation, litigation, and public campaigns aimed at reclassification.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, traditional_sector_unions, excluded,
    organized, generational, constrained, national).

% Estimate whether platform pay tracks marginal product, measure search-cost reductions, and test whether observed wage convergence reflects scarcity pricing or employer-side market power. They publish in peer-reviewed venues and advise both regulators and platforms.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_economists, observer,
    institutional, generational, analytical, continental).

% Decide whether platform workers are contractors or employees, set minimum earnings floors, and administer benefits portability schemes. They hold hearings, commission studies, and can rewrite the legal category the whole arrangement sits inside.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, gig_classification_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, gig_platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real-time search-and-matching problem: fragmented, heterogeneous labor supply meets spiky, geographically distributed demand. The platform standardizes trust (ratings, verification), prices the match, and dispatches, replacing the idle-capacity and queueing losses of both scheduled employment and street-hail markets.
% TRANSFER_FUNCTION: Moves payment per completed task from consumers and firms to workers; moves a commission on each transaction to the platform operator; allocates schedule and income volatility to workers, which this reading prices as the compensated premium of flexibility and autonomy.
% ABSENT_VOICES: Traditional-sector unions and workers displaced from licensed taxi and courier trades would object that the arrangement undercuts negotiated standards; gig workers who experience deactivation or income shocks have no seat in algorithm design; future retirees dependent on gig income carry benefit gaps nobody currently represents. They are outside the platform's governance surface entirely — their objections surface only in legislatures and courtrooms, never in the matching system itself.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, urban transport and delivery capacity would collapse until legacy dispatch systems rebuilt, millions of workers would lose an income channel they entered voluntarily, firms would re-carry fixed payroll or ration capacity, and consumers would face longer waits and higher prices — the matching infrastructure is now load-bearing for entire service categories.
% FOUNDING_PROBLEM: Search frictions and idle capacity: willing workers could not find demand spikes and demand spikes could not find willing workers; intermediation was slow, opaque, and geographically bounded, leaving both sides of the labor market holding unused inventory.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economics research documents large reductions in search and transaction costs and measurable service expansion in previously underserved areas; worker surveys outside platform sponsorship report flexibility as a primary stated reason for participation. Critics concede the matching function operates while disputing the welfare distribution — that concession, from outside the benefiting parties, corroborates that the founding problem was and remains real.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.26) because this reading holds that posted prices clear markets: workers are paid approximately what the match is worth, and the commission prices a real intermediation service. The gentle upward drift in the series (0.16 to 0.26) records rising take rates and network concentration even on sympathetic terms — the reading can observe that pricing power grew without conceding the extraction frame. Suppression is low (0.16): no one is compelled to participate, and deactivation functions as quality control rather than coercion, though the suppression_requirement series records the growing legal-compliance machinery platforms built to defend contractor classification. Theater is low (0.14): the matching function is real and load-bearing, with only a modest performative component (flexibility rhetoric deployed in regulatory fights). Accessibility_collapse is moderate-low (0.32): traditional employment, licensure-trades work, and non-participation remain live alternatives, which is precisely the reading's voluntariness premise. Resistance is moderate (0.42): unionization drives, minimum-earnings-floor campaigns, and classification litigation are observable facts that even this reading's account must register, interpreted as misunderstanding or incumbent protectionism. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine through directionality and scope. All three tracked series share one seven-point grid so no metric row is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the platform seat, the arrangement is coordination it built and continuously maintains — the operator experiences a rope it administers. Consumers and elastic-labor firms sit near the pure-beneficiary end: convenience and capacity without obligation. The worker seat is the hinge: even within this reading's own lights, workers collect flexibility and immediate income while bearing volatility the reading prices as a compensated premium — a beneficiary seat whose computed extraction depends heavily on whether exit is truly cheap. The excluded union seat and displaced licensed trades experience the same arrangement as standards erosion with no compensating access. The engine computes these per-seat classifications from the structural data; this file's rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared group is a beneficiary, so derived directionality sits near the beneficiary end across seats. Operators sit lowest (they collect the commission and control the rules); workers sit slightly above them — still net beneficiaries under this reading, but the only beneficiary seat that supplies the labor being matched and absorbs the volatility, so their d is the sensitive parameter. Consumers and firms sit near zero. The excluded unions receive no beneficiary declaration, placing their derived d away from the subsidized end and reflecting their exposed competitive position. No directionality overrides are used: the beneficiary/victim-plus-exit derivation captures the structure this reading asserts, and the exit_options atoms (mobile for workers, arbitrage for operators and firms) carry the differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's classification discipline cuts both ways. Against the precarity failure mode: declaring the genuine coordination function (search-and-matching, trust standardization) with real beneficiaries prevents a rival corpus from mislabeling working coordination as pure extraction. Against the laundering failure mode: the engine's per-seat computation means the rope claim earns nothing automatically — if the worker seat's effective extraction computes high despite declared beneficiary status, the divergence flags the voluntariness premise itself, routing investigation to the exit-cost omega rather than letting the efficiency frame absorb the anomaly. The founding problem (search frictions and idle capacity) remains live — matching problems recur every demand cycle — so no mandatrophy declaration is made; the arrangement has not outlived its function on this reading's account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_provenance,
    'This constraint is one reading of the flexible_employment_legitimacy kernel — the market_efficiency_reading. What would the sibling readings change structurally?',
    'Read against the sibling files: precarity_extraction_reading declares platform-extracted surplus with a named victim set (high epsilon, snare/tangled_rope territory); developmental_state_reading declares a transitional arrangement with a state-steering sunset frame (scaffold territory). Compare beneficiary/victim declarations and epsilon across the family.',
    'This file''s low epsilon and rope claim are reading-indexed, not topic-level facts. If the corpus aggregated across readings without the kernel tag, the three files would look like contradictory measurements of one constraint rather than three constraints sharing a referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Committer-frame provenance: one of three readings of a contested labor-legitimacy kernel.').

omega_variable(
    exit_cost_voluntariness,
    'Is worker exit from platform gigs genuinely cheap (mobile), or is apparent mobility constrained by income necessity, asset commitments, and the absence of comparable flexible alternatives?',
    'Panel data on worker transitions: reservation wages, multi-apping rates, re-entry behavior after deactivation, and what former gig workers move to. If most exits land in equivalent-or-worse positions, exit is nominal rather than real.',
    'If exit is constrained, the worker seat''s derived directionality moves toward full target, effective extraction amplifies, and the rope claim degrades seat-by-seat toward tangled_rope or snare — the single most consequential parameter separating this reading from the precarity sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_voluntariness, empirical, 'Whether the voluntariness premise survives measurement of actual exit costs.').

omega_variable(
    monopsony_vs_scarcity_signal,
    'Does observed wage convergence between gig and traditional blue-collar work reflect genuine scarcity pricing (this reading''s foundational axiom) or employer-side concentration holding wages below marginal product?',
    'Quasi-experimental designs exploiting platform entry and exit variation across local labor markets; pass-through estimates of demand shocks to worker pay; comparison of pay in markets served by one versus several competing apps.',
    'If monopsony explains convergence, the foundational axiom wage_convergence_signals_scarcity is empirically overridden, the clearing-price warrant fails, and the reading collapses toward the precarity sibling''s extraction account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopsony_vs_scarcity_signal, empirical, 'Scarcity-signal versus monopsony accounts of observed wage patterns.').

omega_variable(
    take_rate_competitive_benchmark,
    'Do platform commissions approximate competitive prices for matching, payment, and trust services, or do network effects sustain above-competitive take rates?',
    'Benchmark commissions against pre-platform intermediation costs for the same services and against commission levels in markets with active multi-app competition; examine take-rate trajectories after local monopoly is established.',
    'Above-competitive take rates recode the commission from service fee to rent, shifting the operator seat''s computed type toward extraction and raising the arrangement''s measured epsilon even within this reading''s own framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(take_rate_competitive_benchmark, empirical, 'Whether the platform''s cut prices a service or collects a positional rent.').

omega_variable(
    authority_grounding_framing,
    'Is the market-efficiency reading''s authority genuinely grounded in economic expertise, or in platform commercial interest wearing expertise as legitimation?',
    'Trace the citation network behind policy claims: do deregulatory conclusions follow independent, replicated econometric findings or platform-commissioned studies; audit conflict disclosures and replication rates in the supporting literature.',
    'If the authority structure is extraction-grounded, cs_structure.authority_grounding shifts from expertise to extraction, the interpretive layer functions as an advocacy buffer rather than a scientific one, and the reading''s certification weight drops accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing under-determination: expertise grounding versus interest-driven legitimation of the same claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 2009, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2009, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2009, 0.06).
narrative_ontology:measurement(flex_tr_t2012, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2012, 0.08).
narrative_ontology:measurement(flex_tr_t2015, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(flex_tr_t2018, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(flex_tr_t2021, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2021, 0.13).
narrative_ontology:measurement(flex_tr_t2023, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2023, 0.14).
narrative_ontology:measurement(flex_tr_t2025, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 2025, 0.14).

% Extraction over time
narrative_ontology:measurement(flex_be_t2009, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2009, 0.16).
narrative_ontology:measurement(flex_be_t2012, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2012, 0.19).
narrative_ontology:measurement(flex_be_t2015, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2015, 0.21).
narrative_ontology:measurement(flex_be_t2018, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2018, 0.23).
narrative_ontology:measurement(flex_be_t2021, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2021, 0.25).
narrative_ontology:measurement(flex_be_t2023, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2023, 0.26).
narrative_ontology:measurement(flex_be_t2025, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 2025, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2009, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2009, 0.08).
narrative_ontology:measurement(flex_su_t2012, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2012, 0.1).
narrative_ontology:measurement(flex_su_t2015, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2015, 0.12).
narrative_ontology:measurement(flex_su_t2018, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2018, 0.13).
narrative_ontology:measurement(flex_su_t2021, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2021, 0.15).
narrative_ontology:measurement(flex_su_t2023, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2023, 0.16).
narrative_ontology:measurement(flex_su_t2025, flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 2025, 0.16).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'flexible employment' decomposes into three structurally distinct constraints sharing one kernel (flexible_employment_legitimacy) and one referent (the standing platform-mediated arrangement). This file is the market_efficiency_reading (rope claim, low reading-indexed epsilon, no declared victims). The precarity_extraction_reading sibling authors high epsilon with a declared victim set; the developmental_state_reading sibling authors a transitional/sunset frame. The upstream-downstream pattern runs from this reading outward: its empirical claims (clearing prices, cheap exit) are cited as evidence AGAINST both siblings, while the siblings' empirical findings (monopsony estimates, exit-cost panels) feed back as challenges to this file's axioms. Family members are linked exclusively via network.affects_constraints; no reading's classification is folded into another's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
