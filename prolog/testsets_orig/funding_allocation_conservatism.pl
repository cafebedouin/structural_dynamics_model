% ============================================================================
% CONSTRAINT STORY: funding_allocation_conservatism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_funding_allocation_conservatism, []).

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
 *   constraint_id: funding_allocation_conservatism
 *   human_readable: Funding Allocation Conservatism: The Risk Aversion Ratchet
 *   domain: political_economy/institutional_finance
 *
 * SUMMARY:
 *   Funding allocation conservatism operates at the intersection of
 *   legitimate risk management and extractive gatekeeping. Research funding
 *   agencies face genuine portfolio management challenges: high-variance
 *   outcomes on novel research require careful calibration of bet sizes, and
 *   institutional credibility depends partly on demonstrable success rates.
 *   However, the mechanisms that enforce risk aversion — peer review panels
 *   dominated by established researchers, metrics-driven evaluation that
 *   penalizes novelty, institutional incentives that reward safe bets — have
 *   accumulated extractive overlay that suppresses novel research programs
 *   beyond what optimal risk management would justify. The constraint
 *   exhibits both coordination (legitimate risk-spreading) and extraction
 *   (novel investigators blocked from resources). The increasing theater
 *   ratio (0.48 → 0.68 over the interval) signals that the peer review
 *   mechanism has become increasingly performative: panels evaluate
 *   plausibility and fit with consensus rather than actual merit of novel
 *   directions. Simultaneously, alternative funding mechanisms (ARPA, venture
 *   models, online funding) have proven capable of identifying and funding
 *   breakthrough research, suggesting that the traditional system's
 *   conservatism is not a natural law but a contingent institutional
 *   arrangement that could shift.
 *
 * KEY AGENTS:
 *   - Novel Investigator: Primary victim (powerless/trapped) — lacks institutional credentials and publication record; structurally excluded by conservatism; cannot exit without abandoning research direction
 *   - Established Research Program: Primary beneficiary (institutional/arbitrage) — incremental extensions face low friction; can leverage existing reputation; arbitrage across related directions
 *   - Risk-Averse Funding Agency: Mixed actor (institutional/constrained) — genuinely manages portfolio risk but also enforces conservatism through metrics and implicit criteria; extraction mechanism embedded in institutional structure
 *   - Mid-Career Transitioning Researcher: Secondary victim (moderate/constrained) — some publication record but attempting novel direction; faces high friction but not impossible barriers; constrained but not trapped
 *   - Open Funding Coalition: Organized agents (organized/constrained) — ARPA-E, NSF SBIR, venture funding for research; building alternative pathways with higher risk tolerance and sunset logic
 *   - Peer Review Ritual System: Institutional mechanism (institutional/arbitrage) — maintains legitimacy through performative evaluation; theater has increased as metrics-driven assessment tightened; sees own process as degraded but maintains through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies mandatrophy signature: legitimate risk coordination has accumulated extractive overlay; suppression increases over interval despite unchanged functional requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(funding_allocation_conservatism, 0.58).
domain_priors:suppression_score(funding_allocation_conservatism, 0.62).
domain_priors:theater_ratio(funding_allocation_conservatism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(funding_allocation_conservatism, extractiveness, 0.58).
narrative_ontology:constraint_metric(funding_allocation_conservatism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(funding_allocation_conservatism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(funding_allocation_conservatism, tangled_rope).
narrative_ontology:human_readable(funding_allocation_conservatism, "Funding Allocation Conservatism: The Risk Aversion Ratchet").
narrative_ontology:topic_domain(funding_allocation_conservatism, "political_economy/institutional_finance").

domain_priors:requires_active_enforcement(funding_allocation_conservatism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(funding_allocation_conservatism, incumbent_institutions).
narrative_ontology:constraint_beneficiary(funding_allocation_conservatism, established_researchers).
narrative_ontology:constraint_beneficiary(funding_allocation_conservatism, risk_averse_funders).
narrative_ontology:constraint_victim(funding_allocation_conservatism, novel_research_programs).
narrative_ontology:constraint_victim(funding_allocation_conservatism, innovative_investigators).
narrative_ontology:constraint_victim(funding_allocation_conservatism, underexplored_domains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL INVESTIGATOR (SNARE) — Structurally excluded from funding allocation. Lacks institutional credentials, publication record in establishment journals, and social capital within peer review networks. Cannot exit without abandoning research direction. Maximum suppression: needs institutional affiliation to apply, peer reviewers are gatekeepers from competing established labs, funding panels default to low-risk incremental work from known teams. High extractiveness experienced: career window is finite, and rejection during early years creates permanent deficit.
constraint_indexing:constraint_classification(funding_allocation_conservatism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER TRANSITIONING RESEARCHER (TANGLED ROPE) — Constrained but not trapped. Has some publication record and institutional affiliation but attempting high-novelty direction away from established expertise. Faces extraction (skepticism from reviewers, reduced success rates, slower progression) but also genuine coordination benefits from the system (access to peer review networks, institutional infrastructure, collaborative opportunities). Can exit by returning to prior research direction, but this is costly. Moderate effective extraction.
constraint_indexing:constraint_classification(funding_allocation_conservatism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED RESEARCH PROGRAM (ROPE) — Net beneficiary of funding conservatism. Incremental extensions of prior work face low friction in review process. Can arbitrage: shift resources within domain, leverage existing publications as evidence of competence, access collaborative networks easily. Experiences the constraint as pure coordination: the conservative allocation mechanism ensures continued support for productive research directions. Minimal effective extraction — the constraint runs toward this agent.
constraint_indexing:constraint_classification(funding_allocation_conservatism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RISK-AVERSE FUNDING AGENCY (TANGLED ROPE) — Genuinely coordinates risk management: high-variance research bets require portfolio approach, and individual failures damage agency credibility. Constrained by political and reputational pressure to demonstrate success rates and impact metrics. The funding agency's own extraction mechanism is embedded in metrics (publication counts, citation impact, grant success rates) that incentivize backing established research. Benefits from the system through reputation (high success rate) but also trapped by metrics that punish actual risk-taking. Active enforcement required: agency staff use implicit criteria and panel feedback to suppress novel applications.
constraint_indexing:constraint_classification(funding_allocation_conservatism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN FUNDING COALITION (SCAFFOLD) — Organized agents (ARPA models, venture funding in research, alternative peer review systems) are building parallel funding pathways with explicit high-risk tolerance and sunset logic. ARPA-E, DARPA, NSF SBIR programs represent temporary interventions designed to sunset as markets mature. These mechanisms explicitly fund breakthrough-or-bust research that risk-averse traditional funders reject. Suppression still moderate (constrained by institutional norms, career risk for program officers), but has exit path and declining theater as alternative mechanisms mature.
constraint_indexing:constraint_classification(funding_allocation_conservatism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PEER REVIEW RITUAL SYSTEM (PITON) — Peer review panels serve as the theater mechanism enforcing conservatism. The ritual persists despite knowing its limitations: reviewers cannot assess true novelty objectively, tend toward risk-aversion when evaluating unfamiliar directions, and default to incremental extensions of consensus. The institutional system sees peer review as degraded — nearly all research funders acknowledge the system's flaws — but maintains it through inertia because it provides apparent legitimacy and distributes political blame ('the experts decided'). Theater ratio extremely high (0.68 system-wide): significant portion of review activity is performative legitimation rather than functional evaluation.
constraint_indexing:constraint_classification(funding_allocation_conservatism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational view, funding allocation must balance exploration and exploitation: purely random funding is chaos, purely conservative funding misses transformative breakthroughs. The constraint coordinates genuine risk management but extracts from novelty-seeking research programs. The analytics reveal that the suppression level (0.62) has been increasing over the interval as metrics-driven assessment (publication counts, success rates, impact factors) has tightened risk evaluation. The extraction mechanism has become more efficient (better-hidden) even as the functional coordination role has remained constant. This is the mandatrophy signature: what began as legitimate risk management has accumulated extractive overlay.
constraint_indexing:constraint_classification(funding_allocation_conservatism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(funding_allocation_conservatism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(funding_allocation_conservatism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(funding_allocation_conservatism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(funding_allocation_conservatism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(funding_allocation_conservatism, TR),
    TR >= 0.70.

:- end_tests(funding_allocation_conservatism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Novel research programs face significant extraction in the form of reduced resource access, slower career progression, and epistemic dismissal. However, the extraction is not total — some novel research does get funded (through alternative mechanisms and by risk-taking program officers), and the constraint coordinates genuine portfolio management. The 0.58 value reflects that extractiveness has been increasing over the interval (0.42 → 0.58) without corresponding increase in functional risk management requirements, suggesting accumulation of extractive overlay. Suppression (0.62): Moderate-high. Multiple barriers suppress novel research: peer reviewers from established labs skeptical of unfamiliar directions, institutional metrics that penalize variance, funding announcement language that emphasizes feasibility over novelty, unwritten norms in peer review communities about 'responsible risk.' These are real barriers but not absolute — alternative funding mechanisms have found ways to reduce suppression. Theater ratio (0.68): High and increasing. Peer review panels nominally evaluate scientific merit but increasingly operate as legitimacy theater: review panels cannot assess true novelty prospectively, tend toward consensus-defense, and use plausibility as proxy for merit. The increasing theater ratio (0.48 → 0.68) indicates that performative content has grown relative to functional evaluation. This is the Goodhart drift: as metrics have been introduced to measure success, the metrics become the target, divorcing evaluation from actual merit.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the novel investigator's snare and the established researcher's rope reveals the extraction mechanism. Both are evaluating the same institutional mechanism (peer review + metrics-driven funding), but experience it oppositely. For the established researcher, peer review is coordination: it legitimates their work and protects them from frivolous criticism. For the novel investigator, the same mechanism is extraction: it blocks access to resources based on unfamiliarity rather than merit. The gap is structural, not perspectival — it reveals that the mechanism has asymmetric effects. The piton perspective (peer review system seeing itself as degraded) indicates that the mechanism's functional role has decayed relative to its theater content. The scaffold perspective (organized agents building alternatives) indicates that the constraint is partially decomposable: some of the suppression is surmountable through alternative mechanisms, but not yet at scale.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Established researchers and risk-averse funders are beneficiaries with arbitrage-level exit options: they can shift resources, leverage alternative approaches, or exit the constraint entirely without severe cost. The engine derives low d values for these agents, producing negative or low χ. Novel investigators are victims with trapped-level exit options: they cannot exit without abandoning research direction, cannot arbitrage (no alternative funding pathways at career start), and face maximum suppression. The engine derives high d values for these agents, producing high χ. Mid-career researchers occupy the middle: victims with constrained (not trapped) exit options, so moderate d and moderate χ. The institutional funding agency is both beneficiary (gains reputation from success metrics) and enforcer of suppression, so its directionality depends on whether it is measured as administrator (beneficiary side) or subject (victim side of metrics pressure). The analytical observer is pure analyst with no structural stake in the extraction, so d is derived from canonical analytical position (0.73 → f(d) ≈ 1.15).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating accumulated extraction overlay on legitimate coordination. The base coordination function is genuine: research funding requires risk management, and not all proposals can be funded. However, the extractiveness has increased over the interval (0.42 → 0.58) without corresponding increase in portfolio risk. This suggests that the extraction mechanism has become more efficient (better-hidden) even as the functional requirement has remained constant. The theater ratio increase (0.48 → 0.68) indicates Goodhart drift: as metrics for evaluation success were introduced, these metrics became the target, and the evaluation process increasingly became theater in service of metric optimization rather than merit assessment. The mandatrophy is resolved by separating legitimate coordination (portfolio risk management at 0.25 base extractiveness) from accumulated extraction overlay (0.33 additional extractiveness from metrics-driven suppression and institutional incentive asymmetry). The constraint could be reformed by reducing theater (improving peer review mechanisms or using alternative evaluation), reducing suppression (expanding alternative funding pathways), or rebalancing institutional incentives (rewarding risk-taking, not penalizing it). The scaffold perspective shows that partial solutions are already emerging: ARPA-style programs achieve 0.30 extractiveness through reduced theater and suppression while maintaining coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conservatism_threshold_optimality,
    'What level of risk aversion in funding allocation maximizes long-term innovation rather than minimizing short-term variance?',
    'Long-term outcome analysis: tracking transformative discoveries that emerged from high-risk funding vs. incremental advances from safe bets; identifying optimal conservatism level that balances exploration/exploitation trade-off',
    'If optimal conservatism is significantly lower than current level (0.58 extractiveness): constraint is overextracted. If optimal is comparable or higher: constraint may be functional rather than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservatism_threshold_optimality, empirical, 'Optimal level of funding conservatism for innovation').

omega_variable(
    novel_breakthrough_detection,
    'Can peer reviewers reliably identify breakthrough research at the time of proposal, or does the identification require hindsight over years/decades?',
    'Historical analysis of rejected novel proposals vs. accepted conventional ones; correlation between initial peer assessment and eventual impact; examination of famous rejections vs. successful incremental research in same period',
    'If breakthrough identification requires hindsight: peer review mechanism is fundamentally incapable of reducing false positives and cannot justify conservatism. Suppression increases (mechanism is theater, not functional). If reviewers can identify breakthroughs: conservatism may be legitimate risk management.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(novel_breakthrough_detection, empirical, 'Whether peer review can identify breakthrough research prospectively').

omega_variable(
    alternative_funding_mechanism_viability,
    'Do alternative funding models (venture capital for research, ARPA-style programs, online peer funding) achieve comparable or better outcomes per dollar than traditional risk-averse mechanisms?',
    'Comparative analysis of research productivity, breakthrough rate, and cost-per-significant-discovery across funding mechanisms; longitudinal tracking of novel research directions funded by alternative mechanisms',
    'If alternatives achieve better outcomes: traditional conservatism is extractive overhead, not necessary risk management. Scaffold perspective is validated, sunset logic applies. If alternatives perform worse: conservatism may be justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_mechanism_viability, empirical, 'Comparative effectiveness of alternative funding mechanisms').

omega_variable(
    metrics_driven_extraction_causation,
    'Does the rise in conservatism correlate causally with the introduction of metrics-driven evaluation (impact factors, success rates, h-index, grant outcomes tracking), or are these independent phenomena?',
    'Time-series analysis of conservatism level pre/post metrics adoption; international comparison of conservatism levels across funding systems with different metrics intensity; mechanism study of how metrics affect panel decision-making',
    'If metrics drive conservatism: the constraint''s extraction mechanism is endogenous (self-reinforcing through measurement). Removing metrics could reduce suppression and extractiveness. If independent: conservatism would persist even with alternative evaluation frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metrics_driven_extraction_causation, empirical, 'Whether metrics-driven evaluation causes increased conservatism').

omega_variable(
    institutional_liability_asymmetry,
    'Does the institutional penalty for funding failed breakthrough research exceed the benefit/reward for funding successful breakthrough research?',
    'Analysis of career consequences for program officers who fund high-risk research; comparison of publicity and reward for successful vs. failed bets; institutional incentive structure for funding managers',
    'If penalty > reward: institutional structure enforces conservatism regardless of actual optimal risk level. Suppression is structural (institutional design) rather than epistemic. If penalty ≈ reward: decision-makers are rational risk-takers, not extraction agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_liability_asymmetry, empirical, 'Institutional penalty/reward asymmetry for funding risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(funding_allocation_conservatism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(funding_cons_tr_t0, funding_allocation_conservatism, theater_ratio, 0, 0.48).
narrative_ontology:measurement(funding_cons_tr_t10, funding_allocation_conservatism, theater_ratio, 10, 0.58).
narrative_ontology:measurement(funding_cons_tr_t20, funding_allocation_conservatism, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(funding_cons_be_t0, funding_allocation_conservatism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(funding_cons_be_t10, funding_allocation_conservatism, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(funding_cons_be_t20, funding_allocation_conservatism, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(funding_allocation_conservatism, resource_allocation).
narrative_ontology:boltzmann_floor_override(funding_allocation_conservatism, 0.18).
narrative_ontology:affects_constraint(funding_allocation_conservatism, academic_careerism_lock).
narrative_ontology:affects_constraint(funding_allocation_conservatism, research_replication_crisis).
narrative_ontology:affects_constraint(funding_allocation_conservatism, institutional_path_dependence).

% DUAL FORMULATION NOTE:
% Funding allocation conservatism is upstream of academic careerism (conservatism forces researchers into safe publication tracks) and replication crisis (conservative funding prevents replication research which has low novelty). Institutional path dependence manifests as inability to reform metrics despite widespread acknowledgment of their problems — the constraint is locked in by institutional incentives even when recognized as suboptimal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(funding_allocation_conservatism, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
