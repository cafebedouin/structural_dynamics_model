% ============================================================================
% CONSTRAINT STORY: innovation_opportunity_cost
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_innovation_opportunity_cost, []).

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
 *   constraint_id: innovation_opportunity_cost
 *   human_readable: Innovation Opportunity Cost: Resource Allocation Between Exploration and Exploitation
 *   domain: organizational/economic/systems
 *
 * SUMMARY:
 *   The innovation opportunity cost constraint describes the systematic
 *   resource allocation tradeoff between exploration (investing in novel,
 *   uncertain possibilities) and exploitation (scaling proven, high-certainty
 *   innovations). Organizations, investors, and institutions face genuine
 *   pressure to allocate finite capital to maximize near-term returns, which
 *   structurally favors exploitation over exploration. This creates an
 *   asymmetric extraction mechanism: those pursuing novel research (powerless
 *   agents with no exit) bear the cost of underfunding, while capital
 *   allocators and incumbent organizations (institutional agents with
 *   arbitrage options) benefit from concentration on high-certainty returns.
 *   The constraint exhibits tangled rope characteristics: it serves a genuine
 *   coordination function (rationally managing risk and resource scarcity)
 *   while simultaneously extracting resources from exploratory agents who
 *   cannot exit. The opportunity cost is both real (time/capital spent on
 *   exploitation cannot simultaneously be spent on exploration) and enforced
 *   (institutional incentive structures, funding allocation mechanisms,
 *   publication bias, and career metrics all suppress exploration in favor of
 *   exploitation). From the analytical observer's view, the explore-exploit
 *   tradeoff appears immutable — any finite agent must choose. But the
 *   specific severity and asymmetry in contemporary capitalism is contingent:
 *   different policy choices (exploration-mandated funding percentages,
 *   alternative reward structures, organizational slack norms) would
 *   rebalance the tradeoff without eliminating the underlying structural
 *   feature.
 *
 * KEY AGENTS:
 *   - Novel Researchers: Primary victims (powerless/trapped) — early-stage exploratory researchers systematically underfunded and underrewarded; no meaningful exit from the academic/innovation system without career damage
 *   - Exploratory Research Community: Secondary victim (moderate/constrained) — collective with some advocacy power but insufficient to shift capital allocation; can organize alternative funding but remains marginalized
 *   - Capital Allocators (VC/PE/Institutions): Primary beneficiaries (institutional/arbitrage) — benefit from concentration of capital on exploitation; can arbitrage between exploration and exploitation based on market signals
 *   - Incumbent Organizations: Primary beneficiaries (institutional/arbitrage) — maximize returns and competitive positioning by focusing on exploitation; frame exploration as future optionality they control
 *   - Open Science Coalition: Organized alternative (organized/constrained) — government agencies, academic consortia, open-source communities building parallel exploration funding streams
 *   - Academic Institutions: Institutional actor maintaining piton performance (institutional/arbitrage) — rhetorical commitment to 'pure research' while structural incentives reward applied outcomes
 *   - Powerful Individual Innovators: Privileged agents (powerful/mobile) — can partially escape the opportunity cost through reputation, dedicated funding, network access
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable tradeoffs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(innovation_opportunity_cost, 0.52).
domain_priors:suppression_score(innovation_opportunity_cost, 0.48).
domain_priors:theater_ratio(innovation_opportunity_cost, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(innovation_opportunity_cost, extractiveness, 0.52).
narrative_ontology:constraint_metric(innovation_opportunity_cost, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(innovation_opportunity_cost, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(innovation_opportunity_cost, tangled_rope).
narrative_ontology:human_readable(innovation_opportunity_cost, "Innovation Opportunity Cost: Resource Allocation Between Exploration and Exploitation").
narrative_ontology:topic_domain(innovation_opportunity_cost, "organizational/economic/systems").

domain_priors:requires_active_enforcement(innovation_opportunity_cost).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(innovation_opportunity_cost, incumbent_organizations).
narrative_ontology:constraint_beneficiary(innovation_opportunity_cost, capital_allocators).
narrative_ontology:constraint_victim(innovation_opportunity_cost, novel_researchers).
narrative_ontology:constraint_victim(innovation_opportunity_cost, exploratory_projects).
narrative_ontology:constraint_victim(innovation_opportunity_cost, future_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL RESEARCHER (SNARE) — Trapped in a system that systematically extracts their novel ideas while providing minimal resources or career security. Researchers with early-stage, high-risk ideas face funding rejection, publication bias against null results, and career punishment for pursuing speculative directions. No meaningful exit option: abandoning novel research means accepting career mediocrity within existing paradigms. Bears full extraction cost.
constraint_indexing:constraint_classification(innovation_opportunity_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXPLORATORY RESEARCH COMMUNITY (TANGLED ROPE) — Moderately constrained by funding scarcity and institutional metrics that reward exploitation over exploration. Benefits from a coordination function: shared standards, open-access preprint infrastructure (arXiv), and collaborative funding pools enable some exploratory work. But asymmetric extraction persists: exploratory projects receive 5-10% of research funding despite generating disproportionate long-term value. Constrained by career pressures but not fully trapped — can advocate collectively and create alternative funding mechanisms.
constraint_indexing:constraint_classification(innovation_opportunity_cost, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL ALLOCATOR (ROPE) — Institutional actors (venture capital, private equity, large research foundations) benefit from the coordination function the opportunity cost creates: by concentrating capital on proven, exploitative innovations, they reduce portfolio risk and accelerate returns. They experience the constraint as a coordination mechanism — allocating to low-risk, high-certainty exploitation maximizes predictable cash flow. Net beneficiary through arbitrage access: can deploy capital to explore when markets reward it, exploit when they don't. The extraction runs toward them.
constraint_indexing:constraint_classification(innovation_opportunity_cost, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT ORGANIZATION (ROPE) — Experiences the opportunity cost constraint as pure coordination. By redirecting resources from exploration (expensive, uncertain) to exploitation (proven, high-margin), incumbents maximize shareholder returns and competitive positioning. The constraint solves their fundamental problem: how to allocate capital between maintaining current dominance (exploitation) and hedging against disruption (exploration). They can arbitrage between the two — increasing exploration when competitive pressure rises, decreasing it during stable periods. Net beneficiary.
constraint_indexing:constraint_classification(innovation_opportunity_cost, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE COALITION (SCAFFOLD) — Organized actors (government science agencies, open-source communities, academic consortia) view the opportunity cost as a temporary coordination failure with a sunset. Mechanisms like NIH/NSF SBIR programs, open-access publishing, preprint infrastructure, and prize-based funding (e.g., X-Prize, XPRIZE) create alternative pathways for exploration that bypass traditional capital allocation. Suppression is high (career incentives still favor exploitation) but declining over the generational horizon — cultural shift toward 'exploration as cultural good' is real. Has sunset logic: as open science institutions mature, the traditional opportunity cost mechanism loses force because alternative resource flows enable exploration outside incumbent control.
constraint_indexing:constraint_classification(innovation_opportunity_cost, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ACADEMIC INSTITUTION (PITON) — Universities and research institutes maintain performative exploration funding (basic science grants, sabbaticals, 'curiosity-driven research' narratives) despite structural incentives favoring exploitation (publication metrics, grant success rates, startup commercial outcomes). The rhetoric of exploration persists while the actual resource flows concentrate on exploitation. Theater ratio is moderate-high because the institution performs exploration commitment while allocating marginally. The constraint is maintained through institutional inertia — universities still believe in 'pure research' even as faculty incentives reward applied outcomes.
constraint_indexing:constraint_classification(innovation_opportunity_cost, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: POWERFUL INDIVIDUAL INNOVATOR (TANGLED ROPE) — Powerful agents (established researchers, serial entrepreneurs, Nobel laureates) experience the opportunity cost as a mixed benefit-extraction dynamic. They have mobile exit options: can pursue exploration by virtue of established reputation, can attract dedicated funding, can bypass institutional gatekeeping. But they also benefit from the exploitation-focused capital allocation (which funds their applied work and generates resources for exploration investments). Experience moderate extraction because they can partly arbitrage the system, but they remain embedded in it and constrained by broader trend toward exploitation.
constraint_indexing:constraint_classification(innovation_opportunity_cost, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the explore-exploit tradeoff appears as an immutable property of rational resource allocation: any finite agent with finite time must choose between investing in current returns (exploitation) or future possibility space (exploration). This trade-off is invariant across all economic systems, all timescales, all resource constraints. The analytical observer risks naturalizing what may be a contingent institutional arrangement — the specific urgency and asymmetry of the opportunity cost in contemporary capitalism may not be a law of nature but rather a policy choice. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(innovation_opportunity_cost, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(innovation_opportunity_cost_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(innovation_opportunity_cost, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(innovation_opportunity_cost, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(innovation_opportunity_cost, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(innovation_opportunity_cost, TR),
    TR >= 0.70.

:- end_tests(innovation_opportunity_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The opportunity cost is genuinely present — capital/time spent on exploitation cannot simultaneously be spent on exploration, creating a zero-sum dynamic. However, the value of exploration is systematically underestimated by financial markets, creating additional extraction beyond the pure opportunity cost: novel researchers bear the cost of both the structural tradeoff AND the mispricing of their work's future value. The rising trajectory (0.35 → 0.52 over the interval) reflects increasing urgency of short-term capital returns, financialization of innovation, and winner-take-most dynamics in technology that concentrate resources on proven winners. Suppression (0.48): Moderate. Barriers to pursuing exploration include funding scarcity, publication bias, career metrics that reward exploitation, and institutional structures designed for efficiency rather than discovery. But suppression is not total — grant mechanisms exist, alternative funding pathways are emerging, and some organizations explicitly fund exploration. The constraint's suppression comes from incentive asymmetry rather than absolute prohibition. Theater ratio (0.35): Low-moderate. The opportunity cost constraint is largely functional rather than performative — it reflects genuine economic tradeoffs rather than ritual or misdirection. However, some theater exists in the rhetoric of exploration commitment ('investing in innovation,' 'breakthrough research initiatives') while funding flows concentrate on exploitation. The low theater distinguishes this from the verification bottleneck (where theater dominates) — the opportunity cost is a real structural feature, not primarily a performative one.
 *
 * PERSPECTIVAL GAP:
 *   The mechanism generating the perspectival gap is the asymmetry in exit options combined with structural beneficiary/victim status. Researchers cannot exit exploration (or exit means abandoning their calling); capital allocators can exit exploration investments instantly. This creates a fundamental difference in how the opportunity cost is experienced. For researchers, the cost is structural and internalized — exploration is what they do, and the underfunding is a constraint on doing it. For allocators, the cost is optional — they can allocate to exploration or not, based on market signals. The cognitive/identity dimension adds another layer: researchers often see exploration as intrinsically valuable and world-improving, which can lock them into identity-fusion with exploratory work (identity_locked exit). This makes the opportunity cost feel more severe — it's not just underfunding, it's a conflict between how they see themselves (explorers) and the system's treatment of exploration (marginal). Capital allocators experience no such identity lock — they are neutral between exploration and exploitation, purely responsive to expected returns.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation flows from beneficiary/victim declarations + exit options + power level. Researchers declared as victims + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extraction. Capital allocators declared as beneficiaries + arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.12 → negative effective extraction (they are subsidized by the constraint). The research community as moderate power + victims + constrained exit → d ≈ 0.65 → f(d) ≈ 1.00 → moderate extraction. The open science coalition as organized power + not clearly beneficiary/victim + constrained exit → d ≈ 0.50 → f(d) ≈ 0.65 → mixed extraction. These derivations explain why the powerless researcher and capital allocator see completely different constraints despite measuring the same structural phenomenon: their structural relationships to the extraction flow are opposite.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the tangled rope classification, which accommodates both the genuine coordination function (managing resource allocation under scarcity) and the asymmetric extraction (concentration on exploitation systematically deprives exploration). The constraint cannot be classified as pure rope (which would erase the extraction of researcher resources) or pure snare (which would erase the genuine coordination problem). The tangled rope captures that the opportunity cost is simultaneously functional AND extractive. The mandatrophy test prevents misclassification by requiring three data points: (1) genuine beneficiaries exist and declare coordination benefit (capital allocators, incumbents declare resource allocation coordination), (2) genuine victims exist and declare asymmetric extraction (researchers declare systematic underfunding), (3) active enforcement exists (institutional structures enforce the extraction). All three are satisfied, confirming tangled rope. If beneficiaries were absent (pure cost with no one gaining), it would be snare. If victims were absent (pure coordination with no exploitation), it would be rope. The presence of both confirms hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exploration_value_measurement,
    'How should the long-term value contribution of exploratory research be measured relative to immediate exploitation returns?',
    'Longitudinal outcome tracking: retrospective analysis of breakthrough innovations; correlation of exploration investment levels with disruptive innovation emergence; case studies of failed incumbent organizations vs. exploration-heavy competitors',
    'If exploration is undervalued: opportunity cost extraction is more severe than metrics suggest (higher ε). If exploration value is properly accounted for: opportunity cost is a more balanced coordination problem (lower ε, higher rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploration_value_measurement, empirical, 'How to measure long-term value of exploratory research').

omega_variable(
    slack_resource_availability,
    'What level of organizational slack (unused capacity, unallocated budget) is available for exploration without degrading exploitation performance?',
    'Comparative analysis of organizations with different slack policies; measurement of minimum exploration investment thresholds needed to maintain competitive positioning; simulation of tradeoff curves',
    'If slack can be substantial: many organizations could afford exploration without sacrifice (snare extraction is overestimated). If slack must be minimal: opportunity cost is genuine structural tradeoff (snare extraction confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slack_resource_availability, empirical, 'Availability of organizational slack for exploration').

omega_variable(
    time_horizon_alignment,
    'Do the decision-makers experiencing the opportunity cost (short-term focused investors, quarterly-driven executives) have authority over exploration investment, or is there structural misalignment between cost-bearers and decision-makers?',
    'Organizational structure analysis: who funds exploration vs who bears consequences of innovation failure; incentive analysis; capital flow tracing',
    'If aligned: opportunity cost reflects genuine tradeoff (tangled rope confirmed). If misaligned: extraction is structural because those bearing cost have no voice in allocation (snare severity increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(time_horizon_alignment, empirical, 'Alignment between opportunity cost bearers and decision-makers').

omega_variable(
    alternative_funding_effectiveness,
    'Can alternative funding mechanisms (government grants, crowdfunding, decentralized science) actually sustain exploration at rates comparable to traditional capital allocation?',
    'Comparison of exploration funding levels and success rates: traditional VC vs. SBIR vs. open-source vs. decentralized science; tracking of breakthrough innovations by funding source; cost analysis',
    'If effective: scaffold sunset is real — alternative pathways can displace traditional opportunity cost. If ineffective: scaffold is aspirational and alternative funding remains marginal (scaffold classification is optimistic).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_funding_effectiveness, empirical, 'Effectiveness of alternative exploration funding mechanisms').

omega_variable(
    exploration_extraction_circularity,
    'Does the opportunity cost mechanism create a feedback loop where exploration is systematically deprived of resources, creating a shortage that justifies continued deprivation?',
    'Historical analysis of exploration funding ratios; identification of inflection points where funding shifted between exploration/exploitation; comparison with simulated counterfactuals where exploration received baseline funding',
    'If feedback loop exists: extraction is self-reinforcing and may require policy intervention to break (snare classification sustained). If cycle can be disrupted: constraint may be more mutable than power dynamics suggest (downgrade to tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploration_extraction_circularity, empirical, 'Self-reinforcing feedback loop in exploration deprivation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(innovation_opportunity_cost, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(innov_tr_t0, innovation_opportunity_cost, theater_ratio, 0, 0.28).
narrative_ontology:measurement(innov_tr_t10, innovation_opportunity_cost, theater_ratio, 10, 0.32).
narrative_ontology:measurement(innov_tr_t20, innovation_opportunity_cost, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(innov_be_t0, innovation_opportunity_cost, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(innov_be_t10, innovation_opportunity_cost, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(innov_be_t20, innovation_opportunity_cost, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(innovation_opportunity_cost, resource_allocation).
narrative_ontology:affects_constraint(innovation_opportunity_cost, publication_bias_against_null_results).
narrative_ontology:affects_constraint(innovation_opportunity_cost, venture_capital_winner_take_most).
narrative_ontology:affects_constraint(innovation_opportunity_cost, academic_career_metrics_applied_focus).

% DUAL FORMULATION NOTE:
% The innovation opportunity cost is a network hub affecting multiple downstream constraints in research funding and career incentives. The upstream constraint is the fundamental explore-exploit mathematical tradeoff (arguably a mountain from the analytical perspective). This story focuses on the institutional implementation of that tradeoff — how organizations and markets operationalize the choice in ways that systematically favor exploitation. Decomposition into separate stories (pure mathematical tradeoff vs. institutional implementation) would clarify the ε difference, but the institutional story is the actionable one for policy analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(innovation_opportunity_cost, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
