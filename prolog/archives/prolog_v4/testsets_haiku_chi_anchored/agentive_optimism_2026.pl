% ============================================================================
% CONSTRAINT STORY: agentive_optimism_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agentive_optimism_2026, []).

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
 *   constraint_id: agentive_optimism_2026
 *   human_readable: The Agentive Optimism Gap
 *   domain: political/social
 *
 * SUMMARY:
 *   The Agentive Optimism Gap represents a fundamental structural divide in
 *   how different populations experience agency, probability, and the
 *   possibility of change. Policy elites — defined by credential
 *   accumulation, institutional position, and dense networks — maintain a
 *   sense of personal agency and optimism that is empirically justified
 *   within their domain: their actions produce outcomes, their plans
 *   materialize, their networks provide safety nets. A large segment of the
 *   public — economically precarious, structurally mobile-constrained,
 *   exposed to systemic risks beyond individual control — has developed what
 *   psychologists term 'learned helplessness' but what DR analysis reveals as
 *   rational pessimism: their actions are mediated by algorithmic scheduling,
 *   their upward mobility is constrained by benefit cliffs and credential
 *   inflation, their exposure to climate, pandemic, and economic risk is
 *   structural rather than individual. This gap is not merely a psychology
 *   problem (treatable by motivational speakers) or a communication problem
 *   (solvable by better narrative). It is a constraint enforced by
 *   suppression mechanisms: precarity itself suppresses agency. The
 *   constraint extracts value from the pessimistic segment by requiring them
 *   to maintain hope despite evidence that hope is uncoupled from outcomes,
 *   while the optimistic elite benefit from narratives that attribute success
 *   to their own merit rather than structural advantage. The theater ratio
 *   (0.64) reflects that much of the policy discussion around 'closing
 *   opportunity gaps' and 'empowering communities' is performative: it
 *   addresses narrative-level pessimism while leaving structural precarity
 *   intact.
 *
 * KEY AGENTS:
 *   - Policy Elites & Technocrats: Primary beneficiaries (institutional/arbitrage) — their optimism is self-reinforcing because their networks and credentials actually do produce outcomes; they capture credit for success while externalizing risk
 *   - Economically Precarious Populations: Primary victims (powerless/trapped) — precarity suppresses agency; their pessimism is rational response to structural constraints they cannot individually escape
 *   - Youth Movements & Precariat Organizing: Secondary agents (organized/constrained) — coordinate around shared grievances; optimism requirement constrains their organizing (must maintain hope despite defeats)
 *   - Public Narratives & Media: Institutional structure (institutional/arbitrage) — demand optimism framing; create gap between narrative promise and structural reality
 *   - Feedback Loop Mechanisms: Structural enforcer — credential inflation, algorithmic scheduling, benefit cliffs, geographic sorting create the conditions that suppress precariat agency while enabling elite agency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agentive_optimism_2026, 0.52).
domain_priors:suppression_score(agentive_optimism_2026, 0.68).
domain_priors:theater_ratio(agentive_optimism_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agentive_optimism_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(agentive_optimism_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(agentive_optimism_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agentive_optimism_2026, snare).
narrative_ontology:human_readable(agentive_optimism_2026, "The Agentive Optimism Gap").
narrative_ontology:topic_domain(agentive_optimism_2026, "political/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agentive_optimism_2026, policy_elites).
narrative_ontology:constraint_beneficiary(agentive_optimism_2026, technocratic_class).
narrative_ontology:constraint_victim(agentive_optimism_2026, pessimistic_public_segment).
narrative_ontology:constraint_victim(agentive_optimism_2026, economically_precarious_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PESSIMISTIC PUBLIC (SNARE) — Trapped by structural conditions (job precarity, declining social mobility, climate anxiety) that generate learned helplessness. Cannot exit pessimism without material change; cannot exit the constraint system itself. Policy discourse offers aspirational narratives that feel disconnected from lived reality. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(agentive_optimism_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ECONOMICALLY PRECARIOUS (SNARE) — Constrained by gig economy, automation risks, and benefit-cliff structures that penalize upward mobility. Experience agency as illusory; policy optimism appears as gaslighting. Labor participation constrained by scheduling, health, and care obligations. d≈0.82, f(d)≈1.25, σ=0.9 → χ≈0.59.
constraint_indexing:constraint_classification(agentive_optimism_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POLICY ELITES (ROPE) — High educational attainment, institutional position, and network density enable genuine agency. Optimism is earned: their personal agency works (funding mechanisms, network access, credential portability). They experience the constraint as coordination: shared optimism enables collective action on infrastructure, innovation, policy. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary through arbitrage (can exit failing initiatives, capture upside from successful ones).
constraint_indexing:constraint_classification(agentive_optimism_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: YOUTH ORGANIZING (TANGLED ROPE) — Organized agents (climate strikers, housing justice movements, precariat unions) coordinate around shared grievances (coordination function) while also bearing extraction costs from the gap itself — their organizing labor is often unpaid, risky to employment, and produces aspirational rather than material change. Optimism becomes a constraint: required to maintain morale despite material constraints. d≈0.58, f(d)≈0.73, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(agentive_optimism_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPTIMISM NARRATIVE (PITON) — The cultural requirement to express optimism persists through institutional inertia despite losing functional power. TED talks, startup culture, policy white papers all demand optimistic framing regardless of structural constraints. This performative optimism masks rather than solves the gap. theater_ratio=0.64: optimism narratives dominate public discourse but generate minimal material change. The constraint is maintained because alternatives (admitting structural limits, redistributing resources to constrain options) have higher political cost.
constraint_indexing:constraint_classification(agentive_optimism_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systems perspective, the agentive optimism gap has genuine coordination benefits (it maintains collective action, enables long-term planning, prevents exit cascades) while simultaneously extracting from those without actual agency (it blames their pessimism as psychological failure rather than structural response). The constraint enables elite coordination (rope function) while simultaneously trapping non-elites (snare function). d≈0.65, f(d)≈0.98, σ=1.2 → χ≈0.61.
constraint_indexing:constraint_classification(agentive_optimism_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agentive_optimism_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agentive_optimism_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agentive_optimism_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agentive_optimism_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agentive_optimism_2026, TR),
    TR >= 0.70.

:- end_tests(agentive_optimism_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from precariat populations by requiring emotional labor (maintaining optimism) without providing material change, while concentrating credit for success among elites. The extraction mechanism is asymmetric agency: elites' agency works, precariat's agency is mediated/nullified. Suppression (0.68): High. Multiple reinforcing mechanisms: precarious employment limits exit options, credential inflation creates illusion of meritocracy while locking out those without early educational access, benefit-cliff structures actively penalize precariat agency attempts (taking a better job can mean losing healthcare), geographic sorting prevents observation of alternative success patterns. Theater ratio (0.64): Moderate-high. The optimism narrative dominates public discourse (TED talks, startup culture, policy white papers) with minimal material change in structural precarity. The theater has increased over the interval as gap between narrative optimism and lived precarity has widened. Claimed type is Snare from primary victim perspective, but Tangled Rope from analytical view because the constraint does serve genuine coordination function (shared narrative enables collective action, prevents total exit cascades) while simultaneously extracting.
 *
 * PERSPECTIVAL GAP:
 *   The gap between policy elites and precariat populations is not primarily epistemological (disagreement about facts) but structural (different material conditions that generate different rational responses to probability and agency). Policy elites see Rope — their optimism is functional, their networks enable coordination, their agency works. Precariat populations see Snare — optimism is demanded despite evidence that their agency is suppressed, their choices are constrained, their outcomes are mediated by systems beyond individual control. Youth organizing sees Tangled Rope — genuine coordination around shared grievances, but optimism requirement as extraction (emotional labor without material payoff). The analytical observer sees the full structure: the constraint is not about beliefs or narratives, it's about whose agency actually works. The gap persists because narrative-level interventions ('be more optimistic') misdiagnose the problem as psychological rather than structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy elites: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Negative effective extraction = net beneficiary. Their optimism is validated; their agency works; they capture credit. Precariat populations: Victim + trapped → d≈0.93, f(d)≈1.40. High extraction. Precarity suppresses agency; optimism is demanded without justification; pessimism is blamed as personal failure rather than rational response. Economically precarious workers: Victim + constrained → d≈0.82, f(d)≈1.25. Moderate-high extraction. Some constrained agency exists (labor participation choices) but is heavily mediated by systemic constraints (scheduling, healthcare benefits, care responsibilities). Youth organizing: Organized + constrained → d≈0.58, f(d)≈0.73. Mixed extraction. Genuine coordination and agency within the movement, but movement labor is often unpaid/risky and produces narrative rather than material change. Optimism narrative infrastructure: Institutional + arbitrage → d≈0.05, f(d)≈-0.11. Piton classification comes from theater gate (0.64 ≥ 0.70 threshold approached), not from extraction. Maintains itself through institutional inertia; alternatives have higher immediate political cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Agentive Optimism Gap is a genuine Snare FROM THE PERSPECTIVE OF THOSE TRAPPED IN IT (powerless/trapped demographics) while functioning as Rope FROM THE PERSPECTIVE OF BENEFICIARIES (policy elites) and as Tangled Rope from the analytical perspective that sees both the coordination function (shared optimism enables collective planning) and the extraction function (pessimism is blamed as personal psychological failure rather than rational structural response). The constraint prevents mislabeling by making clear: (1) whose agency is suppressed (trapped populations), (2) whose agency works (elites with dense networks), (3) what the coordination benefit is (shared optimism prevents total exit cascades, enables long-term planning), and (4) what is extracted (emotional labor, cognitive dissonance, self-blame for outcomes mediated by systems beyond individual control). The false natural law in this case would be: 'Optimism is a personality trait; some people are just naturally optimistic.' The constraint reveals this as institutional structure, not psychology: optimism is rational where outcomes are controllable; pessimism is rational where outcomes are mediated by systems beyond control. The gap will persist as long as agency-suppression mechanisms (precarity, credential inflation, algorithmic mediation) are more powerful than individual effort.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_threshold_ambiguity,
    'What material thresholds distinguish genuine agency from illusion? How much control over outcomes is required before optimism becomes rational rather than delusional?',
    'Longitudinal tracking of outcome variance by income decile; measurement of subjective control vs actual control by demographic; analysis of feedback loops (do elite actions produce outcomes; do precariat actions produce outcomes?)',
    'If threshold is high (elites have >80% outcome control): gap is truly structural, snare classification confirmed across most perspectives. If threshold is low (outcomes equally random for all): optimism gap is purely narrative, piton or theater classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agency_threshold_ambiguity, empirical, 'What degree of actual control justifies optimism vs marks it as delusion').

omega_variable(
    pessimism_causation,
    'Is public pessimism a rational response to structural conditions or a learned psychological state that could be shifted by different narratives?',
    'Experiments on narrative exposure; analysis of cohorts with identical material conditions but different cultural narratives; historical comparison of periods with similar structural precarity but different dominant mood',
    'If rational response: pessimism is justified; constraint classification depends on how material conditions actually are. If psychological/narrative: pessimism might be removable without structural change, shifting classification toward piton (theater). If both: constraint is hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pessimism_causation, empirical, 'Whether pessimism is rational structural response or malleable psychology').

omega_variable(
    elite_optimism_genuineness,
    'Is elite optimism grounded in actual superior outcomes or in information asymmetry and selection bias (they only see their successes, are surrounded by others like them)?',
    'Outcome tracking for elite vs precariat ventures; measurement of elite exposure to failure vs success; comparison of elite optimism prediction accuracy vs actual outcomes',
    'If grounded in superior outcomes: constraint classifies as pure extraction (elite agency works, precariat is genuinely trapped). If grounded in bias: constraint is tangled rope (elites also trapped in false optimism, but with better landing zones).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_optimism_genuineness, empirical, 'Whether elite optimism reflects actual agency advantage or selection bias').

omega_variable(
    narrative_plasticity,
    'Can the optimism gap be closed by changing shared narratives without material redistribution?',
    'Analysis of periods with successful narrative shifts (post-WWII optimism, etc.); measurement of optimism changes independent of material conditions; policy experiments on narrative reframing',
    'If yes: constraint is primarily theater (piton), fixable by narrative work. If no: constraint requires material redistribution (it''s a snare); narrative work alone is extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_plasticity, preference, 'Whether narrative reframing alone can close the agentive optimism gap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agentive_optimism_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agopt_tr_t0, agentive_optimism_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(agopt_tr_t5, agentive_optimism_2026, theater_ratio, 5, 0.54).
narrative_ontology:measurement(agopt_tr_t10, agentive_optimism_2026, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(agopt_be_t0, agentive_optimism_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(agopt_be_t5, agentive_optimism_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(agopt_be_t10, agentive_optimism_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agentive_optimism_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(agentive_optimism_2026, credential_inflation_trap).
narrative_ontology:affects_constraint(agentive_optimism_2026, algorithmic_scheduling_constraint).
narrative_ontology:affects_constraint(agentive_optimism_2026, benefit_cliff_structure).

% DUAL FORMULATION NOTE:
% The agentive optimism gap is downstream of multiple material constraints (precarious employment, credential inflation, benefit-cliff structures) that actively suppress agency. Each of these upstream constraints has its own ε value; the gap itself represents the emergent extraction that results from their cumulative effect on the pessimism-agency relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
