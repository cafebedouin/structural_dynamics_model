% ============================================================================
% CONSTRAINT STORY: executive_compensation_incentives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_executive_compensation_incentives, []).

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
 *   constraint_id: executive_compensation_incentives
 *   human_readable: Executive Compensation Incentive Structures
 *   domain: corporate_governance/financial_extraction
 *
 * SUMMARY:
 *   Executive compensation incentive structures represent a hybrid constraint
 *   that combines genuine coordination (aligning executive effort with
 *   organizational outcomes) with systematic extraction (asymmetric transfer
 *   of value from employees and long-term stakeholders to senior executives).
 *   Over the past 40 years, the ratio of CEO-to-median-worker compensation
 *   has grown from approximately 20:1 to 300+:1, while executive productivity
 *   metrics have not increased proportionally. The constraint operates
 *   through multiple mechanisms: captured compensation committees that lack
 *   functional independence, benchmarking processes that create ratchet
 *   effects, stock-based incentives that reward short-term price increases at
 *   the expense of long-term stability, and governance structures that allow
 *   executives to substantially influence their own pay. The theater ratio
 *   has increased as the formal governance apparatus (independent directors,
 *   compensation advisors, peer benchmarking) has become more elaborate
 *   without actually constraining executive compensation growth. The
 *   constraint exhibits all perspectives from snare (powerless employees)
 *   through rope (senior executives) to false mountain (naturalized as
 *   immutable organizational necessity).
 *
 * KEY AGENTS:
 *   - Senior Executives: Primary beneficiaries (institutional/arbitrage) — design compensation structures, capture boards, extract upward through stock options and bonuses
 *   - Rank-and-File Employees: Primary victims (powerless/trapped) — wages stagnate while executives capture productivity gains; locked in by benefits and sunk costs
 *   - Mid-Managers: Secondary actors (powerful/mobile) — benefit from incentives above, extract from below, maintain hierarchical structure
 *   - Compensation Committees: Governance actors (institutional/arbitrage) — formally independent but functionally captured; maintain theater of oversight
 *   - Institutional Investors: Secondary beneficiaries (moderate/constrained) — capture some benefits through stock appreciation but lose returns to executive extraction; constrained by index obligations
 *   - Long-Term Firm Stakeholders: Victims (powerless/trapped) — bear costs of short-termism and risk externalization; cannot exit or organize
 *   - Future Economy: Intergenerational victims (powerless/trapped) — will bear costs of systemic instability created by current short-term optimization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(executive_compensation_incentives, 0.58).
domain_priors:suppression_score(executive_compensation_incentives, 0.62).
domain_priors:theater_ratio(executive_compensation_incentives, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(executive_compensation_incentives, extractiveness, 0.58).
narrative_ontology:constraint_metric(executive_compensation_incentives, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(executive_compensation_incentives, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(executive_compensation_incentives, tangled_rope).
narrative_ontology:human_readable(executive_compensation_incentives, "Executive Compensation Incentive Structures").
narrative_ontology:topic_domain(executive_compensation_incentives, "corporate_governance/financial_extraction").

domain_priors:requires_active_enforcement(executive_compensation_incentives).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(executive_compensation_incentives, senior_executives).
narrative_ontology:constraint_beneficiary(executive_compensation_incentives, compensation_committees).
narrative_ontology:constraint_beneficiary(executive_compensation_incentives, institutional_investors_holding_voting_blocks).
narrative_ontology:constraint_victim(executive_compensation_incentives, rank_and_file_employees).
narrative_ontology:constraint_victim(executive_compensation_incentives, long_term_shareholder_value).
narrative_ontology:constraint_victim(executive_compensation_incentives, firm_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MID-CAREER EMPLOYEE (SNARE) — Trapped in organization by health insurance, pension vesting, and career sunk costs. Watches executive compensation rise while their wage stagnates. No exit option that preserves accumulated benefits. Extraction is pure: executive gains are funded by wage suppression and eliminated pension obligations. Maximum experienced chi — structural immobility combined with visible asymmetry.
constraint_indexing:constraint_classification(executive_compensation_incentives, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUTURE STAKEHOLDERS / ECONOMY (SNARE) — Cannot exit because they do not yet exist. Current executive incentive structures (quarterly earnings targets, stock buybacks, risk externalization) are optimized for short-term extraction at the cost of long-term firm stability and systemic risk. The constraint operates intergenerationally: extraction today is deferred cost tomorrow. Powerless because future actors have no vote in current governance.
constraint_indexing:constraint_classification(executive_compensation_incentives, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTOR (TANGLED ROPE) — Holds voting shares and participates in compensation committee oversight. Faces asymmetry: executives design their own pay via captured boards and advisory firms; investors face costs of stock dilution and misaligned incentives. But also benefits from coordination function — compensation structure does coordinate effort allocation, even if asymmetrically extracted. Constrained by index fund obligations and fiduciary duty norms that prevent exit. Mixed extraction and coordination.
constraint_indexing:constraint_classification(executive_compensation_incentives, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SENIOR EXECUTIVE (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: compensation committee meetings, board negotiations, and incentive structures align their effort with shareholder and stakeholder interests (the stated rationale). Net flow of resources moves toward this agent. Arbitrage exit: can move to competitor firms with similar structures or to private equity with even higher extraction tolerance. Pure beneficiary perspective.
constraint_indexing:constraint_classification(executive_compensation_incentives, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MID-MANAGER CLASS (TANGLED ROPE) — Benefits from incentive structures that reward business unit performance and rank-based advancement. Also extracts from below (can demand productivity increases from direct reports). Constrained from above (executive compensation decisions are not transparent; advancement criteria shift with executive strategy changes). Mobile exit: can move to peer organizations or startups. Hybrid experience: partial beneficiary, partial victim. Moderate power allows some navigation of the structure.
constraint_indexing:constraint_classification(executive_compensation_incentives, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPENSATION COMMITTEE (PITON) — The formal governance mechanism (compensation committees, independent advisors, peer benchmarking reports) is substantially performative. Committees exist to justify decisions already made by executives in private negotiation with boards. Benchmarking against peer firms creates a ratchet effect — every firm's executives point to competitors' higher pay to demand raises. The theater persists through institutional inertia: committees are legally required and boards feel obligated to use them, even though their verification function is degraded. Theater ratio is high because the ritual maintains appearance of oversight while actual extraction decisions bypass the formal process.
constraint_indexing:constraint_classification(executive_compensation_incentives, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational frame, some executive-employee compensation asymmetry appears inherent to hierarchical organization: executives must be paid premium wages to attract talent and incentivize effort. This perspective naturalizes the constraint as immutable law of organizational economics. However, the structural data contradicts this: compensation asymmetry has grown 10x+ over 40 years while executive productivity has not; the constraint is contingent on specific governance structures (board composition, advisor networks, regulatory capture) that could be reformed. The mountain classification is a false summit — it naturalizes what comparative evidence shows is a contingent institutional arrangement. Scandinavian firms with mandatory board representation for workers and lower compensation ratios outperform equivalent US firms on long-term stability and innovation metrics, disproving the immutability claim.
constraint_indexing:constraint_classification(executive_compensation_incentives, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(executive_compensation_incentives_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(executive_compensation_incentives, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(executive_compensation_incentives, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(executive_compensation_incentives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(executive_compensation_incentives, TR),
    TR >= 0.70.

:- end_tests(executive_compensation_incentives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Executive compensation has grown substantially faster than worker compensation or firm productivity, indicating systematic value transfer. However, the constraint is not pure extraction (which would be >0.70) because executives do coordinate effort allocation — the compensation structure does align some behaviors with firm outcomes, even if captured by executive self-interest. The extractiveness measurement reflects both genuine coordination cost (~0.25) and excess capture (~0.33). Suppression (0.62): High. Multiple mechanisms prevent exit and suppress alternatives: health insurance lock-in, pension cliff effects, geographic immobility costs, non-compete clauses, at-will employment law, and labor market power asymmetry. Employees cannot organize due to union decline and anti-collective-action norms. Suppression operates intergenerationally: workers cannot credibly commit to long-term firm stability because executives' compensation incentives do not align with long-term firm value. Theater ratio (0.58): Moderate-high. Compensation committee processes, peer benchmarking studies, and independent advisor reports maintain appearance of rigorous oversight while actual extraction decisions are made through executive negotiation and board capture. The formal governance apparatus has become more elaborate over time while its verification function has degraded — classic piton signature. The measurements show theater_ratio and extractiveness both increasing in parallel, indicating that the governance ritual is becoming more elaborate even as extraction becomes more naked.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement produces radically different classifications depending on agent position. The senior executive sees pure coordination (Rope) — incentives are designed to align effort, and they experience net resource flow as compensation. The powerless employee sees pure extraction (Snare) — trapped by benefits, watching wages stagnate while executives capture productivity gains. The institutional investor sees tangled rope — participates in governance, captures some stock appreciation, but faces dilution and misaligned incentives. The compensation committee sees piton — goes through elaborate formal processes that feel important while actual extraction happens through side channels and board negotiations. The false mountain perspective naturalizes the constraint as immutable law of organizational hierarchy, but the structural data (40-year growth trend, comparative evidence from different governance models, explicit executive design of compensation structures) proves this is a contingent institutional arrangement, not an unchangeable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by structural position within the extraction flow. Senior executives as beneficiaries with arbitrage exit options (can move to any firm with similar compensation structures) derive low directionality (d ≈ 0.15) — they experience the constraint as beneficial. Powerless employees with trapped exit options (locked by benefits and sunk costs) derive high directionality (d ≈ 0.92) — they experience the constraint as bearing maximum extraction cost. Mid-managers with mobile exit (can move to peer firms or startups) derive moderate directionality (d ≈ 0.55) — they benefit from some extraction while being extracted from above. Institutional investors with constrained exit (index fund obligations prevent moving capital) derive moderate-high directionality (d ≈ 0.68) — they face extraction through dilution despite governance participation. The compensation committee members, though institutional actors with arbitrage options, are partially captured by executives, so their directionality is higher than a purely beneficiary institutional actor would suggest (d ≈ 0.35 instead of 0.15). These directionality values drive the perspectival gap: high-d agents classify as Snare, moderate-d agents as Tangled Rope, low-d beneficiary agents as Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by distinguishing the genuine coordination function (aligning executive effort with organizational outcomes) from the pure extraction mechanism (capturing surplus through asymmetric governance). The Tangled Rope classification holds at the claimed level because (1) authentic coordination occurs — compensation structures do incentivize effort and performance feedback; (2) asymmetric extraction is real — executives have substantially more influence over pay outcomes than workers; (3) active enforcement is required — compensation committees, board oversight, and advisory firm networks maintain the structure. The snare perspectives (powerless employees, future stakeholders) reveal that the coordination narrative masks extraction dynamics: the constraint could align incentives much more symmetrically (as Scandinavian models show), but the current design is optimized to maximize executive extraction within the coordination frame. The piton perspective on compensation committees shows that the formal governance apparatus has degraded from verification mechanism to theatrical justification. The false mountain perspective is correctly identified as false: the 10x growth in CEO-to-worker ratios over 40 years while productivity gains have plateaued proves the constraint is contingent on specific governance choices, not an immutable law. The mandatrophy is resolved by mapping the coordinate system: from powerless/trapped perspectives, the constraint appears as snare; from institutional/arbitrage perspectives, as rope; from moderate/constrained perspectives, as tangled rope. The system is not self-contradictory — it is multi-perspectival. The error is claiming any single perspective is universal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    talent_premium_necessity,
    'What portion of executive compensation growth reflects genuine competitive talent premium versus pure extraction enabled by governance failure?',
    'Comparative analysis of firm performance and executive talent retention under different compensation structures (US vs Scandinavian governance models); executive mobility and recruitment success under compressed wage ratios; performance metrics before and after compensation cuts during crises (2008-09, 2020-21)',
    'If premium is real (>30% of current gap): partial justification for rope classification. If negligible (<10%): compensation structure is pure snare, suppression of wage competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_premium_necessity, empirical, 'Proportion of compensation growth due to genuine talent scarcity vs governance failure').

omega_variable(
    incentive_alignment_efficacy,
    'Do stock-based and performance-based compensation schemes actually align executive behavior with long-term shareholder and stakeholder value, or do they create short-termism and risk externalization incentives?',
    'Longitudinal analysis of firm performance metrics (stock price vs operational efficiency, dividend stability, R&D investment, labor productivity, safety records) before and after major compensation structure changes; comparison of executive behavior under variable vs fixed compensation; analysis of executive decision patterns when personal stake aligns vs conflicts with firm stability',
    'If alignment is real: compensation is genuine coordination mechanism (Rope). If incentives misalign: compensation structure is extraction mechanism that uses coordination framing as cover (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_alignment_efficacy, empirical, 'Whether stock-based compensation aligns executive behavior with firm value').

omega_variable(
    compensation_committee_independence,
    'Are compensation committees genuinely independent oversight bodies, or are they captured by the executives they are supposed to oversee?',
    'Analysis of committee composition, advisor firm conflicts of interest, correlation between committee decisions and executive negotiation outcomes, voting records on compensation proposals, rate of compensation overrides vs committee recommendations, financial ties between advisors and executive candidates',
    'If independent: committee provides genuine coordination function, reducing theater ratio and snare classification. If captured: committees are pure performance theater, increasing theater ratio and entrenching snare dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compensation_committee_independence, empirical, 'Whether compensation committees are functionally independent').

omega_variable(
    systemic_risk_externalization,
    'Do executive compensation structures that reward short-term stock price increases create systemic financial risk that is externalized to employees, creditors, and the broader economy?',
    'Historical analysis of executive compensation vs firm leverage, risk-taking behavior, and bankruptcy rates; correlation between executive compensation structures and financial crisis participation; measurement of employee layoff frequency and wage suppression during executive compensation growth periods',
    'If externalization is significant: constraint represents intergenerational snare (future bears costs of current extraction). If minimal: constraint is pure tangled rope coordination with asymmetric benefit distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_risk_externalization, empirical, 'Degree of systemic financial risk externalization via compensation incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(executive_compensation_incentives, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exec_comp_tr_t0, executive_compensation_incentives, theater_ratio, 0, 0.35).
narrative_ontology:measurement(exec_comp_tr_t20, executive_compensation_incentives, theater_ratio, 20, 0.48).
narrative_ontology:measurement(exec_comp_tr_t40, executive_compensation_incentives, theater_ratio, 40, 0.58).
narrative_ontology:measurement(exec_comp_tr_t10, executive_compensation_incentives, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(exec_comp_be_t0, executive_compensation_incentives, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(exec_comp_be_t20, executive_compensation_incentives, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(exec_comp_be_t40, executive_compensation_incentives, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(exec_comp_be_t10, executive_compensation_incentives, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(executive_compensation_incentives, resource_allocation).
narrative_ontology:boltzmann_floor_override(executive_compensation_incentives, 0.18).
narrative_ontology:affects_constraint(executive_compensation_incentives, labor_wage_suppression).
narrative_ontology:affects_constraint(executive_compensation_incentives, corporate_short_termism).
narrative_ontology:affects_constraint(executive_compensation_incentives, shareholder_primacy_doctrine).

% DUAL FORMULATION NOTE:
% Executive compensation incentive structures are downstream of shareholder primacy doctrine and upstream of specific labor extraction mechanisms (wage suppression, benefit elimination, outsourcing). The 0.58 extractiveness value reflects both the coordination function (aligning effort) and the asymmetric capture mechanism (allowing executives to design their own pay). Sister constraints in the family operate at higher extractiveness (shareholder primacy: 0.70+) or lower extractiveness (labor wage suppression viewed as isolated wage dynamics: 0.42), but executive compensation is the coordinating mechanism that transmits extraction from shareholders to workers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(executive_compensation_incentives, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
