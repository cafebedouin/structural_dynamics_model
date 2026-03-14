% ============================================================================
% CONSTRAINT STORY: research_and_development_short_termism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_research_and_development_short_termism, []).

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
 *   constraint_id: research_and_development_short_termism
 *   human_readable: Research and Development Short Termism
 *   domain: innovation/economic_policy
 *
 * SUMMARY:
 *   Research and development short-termism represents a structural constraint
 *   where the incentive systems governing capital allocation, corporate
 *   performance metrics, and individual researcher careers systematically
 *   favor near-term measurable outputs over long-term breakthrough
 *   innovation. The constraint creates genuine coordination benefits
 *   (efficient capital allocation, measurable accountability, predictable
 *   resource flows) while simultaneously extracting the long-term innovation
 *   capacity of the research ecosystem. Different institutional actors
 *   experience this constraint through fundamentally different mechanisms:
 *   investors perceive pure coordination (Rope), corporate managers navigate
 *   mixed incentives (Tangled Rope), researchers face structural traps
 *   (Snare), while the future innovation system — unable to participate in
 *   current decision-making — bears the full cost. The theater ratio (0.68)
 *   reflects the pervasive use of publication counts, citation metrics, and
 *   grant success rates as proxy measures for research impact, when these
 *   metrics often substitute for actual innovation outcomes. The constraint's
 *   extractiveness has increased monotonically over the 30-year interval as
 *   financial markets have accelerated quarterly reporting cycles and
 *   shareholder return expectations have intensified pressure on corporate
 *   R&D budgets.
 *
 * KEY AGENTS:
 *   - Fundamental Researchers: Primary victim (powerless/trapped) — careers and funding tied to short-term publication cycles; cannot pursue multi-decade research programs
 *   - Long-Term Innovation Ecosystem: Structural victim (powerless/trapped at generational scale) — abstract future capacity that bears cost of foregone research but has no voice in current decisions
 *   - Quarterly-Focused Investors: Primary beneficiary (institutional/arbitrage) — benefit from predictable returns and risk reduction through short-term metrics; low extraction experience
 *   - Corporate R&D Managers: Secondary actor (powerful/constrained) — caught between institutional budget pressures and genuine research needs; experience mixed extraction and coordination benefits
 *   - Policy Reform Coalition: Organized agents (organized/constrained) — national research agencies, universities, patient capital providers building alternative funding pathways with longer horizons
 *   - Academic Institutions: Degraded institutional actor (institutional/arbitrage) — maintain research identity while increasingly dependent on metrics-driven funding; high theater ratio reflects simulation of pure research
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing capital market time horizons as immutable properties of innovation systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(research_and_development_short_termism, 0.58).
domain_priors:suppression_score(research_and_development_short_termism, 0.65).
domain_priors:theater_ratio(research_and_development_short_termism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(research_and_development_short_termism, extractiveness, 0.58).
narrative_ontology:constraint_metric(research_and_development_short_termism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(research_and_development_short_termism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(research_and_development_short_termism, tangled_rope).
narrative_ontology:human_readable(research_and_development_short_termism, "Research and Development Short Termism").
narrative_ontology:topic_domain(research_and_development_short_termism, "innovation/economic_policy").

domain_priors:requires_active_enforcement(research_and_development_short_termism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(research_and_development_short_termism, quarterly_focused_investors).
narrative_ontology:constraint_beneficiary(research_and_development_short_termism, executive_compensation_structures).
narrative_ontology:constraint_victim(research_and_development_short_termism, long_term_innovation_pipeline).
narrative_ontology:constraint_victim(research_and_development_short_termism, fundamental_research_capability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUNDAMENTAL RESEARCHER (SNARE) — Trapped by funding cycles, career metrics, and institutional pressure. Cannot pursue multi-decade research programs; must demonstrate annual progress toward publishable results. The constraint extracts decades of potential innovation capacity in exchange for precarious short-term grants. No alternative exists within the current institutional structure.
constraint_indexing:constraint_classification(research_and_development_short_termism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE INNOVATION ECOSYSTEM (SNARE) — Structural victim with no voice in current decision-making. Extraction of today's R&D resources for near-term shareholder returns degrades the foundation for tomorrow's breakthrough innovations. Cannot organize or exit; bears the full cost of foregone research capacity.
constraint_indexing:constraint_classification(research_and_development_short_termism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: QUARTERLY-FOCUSED INVESTOR (ROPE) — Experiences the constraint as pure coordination. Short-term R&D efficiency metrics align incentives across portfolio companies and analysts. Exit is trivial (reallocate capital elsewhere). Net beneficiary with low experience of extraction — the system provides arbitrage opportunity and reduces uncertainty through measurable near-term metrics.
constraint_indexing:constraint_classification(research_and_development_short_termism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CORPORATE R&D MANAGER (TANGLED ROPE) — Significant extraction but also genuine coordination benefits. Pressured to deliver quarterly results but also benefits from institutional funding, collaborative networks, and career advancement through patent portfolios. Can exit to academia or startups but at high career cost. Mixed experience of extraction and benefit.
constraint_indexing:constraint_classification(research_and_development_short_termism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POLICY REFORM COALITION (SCAFFOLD) — Organized agents (national research foundations, academic institutions, venture capital with longer horizons) recognize short-termism as a temporary institutional failure. Tax incentives, R&D credits, and long-term investment frameworks provide alternative pathways. Lower extraction because the coalition has visibility into sunset mechanisms: patient capital funds, DARPA-style mission-driven programs, and university-industry partnerships bypass quarterly metrics.
constraint_indexing:constraint_classification(research_and_development_short_termism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ACADEMIC RESEARCH INSTITUTION (PITON) — Maintains the fiction of pure knowledge pursuit while increasingly dependent on industry partnerships and grant cycles that enforce short-term metrics. The institutional identity (basic research, long-term inquiry) is eroded but the structure persists through inertia. Theater is high — grant applications, impact statements, and publication pressure simulate intellectual freedom while actual research agendas compress toward measurable outcomes.
constraint_indexing:constraint_classification(research_and_development_short_termism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAPITAL EFFICIENCY VIEW (MOUNTAIN) — From a universal analytical perspective, the constraint appears as an immutable property of capital markets: financial systems fundamentally require regular returns to allocate resources efficiently. Long-term R&D projects conflict with this basic mechanism. However, the structural data contradicts the mountain classification — historical periods with different time horizons (1950s defense R&D, pharmaceutical golden age pre-patent reform) demonstrate the constraint is contingent, not natural law. The engine identifies this as a false summit.
constraint_indexing:constraint_classification(research_and_development_short_termism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(research_and_development_short_termism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(research_and_development_short_termism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(research_and_development_short_termism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(research_and_development_short_termism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(research_and_development_short_termism, TR),
    TR >= 0.70.

:- end_tests(research_and_development_short_termism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts long-term research capacity by compressing development timelines and redirecting R&D budgets toward near-term metrics. However, the extraction is not total — some fundamental research persists through academic institutions, government funding, and patient capital sectors. The measure reflects the dominant capital allocation structure (quarterly-focused) without treating it as absolute. Suppression (0.65): High. Barriers to long-term research are substantial and multiple: funding cycles (typically 3-5 years), grant application burdens, publication pressure for career advancement, and institutional dependence on metrics-driven budgets. Researchers face genuine material constraints (no funding without measurable progress) and cognitive constraints (internalized metrics defining research value). These barriers are not insurmountable but sufficiently high that most researchers operate within them rather than against them. Theater ratio (0.68): High and increasing. Grant proposals require impact statements and commercialization potential framing (theater). Publication metrics are used as innovation proxies despite weak correlation. Progress reports measure publication and patent count, not actual breakthroughs. The theater has increased because measurement systems have proliferated while actual innovation outcomes remain hard to predict, forcing institutions to substitute easier metrics for harder outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence, with classifications ranging from pure coordination (Rope for investors) to pure extraction (Snare for future research ecosystem). The gap reveals that short-termism is not a unified phenomenon but a relational one — it is coordination from the capital allocator's perspective and extraction from the researcher's perspective. The corporate R&D manager sees both (Tangled Rope) because they occupy a mediate position. The policy coalition sees it as a temporary institutional problem with known solutions (Scaffold), while the academic institution sees its own degradation (Piton) without the analytical distance to recognize the degradation. The analytical observer's false mountain classification (naturalizing short-termism as inherent to capital markets) is the most dangerous misreading — it prevents institutional reform by framing a contingent arrangement as inevitable. This gap is diagnostic: when a constraint appears as natural law from an analytical perspective but as institutional arrangement from multiple first-order perspectives, the constraint is likely a contingent power structure being naturalized.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Investors and executives have low d (~0.15-0.25) because they benefit from the constraint's coordination function and have exit options (reallocate capital). Researchers have high d (~0.85-0.95) because they bear the suppression cost (no funding for long-term work) and have limited exit (stay in the system or leave research entirely). Corporate managers have moderate d (~0.55-0.65) because their exit options are constrained (mid-career change is costly) but they also benefit from the institutional structure (funding, status, career advancement paths). The future innovation system has maximal d (~0.98) because it is structurally absent from current decisions and has no ability to exit or negotiate. The analytical observer's perspective derives d from the universal scope and civilizational time horizon, producing a synthesis view that risk-weighs all structural positions (~0.72). Each perspective's classification follows from its d value through the sigmoid function f(d) and scope modifier σ(S).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED BY STRUCTURAL DECOMPOSITION: The mandatrophy appears as 'Is short-termism a natural property of capital markets (Mountain) or a policy choice (Tangled Rope/Snare)?' The resolution lies in recognizing that capital market structure is a choice, not a natural law. Historical periods with different time horizons (1950s-1970s corporate R&D, pharma development model pre-patent term extension, Bell Labs model) demonstrate that capital allocation can support long-term research. The constraint's persistence is not due to immutable economic laws but to the reinforcing feedback loop: short-term metrics → investor expectations → researcher pressure → metrics-driven research → short-term results, confirming the metrics' validity. Breaking the loop requires simultaneous institutional changes (patient capital mechanisms, tax incentives, ARPA-style mission funding, long-term performance metrics) that no single actor can implement unilaterally. This is the signature of a tangled_rope constraint: genuine coordination problem (efficient capital allocation requires some feedback mechanism) plus genuine extraction (current feedback mechanism privileges short-term over long-term), plus the need for active enforcement of alternatives. The mountain perspective is a false summit maintained by naturalizing institutional choices. The snare perspective is correct for those locked into the system (trapped researchers). The scaffold perspective is correct for those with organized exit pathways. The constraint is resolvable but requires institutional reform beyond individual agent choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    breakthrough_time_horizon_threshold,
    'What minimum time horizon is required for genuine breakthrough innovation versus incremental improvement?',
    'Historical analysis of major innovations (transistor, DNA sequencing, monoclonal antibodies) tracking development timeline from conception to commercial viability; categorization by breakthrough vs incremental',
    'If threshold is 5-10 years: short-termism eliminates fundamental research. If threshold is 2-3 years: much existing fundamental research is already constrained below the necessary horizon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(breakthrough_time_horizon_threshold, empirical, 'Minimum time horizon required for breakthrough innovation').

omega_variable(
    capital_allocation_mechanism_flexibility,
    'Can capital allocation systems be structurally reformed to support long-term R&D without abandoning efficient resource distribution?',
    'Comparative institutional analysis of long-horizon funding mechanisms (patient capital, endowments, sovereign wealth, ARPA models); measurement of innovation output vs capital efficiency across funding models',
    'If yes: constraint is institutional (fixable policy), snare classification too severe. If no: constraint has structural inevitability, mountain classification gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_allocation_mechanism_flexibility, conceptual, 'Whether capital systems can be reformed for long-term R&D').

omega_variable(
    spillover_benefit_measurement,
    'How much of long-term fundamental research''s value accrues to parties other than the funding institutions?',
    'Patent citation analysis, academic citations in industry, measurement of basic research applications in derivative industries; estimation of private capture vs public benefit',
    'If spillover is high (>60%): extraction measure understates harm — society loses more than investors gain. If spillover is low (<30%): investors are capturing value they could recapture through longer horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spillover_benefit_measurement, empirical, 'Magnitude of spillover benefits from fundamental research').

omega_variable(
    institutional_identity_degradation_mechanism,
    'Is the piton classification describing a genuinely degraded research institution or a necessary adaptation to resource constraints?',
    'Longitudinal tracking of research output quality, citation impact, breakthrough innovation rate in institutions with different grant pressure ratios; measurement of theater ratio (publication pressure / actual innovation impact)',
    'If degradation is genuine: piton classification is diagnostic. If adaptation is necessary: institutional identity shift is not pathological, and the constraint is more benign than the tangled_rope perspective suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_degradation_mechanism, conceptual, 'Whether piton describes degradation or necessary adaptation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(research_and_development_short_termism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rdst_tr_t0, research_and_development_short_termism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rdst_tr_t15, research_and_development_short_termism, theater_ratio, 15, 0.58).
narrative_ontology:measurement(rdst_tr_t30, research_and_development_short_termism, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(rdst_be_t0, research_and_development_short_termism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rdst_be_t15, research_and_development_short_termism, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(rdst_be_t30, research_and_development_short_termism, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(research_and_development_short_termism, resource_allocation).
narrative_ontology:boltzmann_floor_override(research_and_development_short_termism, 0.12).
narrative_ontology:affects_constraint(research_and_development_short_termism, pharmaceutical_development_timeline_compression).
narrative_ontology:affects_constraint(research_and_development_short_termism, academic_prestige_metric_inflation).
narrative_ontology:affects_constraint(research_and_development_short_termism, venture_capital_burn_rate_escalation).

% DUAL FORMULATION NOTE:
% R&D short-termism decomposes into domain-specific constraints with different ε values: pharmaceutical development (ε≈0.52, extraction of safety data completeness), academic research (ε≈0.61, extraction of fundamental research capacity), venture capital (ε≈0.38, coordination with underlying business model risk). Each domain story has different beneficiaries and victims, but all are downstream of the capital market constraint that produces quarterly reporting pressures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(research_and_development_short_termism, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
