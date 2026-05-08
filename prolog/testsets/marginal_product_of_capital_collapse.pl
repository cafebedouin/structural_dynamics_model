% ============================================================================
% CONSTRAINT STORY: marginal_product_of_capital_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marginal_product_of_capital_collapse, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marginal_product_of_capital_collapse
 *   human_readable: Marginal Product of Capital Collapse in Infrastructure-Saturated Economies
 *   domain: political_economy/development_economics/comparative_systems
 *
 * SUMMARY:
 *   The marginal product of capital (MPK) collapse in
 *   infrastructure-saturated economies presents a canonical test case for the
 *   false summit mountain (FSM) detection system. The constraint exhibits the
 *   metric profile of a natural law: very low extractiveness (0.08), minimal
 *   suppression (0.03), high accessibility collapse (0.92), low resistance
 *   (0.08), and emerges_naturally flag set to true. From the analytical
 *   observer's civilizational perspective, diminishing returns to capital
 *   appear as a mathematical necessity — production functions exhibit
 *   declining marginal products as factor inputs increase, and infrastructure
 *   saturation represents the point where additional capital yields minimal
 *   output gains. However, the constraint declares three beneficiary groups:
 *   capital goods exporters who profit from equipment sales regardless of
 *   project productivity, construction sector incumbents who capture rents
 *   from continued building activity, and development finance institutions
 *   whose loan portfolios depend on large-scale infrastructure projects. The
 *   presence of identifiable beneficiaries who gain from the constraint's
 *   persistence triggers the FSM evaluation chain: is this genuinely an
 *   immutable economic law, or is the 'natural law' framing naturalizing a
 *   contingent institutional arrangement sustained by actors who benefit from
 *   continued low-productivity investment? The empirical observables (MPK
 *   declining from 0.096 to 0.059; TFP growth falling from 3.1% to 1.1%;
 *   rising capital-output ratios) are consistent with both interpretations:
 *   genuine saturation effects would produce exactly this pattern, but so
 *   would politically-sustained misallocation where beneficiaries lobby for
 *   continued infrastructure spending beyond the point of positive returns.
 *   The omega variables document the irreducible uncertainties: saturation vs
 *   misallocation, TFP substitution viability, beneficiary influence
 *   mechanisms, and the natural law vs constructed constraint distinction.
 *   This story instantiates the FSM archetype: a constraint that passes all
 *   mountain metric gates and appears immutable from the analytical position,
 *   but whose beneficiary structure reveals potential extraction masked by
 *   naturalization.
 *
 * KEY AGENTS:
 *   - Domestic Taxpayer: Primary victim candidate (powerless/trapped) — bears fiscal cost of low-productivity infrastructure investment; cannot exit tax jurisdiction; experiences declining returns as immutable reality
 *   - Development Planner: Constrained observer (moderate/constrained) — recognizes saturation as structural limit but faces political economy barriers to shifting development strategy away from infrastructure focus
 *   - Capital Goods Exporter: Primary beneficiary (institutional/arbitrage) — profits from equipment sales regardless of project productivity; can arbitrage across multiple borrower nations; experiences constraint as coordination mechanism for predictable demand
 *   - Development Finance Institution: Primary beneficiary (institutional/arbitrage) — multilateral lenders benefit from loan volume; infrastructure projects provide large-scale, sovereign-guaranteed deployment opportunities; can shift lending across countries
 *   - Construction Sector Coalition: Mixed beneficiary (organized/constrained) — domestic construction firms and labor unions capture rents from continued spending but also solve genuine coordination problems; constrained by national market but organized for political influence
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as mathematical necessity; FSM detector evaluates whether diminishing returns framing masks beneficiary extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marginal_product_of_capital_collapse, 0.08).
domain_priors:suppression_score(marginal_product_of_capital_collapse, 0.03).
domain_priors:theater_ratio(marginal_product_of_capital_collapse, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marginal_product_of_capital_collapse, extractiveness, 0.08).
narrative_ontology:constraint_metric(marginal_product_of_capital_collapse, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(marginal_product_of_capital_collapse, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marginal_product_of_capital_collapse, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(marginal_product_of_capital_collapse, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marginal_product_of_capital_collapse, mountain).
narrative_ontology:human_readable(marginal_product_of_capital_collapse, "Marginal Product of Capital Collapse in Infrastructure-Saturated Economies").
narrative_ontology:topic_domain(marginal_product_of_capital_collapse, "political_economy/development_economics/comparative_systems").

domain_priors:emerges_naturally(marginal_product_of_capital_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marginal_product_of_capital_collapse, capital_goods_exporters).
narrative_ontology:constraint_beneficiary(marginal_product_of_capital_collapse, construction_sector_incumbents).
narrative_ontology:constraint_beneficiary(marginal_product_of_capital_collapse, development_finance_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC TAXPAYER (MOUNTAIN) — Trapped within national fiscal system; experiences declining returns on infrastructure investment as immutable economic reality. Cannot exit tax jurisdiction; perceives saturation effects as natural limit rather than policy choice.
constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEVELOPMENT PLANNER (MOUNTAIN) — Constrained by political economy of infrastructure spending; sees MPK collapse as fundamental constraint on growth strategy. High switching costs to alternative development models but recognizes saturation as structural limit.
constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL GOODS EXPORTER (ROPE) — Benefits from continued infrastructure investment regardless of productivity. Experiences constraint as coordination mechanism: standardized procurement processes, predictable demand cycles, established supply chains. Net beneficiary of investment continuation even as MPK declines.
constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVELOPMENT FINANCE INSTITUTION (ROPE) — Multilateral lenders benefit from loan volume regardless of project productivity. Experiences constraint as coordination: infrastructure lending provides stable, large-scale deployment opportunities with sovereign guarantees. Can arbitrage across borrower nations.
constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: CONSTRUCTION SECTOR COALITION (TANGLED ROPE) — Organized domestic construction firms and labor unions benefit from continued infrastructure spending but also face genuine coordination challenges (project sequencing, skill development, supply chain management). Mixed extraction and coordination: the sector captures rents from low-productivity projects while also solving real logistical problems.
constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, diminishing returns to capital are a fundamental property of production functions. As capital stock approaches optimal infrastructure density, additional investment yields declining marginal product — this is mathematical necessity, not institutional artifact. However, beneficiary presence triggers FSM evaluation: is the 'natural law' framing naturalizing what is actually sustained by identifiable actors who benefit from continued low-productivity investment?
constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marginal_product_of_capital_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marginal_product_of_capital_collapse, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(marginal_product_of_capital_collapse, ExtMetricName, E),
    domain_priors:suppression_score(marginal_product_of_capital_collapse, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(marginal_product_of_capital_collapse),
    narrative_ontology:constraint_metric(marginal_product_of_capital_collapse, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(marginal_product_of_capital_collapse, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(marginal_product_of_capital_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The base metric reflects that most of the constraint's effect is genuine coordination (infrastructure does provide public goods, even at declining marginal returns) with minimal extraction overhead. The slight extractiveness (0.08 rather than 0.05) captures the possibility that some continued investment is sustained by beneficiary influence rather than genuine public benefit, but this is a small component. The measurement trajectory shows gradual increase (0.05 → 0.08 over 30 years) as beneficiary influence potentially accumulates, but the increase is modest — consistent with a constraint that is primarily natural law with a minor extractive overlay. Suppression (0.03): Minimal. Economies are not forced to continue infrastructure investment — they have policy alternatives (human capital, institutional reform, innovation policy). The low suppression reflects that the constraint operates primarily through genuine economic limits rather than coercion. Accessibility collapse (0.92): Very high. Once infrastructure saturation is reached, the declining MPK is highly accessible to all observers — the empirical pattern is clear in national accounts data, and the diminishing returns logic is straightforward. Resistance (0.08): Very low. Minimal organized opposition to the saturation thesis — economists across theoretical traditions recognize diminishing returns as a fundamental property of production functions. Theater ratio (0.15): Low. Infrastructure investment decisions involve some performative elements (ribbon-cutting ceremonies, political credit-claiming, consultant reports justifying predetermined conclusions), but the core function (building infrastructure) is real rather than theatrical. The slight increase over time (0.10 → 0.15) reflects growing gap between projected and realized returns as saturation deepens, suggesting some projects are approved for political rather than economic reasons.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a two-tier perspectival structure characteristic of FSM candidates. Tier 1 (natural law consensus): Domestic taxpayers, development planners, and the analytical observer all classify as mountain — they perceive diminishing returns to capital as an immutable economic limit. The perspectival variation within this tier is minimal (trapped vs constrained exit, biographical vs civilizational time) but the classification is invariant. Tier 2 (beneficiary experience): Capital goods exporters, development finance institutions, and the construction sector coalition experience the constraint differently — as rope (pure coordination for the first two) or tangled rope (mixed coordination-extraction for the construction sector). These agents benefit from the constraint's persistence and have exit options (arbitrage for the first two, organized influence for the third). The gap between tiers is the FSM diagnostic signal: if the constraint is genuinely a natural law, the beneficiary experience should be incidental (they happen to profit from an immutable limit). If the constraint is a false summit, the beneficiary experience reveals the extraction mechanism (they sustain low-productivity investment through political influence, and the 'natural law' framing naturalizes this arrangement). The analytical observer's mountain classification is the hypothesis under test — the FSM detector evaluates whether the beneficiary structure and omega variable resolutions support or undermine the natural law claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality structure reveals the FSM diagnostic pattern. Domestic taxpayers are victims (bear fiscal cost of low-productivity investment) with trapped exit (cannot leave tax jurisdiction), yielding high d → high experienced extraction — but the extraction is modest in absolute terms because base ε is very low. Development planners are constrained observers with moderate power, experiencing the constraint as a structural limit with high switching costs. Capital goods exporters and development finance institutions are beneficiaries with arbitrage exit options, yielding very low d → negative experienced extraction — they profit from the constraint regardless of its productivity impact. The construction sector coalition is a mixed case: beneficiaries (capture rents from continued spending) but constrained (cannot exit national market), with organized power allowing some influence over project selection. The analytical observer at civilizational scope sees mathematical necessity (mountain), but the beneficiary declarations trigger FSM evaluation: if the constraint is genuinely immutable, why do identifiable actors benefit from its persistence? The directionality pattern is consistent with both natural law (beneficiaries happen to profit from an immutable constraint) and false summit (beneficiaries sustain the constraint through political influence). The omega variables document what evidence would distinguish these interpretations.
 *
 * MANDATROPHY ANALYSIS:
 *   FSM RESOLUTION PATTERN: This constraint resolves the mandatrophy by demonstrating that mountain classification with declared beneficiaries is not a contradiction but a diagnostic configuration. The mandatrophy question is not 'can a natural law have beneficiaries?' (yes — gravity benefits aerospace engineers) but 'does beneficiary presence indicate that the natural law framing is masking extraction?' The FSM detector evaluates this through multiple channels: (1) Beneficiary influence on constraint persistence — omega variable 'beneficiary_influence_on_investment_persistence' asks whether infrastructure investment continues beyond positive returns due to lobbying or due to genuine uncertainty about saturation thresholds. (2) Constraint mutability across institutional regimes — omega variable 'natural_law_vs_constructed_constraint' asks whether MPK collapse is invariant across development policy regimes or varies systematically with institutional arrangements. (3) Alternative pathway viability — omega variable 'alternative_growth_pathway_viability' asks whether TFP growth can substitute for capital accumulation, which would reveal policy degrees of freedom inconsistent with mountain immutability. (4) Saturation vs misallocation decomposition — omega variable 'saturation_threshold_ambiguity' asks whether the observed MPK collapse reflects genuine infrastructure density limits or politically-driven investment in low-productivity projects. If the omega resolutions support natural law (no beneficiary influence, invariant across regimes, no TFP substitution, genuine saturation), the mountain classification is confirmed and beneficiaries are incidental. If the omega resolutions support false summit (beneficiary influence detectable, varies by regime, TFP can substitute, misallocation dominates), the engine reclassifies to tangled_rope via the FSM override chain, revealing that the 'immutable economic law' framing was naturalizing a contingent institutional arrangement sustained by actors who profit from continued low-productivity investment. The current evidence is ambiguous — hence the omega variables document the irreducible uncertainties rather than pre-adjudicating the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    saturation_threshold_ambiguity,
    'Is the observed MPK collapse a genuine saturation effect (infrastructure density approaching optimal level) or a measurement artifact of misallocated investment (building the wrong infrastructure)?',
    'Decompose capital stock by infrastructure type; compare MPK trajectories for different categories (transport vs energy vs telecom). If all categories show similar collapse, saturation is genuine. If collapse is concentrated in specific categories with continued high returns elsewhere, misallocation dominates.',
    'If genuine saturation: mountain classification confirmed — diminishing returns are structural. If misallocation: the constraint is institutional (political economy of project selection) rather than natural law, and beneficiaries are extracting rents through continued low-productivity investment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(saturation_threshold_ambiguity, empirical, 'Whether MPK collapse reflects genuine saturation or misallocation').

omega_variable(
    alternative_growth_pathway_viability,
    'Can economies sustain growth through TFP improvements (innovation, institutional quality, human capital) once infrastructure saturation is reached, or does low MPK imply unavoidable growth deceleration?',
    'Cross-country comparison of high-income economies with mature infrastructure: identify cases where TFP growth accelerated as MPK declined vs cases where both declined together. Econometric decomposition of growth sources in post-saturation phase.',
    'If TFP can substitute: the constraint is less binding than mountain classification suggests — policy has degrees of freedom. If TFP cannot substitute: growth deceleration is structural, and the mountain classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_growth_pathway_viability, empirical, 'Whether TFP growth can substitute for capital accumulation post-saturation').

omega_variable(
    beneficiary_influence_on_investment_persistence,
    'Do capital goods exporters, construction sector incumbents, and development finance institutions exert political influence to sustain infrastructure investment beyond the point of positive returns, or does investment persist due to genuine uncertainty about saturation thresholds?',
    'Analysis of infrastructure project selection processes: correlation between project approval and beneficiary lobbying; comparison of ex-ante projected returns vs ex-post realized returns; identification of projects approved despite negative cost-benefit analysis. If beneficiary influence is detectable and projects systematically underperform projections, extraction mechanism is present.',
    'If beneficiary influence is significant: the ''natural law'' framing is a false summit — the constraint persists because identifiable actors benefit, not because it is immutable. If influence is minimal: continued investment reflects genuine epistemic uncertainty about saturation, and mountain classification is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_influence_on_investment_persistence, empirical, 'Whether beneficiaries sustain low-productivity investment through political influence').

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is diminishing MPK an immutable property of production functions (natural law) or a contingent outcome of institutional arrangements that favor capital accumulation over alternative growth strategies?',
    'Historical analysis of development policy regimes: compare MPK trajectories under different institutional frameworks (state-led vs market-led investment; infrastructure-focused vs human-capital-focused development strategies). If MPK collapse is invariant across regimes, natural law confirmed. If MPK trajectories differ systematically by regime, the constraint is institutional.',
    'If natural law: mountain classification is correct — no policy can avoid diminishing returns. If institutional: the constraint is constructed, beneficiaries are identifiable, and the mountain framing naturalizes a contingent arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, conceptual, 'Whether diminishing MPK is natural law or institutional artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marginal_product_of_capital_collapse, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mpk_tr_t0, marginal_product_of_capital_collapse, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mpk_tr_t10, marginal_product_of_capital_collapse, theater_ratio, 10, 0.12).
narrative_ontology:measurement(mpk_tr_t20, marginal_product_of_capital_collapse, theater_ratio, 20, 0.14).
narrative_ontology:measurement(mpk_tr_t30, marginal_product_of_capital_collapse, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(mpk_be_t0, marginal_product_of_capital_collapse, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mpk_be_t10, marginal_product_of_capital_collapse, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(mpk_be_t20, marginal_product_of_capital_collapse, base_extractiveness, 20, 0.07).
narrative_ontology:measurement(mpk_be_t30, marginal_product_of_capital_collapse, base_extractiveness, 30, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marginal_product_of_capital_collapse, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is formulated at the aggregate level (national MPK trajectory). Decomposition by infrastructure type (transport, energy, telecom, water) would yield a constraint family where each member has its own ε value reflecting sector-specific saturation dynamics. The aggregate formulation is appropriate for the natural law vs false summit question (does diminishing returns apply universally or only to specific misallocated categories?), but sector-level decomposition would be required to distinguish genuine saturation from misallocation empirically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
