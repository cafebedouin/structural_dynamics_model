% ============================================================================
% CONSTRAINT STORY: sovereign_development_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_development_capacity, []).

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
 *   constraint_id: sovereign_development_capacity
 *   human_readable: Sovereign Development Capacity Constraint
 *   domain: political_economy/post_colonial_development
 *
 * SUMMARY:
 *   The sovereign development capacity constraint describes the structural
 *   limitation on policy autonomy imposed on low-income nations through
 *   international debt structures, conditionality regimes, and capital flow
 *   dependencies. A low-income nation seeking to pursue autonomous
 *   development policies—industrial policy, currency controls, public
 *   investment in sectors competing with multinational interests, land
 *   redistribution, or alternative monetary frameworks—faces material and
 *   institutional barriers enforced through debt conditionality, currency
 *   speculation, capital flight, and international credit access. The
 *   constraint exhibits the full six-type spectrum depending on structural
 *   position. From the debt-dependent nation's perspective it is a snare—no
 *   exit, maximum extraction, minimal coordination benefit. From the
 *   financial institution's perspective it is rope—coordination of debt risk
 *   and capital allocation. From domestic labor markets it is tangled
 *   rope—mixed extraction and coordination. The constraint's theater ratio
 *   (0.64) reflects that significant enforcement is symbolic: 'country
 *   ownership' rhetoric, 'participatory' structural adjustment planning, and
 *   'poverty reduction strategy' framing mask structural coercion. The
 *   measurement trajectory shows increasing extractiveness and theater over
 *   30 years, consistent with post-Washington Consensus rhetoric expansion
 *   alongside deepening debt dependency and capital mobility constraints.
 *
 * KEY AGENTS:
 *   - Low-income nation governments: Primary victim (powerless/trapped) — stripped of macroeconomic and sectoral policy autonomy; constrained by external debt service, capital flight risk, currency speculation
 *   - International financial institutions (IMF/World Bank/regional development banks): Primary beneficiary (institutional/arbitrage) — manage debt exposure, enforce policy conformity, extract rents from conditionality and technical assistance
 *   - Multinational corporations: Secondary beneficiary (powerful/arbitrage) — benefit from labor cost advantages, capital mobility, tariff reduction, intellectual property enforcement
 *   - Developed nation treasuries: Tertiary beneficiary (powerful/arbitrage) — financial sector profits, export market access, geopolitical alignment with stable capital flows
 *   - Domestic labor markets and productive capacity: Primary victim (moderate/constrained) — face wage competition, deskilling through education-export, dependence on commodity exports, vulnerability to terms-of-trade shocks
 *   - Civic coalitions for policy autonomy: Organized actor (organized/constrained) — see the constraint as blocking alternative development models; pushing for policy space and debt relief with limited success
 *   - Post-colonial institutional inheritance: Structural actor (institutional/arbitrage) — central banks, treasuries, development agencies designed in colonial or Cold War frameworks; maintain performative compliance despite seeing frameworks as outmoded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_development_capacity, 0.58).
domain_priors:suppression_score(sovereign_development_capacity, 0.68).
domain_priors:theater_ratio(sovereign_development_capacity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_development_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(sovereign_development_capacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sovereign_development_capacity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_development_capacity, tangled_rope).
narrative_ontology:human_readable(sovereign_development_capacity, "Sovereign Development Capacity Constraint").
narrative_ontology:topic_domain(sovereign_development_capacity, "political_economy/post_colonial_development").

domain_priors:requires_active_enforcement(sovereign_development_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_development_capacity, global_financial_institutions).
narrative_ontology:constraint_beneficiary(sovereign_development_capacity, multinational_corporations).
narrative_ontology:constraint_beneficiary(sovereign_development_capacity, developed_nation_treasuries).
narrative_ontology:constraint_victim(sovereign_development_capacity, low_income_nations).
narrative_ontology:constraint_victim(sovereign_development_capacity, domestic_labor_markets).
narrative_ontology:constraint_victim(sovereign_development_capacity, local_productive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-DEPENDENT NATION (SNARE) — Low-income nation trapped by structural adjustment requirements, currency dependency, and external debt service obligations. No genuine exit: capital flight, currency speculation, and international credit access are mediated through the same institutions imposing constraints. The nation's policy space is hollowed out — domestic development priorities are subordinated to creditor requirements. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(sovereign_development_capacity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC LABOR MARKET (TANGLED ROPE) — Constrained by capital flows, currency volatility, and skill-export incentives that drain human capital. The constraint coordinates labor mobility across borders and attracts foreign investment, generating some employment and technology spillovers. But extraction dominates: workers face wage competition with imported labor and remittance dependency fragments domestic productive capacity. Asymmetric extraction with genuine but unequal coordination function.
constraint_indexing:constraint_classification(sovereign_development_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GLOBAL FINANCIAL INSTITUTION (ROPE) — IMF, World Bank, regional development banks experience the constraint as pure coordination: they solve the collective action problem of managing sovereign debt exposure and facilitating capital flows. Their experience is genuinely functional and stabilizing from their position. Net beneficiary with arbitrage options (can reallocate capital, change lending terms, write down debt). Experiences the constraint as coordination mechanism.
constraint_indexing:constraint_classification(sovereign_development_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CIVIC COALITION FOR POLICY SPACE (TANGLED ROPE) — Organized agents (civil society networks, development NGOs, progressive policy coalitions) see the constraint as hybrid: it coordinates international stability and debt risk management, yet extracts by enforcing a narrow policy framework that prevents experimentation with alternative development models. The coalition experiences the constraint as having real coordination function (preventing financial chaos) alongside genuine extraction (preventing policy autonomy). Constrained exit through advocacy and norm-building.
constraint_indexing:constraint_classification(sovereign_development_capacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-COLONIAL INSTITUTIONAL FRAMEWORK (PITON) — The institutional inheritance from colonial monetary systems, commodity specialization, and international trade subordination persists through institutional inertia. The theater ratio (0.64) reflects that much of the constraint's enforcement is symbolic: sovereignty discourse, development rhetoric, country ownership language, and participatory planning theater mask the underlying structural coercion. The original coordination function (managing debt crises) has atrophied; the constraint persists because alternatives to the Bretton Woods framework have not fully displaced it. Institutional actors (central banks, treasuries) maintain performative compliance with frameworks they see as outdated.
constraint_indexing:constraint_classification(sovereign_development_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — The analytical observer at civilizational scope risks naturalizing the constraint as an immutable law: capital always flows from high-return to low-return economies, development requires integration into global markets, sovereignty is always conditional on creditor confidence. This perspective frames the constraint as emerging naturally from economic fundamentals. However, the structural data (beneficiaries, victims, enforceability) contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'economic inevitability' naturalizes what is actually a contingent institutional arrangement rooted in historical choice.
constraint_indexing:constraint_classification(sovereign_development_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_development_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereign_development_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereign_development_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_development_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereign_development_capacity, TR),
    TR >= 0.70.

:- end_tests(sovereign_development_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The constraint extracts substantial rents through debt conditionality, forces labor and capital flows advantageous to external actors, and prevents policy experimentation. However, it is not a pure snare because some genuine coordination function exists—international financial institutions do reduce systemic risk and coordinate capital allocation. The 0.58 reflects the hybrid nature: substantial extraction mixed with functional (if lopsided) coordination. Suppression (0.68): High. Barriers to exit include external debt stocks denominated in foreign currency, capital mobility that punishes policy deviation through currency crisis, dependency on commodity exports with volatile terms of trade, and loss of international credit access that enforces conformity. Physical force is rarely necessary—financial mechanisms enforce compliance. Theater ratio (0.64): Moderate-high. Much institutional activity is symbolic: poverty reduction strategy papers perform 'country ownership,' structural adjustment lists perform 'reform,' and World Bank mission statements perform 'development support' while underlying policy space is determined by creditor requirements. Theater has increased over time as post-Washington Consensus rhetoric expanded alongside actual policy constraints.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival gap between beneficiary and victim positions. The IMF/World Bank perspective (Rope) sees coordination: they are solving the collective action problem of managing sovereign debt exposure and preventing financial contagion. Their lived experience is genuinely functional—they can move capital, change lending terms, negotiate with multiple sovereigns. The low-income nation perspective (Snare) experiences maximal extraction with no coordination benefit—policy space is gone, currency stability is externally determined, development options are constrained to whatever foreign investors find profitable. The civic coalition perspective (Tangled Rope) sees both the coordination function (genuine risk management) and the extraction mechanism (enforced policy conformity). The post-colonial institutional elite perspective (Piton) sees the constraint as outmoded and performative—maintained through inertia despite being replaced by rhetorical frameworks that no longer match practice. The analytical false natural law perspective risks seeing the constraint as an immutable economic law: 'capital flows seek returns, development requires integration, sovereignty is always conditional on creditor confidence.' The engine's false summit detector identifies this as naturalization of a historical choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the extraction flow. Low-income nations as victims with trapped exit options experience maximum directionality toward extraction (d ≈ 0.90-0.95), yielding the highest f(d) and highest experienced χ. Financial institutions as beneficiaries with arbitrage options experience low directionality (d ≈ 0.10-0.15), yielding low or negative f(d). Organized civic coalitions as constrained secondary actors experience moderate directionality (d ≈ 0.50-0.60). The measurement trajectory shows increasing extraction over time: as debt stocks accumulated and terms-of-trade shocks exposed vulnerability, low-income nations became progressively more trapped. The theater ratio increase reflects institutional evolution—as raw coercion became politically costly, performative development rhetoric expanded to mask underlying constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy through structural position differentiation. There is no single 'true' type—there are six legitimate perspectival readings reflecting different structural relationships to the constraint. The mandatrophy would arise if one observer tried to claim that the constraint is simultaneously a rope (coordination function) and a snare (pure extraction) from the same perspective. The resolution is to recognize that both are true from different positions: rope from the beneficiary's structural position, snare from the victim's. The false natural law perspective (mountain) is correctly identified as a false summit by the engine—the constraint is not an immutable law of economics but a contingent institutional arrangement rooted in debt structures created by historical choice. The increasing theater ratio indicates that the constraint's enforcement is increasingly performative rather than functional, consistent with piton degradation over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_policy_space_threshold,
    'What degree of policy restriction constitutes extractive constraint versus legitimate creditor risk management?',
    'Comparative analysis of policy space across nations with different debt-to-GDP ratios and institutional arrangements; correlation between policy restriction and actual economic outcomes; case studies of nations that expanded policy space despite high debt',
    'If restriction threshold is high (80%+ policy conformity required): most debt-dependent nations are snare-trapped. If threshold is low (20% restriction): most arrangements are rope-coordinated. Classification sensitivity ≥ ±2 types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_policy_space_threshold, empirical, 'Threshold between legitimate creditor risk management and extractive policy subordination').

omega_variable(
    alternative_development_model_viability,
    'Are alternative development models (state-directed capitalism, national industrial policy, heterodox monetary frameworks) genuinely blocked by structural constraint or voluntarily rejected by policy elites?',
    'Historical case analysis (South Korea, Taiwan, Vietnam state-directed models); analysis of policy elite preferences vs institutional barriers; correlation between civic pressure for policy autonomy and adoption of alternative frameworks',
    'If truly blocked: constraint is snare (trapped). If elite-preferred: constraint is piton (performative maintenance of framework that elites support). If blocked by identity-lock: constraint is rope from elite perspective but identity_locked from civic perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_development_model_viability, empirical, 'Whether alternative development models are structurally blocked or institutionally preferred').

omega_variable(
    coordination_value_versus_extraction_asymmetry,
    'What proportion of the observed constraint is genuine coordination (preventing financial contagion, stabilizing capital flows) versus asymmetric extraction (enforcing policy conformity, concentrating benefits)?',
    'Decomposition analysis: compare outcomes under constraint vs counterfactual scenarios with alternative financial governance (regional development banks, currency unions, capital controls); measure welfare distribution across agent groups',
    'If coordination > 60% of constraint function: tangled_rope or rope classification. If extraction > 60%: snare or pure tangled_rope. Splits the constraint family if decomposition yields distinct ε values.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_value_versus_extraction_asymmetry, empirical, 'Proportion of constraint function that is coordination versus extraction').

omega_variable(
    identity_lock_in_policy_elite,
    'Are policy elites in low-income nations identity-locked into development frameworks inherited from colonial institutions, or do they face material constraints on policy autonomy?',
    'Discourse analysis of policy elite framing (do they perceive alternative models as impossible or merely risky?); measurement of actual vs perceived policy space; analysis of moments when policy elites successfully deviated from framework requirements',
    'If identity-locked: rope from elite perspective despite snare from citizen perspective. If materially constrained: snare from both perspectives. Affects perspectival gap magnitude and mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_policy_elite, conceptual, 'Whether policy elite constraint is structural or cognitive (identity-locked)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_development_capacity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdc_tr_t0, sovereign_development_capacity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sdc_tr_t15, sovereign_development_capacity, theater_ratio, 15, 0.56).
narrative_ontology:measurement(sdc_tr_t30, sovereign_development_capacity, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(sdc_be_t0, sovereign_development_capacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sdc_be_t15, sovereign_development_capacity, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(sdc_be_t30, sovereign_development_capacity, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_development_capacity, resource_allocation).
narrative_ontology:affects_constraint(sovereign_development_capacity, commodity_price_volatility).
narrative_ontology:affects_constraint(sovereign_development_capacity, human_capital_flight).
narrative_ontology:affects_constraint(sovereign_development_capacity, currency_speculative_attack).
narrative_ontology:affects_constraint(sovereign_development_capacity, intellectual_property_enforcement).

% DUAL FORMULATION NOTE:
% The sovereign development capacity constraint is decomposable into distinct structural claims with different empirical status: (1) debt-service enforcement (high confidence, ε ≈ 0.48, structural) versus (2) policy autonomy prevention (medium confidence, ε ≈ 0.62, contested); (3) capital-flow coordination (high confidence, ε ≈ 0.35, functional) versus (4) labor-market extraction (high confidence, ε ≈ 0.70, extractive). A full constraint family would separate these into distinct stories linked by affects_constraints. Current story represents integrated ε reflecting mix of all components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_development_capacity, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
