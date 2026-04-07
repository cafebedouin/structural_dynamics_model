% ============================================================================
% CONSTRAINT STORY: intergenerational_resource_transfer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intergenerational_resource_transfer, []).

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
 *   constraint_id: intergenerational_resource_transfer
 *   human_readable: Intergenerational Resource Transfer Asymmetries
 *   domain: economic/social/demographic
 *
 * SUMMARY:
 *   Intergenerational resource transfer operates as a tangled
 *   coordination-extraction hybrid that naturalizes wealth inequality as
 *   family succession while simultaneously degrading the institutional
 *   safeguards (estate taxes, inheritance regulations) intended to mitigate
 *   extreme concentration. The constraint exhibits all six classification
 *   types from different temporal and power perspectives. For powerless
 *   agents without inherited assets, the constraint appears as an inescapable
 *   snare locking them into disadvantage across their biographical lifetime.
 *   For wealth holders, it appears as pure coordination — the legitimate
 *   problem of planning succession and family governance. For organized
 *   working-class cohorts, it appears as tangled rope: pension systems
 *   provide genuine intergenerational coordination while capital gains taxes
 *   incentivize wealth holders to extract through inheritance rather than
 *   wages. The tax and estate law system appears degraded (piton),
 *   maintaining theatrical regulation while its enforcement mechanisms have
 *   atrophied through exemption threshold increases, stepped-up basis
 *   provisions, and dynasty trust proliferation. The analytical observer
 *   risks perceiving intergenerational inequality as a law of nature
 *   (mountain) — parents naturally transfer advantages — but the escalating
 *   extractiveness over the 40-year measurement window reveals this as
 *   naturalization of contingent institutional design choices.
 *
 * KEY AGENTS:
 *   - Wealth Holders: Primary beneficiaries (powerful/arbitrage) — capture tax-advantaged asset appreciation and legal structures for succession planning
 *   - Non-Inheritors: Primary victims (powerless/trapped) — face compounded disadvantage with no exit from birth cohort timing; higher borrowing costs, lower starting capital
 *   - Young Inheritors: Secondary beneficiary-victim (moderate/constrained) — experience both genuine intergenerational coordination and identity fusion with family wealth expectations
 *   - Working-Class Cohorts: Organized victims (organized/constrained) — maintain political voice and organizational capacity but constrained by power asymmetry in wealth concentration
 *   - Tax and Estate Law System: Institutional actor (institutional/constrained) — maintains formal regulatory structures while enforcement and intent have degraded
 *   - Future Generations: Implicit victims (analytical/trapped) — bear costs of environmental extraction, debt accumulation, and depleted intergenerational commons
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intergenerational_resource_transfer, 0.52).
domain_priors:suppression_score(intergenerational_resource_transfer, 0.48).
domain_priors:theater_ratio(intergenerational_resource_transfer, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intergenerational_resource_transfer, extractiveness, 0.52).
narrative_ontology:constraint_metric(intergenerational_resource_transfer, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(intergenerational_resource_transfer, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intergenerational_resource_transfer, tangled_rope).
narrative_ontology:human_readable(intergenerational_resource_transfer, "Intergenerational Resource Transfer Asymmetries").
narrative_ontology:topic_domain(intergenerational_resource_transfer, "economic/social/demographic").

domain_priors:requires_active_enforcement(intergenerational_resource_transfer).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intergenerational_resource_transfer, wealth_holders).
narrative_ontology:constraint_beneficiary(intergenerational_resource_transfer, inheritance_recipients).
narrative_ontology:constraint_victim(intergenerational_resource_transfer, non_inheritors).
narrative_ontology:constraint_victim(intergenerational_resource_transfer, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-INHERITOR (SNARE) — Faces structural barriers to wealth accumulation while peers inherit substantial assets. No exit option from this positioning; generational cohort cannot escape the timing disadvantage. Maximum suppression due to interlocking barriers: lower starting capital, higher cost of borrowing, wage stagnation, and housing market inflation driven by inherited wealth.
constraint_indexing:constraint_classification(intergenerational_resource_transfer, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WEALTH HOLDER (ROPE) — Experiences intergenerational transfer as pure coordination: legally structuring succession, tax planning, and family governance are coordination problems enabling asset preservation. Extraction runs toward this agent; they perceive no significant suppression. Maximum exit optionality through trusts, structures, and jurisdictional arbitrage.
constraint_indexing:constraint_classification(intergenerational_resource_transfer, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: YOUNG INHERITOR (TANGLED ROPE) — Experiences both genuine coordination (family resource planning, intergenerational knowledge transfer) and asymmetric extraction (capital gains are tax-advantaged relative to labor income; asset appreciation outpaces wage growth). Constrained by family expectations and identity fusion with family wealth; also benefits from inherited advantages.
constraint_indexing:constraint_classification(intergenerational_resource_transfer, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WORKING-CLASS COHORT (GENERATIONAL HORIZON) (TANGLED ROPE) — Organized agents (labor unions, housing advocates, wealth tax coalitions) perceive both genuine coordination (intergenerational pension systems, social security) and extraction (inherited wealth concentrates advantage in assets while wages stagnate). Constrained by political power asymmetry but maintain exit-option through organization and political voice.
constraint_indexing:constraint_classification(intergenerational_resource_transfer, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TAX AND ESTATE LAW SYSTEM (PITON) — The institutional infrastructure (estate taxes, inheritance laws, trusts) is theatrically maintained as if regulating wealth transfer but has been progressively degraded through loopholes, stepped-up basis, dynasty trusts, and exemption threshold increases. The system performs regulation while enabling extraction. Theater ratio of 0.55 reflects this degradation: formal law exists but enforcement and intent have hollowed out.
constraint_indexing:constraint_classification(intergenerational_resource_transfer, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, some intergenerational inequality is inherent: parents naturally transfer advantages to children, and this creates unequal starting positions. This perspective sees the constraint as an immutable property of human social reproduction. However, the base properties contradict mountain classification — the level of extraction and institutional enforcement reveal this as naturalization of a contingent institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(intergenerational_resource_transfer, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intergenerational_resource_transfer_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intergenerational_resource_transfer, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intergenerational_resource_transfer, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intergenerational_resource_transfer, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intergenerational_resource_transfer, TR),
    TR >= 0.70.

:- end_tests(intergenerational_resource_transfer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. The constraint extracts through multiple mechanisms: inherited wealth provides compounding advantage unavailable to non-inheritors, capital gains taxation is more favorable than wage income taxation, and asset appreciation outpaces wage growth. The escalation over the 40-year interval from 0.32 to 0.52 reflects regulatory capture of estate law — exemption thresholds have increased faster than median wealth, dynasty trusts bypass generational restrictions, and stepped-up basis provisions eliminate capital gains tax on appreciation before transfer. Suppression (0.48): Moderate. Barriers to non-inheritor wealth accumulation include lower starting capital, higher borrowing costs, opportunity costs of credential-seeking versus inherited asset ownership, and lifestyle inflation from inherited advantages in peer groups. Not total suppression — some non-inheritors do accumulate wealth through exceptional earnings or entrepreneurship. Theater ratio (0.55): Moderate-high. Estate tax laws exist and are formally enforced but apply to fewer estates each cycle as exemptions increase; trusts provide structures that appear to regulate wealth while enabling intergenerational concentration. The theater reflects the gap between statutory law and actual enforcement — for most high-net-worth individuals, estate planning has become routine tax avoidance rather than regulation-constrained succession.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap reflects power asymmetry in how intergenerational transfer is experienced and justified. Wealth holders see coordination — the legitimate problem of family succession planning, tax-efficient asset transfer, and preservation of family enterprises. They point to genuine coordination benefits: estate planning enables smooth intergenerational knowledge transfer, family businesses maintain employment, and testamentary freedom respects autonomous choice. Non-inheritors see extraction — systematic advantage accumulation by birth into wealthy families, compounding inequality, and degraded opportunities for wealth building. They point to genuine extraction mechanisms: inherited assets provide starting capital that borrowing cannot replicate, capital gains taxation favors appreciation over wages, and peer effects create lifestyle inflation that reduces savings rates for aspirational non-inheritors. The analytical observer sees a natural law (mountain) — intergenerational transfer is inherent to human kinship. But the escalating extractiveness (0.32 → 0.52 over 40 years) and the theatrical degradation of regulatory safeguards reveal this as naturalization of institutional choices. The mountain classification is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies significantly by perspective. Wealth holders (beneficiary status + arbitrage exit) experience d ≈ 0.05-0.15 (near-full beneficiary, maximum extraction flows toward them). Non-inheritors (victim status + trapped exit) experience d ≈ 0.90-0.98 (near-full target, maximum extraction flows away from them). Young inheritors (mixed status + constrained exit) experience d ≈ 0.45-0.55 (bifurcated: they benefit from inherited assets but are constrained by family identity expectations and expectations to preserve/grow wealth). Working-class cohorts (victim status + constrained/organized exit) experience d ≈ 0.65-0.75 (high extraction but with organizational capacity to resist or advocate). The institutional tax system (beneficiary of current regime + constrained exit) experiences d ≈ 0.35-0.45 (benefits from complexity that justifies its existence, but constrained by political pressure for reform). This variation in d across perspectives produces the perspectival gap: beneficiaries perceive coordination (rope or low-extraction tangled rope) while victims perceive extraction (snare or high-extraction tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine intergenerational coordination from extraction enabled by coordination mechanisms. Genuine coordination includes: pension system design (current workers fund current retirees; future workers will fund future retirees), knowledge transfer from elder to younger generation, family business continuity, and testamentary freedom to pass assets to chosen heirs. Extraction mechanisms that have captured the coordination functions include: stepped-up basis (eliminates capital gains tax on appreciated assets), unlimited spousal transfers (enables Dynasty Trust structures), exemption thresholds (allow indefinite wealth accumulation in trusts before taxation), and complexity loopholes (estate planning becomes industry dedicated to tax avoidance). The tangled rope classification holds: the constraint genuinely coordinates intergenerational resource flows AND enables asymmetric extraction of those flows toward high-net-worth beneficiaries. The constraint cannot be resolved into pure rope (genuine coordination) because the extraction mechanisms are baked into the coordination structures — they are not separable. The theater ratio (0.55) reflects that much of the coordination apparatus has become performative: the formal regulation (estate taxes, inheritance laws) is maintained, but actual extraction is substantially unregulated through legal structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inheritance_vs_lifetime_accumulation_ratio,
    'What fraction of wealth inequality is attributable to inherited wealth versus lifetime accumulation differentials?',
    'Longitudinal wealth tracking studies; decomposition of wealth Gini coefficient into inherited and acquired components; cross-generational mobility analysis',
    'If inherited wealth > 60% of inequality: constraint is primarily extractive redistribution across generations. If inherited wealth < 30%: much of inequality reflects lifetime earning differentials and constraint is more coordination-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inheritance_vs_lifetime_accumulation_ratio, empirical, 'Proportion of wealth inequality from inheritance versus lifetime accumulation').

omega_variable(
    intergenerational_mobility_causality,
    'Does low social mobility result from inherited disadvantage (constraint-driven) or from differences in human capital, motivation, and institutional quality?',
    'Natural experiments with exogenous wealth shocks (inheritance, lottery, policy changes); instrumental variable analysis of inheritance amounts and outcomes; cross-country comparison controlling for human capital',
    'If mobility varies primarily with inheritance amounts: constraint is primary extraction mechanism (Snare from more perspectives). If mobility reflects opportunity structure independent of family wealth: constraint is secondary to other institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_mobility_causality, empirical, 'Whether low mobility is caused by inheritance inequality or other factors').

omega_variable(
    estate_law_enforcement_gap,
    'Is the degradation of estate law enforcement intentional policy or unintended consequence of complexity and political capture?',
    'Historical analysis of estate tax exemption changes and legislative intent; IRS enforcement resource allocation over time; comparison of tax avoidance rates before and after major legal changes',
    'If intentional policy: piton classification confirmed — system is theatrically maintained. If unintended complexity: constraint may be better classified as Scaffold with imperfect sunset (tax law intended to function but degraded).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(estate_law_enforcement_gap, conceptual, 'Whether estate law degradation is intentional or unintended consequence').

omega_variable(
    future_generation_discount_rate,
    'At what discount rate does intergenerational extraction become morally equivalent to intergenerational coordination?',
    'Ethical analysis of discounting future welfare; empirical measurement of actual intergenerational tradeoffs (e.g., environmental extraction, debt accumulation, infrastructure investment); cross-temporal welfare comparison',
    'If discount rate is zero: current extraction from future generations is equivalent to theft. If positive: some extraction is acceptable as tradeoff for current investment. This determines whether future_generations should be classified as victim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_discount_rate, preference, 'Moral discount rate for intergenerational resource extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intergenerational_resource_transfer, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igrt_tr_t0, intergenerational_resource_transfer, theater_ratio, 0, 0.35).
narrative_ontology:measurement(igrt_tr_t20, intergenerational_resource_transfer, theater_ratio, 20, 0.48).
narrative_ontology:measurement(igrt_tr_t40, intergenerational_resource_transfer, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(igrt_be_t0, intergenerational_resource_transfer, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(igrt_be_t20, intergenerational_resource_transfer, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(igrt_be_t40, intergenerational_resource_transfer, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intergenerational_resource_transfer, resource_allocation).
narrative_ontology:affects_constraint(intergenerational_resource_transfer, wealth_inequality_accumulation).
narrative_ontology:affects_constraint(intergenerational_resource_transfer, housing_market_affordability).
narrative_ontology:affects_constraint(intergenerational_resource_transfer, educational_opportunity_access).
narrative_ontology:affects_constraint(intergenerational_resource_transfer, pension_system_solvency).

% DUAL FORMULATION NOTE:
% Intergenerational resource transfer decomposes into multiple structurally distinct constraints: (1) the coordination problem of succession planning and pension systems (ε ≈ 0.15, Rope), (2) the extraction mechanism of tax-advantaged wealth transfer (ε ≈ 0.52, Tangled Rope or Snare depending on power perspective), and (3) the institutional degradation of regulatory safeguards (ε ≈ 0.55, Piton). These three stories share the same domain but have distinct base properties and measurement dynamics. This story captures the aggregate constraint. Decomposition is possible for domain-specific analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intergenerational_resource_transfer, powerful, 0.1).
constraint_indexing:directionality_override(intergenerational_resource_transfer, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
