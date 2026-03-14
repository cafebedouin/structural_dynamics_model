% ============================================================================
% CONSTRAINT STORY: victorian_advertising_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_victorian_advertising_market, []).

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
 *   constraint_id: victorian_advertising_market
 *   human_readable: Victorian Advertising Market Constraint
 *   domain: economic/media/social_history
 *
 * SUMMARY:
 *   The Victorian advertising market (1837–1901) represents a constraint
 *   where newspapers, advertisers, and consumers negotiate the terms of
 *   information distribution and commercial persuasion in an environment of
 *   limited regulatory oversight and asymmetric literacy. The constraint
 *   exhibits genuine coordination value (newspapers need revenue, merchants
 *   need distribution, consumers gain access to product information)
 *   alongside significant extraction (false health claims, unverifiable
 *   quality assertions, intentional deception targeting the less educated).
 *   The extractiveness value (0.58) reflects that the market simultaneously
 *   solves a real collective action problem (how to fund newspapers serving
 *   mass audiences) and enables systematic overreach (unregulated health
 *   claims that cause documented harm). The suppression value (0.52) captures
 *   the structural barriers to exit: working-class consumers cannot easily
 *   verify claims, no regulatory body enforces truth standards, and
 *   alternative information sources are scarce. Theater ratio (0.68) reflects
 *   that the legitimacy of the advertising market rests heavily on ritualized
 *   displays of journalistic integrity, editorial selectivity, and implied
 *   advertiser vetting — performances that are often inconsistent with
 *   economic reality.
 *
 * KEY AGENTS:
 *   - Working-Class Consumers: Primary victims (powerless/trapped) — vulnerable to health fraud due to literacy barriers and absence of verification mechanisms; wages diverted to ineffective remedies
 *   - Middle-Class Readers: Secondary victims (moderate/constrained) — educated but still subject to sophisticated appeals; can identify some fraud but constrained by switching costs
 *   - Newspaper Proprietors: Primary beneficiaries (institutional/arbitrage) — depend on advertising revenue for profitability; experience constraint as coordination mechanism enabling cheap mass press
 *   - Patent Medicine Manufacturers: Beneficiaries with mixed extraction (powerful/mobile) — benefit from unverifiable health claims but also provide products with modest therapeutic effects; mobile enough to diversify channels
 *   - Advertising Agencies: Emerging intermediaries (institutional/arbitrage) — commission-driven; incentive to maximize advertiser claims regardless of truth
 *   - Editorial Standards: Idealized but degraded institutional function (institutional/arbitrage) — perform selectivity while economically dependent on advertiser revenue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(victorian_advertising_market, 0.58).
domain_priors:suppression_score(victorian_advertising_market, 0.52).
domain_priors:theater_ratio(victorian_advertising_market, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(victorian_advertising_market, extractiveness, 0.58).
narrative_ontology:constraint_metric(victorian_advertising_market, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(victorian_advertising_market, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(victorian_advertising_market, tangled_rope).
narrative_ontology:human_readable(victorian_advertising_market, "Victorian Advertising Market Constraint").
narrative_ontology:topic_domain(victorian_advertising_market, "economic/media/social_history").

domain_priors:requires_active_enforcement(victorian_advertising_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(victorian_advertising_market, newspaper_proprietors).
narrative_ontology:constraint_beneficiary(victorian_advertising_market, advertising_agencies).
narrative_ontology:constraint_beneficiary(victorian_advertising_market, luxury_goods_merchants).
narrative_ontology:constraint_victim(victorian_advertising_market, working_class_consumers).
narrative_ontology:constraint_victim(victorian_advertising_market, advertising_truth_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DECEIVED WORKER (SNARE) — Working-class consumers cannot verify advertising claims (literacy barriers, no regulatory body, no alternative information source). Faces maximum extraction: wages diverted to ineffective patent medicines and fraudulent schemes advertised in newspapers. No exit option — consumer credit systems lock them into purchasing recommended products. Theater is high (elaborate testimonials, medical authority simulation) but functional extraction is severe.
constraint_indexing:constraint_classification(victorian_advertising_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS READER (TANGLED ROPE) — Partially mobile and educated; can recognize some fraudulent claims but remains vulnerable to sophisticated appeals (credible-looking medical testimonials, class-aspirational marketing). Experiences genuine coordination benefit (newspapers provide product information, market pricing signals) alongside asymmetric extraction (advertising premium embeds unverifiable quality claims). Constrained exit: switching newspapers is costly and competitors use same advertising model.
constraint_indexing:constraint_classification(victorian_advertising_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEWSPAPER PROPRIETOR (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences advertising market as pure coordination: advertisements fund newspaper operations, readers benefit from cheap press, advertisers reach consumers. Proprietor has arbitrage option (can exit to alternative revenue models — subscriptions, political patronage). The constraint solves their genuine business problem: how to finance expanding circulation. Extraction is incidental to coordination function.
constraint_indexing:constraint_classification(victorian_advertising_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PATENT MEDICINE MANUFACTURER (TANGLED ROPE) — Powerful actor (mobile) with high exit options — can switch advertising channels, reduce advertising spend, compete on price. Yet benefits from genuine coordination function: advertising creates markets for products that have real (if modest) efficacy for common ailments. Extraction mechanism is asymmetric: manufacturers extract via unverifiable health claims while consumers extract real value (placebo + minor therapeutic effect). Theater is high (elaborate formulations, medical testimonials) but both sides benefit from the market coordination.
constraint_indexing:constraint_classification(victorian_advertising_market, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JOURNALISTIC ETHICS NORM (PITON) — Ideal of editorial independence from advertiser pressure is theoretically present (some editors refuse obviously fraudulent ads) but functionally degraded. Theater ratio high: publishers perform editorial integrity (selective ad rejection, implicit vouching) while economic reality is full dependence on ad revenue. The norm persists through institutional inertia despite systematic violation — replaced later by advertising codes and regulatory oversight.
constraint_indexing:constraint_classification(victorian_advertising_market, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, information asymmetry between advertiser and consumer is a structural feature of all markets where quality cannot be inspected before purchase. The Victorian advertising market is thus an expression of an inherent constraint: unverifiable claims always have extraction potential in asymmetric information environments. However, this perspective risks naturalizing what is actually a contingent institutional choice — the absence of regulatory verification, advertising standards, and consumer legal recourse.
constraint_indexing:constraint_classification(victorian_advertising_market, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(victorian_advertising_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(victorian_advertising_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(victorian_advertising_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(victorian_advertising_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(victorian_advertising_market, TR),
    TR >= 0.70.

:- end_tests(victorian_advertising_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The market has genuine coordination value — advertising funds newspaper expansion, merchants reach new customers, consumers access product information. But extraction is substantial: patent medicines often contain inert or toxic ingredients marketed with false health claims; working-class wages are diverted to ineffective remedies; truth-in-advertising norms are absent. The value reflects that extraction is neither maximal (some products work, some claims are legitimate) nor minimal (systematic deception of less educated consumers). Suppression (0.52): Significant barriers to consumer exit include literacy gaps, no legal recourse for fraudulent claims, limited alternative information sources, and working-class economic dependence on recommended products. Barriers are not total (middle-class readers can identify fraud) but are structural and intentionally maintained (complex medical language, appeals to false authority). Theater ratio (0.68): The legitimacy of the Victorian advertising market depends on ritualized signals of editor integrity — explicit rejection of 'obviously fraudulent' ads, implicit vouching for advertiser credibility, and performance of ethical discrimination. These signals persist despite economic dependence on advertiser revenue. Theater increases over the interval as competitive pressure drives increasingly elaborate advertising formats (medical testimonials, doctor endorsements, scientific-looking testimonies).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates divergent classification across structural positions. Proprietors experience coordination (Rope) — advertising solves their genuine revenue problem. Patent medicine manufacturers experience mixed extraction-coordination (Tangled Rope) — unverifiable claims create markets while some products have real effects. Middle-class readers also experience tangled dynamics (Tangled Rope) — some information benefit, some deception cost. Working-class consumers experience pure extraction (Snare) — maximum asymmetry, literacy barriers, no verification mechanism. The editorial ethics norm is institutionally present but functionally degraded (Piton) — selective ad rejection persists as performance despite economic dependence. The analytical observer risks seeing information asymmetry as a law of markets (Mountain) but this naturalizes the contingent choice to create no regulatory body, no truth standards, and no legal recourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's structural position relative to extraction flow. Newspaper proprietors are beneficiaries with arbitrage options (low d, negative chi) — the constraint solves their fundamental business problem. Patent medicine manufacturers are beneficiaries with mobility (moderate d, moderate chi) — they extract through false claims but also provide products with real effects. Working-class consumers are victims with no exit (high d, high chi) — they bear maximum extraction through literacy barriers and economic dependence. Middle-class readers are victims with constrained exit (moderate-high d, moderate chi) — educated enough to recognize some fraud but locked into the advertising-dependent newspaper model. The proprietor's arbitrage exit option (could theoretically fund newspapers through subscriptions or patronage) is rarely exercised, indicating the constraint has structural advantages — advertising revenue is more profitable than alternatives, making the proprietor's nominal exit option functionally unavailable.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint's classification depends on which agent's structural position is being measured. The proprietor legitimately experiences coordination (Rope) — advertising funds newspaper operations that serve the public good of information distribution. The working-class consumer legitimately experiences extraction (Snare) — systematic deception through literacy asymmetry. Neither is 'the real' classification; both are true. The tangled_rope classification at the moderate (middle-class) level represents the modal experience — genuine coordination benefits alongside extractive deception. The piton classification recognizes that editorial ethics persists as performance rather than function. The mountain classification is a false summit: information asymmetry is not inherent to all markets, but rather a consequence of the specific choice to create no regulatory body and no legal accountability for false health claims. The constraint dissolves as a mountain when regulatory frameworks (the Food and Drugs Act [1906], subsequent advertising codes) are established — what appeared inherent to the market was actually a contingent institutional absence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_barrier_quantification,
    'What proportion of suppression is literacy-driven (unable to read fine print, medical terminology) versus institutional (regulatory absence)?',
    'Comparison of claim-verification rates between literate and illiterate cohorts; analysis of how technical language in advertisements targeted different classes',
    'If mostly literacy: suppression is endogenous to consumer education, not extractive institutional design. If mostly institutional: suppression is deliberately maintained through complex language and regulatory gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_barrier_quantification, empirical, 'Proportion of suppression attributable to literacy barriers vs. institutional absence').

omega_variable(
    patent_medicine_efficacy_distribution,
    'What proportion of advertised patent medicines had detectable therapeutic effect (beyond placebo) versus pure fraud?',
    'Chemical analysis of preserved patent medicine formulations; historical clinical trial data where available; correlation between advertised ingredients and pharmacologically active compounds',
    'If high efficacy: extraction is partial (consumers receive real value alongside premium pricing). If low efficacy: extraction is near-total (premium pricing for inert or harmful substances). Affects whether tangled_rope classification holds or degrades toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_medicine_efficacy_distribution, empirical, 'Therapeutic efficacy distribution of advertised patent medicines').

omega_variable(
    newspaper_revenue_alternative_viability,
    'Could Victorian newspapers have sustained operations without advertising revenue through subscriptions, political patronage, or other models?',
    'Financial reconstruction of non-advertising-dependent newspapers (party organs, subscription journals); modeling of circulation costs and pricing elasticity',
    'If viable alternatives existed: advertising market is extractive choice by proprietors, not structural necessity (snare classification stronger). If no alternatives: advertising market is genuine coordination necessity (rope classification more appropriate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(newspaper_revenue_alternative_viability, empirical, 'Viability of non-advertising newspaper revenue models').

omega_variable(
    consumer_awareness_of_fraud,
    'What proportion of working-class consumers believed advertised patent medicine claims versus recognized them as exaggerated or false?',
    'Analysis of working-class diaries, letters, trial testimony; correlation between repeated advertising and consumer purchasing patterns; identification of naive vs. sophisticated repeat purchasers',
    'If high awareness: suppression is lower (constrained choice), extraction is acknowledged. If low awareness: suppression is higher (trapped), extraction operates through deception.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_awareness_of_fraud, empirical, 'Consumer awareness of advertising fraud').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(victorian_advertising_market, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vict_tr_t0, victorian_advertising_market, theater_ratio, 0, 0.52).
narrative_ontology:measurement(vict_tr_t20, victorian_advertising_market, theater_ratio, 20, 0.64).
narrative_ontology:measurement(vict_tr_t40, victorian_advertising_market, theater_ratio, 40, 0.68).
narrative_ontology:measurement(vict_tr_t60, victorian_advertising_market, theater_ratio, 60, 0.71).

% Extraction over time
narrative_ontology:measurement(vict_be_t0, victorian_advertising_market, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vict_be_t20, victorian_advertising_market, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(vict_be_t40, victorian_advertising_market, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(vict_be_t60, victorian_advertising_market, base_extractiveness, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(victorian_advertising_market, resource_allocation).
narrative_ontology:affects_constraint(victorian_advertising_market, patent_medicine_regulation).
narrative_ontology:affects_constraint(victorian_advertising_market, newspaper_economic_dependence).

% DUAL FORMULATION NOTE:
% The Victorian advertising market decomposes into distinct constraints: (1) newspaper_economic_dependence (ε ≈ 0.30, Tangled Rope) — coordination need to fund mass press alongside extractive dependence on advertiser preferences; (2) patent_medicine_fraud (ε ≈ 0.72, Snare) — systematic health claims fraud targeting illiterate consumers; (3) advertising_standards_absence (ε ≈ 0.45, Piton) — degraded editorial ethics norm. This story treats the bundled market-level constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(victorian_advertising_market, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
