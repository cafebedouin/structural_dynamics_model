% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Article 127 — Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   The ECB Mandate represents the EU's foundational commitment to monetary
 *   authority and its internal prioritization among potentially competing
 *   objectives. The expansive secondary objectives reading emerged from the
 *   2010-2020 sovereign debt crisis when ECB officials began articulating
 *   that supporting employment and growth in peripheral economies was
 *   compatible with price stability — that the 'without prejudice' clause
 *   authorized discretionary monetary expansion beyond what
 *   price-stability-first orthodoxy would permit. This reading serves as
 *   cover for quasi-fiscal stabilization while maintaining the legal fiction
 *   that the ECB remains within its assigned mandate. The constraint exhibits
 *   Tangled Rope structure: the expansive reading genuinely coordinates
 *   monetary response to asymmetric euro-area shocks (providing beneficiaries
 *   — peripheral governments and workers — with policy space), while
 *   simultaneously extracting from those committed to orthodox price
 *   stability (savers, creditor economies, institutional credibility of
 *   price-stability commitment). The theater ratio has risen as the
 *   constraint has become institutionalized — the 'without prejudice'
 *   language and secondary objectives frame permit increasingly expansionary
 *   policy while maintaining price-stability rhetoric.
 *
 * KEY AGENTS:
 *   - Peripheral Wage Earners: Primary victims (powerless/trapped) — structural unemployment is only escapable if ECB interprets mandate expansively
 *   - Member State Governments (esp. Southern): Primary beneficiaries (moderate/constrained) — fiscal space is created by ECB discretion
 *   - ECB Governing Council: Secondary beneficiary (institutional/arbitrage) — 'without prejudice' clause grants discretionary authority
 *   - Creditor-Biased Orthodox Framework: Victim (institutional/arbitrage) — natural law framing of price stability is displaced
 *   - European Stability Mechanism: Residual authority (institutional/constrained) — exists to legitimize ECB orthodoxy through separate fiscal channel
 *   - Fiscal Union Reform Coalition: Organized agents (organized/constrained) — treats expansive secondary objectives as bridge toward fiscal union
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.52).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.48).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.52).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Article 127 — Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'da2c4a06-8e25-4d98-8cd8-cb007f572443').
narrative_ontology:cs_kernel_codification('da2c4a06-8e25-4d98-8cd8-cb007f572443', fixed_text).
narrative_ontology:cs_authority_grounding('da2c4a06-8e25-4d98-8cd8-cb007f572443', lineage).
narrative_ontology:cs_interpretation_layer_present('da2c4a06-8e25-4d98-8cd8-cb007f572443').
narrative_ontology:cs_reading_relation('da2c4a06-8e25-4d98-8cd8-cb007f572443', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('da2c4a06-8e25-4d98-8cd8-cb007f572443', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('da2c4a06-8e25-4d98-8cd8-cb007f572443', foundational, secondary_objectives_genuinely_discretionary).
narrative_ontology:cs_axiom_status(secondary_objectives_genuinely_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('da2c4a06-8e25-4d98-8cd8-cb007f572443', secondary_objectives_genuinely_discretionary, conventional).
narrative_ontology:cs_axiom('da2c4a06-8e25-4d98-8cd8-cb007f572443', foundational, price_stability_not_lexicographically_supreme).
narrative_ontology:cs_axiom_status(price_stability_not_lexicographically_supreme, holdable).
narrative_ontology:cs_axiom_grounding('da2c4a06-8e25-4d98-8cd8-cb007f572443', price_stability_not_lexicographically_supreme, conventional).
narrative_ontology:cs_reference_frame('da2c4a06-8e25-4d98-8cd8-cb007f572443', discretionary_secondary_objectives_framework).
narrative_ontology:cs_drift_state('da2c4a06-8e25-4d98-8cd8-cb007f572443', post_sovereign_debt_crisis_normalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('da2c4a06-8e25-4d98-8cd8-cb007f572443', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, debtor_economies).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, employment_sensitive_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, peripheral_wage_earners).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, inflation_constrained_economies).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, saver_wealth).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, orthodoxy_enforcement_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL WAGE EARNER (SNARE) — Trapped in debt-denominated employment markets where ECB discretion to support growth is the only pathway out of structural unemployment. Cannot exit the monetary regime; entirely dependent on ECB forbearance. The 'without prejudice' clause's interpretation determines access to employment — a purely extractive dependence with no alternatives.
constraint_indexing:constraint_classification(ecb_mandate_article_127__expansive_secondary_objectives, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEMBER STATE GOVERNMENT (TANGLED ROPE) — Experiences coordinating function (ECB support enables countercyclical policy space) AND extraction (ECB discretion means sovereign governments cannot guarantee employment policy autonomy). Constrained by fiscal rules but benefit from ECB willingness to interpret secondary objectives expansively. Mixed experience — significant coordination benefit alongside moderate extraction.
constraint_indexing:constraint_classification(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ECB GOVERNING COUNCIL (ROPE) — Experiences the mandate as a coordination mechanism: the 'without prejudice' clause enables the ECB to respond flexibly to euro-area shocks, coordinating monetary policy across heterogeneous economies. Net beneficiary of discretionary authority. The secondary objectives frame permits legitimizing expansionary policy while maintaining price stability framing.
constraint_indexing:constraint_classification(ecb_mandate_article_127__expansive_secondary_objectives, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: CREDITOR-BIASED ORTHODOX READING (MOUNTAIN) — The primary objective (price stability) is treated as an immutable natural law of sound monetary policy. Secondary objectives are permissible only 'without prejudice' to price stability — the mountain frame naturalizes the priority ordering as a fixed law rather than a distributional choice. This perspective sees the expansive reading as a violation of nature, not a legitimate interpretation.
constraint_indexing:constraint_classification(ecb_mandate_article_127__expansive_secondary_objectives, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: EUROPEAN STABILITY MECHANISM (PITON) — The ESM was created as a crisis backstop when ECB mandate ambiguity became visible. The ESM's quasi-fiscal role is partly performative — it exists to legitimate ECB orthodoxy by appearing to handle fiscal matters separately, while ECB discretionary expansionism in practice coordinates both monetary and quasi-fiscal support. Theater ratio high because the separation of ECB (monetary) and ESM (fiscal) is largely institutional theater.
constraint_indexing:constraint_classification(ecb_mandate_article_127__expansive_secondary_objectives, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FISCAL UNION REFORM COALITION (SCAFFOLD) — Sees the expansive secondary objectives reading as a temporary workaround with a sunset clause: real fiscal risk-sharing (Eurozone budget, progressive taxation, debt mutualisation) would eliminate the need for ECB discretionary maneuver. The coalition constrains the secondary objectives reading as a bridge toward genuine fiscal union, not a permanent framework.
constraint_indexing:constraint_classification(ecb_mandate_article_127__expansive_secondary_objectives, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecb_mandate_article_127__expansive_secondary_objectives, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecb_mandate_article_127__expansive_secondary_objectives, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, TR),
    TR >= 0.70.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The expansive secondary objectives reading permits ECB discretion to support employment/growth at the cost of potentially eroding price stability commitment and distorting capital allocation toward debtors. The extraction is not maximal (0.70+) because the 'without prejudice' language constrains discretion — the ECB cannot explicitly state it is sacrificing price stability. The extraction flow is asymmetric: beneficiaries (peripheral governments, workers) gain access to monetary accommodation; victims (savers losing real wealth, creditor economies losing relative advantage, price-stability-first institutions) bear costs of inflation and credibility erosion. Suppression (0.48): Moderate. The constraint suppresses the orthodox price-stability reading through institutional authority (ECB's legal interpretive power) and through market psychology (repeated ECB commitment to secondary objectives normalizes expansion). Alternatives are suppressed but not eliminated — the mountain perspective maintains that price stability is natural law. Theater ratio (0.62): Rising (0.35→0.62). The constraint's theater has increased as the expansive reading has matured. Early (2008-2015), the secondary objectives were largely rhetorical cover for crisis response. By 2020, the expansive framing had become institutionalized — the 'without prejudice' clause and secondary objectives language permit characterizing increasingly expansionary policy as mandate-compliant, even as inflation and wealth inequality rise. The theater reflects the performative separation between stated price-stability commitment and actual accommodative practice.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap lies between the mountain perspective (price stability as natural law) and the tangled-rope perspective (secondary objectives as legitimate operational weight). The mountain view sees the expansive secondary objectives reading as a violation of the mandate's natural constraints — inflation cannot be permanently suppressed below natural rates without unsustainable resource misallocation. The tangled-rope view sees secondary objectives as genuine policy trade-offs that the mandate legitimately permits. The piton perspective (performative ESM theater) and the scaffold perspective (fiscal union as sunset path) both acknowledge that the expansive reading is not sustainable indefinitely — either orthodox pressure will force reversion, or fiscal union will eliminate the need for ECB discretion. The rope perspective (ECB Governing Council's experience of coordination) sees only the coordination benefit, not the extraction cost. The snare perspective (peripheral wage earner's trapped dependence) sees the full asymmetry — employment access is conditional on ECB goodwill, not a right or structural entitlement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (debtor_economies, employment_sensitive_member_states, peripheral_wage_earners) have low d values because they experience the constraint as enabling rather than constraining: the expansive secondary objectives reading creates monetary space for growth support. The ECB Governing Council (institutional/arbitrage) experiences d near 0.0 because it is the implementer of discretion — arbitrage options mean the ECB can shift between interpretations as political economy changes. Victims (inflation_constrained_economies, saver_wealth, orthodoxy_enforcement_credibility) have high d values because they bear costs of suppressing the price-stability-first reading. The peripheral wage earner (powerless/trapped) has d near 1.0 — maximum extraction, no exit. The creditor-biased orthodoxy (institutional/arbitrage) would have d near 0.5 if treated as an agent, but the constraint structure treats it as a victim of interpretive displacement. The directionality derivation confirms the tangled-rope classification: genuine coordination benefit (secondary objectives enable asymmetric shock response) alongside asymmetric extraction (Orthodox price-stability commitment is suppressed; saver wealth and credibility are targeted).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that 'mandate' itself is contested. The orthodox reading (primary objective supreme) and the expansive reading (secondary objectives genuinely discretionary) are two internally coherent interpretations of the same legal text. The 'without prejudice' clause is the ambiguity that enables both. The mandatrophy is not resolvable through legal interpretation — it requires political choice about whether secondary objectives are permitted or merely aspirational. The expansive reading is not a violation of the mandate; it is a claim about what the mandate permits. The credibility cost is real (savers and orthodox institutions experience extraction), but the coordination benefit is also real (peripheral economies gain policy space). The divergence between the mountain perspective (natural law price stability) and the tangled-rope perspective (legitimate secondary objectives trade-off) is the diagnostic signal that the mandate is genuinely underdetermined — neither party has exclusive claim to the text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_stability_definition_ambiguity,
    'Does ''price stability'' in Article 127 mean strict inflation targeting (< 2%) or broader macro-price-level consistency including asset prices, wage-setting expectations, and distributional fairness?',
    'Historical ECB communication: compare Draghi-era ''whatever it takes'' to Lagarde-era ''price stability mandate'' framings. Empirical data on correlation between ECB rates and employment/wage outcomes across member states.',
    'If strict targeting: expansive reading is foreclosed (secondary objectives must genuinely not prejudice inflation). If broader definition: expansive reading is anchored (secondary objectives are integrated into price stability concept itself).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(price_stability_definition_ambiguity, conceptual, 'What counts as price stability — narrow inflation target or broad macro-price consistency').

omega_variable(
    distributional_authority_scope,
    'Does the ''without prejudice'' clause authorize the ECB to make distributional choices (favoring debtors, workers, periphery) or only to acknowledge distributional consequences of monetary policy?',
    'Legal analysis of ECB decisions during 2010-2020 sovereign debt crisis: did the ECB consciously weight employment/growth tradeoffs, or did it treat distributional effects as incidental to price stability pursuit?',
    'If only consequences acknowledged: ECB remains orthodoxy-constrained (secondary objectives are aspirational cover). If distributional choices authorized: ECB has genuine discretionary mandate for redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_authority_scope, conceptual, 'Whether ECB discretion encompasses intentional distributional choices').

omega_variable(
    escape_velocity_of_expansionist_doctrine,
    'Once ECB commits to expansionist secondary objectives reading, can orthodox creditor pressure (Bundesbank dissent, ECJ legal challenges, market discipline) force reversion to price-stability-first orthodoxy?',
    'Political economy of ECB dissent: track voting coalitions (2015-2026). Empirical: measure market reaction to ECJ preliminary rulings and treaty revision pressure. Longitudinal: survey ECB official rhetoric shifts.',
    'If reversion forces strong: expansive reading is transient (Piton trajectory). If reversion resistance strong: reading becomes structural (genuine Tangled Rope with durable extraction asymmetry).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escape_velocity_of_expansionist_doctrine, empirical, 'Whether expansionist doctrine can be reversed or becomes durable').

omega_variable(
    secondary_objectives_sibling_foreclosure,
    'Can the expansive secondary objectives reading coexist with climate incorporation (Article 127.6 amendments), or does each reading foreclose alternatives by claiming mandate authority?',
    'Treaty amendment analysis: do successive objectives (employment, climate) share discretionary space (''without prejudice'' applies to all) or does priority ordering foreclose others?',
    'If coexistence real: multiple secondary objectives create coalition dynamics (Tangled Rope with shifting victim/beneficiary sets). If foreclosure occurs: later objectives displace earlier ones (mandate revision, not interpretation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_objectives_sibling_foreclosure, conceptual, 'Whether multiple secondary objectives can coexist or foreclose one another').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_expansive_theater_t0_2008, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ecb_expansive_theater_t5_2015, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ecb_expansive_theater_t10_2020, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(ecb_expansive_extractiveness_t0_2008, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ecb_expansive_extractiveness_t5_2015, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ecb_expansive_extractiveness_t10_2020, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ecb_expansive_suppression_t0_2008, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ecb_expansive_suppression_t5_2015, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(ecb_expansive_suppression_t10_2020, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_fiscal_union_absent).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, sovereignty_constrained_by_monetary_union).

% DUAL FORMULATION NOTE:
% The ECB Mandate Article 127 is a single kernel with multiple structurally distinct readings. This story (expansive_secondary_objectives, ε=0.52) and the orthodox_price_stability reading (ε=0.18 expected, Mountain) are siblings, not decomposition along an ε-invariance principle. They have different ε values because they measure different claims: 'what does the mandate permit?' (expansive, ε=0.52) vs 'what is the mandate's natural law'? (orthodox, ε=0.18). Each reading is a coherent interpretation of the same legal text. The constraint family includes climate_incorporation as a third reading that may foreclose or coexist with expansive secondary objectives depending on treaty amendment trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__expansive_secondary_objectives, institutional, 0.15).
constraint_indexing:directionality_override(ecb_mandate_article_127__expansive_secondary_objectives, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
