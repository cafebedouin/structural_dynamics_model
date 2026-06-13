% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate: Orthodox Price Stability Reading (Article 127 TFEU)
 *   domain: monetary_policy/constitutional_law/institutional_governance
 *
 * SUMMARY:
 *   The European Central Bank's mandate under Article 127 TFEU commits it to
 *   'maintaining price stability' as the primary objective; secondary
 *   objectives (supporting employment, growth, and broader EU policy) are
 *   permitted 'without prejudice' to price stability. The orthodox reading
 *   interprets 'without prejudice' as a strict hierarchy: secondary
 *   objectives are operational only if price stability is not threatened. In
 *   practice, this means the 2% inflation target is treated as a binding
 *   constraint, and employment, growth, and climate considerations are
 *   deferred to fiscal authorities and play no role in ECB operational
 *   decisions. This reading produces a narrow beneficiary set (creditors,
 *   savers) and a wide victim set (peripheral labor markets, climate-exposed
 *   economies, those seeking employment stabilization during demand shocks).
 *   The constraint's extractiveness has risen steadily as climate risks mount
 *   and the secondary objectives clause remains inoperative despite repeated
 *   legislative proposals to activate it. The theater ratio tracks rising
 *   performative discussion of secondary objectives (speeches, research)
 *   while operational policy remains locked on price stability.
 *
 * KEY AGENTS:
 *   - ECB Governing Council: institutional agenda-setter; interprets and enforces the orthodox reading; controls communication and rate-setting
 *   - Creditor classes and savers: beneficiary seat; benefit from inflation suppression via asset protection and real returns
 *   - Peripheral labor markets: powerless payer; absorb employment costs of strict targeting during downturns
 *   - Climate-risk-exposed economies: moderate-power payer; cannot exit currency union or demand climate integration
 *   - Legislative bodies and secondary-objective advocates: excluded institutional seat; blocked from operationalizing 'without prejudice' clause
 *   - Fiscal authorities: organized payer/observer; inherit responsibility for employment and climate policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.76).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, mountain).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate: Orthodox Price Stability Reading (Article 127 TFEU)").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/institutional_governance").

domain_priors:emerges_naturally(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '1df0ccfc-461c-4cb9-8fc3-71d8417135a0').
narrative_ontology:cs_kernel_codification('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', fixed_text).
narrative_ontology:cs_authority_grounding('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', lineage).
narrative_ontology:cs_interpretation_layer_present('1df0ccfc-461c-4cb9-8fc3-71d8417135a0').
narrative_ontology:cs_reading_relation('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', foundational, price_stability_primary_mandate).
narrative_ontology:cs_axiom_status(price_stability_primary_mandate, holdable).
narrative_ontology:cs_axiom_grounding('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', price_stability_primary_mandate, conventional).
narrative_ontology:cs_axiom('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', foundational, secondary_objectives_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational, overridden).
narrative_ontology:cs_axiom_grounding('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', secondary_objectives_non_operational, instrumental).
narrative_ontology:cs_reference_frame('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', price_stability_primacy).
narrative_ontology:cs_drift_state('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', contemporary_climate_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1df0ccfc-461c-4cb9-8fc3-71d8417135a0', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, creditor_classes).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, currency_stability_beneficiaries).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, ExtMetricName, E),
    domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ecb_mandate_article_127__orthodox_price_stability),
    narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is AUTHORED as a mountain (emerges_naturally: true) because the orthodox reading frames price stability as a natural law of monetary economics—inflation beyond the target is treated as irreducible collapse of the currency's role as store of value and medium of exchange. However, the authored metrics (extractiveness 0.68, suppression 0.76) describe a highly constructed, actively enforced regime that benefits a narrow set and imposes costs on a wide set. This divergence—claimed mountain, measured as extraction—is the central diagnostic. The temporal series (base_extractiveness rising from 0.35 to 0.68 over 34 years) tracks the accumulation of extraction as the founding anti-stagflation crisis receded, secondary objectives' salience rose (climate, employment), and the orthodox interpretation hardened rather than softened. Theater ratio rising from 0.08 to 0.42 tracks the growing gap between rhetoric about secondary objectives (ECB speeches on climate, financial stability) and operational policy (zero operational weight on either). Suppression requirement rising from 0.55 to 0.76 models the intensifying enforcement machinery needed to block mandate-expansion proposals and prevent reinterpretation. Accessibility collapse high across all time points reflects the institutional lock: member states cannot unilaterally change the mandate, academics dissenting from orthodoxy face professional marginalization, and elected bodies cannot override an independent authority's interpretation. Resistance rises from 0.25 to 0.42 as climate and employment pressures mount, but remains constrained because the benefiting parties (creditors, savers, institutional orthodoxy) have greater institutional power than the dispersed victims.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB's institutional seat, the orthodox reading is a faithful reading of the treaty text and a rational defense of central-bank independence—inflation control is the core function and cannot be compromised. From the fiscal authorities' seats, the same reading is an externalization of extraction: the ECB locks itself into a single objective and leaves fiscal authorities to handle employment and climate costs without monetary cooperation. From the powerless labor-market seat, the reading is a structural entrapment: they absorb employment losses during demand shocks and have no exit from the currency union to seek better monetary policy. From the legislative seat, the reading is a usurpation: the 'without prejudice' clause was meant to be operational, but was read down to meaninglessness. The engine computes each seat's type from the structural asymmetry: agenda-setter with near-total directionality authority over the beneficiary and victim seats, beneficiaries with arbitrage exit and aligned interests, victims with trapped and identity-locked exits and no ability to demand rebalancing. This produces an estimated tangled_rope or snare classification from the victim seats, despite the agenda-setter's own mountain framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (creditors, savers) have directionality near 0.0: the constraint subsidizes them by protecting asset values from inflation. Exit options are arbitrage-grade—if ECB policy shifted, they can reallocate capital to other currency zones or asset classes instantly. Power is institutional/powerful. This produces low effective extraction from their perspective; they experience the constraint as beneficial coordination. Victims (peripheral labor, climate-exposed economies) have directionality near 1.0: the constraint extracts from them by locking out employment and climate considerations. Exit options are trapped (labor cannot leave the currency union) or identity_locked (economies integrated into eurozone fiscal and trade structures). Power is powerless or moderate. This produces high effective extraction; they experience the constraint as coercive. The ECB's own seat has agenda-setting power and analytical exit, producing a high-authority directionality; it derives legitimacy from the orthodoxy's interpretation and benefits from the constraint's structural stability. No directionality overrides are needed; the derivation chain (beneficiary/victim + exit + power) produces the right asymmetry. Scope is continental—the constraint operates at eurozone scale, making verification of compliance hard and amplifying effective extraction modestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing 1970s-style stagflation via a credible inflation-control commitment) is solved. Contemporary inflation undershoots the target more often than it overshoots; labor-market slack and climate risks are the binding constraints, not inflation pressure. Yet the orthodox reading persists and intensifies its enforcement machinery (tightening collateral standards, resisting secondary-objective operationalization, hardening ECB communication against mandate expansion). This is the classic mandatrophy signature: the arrangement's founding justification is dead or reversed, but the constraint persists and extracts because the benefiting parties have institutional power to maintain it. The theater ratio rising steeply (speeches about climate and employment) while operational policy remains locked (no asset-purchase weight on climate risk, no employment stabilization instruments) is the perfected form of mandatrophy theater—the ECB acknowledges secondary objectives while ensuring they remain structurally inoperative. A genuine living mandate would operationalize them when price stability is not threatened; the orthodox reading ensures they never are operational. The constraint is ripe for mandatrophy designation, and a future reading that operationalizes the 'without prejudice' clause (the expansive_secondary_objectives sibling) would be the most natural decompression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    false_summit_mountain_claim,
    'Is the orthodox reading a genuine natural law of monetary economics, or is it a constructed constraint that benefits creditors and savers while externalizing costs to others?',
    'Empirical test: (a) Survey evidence of whether 2% inflation is the unique stable anchor or whether other targets (1.5%, 2.5%, flexible range) are equally viable; (b) historical counterfactual: did non-euro, non-ECB-constrained economies with higher inflation targets experience structural instability?; (c) institutional forensics: trace the path of the orthodox interpretation in ECB governance—was it derived from economic theory or chosen to benefit specific parties?',
    'If the reading is constructed and beneficiary-driven, the false-summit signature fires: reclassify from mountain to tangled_rope or snare. If the reading is genuinely natural, confirm mountain. Current evidence leans constructed: contemporary inflation undercuts the foundational inflation-pressure problem; the secondary objectives clause was drafted with clear operational intent; and the beneficiary set''s institutional power aligns too neatly with the reading''s dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_claim, empirical, 'Whether the 2% inflation primacy is a natural law of monetary stability or an institutional choice benefiting creditors.').

omega_variable(
    without_prejudice_clause_semantics,
    'What is the correct interpretation of ''without prejudice'' in Article 127(1)? Does it mean secondary objectives are subordinate (never operational) or does it mean they are operational when price stability is not threatened?',
    'Comparative legal analysis: (a) trace the negotiating history of the Maastricht Treaty and the European Central Bank Statute; (b) examine how ''without prejudice'' is used elsewhere in EU law (environmental law, human rights law) to resolve the semantic ambiguity; (c) assess whether the Governing Council''s interpretation (exclusive focus) is the only textualist reading or one reading among multiple valid ones.',
    'If ''without prejudice'' means secondary objectives are live when price stability is not threatened, the expansive_secondary_objectives reading is legally superior to the orthodox reading, and the constraint undergoes a phase transition. If it means strict hierarchy, the orthodox reading''s legal grounding is stronger. Current institutional consensus (ECB interpretation) favors hierarchy, but legal scholars and EU legislative proposals challenge this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(without_prejudice_clause_semantics, conceptual, 'Whether Article 127''s ''without prejudice'' clause operationalizes secondary objectives or subordinates them definitively.').

omega_variable(
    climate_financial_stability_coupling,
    'Does climate risk materially threaten price stability and financial stability in the eurozone, such that ignoring climate in collateral and asset decisions is itself a violation of the primary price-stability mandate?',
    'Empirical: (a) measure correlation between climate-driven asset volatility and inflation; (b) stress-test collateral frameworks for climate-shock scenarios; (c) assess whether peripheral economies'' climate exposure (water scarcity, flooding) creates systemic eurozone financial stability risks that the ECB''s price-stability mandate implicitly requires it to address.',
    'If climate risk is material to financial stability, then the orthodox reading''s externalization of climate becomes logically inconsistent with the price-stability mandate itself—climate integration becomes a requirement, not a secondary consideration. This would support the climate_incorporation reading and undermine the orthodox reading''s coherence. If climate risk is remote or exogenous to monetary policy''s domain, the orthodox reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_financial_stability_coupling, empirical, 'Whether climate risk is material to eurozone price and financial stability.').

omega_variable(
    mandate_inflation_anchor_necessity,
    'Is the 2% inflation target a necessary institutional commitment for price-stability credibility, or is it a contingent choice that could be updated or made flexible without destabilizing expectations?',
    'Empirical: (a) survey expectations-formation literature: do inflation expectations deanchor if the target is revised?; (b) natural experiment analysis: did Switzerland, UK, Canada, or other non-eurozone central banks suffer credibility loss when adjusting inflation targets or adopting flexible ranges?; (c) forward guidance literature: can flexible ranges, target bands, or asymmetric targets anchor expectations as well as a fixed point?',
    'If the 2% target is not uniquely necessary—if flexible ranges or asymmetric targets (e.g., 1-3%) can anchor expectations equally well—then the orthodox reading''s inflexibility is a choice, not a constraint of nature. This supports the false-summit hypothesis and the mandatrophy diagnosis. If the target is uniquely necessary, the orthodox reading''s rigidity is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_inflation_anchor_necessity, empirical, 'Whether 2% inflation is a necessary institutional anchor or a contingent policy choice.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'How much of the suppression of mandate-expansion proposals comes from structural/legal barriers (treaty rigidity, ECB institutional independence) versus internalized professional norms (economists'' orthodox training, career incentives favoring stability readings)?',
    'Institutional ethnography: interview ECB staff, legislative participants, and academic economists about their reasoning for and against mandate expansion. Measure: (a) how many proposed secondary-objective amendments would succeed if the ECB itself supported them (structural barrier measure); (b) how many economists with orthodox training endorse expansion when given anonymity or professional-cost forgiveness (internalization measure).',
    'If suppression is primarily structural (external barriers), the constraint is robust and legal amendment is the only decompression path. If internalized, the suppression is more fragile—a cohort shift or new institutional norms could shift the reading. The coercion grid''s suppression trajectory tracks both; disentangling them informs the stability of the orthodox reading''s dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether mandate-expansion suppression is structural or internalized in professional culture.').

omega_variable(
    beneficiary_heterogeneity_within_creditor_class,
    'Do all creditors and savers benefit equally from the orthodox reading, or are there subgroups within the creditor class whose welfare diverges (e.g., long-term savers vs. short-term traders, or creditors in peripheral vs. core economies)?',
    'Distributional analysis: (a) measure real returns to different creditor cohorts under orthodox vs. expansive policy scenarios; (b) assess whether peripheral creditors (whose economies would benefit from employment stabilization and climate investment) have different welfare profiles than core-economy creditors; (c) trace credit allocation: does the orthodox reading produce consistent favor or does it shift among creditor cohorts?',
    'If the beneficiary set is heterogeneous and some creditors would benefit from expansive policy (e.g., those in economies needing employment growth), the constraint''s beneficiary structure is less clear-cut. This complicates the snare/tangled-rope diagnosis and opens negotiation space for coalition-building between expansive-reading advocates and creditor subgroups. If creditors are aligned in preference for orthodoxy, the constraint''s beneficiary lock is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_heterogeneity_within_creditor_class, empirical, 'Whether all creditor and saver groups benefit equally from the orthodox reading or whether there is significant intra-class divergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_mandate_ps_tr_t1992, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(ecb_mandate_ps_tr_t2001, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(ecb_mandate_ps_tr_t2008, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(ecb_mandate_ps_tr_t2012, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2012, 0.25).
narrative_ontology:measurement(ecb_mandate_ps_tr_t2018, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(ecb_mandate_ps_tr_t2026, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(ecb_mandate_ps_be_t1992, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(ecb_mandate_ps_be_t2001, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(ecb_mandate_ps_be_t2008, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(ecb_mandate_ps_be_t2012, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(ecb_mandate_ps_be_t2018, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(ecb_mandate_ps_be_t2026, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ecb_mandate_ps_su_t1992, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 1992, 0.55).
narrative_ontology:measurement(ecb_mandate_ps_su_t2001, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(ecb_mandate_ps_su_t2008, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2008, 0.64).
narrative_ontology:measurement(ecb_mandate_ps_su_t2012, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement(ecb_mandate_ps_su_t2018, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement(ecb_mandate_ps_su_t2026, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2026, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__orthodox_price_stability, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eurozone_fiscal_policy_constraint).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, peripheral_labor_market_stabilization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (Article 127 TFEU ECB mandate). Three structurally distinct constraints are generated from the same treaty text: (1) ORTHODOX_PRICE_STABILITY (this file): 2% inflation target is primary, secondary objectives subordinate and inoperative. (2) EXPANSIVE_SECONDARY_OBJECTIVES: 'without prejudice' clause operationalizes employment, growth, financial stability as live policy levers when price stability not threatened. (3) CLIMATE_INCORPORATION: Article 11 TFEU environmental integration obligates climate integration into collateral and asset frameworks. Each reading instantiates a different ε, different beneficiary/victim structure, and different constraint type. The ε values diverge because the readings dispute what monetary policy's operational scope is—they are not different perspectives on the same scope, but different claims about the scope itself. This follows the ε-invariance principle: when observables/claims differ structurally, write separate constraints and link them via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, moderate, 0.82).
constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
