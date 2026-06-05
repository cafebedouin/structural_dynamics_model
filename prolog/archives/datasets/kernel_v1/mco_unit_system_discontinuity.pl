% ============================================================================
% CONSTRAINT STORY: mco_unit_system_discontinuity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mco_unit_system_discontinuity, []).

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
 *   constraint_id: mco_unit_system_discontinuity
 *   human_readable: Persistence of Imperial Units in Metric-Standardized Industries
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   The persistence of Imperial units in aerospace manufacturing,
 *   construction, and engineering is a canonical Piton — a constraint
 *   maintained by institutional inertia rather than functional necessity. The
 *   International System of Units (SI) was adopted globally in 1960, with
 *   metrication waves spreading through the 1970s-1980s across most
 *   industries. However, the aerospace sector, locked into decades of NASA
 *   and FAA specifications written in feet, inches, pounds, and knots, never
 *   completed the transition. This constraint exhibits the characteristic
 *   Piton signature: a mechanism (imperial specification standards) that once
 *   served a genuine coordination function (international aviation safety
 *   standardization) but whose primary function has atrophied, leaving only
 *   theatrical maintenance and switching-cost inertia. The constraint's
 *   theater_ratio has risen from 0.62 in 1975 to 0.78 by 2005, reflecting the
 *   growing gap between the specification's performative role (risk
 *   management ritual) and its actual function (converted to metric
 *   internally by most modern systems). New entrants face dual-unit knowledge
 *   requirements and conversion labor costs; legacy contractors capture
 *   arbitrage value from high switching costs; regulatory bodies maintain
 *   imperial specifications through conservative risk aversion rather than
 *   empirical safety justification; and the broader metric coordination
 *   system (SI) functions perfectly well in isolation. The constraint
 *   persists not because it solves a problem but because changing it is
 *   treated as higher-risk than maintaining it.
 *
 * KEY AGENTS:
 *   - Legacy Aerospace Contractors (institutional/arbitrage): Primary beneficiaries — high switching costs lock them into imperial specifications, creating defensive advantage against metric-native competitors.
 *   - New Manufacturing Engineers (powerless/trapped): Primary victims — must learn dual-unit systems, face conversion error risk, lack accumulated tacit knowledge of imperial conventions.
 *   - Cross-System Integrators (moderate/constrained): Secondary beneficiaries — capture coordination value by managing conversions between imperial suppliers and metric customers.
 *   - FAA/NASA Regulatory Apparatus (institutional/arbitrage): Maintains theater through specification inertia; treats specification changes as higher-risk than legacy continuation despite metric safety record.
 *   - SI Standardization Bodies (institutional/arbitrage): Achieved genuine global coordination via metric standards; imperial persistence is external legacy they don't control.
 *   - Analytical Observer (analytical/analytical): Risks false summit by naturalizing unit system choice as convention with no objective grounds.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mco_unit_system_discontinuity, 0.28).
domain_priors:suppression_score(mco_unit_system_discontinuity, 0.35).
domain_priors:theater_ratio(mco_unit_system_discontinuity, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, extractiveness, 0.28).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mco_unit_system_discontinuity, piton).
narrative_ontology:human_readable(mco_unit_system_discontinuity, "Persistence of Imperial Units in Metric-Standardized Industries").
narrative_ontology:topic_domain(mco_unit_system_discontinuity, "technological/institutional").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, legacy_aerospace_contractors).
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, construction_trade_practitioners).
narrative_ontology:constraint_victim(mco_unit_system_discontinuity, metric_standardization_adoption).
narrative_ontology:constraint_victim(mco_unit_system_discontinuity, new_entrant_manufacturers).
narrative_ontology:constraint_victim(mco_unit_system_discontinuity, cross_system_integration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW ENGINEER IN METRIC JURISDICTION (SNARE) — Trapped in a dual-unit environment without the tacit knowledge accumulated by legacy practitioners. Must master both systems, cannot exit the requirement, bears conversion error risk. Metrication was mandated decades ago but the legacy standard persists structurally — extraction flow runs toward legacy holders who control implicit knowledge.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CROSS-SYSTEM INTEGRATOR (TANGLED ROPE) — Coordinates between imperial-based suppliers and metric-standardized buyers; bears conversion labor costs but also captures coordination value through gatekeeping. Mixed extraction and genuine coordination function — the constraint creates the role that extracts from both sides.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: LEGACY AEROSPACE CONTRACTOR (ROPE) — Benefits from frozen technical standards. The imperial specification became locked into decades of NASA contracts, FAA certification, and supplier networks. Switching to metric entails re-qualifying all manufacturing processes, re-certifying parts, and retraining workforces — arbitrage value (switching cost so high it functionally locks the contractor in) creates benefit without overt extraction mechanism.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SI STANDARDIZATION BODY (ROPE) — Achieved global coordination around metric standards through legitimate institutional work. The persistence of imperial units in aerospace is externally caused (legacy lock-in), not failure of the coordination mechanism. SI coordinates all the metricated systems successfully; the constraint is a residual legacy that SI doesn't control.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AEROSPACE REGULATORY APPARATUS (PITON) — Maintains dual-unit specifications through inertia and risk aversion. Federal Aviation Regulations (FARs) specify altitude in feet, airspeed in knots, vertical speed in feet-per-minute despite SI adoption. The standards persist because regulators treat 'changing a specification that has never failed' as higher risk than maintaining legacy. Theater ratio is extremely high: the regulation's function (safety coordination) is decoupled from its form (imperial units). A metric version would be functionally identical but carries perceived re-certification risk. Regulatory theater maintains the constraint.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, unit systems are pure conventions with no natural grounding; any coordinate system is equally valid if consistently applied. The claim that 'metric is objectively better' is itself a false summit — units have no intrinsic superiority, only network effects and institutional convention. However, the structural data contradicts the mountain classification. The persistence of imperial units is not a law of nature but a specific institutional arrangement maintained by legacy lock-in and switching costs. The analytical observer risks naturalizing contingent path dependence as necessity.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mco_unit_system_discontinuity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mco_unit_system_discontinuity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(mco_unit_system_discontinuity, TR),
    TR >= 0.70.

:- end_tests(mco_unit_system_discontinuity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint extracts value from new entrants (who pay conversion labor and knowledge acquisition costs) and transfers it to legacy contractors (who avoid re-qualification costs). However, the total extraction is limited because metric alternatives are available and functional — no agent is trapped by absolute necessity. The rising trajectory (0.18 → 0.28 over 30 years) reflects gradual accumulation of legacy investments that deepen switching costs rather than extraction inherent to the constraint itself. Suppression (0.35): Moderate. Barriers to exit include regulatory re-certification requirements, supplier lock-in, and high capital redeployment costs, but these are surmountable for well-capitalized actors. New entrants face steeper suppression (must learn both systems) than incumbents (use existing infrastructure). Theater ratio (0.78): High. The constraint's primary function is no longer coordination (SI handles that) but maintenance of regulatory legitimacy. FAA altitude specifications in feet have no safety justification independent of consistency — they could be feet, meters, or arbitrary units and produce identical safety outcomes if uniformly applied. The theater has risen because the constraint is increasingly disconnected from functional necessity yet maintained through risk-averse specification review. The regulatory apparatus performs theater (justifying unchanged specifications through conservative risk language) to avoid the perceived risk of formal specification revision.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates wide perspectival divergence. Legacy contractors see stable arbitrage coordination (Rope) — their lock-in provides advantage. Regulators see risk-minimization through specification inertia (Piton) — maintaining the standard feels safer than changing it. New entrants see extraction with no compensation (Snare) — they bear dual-unit labor costs to serve a system designed around incumbent convenience. Cross-system integrators see mixed coordination and extraction (Tangled Rope) — their role would disappear if systems fully unified. The SI standardization body sees complete success with residual legacy (Rope) — metric coordination works everywhere except aerospace. The analytical observer risks natural-law framing (Mountain) — claiming unit systems are conventionally equivalent and therefore the persistence of imperial is not a constraint but merely a choice. But the structural data reveals this as a false summit: the constraint IS empirically present (measured by extractiveness and suppression), it DOES benefit identifiable agents (legacy contractors, regulators), it DOES impose costs on others (new entrants, metric advocates), and it DOES persist through institutional inertia rather than functional justification (piton theater_ratio = 0.78).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from its structural position in the extraction flow. Legacy contractors occupy the arbitrage position (can exit but face extreme costs, making exit functionally impossible) and benefit from the constraint (locked-in customer base guarantees demand). The engine derives d ≈ 0.15 (low target load) for institutional/arbitrage agents. New entrants are trapped in the dual-unit requirement without knowledge or capital to navigate it — they occupy the victim/trapped position with d ≈ 0.95 (high target load), experiencing maximum extraction through conversion labor and error risk. Regulators occupy an institutional position with arbitrage options (changing specifications is possible but perceived as high-risk) and accrue benefit from specification inertia (regulatory continuity minimizes their decision-making burden). The engine derives institutional/arbitrage → d ≈ 0.10, but the omega on regulatory capture suggests this derivation may underestimate the institutional extraction flowing toward regulators who benefit from unchanged specifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is NOT natural law but institutional artifact, and therefore piton is the correct primary classification. The false summit (mountain perspective) falsely naturalizes the constraint by appealing to unit system conventionality. While it is true that unit systems are pure mathematical conventions, the structural persistence of imperial units in a metric-standardized world is NOT a consequence of convention alone — it is a consequence of specific path-dependent investments, regulatory inertia, and incumbent lock-in. If metric had emerged as the legacy system (hypothetical: if French aerospace had dominated post-WWII), the exact same constraint-structure would persist but with metric locked in — the 'convention' argument would be identical but the beneficiaries would be different. The constraint is maintained by extraction mechanisms (switching costs, regulatory conservatism) that are contingent on this specific history, not on the mathematical properties of unit systems. Piton classification is stable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conversion_error_frequency,
    'What is the actual rate of conversion errors in dual-unit environments relative to single-metric systems?',
    'Comparative analysis of manufacturing defect rates, quality control audit trails, and incident root-cause analysis in imperial-metric hybrid operations vs fully metricated operations. Correlation with incident severity (near-miss vs catastrophic failure).',
    'If error rate is significantly elevated: suppression (0.35) is underestimated; the constraint represents genuine risk that justifies regulatory conservatism. If error rate is negligible: suppression can be reduced; regulatory theater (piton classification) is unsupported by empirical evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conversion_error_frequency, empirical, 'Rate of conversion errors in dual-unit vs metric-only systems').

omega_variable(
    switching_cost_fungibility,
    'Are re-qualification and re-certification costs for metric conversion genuinely prohibitive or are they exaggerated by institutional risk aversion?',
    'Cost analysis of full metric conversion for a legacy aerospace program (tooling, part inventory, supplier qualification, FAA certification). Comparison with actual historical metrication case studies (France Telecom, Australian defense, UK automotive). Decompose labor, capital, and regulatory components.',
    'If switching costs > 40% of legacy program lifetime value: arbitrage lock-in is genuine and piton classification is stable. If switching costs < 15%: piton is maintained by regulatory theater rather than economic constraint; reclassify toward rope or dissolve the constraint entirely (metrication becomes optional path).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_fungibility, empirical, 'Actual cost of switching legacy aerospace to metric units').

omega_variable(
    network_effect_reversal_threshold,
    'At what point would metric adoption reach critical mass such that remaining imperial suppliers become economically irrational?',
    'Network economic modeling: calculate supplier switching threshold (when metric customer base > imperial customer base by margin sufficient to overcome re-tooling costs). Empirical tracking of metric vs imperial specification prevalence in new contracts/programs.',
    'If threshold is already crossed (metric > imperial in new procurement): constraint is in terminal decay phase; piton classification stable but with accelerating sunset. If threshold is remote: constraint may persist for decades regardless of metrication mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_reversal_threshold, empirical, 'Critical mass threshold for metric network dominance').

omega_variable(
    regulatory_conservatism_bias,
    'Is regulatory reluctance to change unit specifications a principled safety stance or institutional capture by incumbent contractors?',
    'Historical analysis: did FAA/NASA specifications change in response to technical evidence (e.g., proven safety of metric alternatives) or remain static despite evidence? Interview regulators on decision-making criteria for specification updates. Audit whether metric and imperial specifications have equivalent safety records.',
    'If conservatism is principled: theater_ratio may be overstated; regulation is performing genuine safety function. If capture: theater_ratio accurate; constraint maintained by incumbent benefit rather than public safety.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_conservatism_bias, conceptual, 'Whether regulatory conservatism is principled or captured').

omega_variable(
    false_summit_natural_law_claim,
    'Is the claim that ''unit systems are pure convention with no objective superiority'' a legitimate analytical observation or a false summit that naturalizes specific institutional arrangements?',
    'Examine whether metric and imperial systems have genuinely equivalent network effects and institutional inertia. Test whether metric''s global dominance (SI adoption by 195 countries, universal in science) is due to natural equivalence or contingent historical path (post-WWII US/French political influence). Assess whether ''all unit systems equally valid'' claim would be made if imperial had achieved global dominance instead.',
    'If legitimate: mountain classification stands; unit system choice is arbitrary convention. If false summit: constraint is contingent institutional arrangement; reclassify piton as primary classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether unit system equivalence is genuine or naturalized contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mco_unit_system_discontinuity, 1975, 2005).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mco_tr_t0, mco_unit_system_discontinuity, theater_ratio, 0, 0.62).
narrative_ontology:measurement(mco_tr_t15, mco_unit_system_discontinuity, theater_ratio, 15, 0.7).
narrative_ontology:measurement(mco_tr_t30, mco_unit_system_discontinuity, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(mco_be_t0, mco_unit_system_discontinuity, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mco_be_t15, mco_unit_system_discontinuity, base_extractiveness, 15, 0.23).
narrative_ontology:measurement(mco_be_t30, mco_unit_system_discontinuity, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(mco_su_t0, mco_unit_system_discontinuity, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(mco_su_t15, mco_unit_system_discontinuity, suppression_requirement, 15, 0.32).
narrative_ontology:measurement(mco_su_t30, mco_unit_system_discontinuity, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mco_unit_system_discontinuity, information_standard).
narrative_ontology:affects_constraint(mco_unit_system_discontinuity, aviation_standardization_lock_in).
narrative_ontology:affects_constraint(mco_unit_system_discontinuity, aerospace_supplier_ecosystem_consolidation).

% DUAL FORMULATION NOTE:
% The imperial unit persistence is a single constraint but can be decomposed into distinct stories by geographic jurisdiction: US aerospace (piton + active institutional beneficiaries), European aerospace (mostly metric, with legacy NATO compatibility requirements), and international civil aviation (mixed, with ICAO standards attempting coordination). The parent constraint affects both downstream constraints through specification cascading — lock-in in US aerospace extends to suppliers and integrators globally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mco_unit_system_discontinuity, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
