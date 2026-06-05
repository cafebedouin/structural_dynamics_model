% ============================================================================
% CONSTRAINT STORY: usc_26_469_passive_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usc_26_469_passive_loss, []).

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
 *   constraint_id: usc_26_469_passive_loss
 *   human_readable: Passive Activity Loss (PAL) Rules Under IRC Section 469
 *   domain: economic/legal
 *
 * SUMMARY:
 *   The Passive Activity Loss (PAL) rules, enacted in the Tax Reform Act of
 *   1986 (IRC Section 469), represent a foundational constraint on how U.S.
 *   taxpayers can recognize and offset business losses. By prohibiting the
 *   use of losses from passive activities (rental properties, businesses
 *   without material participation) to offset active income (wages) or
 *   portfolio income, the rule creates a structural division in the tax code
 *   that simultaneously serves coordination (preventing pre-1986 tax shelter
 *   abuse) and extraction (deferring or denying legitimate losses). The
 *   constraint exhibits the full diagnostic range: snare to powerless real
 *   estate entrepreneurs trapped by the definition of 'material
 *   participation,' rope to wage earners who benefit from base stabilization,
 *   tangled rope to organized syndicated investors navigating mixed
 *   incentives, scaffold to reform coalitions seeking carveouts and
 *   relaxation, and piton to the compliance regime itself, whose theater has
 *   risen as planning strategies have eroded the original tax base
 *   protection. The constraint's extractiveness has actually declined over
 *   the 40-year interval (0.65 → 0.52) as sophisticated taxpayers have
 *   learned to satisfy material participation tests and exploit carveouts
 *   like real estate professional status, indicating that the rule's
 *   protective effect has degraded despite increased compliance complexity.
 *
 * KEY AGENTS:
 *   - Real estate entrepreneurs and passive investors: Primary victims (powerless/trapped) — subject to loss limitation without material participation
 *   - Wage-earning taxpayers: Primary beneficiaries (institutional/arbitrage) — tax base stabilization protects income structure
 *   - Federal revenue system: Institutional beneficiary (institutional/arbitrage) — prevents tax shelter losses, stabilizes revenue
 *   - Syndicated passive investment pools: Secondary organized agents (organized/constrained) — mixed exposure; aggregation rules provide some coordination benefit
 *   - Sophisticated tax planners and advisors: Organized actors (powerful/mobile) — navigate material participation tests and grouping elections
 *   - Real estate industry and reform coalitions: Organized agents (organized/constrained) — lobby for carveouts and material participant exemptions
 *   - IRS enforcement apparatus: Institutional actor (institutional/arbitrage) — administers PAL compliance and audit selection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usc_26_469_passive_loss, 0.52).
domain_priors:suppression_score(usc_26_469_passive_loss, 0.68).
domain_priors:theater_ratio(usc_26_469_passive_loss, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usc_26_469_passive_loss, extractiveness, 0.52).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usc_26_469_passive_loss, tangled_rope).
narrative_ontology:human_readable(usc_26_469_passive_loss, "Passive Activity Loss (PAL) Rules Under IRC Section 469").
narrative_ontology:topic_domain(usc_26_469_passive_loss, "economic/legal").

domain_priors:requires_active_enforcement(usc_26_469_passive_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, wage_earning_taxpayers).
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, portfolio_investors).
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, federal_revenue_system).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, passive_activity_investors).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, real_estate_entrepreneurs).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, startup_founders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REAL ESTATE ENTREPRENEUR (SNARE) — Trapped by the passive loss limitation. Cannot offset genuine business losses from rental properties or development ventures against W-2 income without meeting material participation standard (500+ hours annually, significant involvement). No exit unless abandoning the business or meeting IRS material participation tests. Extraction is severe: deferred losses accumulate indefinitely or die with the taxpayer. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SYNDICATED PASSIVE INVESTMENT POOL (TANGLED ROPE) — Organized groups (real estate investment syndicates, master limited partnerships) experience mixed coordination and extraction. The rule provides coordination function: prevents shelter abuse, stabilizes tax base. But also extracts through loss deferral and cumulative limitations. Passive activity grouping rules allow some coordination through related activity aggregation, but this is constrained. d≈0.65, f(d)≈0.98, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WAGE-EARNING TAXPAYER (ROPE) — Benefits from PAL rules. Pure coordination function: prevents wealthy individuals from using tax-loss carryforwards to shelter ordinary income. Enables simpler, flatter tax structure for wage earners. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL REVENUE SYSTEM (ROPE) — Primary beneficiary (institutional/arbitrage). PAL rules prevent revenue erosion from tax-loss farming and shelter arrangements. Coordination function is clear: prevents a well-known tax avoidance strategy (pre-1986) where wealthy taxpayers created artificial losses to shelter income. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SOPHISTICATED TAX PLANNER (TANGLED ROPE) — Powerful agents with resources can navigate PAL through material participation tests, grouping elections, real estate professional status, or by structuring as active businesses. The rule has coordination (prevents naive shelter abuse) and extraction (forces costly compliance and planning overhead). Mobility is high: can restructure entities, relocate portfolio, or engage in alternative strategies. d≈0.48, f(d)≈0.63, σ=1.0 → χ≈0.33.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PASSIVE ACTIVITY COMPLIANCE THEATER (PITON) — The PAL reporting and documentation regime (Form 8582, activity grouping elections, material participation tracking) has substantial performative content. Theater ratio=0.58 reflects significant compliance overhead (documentation, professional fees, audit risk) relative to actual loss prevention function. The IRS enforcement has degraded over time as sophisticated planning tools (real estate professional status, syndication structures) have eroded the intended tax base protection. Institutional inertia maintains the regime despite reduced functional effect.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: TAX REFORM COALITION (SCAFFOLD) — Various reform proposals (carried interest reclassification, passive loss rule relaxation for real estate professionals, small business carveouts) suggest the PAL framework is viewed as a temporary constraint that may be replaced or narrowed. Some sunset logic exists: the real estate industry has successfully lobbied for material participant exemptions and cost segregation provisions that create partial exits. The constraint has a genuine sunset possibility if comprehensive tax reform occurs. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / TAX BASE ECONOMICS (MOUNTAIN) — From a civilizational economic perspective, the separability of income streams (active vs passive vs portfolio) may appear as a natural law of tax administration: some limit on loss-offsetting is inherent to any system that distinguishes income types. However, the structural data (ε=0.52, suppression=0.68) contradicts the mountain classification. The analytical observer risks naturalizing what is actually a contestable policy choice: loss-offsetting rules exist on a spectrum, and pre-1986 rules allowed full offset. The mountain perspective falsely locates the constraint as inevitable rather than contingent on 1986 legislative choice.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usc_26_469_passive_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(usc_26_469_passive_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(usc_26_469_passive_loss, TR),
    TR >= 0.70.

:- end_tests(usc_26_469_passive_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, declining over interval. The PAL rule extracts through loss deferral and potential permanent denial of losses, but the extraction is not absolute: passive losses are preserved on the taxpayer's balance sheet until death (when they are eliminated) or until the passive activity is sold in taxable disposition (when suspended losses can be recognized). The initial (1986) extractiveness was higher (0.65) because the planning workarounds were less developed. As sophisticated agents learned to satisfy material participation tests or claim real estate professional status, the effective extraction declined. The current 0.52 reflects both the deferral mechanism (real) and the degradation of enforcement (actual). Suppression (0.68): High. Significant barriers to loss recognition include: (a) the 500-hour material participation test requires documented involvement incompatible with passive investment; (b) definition of 'passive activity' is narrow and difficult to satisfy for real estate; (c) passive activity grouping rules create additional complexity; (d) once losses are suspended, recognition is limited to future passive income or taxable disposition; (e) if activity never generates passive income and is never sold, losses die with the taxpayer. Theater ratio (0.58): Moderate-high, increasing over interval. The compliance burden (Form 8582, activity grouping elections, participation documentation, professional fees) has grown relative to the actual tax base protection. Initial theater (0.35 in 1986) reflected tight enforcement and clearer rules. Current theater (0.58) reflects that sophisticated agents can navigate the rule through planning (real estate professional status, entity restructuring, cost segregation) while unsophisticated agents face high compliance costs without comparable relief.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows dramatic perspectival divergence across structural positions. The real estate entrepreneur (snare, d=0.92) experiences extraction: genuine business losses cannot be used to shelter ordinary income. The wage earner (rope, d=0.08) experiences coordination: the rule protects their income from shelter erosion. The syndicated investor (tangled rope, d=0.65) experiences both: passive activity grouping rules allow some coordination through aggregation, but loss deferral still extracts. The sophisticated planner (tangled rope, d=0.48) has mobile exit options (material participation strategies, real estate professional status) and experiences lower extraction. The reform coalition (scaffold, d=0.35) sees a temporary constraint with possible sunset through legislative carveouts. The IRS compliance theater (piton) shows the rule persisting through institutional inertia despite degraded functional effect. The analytical observer (mountain) risks naturalizing the rule as inevitable tax administration necessity, when pre-1986 law allowed full loss offsetting — the constraint is contingent on legislative choice, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Real estate entrepreneur: Victim + trapped → d≈0.92, f(d)≈1.38. Extraction is near-maximal; no meaningful exit unless business is structured to satisfy material participation. Wage-earning taxpayer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit via increased sophisticated planning but does not need to. Federal revenue system: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Institutional beneficiary; PAL is a revenue-protective mechanism. Syndicated investor: Victim + constrained → d≈0.65, f(d)≈0.98. Significant extraction but organized groups can partially exit through passive activity aggregation and grouping elections. Sophisticated planner: Victim + mobile → d≈0.48, f(d)≈0.63. Can navigate material participation tests, real estate professional status, or structure as active business; higher exit capacity reduces effective extraction. Reform coalition: Victim + constrained → d≈0.35, f(d)≈0.32. Can lobby for carveouts and exemptions but constrained by revenue concerns; partial exit through legislative reform.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for PAL is resolved by recognizing that the rule is genuinely tangled: it serves both coordination (prevents tax shelter abuse) and extraction (defers or denies losses). The snare classification for the powerless entrepreneur reflects their structural reality: they cannot exit. The rope classifications for wage earners and the revenue system reflect genuine coordination benefit. The scaffold classification for reform coalitions reflects that the constraint is not immutable: Congress has already added carveouts (real estate professional status, $25K passive loss exception for individual investors) and could further modify or repeal the rule. The piton classification reflects degradation: as planning strategies have proliferated, the enforcement burden has grown while the tax base protection has declined. The mandatrophy is not 'is PAL a snare or a rope?' but 'PAL is a tangled rope where the coordination and extraction components are distributed across different agent classes.' The wage earner experiences pure coordination (rope). The entrepreneur experiences pure extraction (snare). The organized investor experiences both (tangled rope). This presheaf of perspectives is the complete description of the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_participation_boundary,
    'Where is the bright-line threshold between passive and active business participation? Does 500 hours capture genuine material participation or is it arbitrary?',
    'Empirical analysis of audit outcomes, litigation, and IRS guideline consistency; comparison with other definitions of ''material'' participation in tax law (e.g., substantial understatement, economic substance doctrine)',
    'If 500-hour threshold is arbitrary: boundary becomes extractive mechanism (easy manipulation by planning). If threshold is defensible: classification shifts toward coordination. Either outcome affects χ for organized investor perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_participation_boundary, empirical, 'Threshold for material participation in passive activities').

omega_variable(
    loss_deferral_vs_permanent_denial,
    'Is the PAL rule a temporary deferral (losses preserved until death/sale) or permanent denial (losses expire with statute of limitations)?',
    'Legislative history analysis, case law on suspended loss treatment, actuarial modeling of loss recovery rates across lifespan distributions',
    'If pure deferral (losses eventually usable): extraction reduced by present value discount; snare classification weakens. If partial denial (many losses expire): extraction increases; snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(loss_deferral_vs_permanent_denial, empirical, 'Whether PAL represents temporary deferral or permanent loss denial').

omega_variable(
    prevention_of_legitimate_loss_offset,
    'Does PAL prevent genuine tax avoidance or does it also block legitimate loss recognition by real entrepreneurs with economic losses?',
    'Empirical study of PAL-affected taxpayers: proportion with genuine economic losses vs artificial shelter losses; correlation between PAL enforcement and business failure rates or capital allocation distortions',
    'If blocking legitimate losses: extraction is significant and efficiency-reducing. If mostly preventing shelters: extraction is justified coordination. This determines whether tangled_rope or snare classification is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prevention_of_legitimate_loss_offset, empirical, 'Whether PAL blocks legitimate losses or primarily prevents shelter abuse').

omega_variable(
    enforcement_consistency_and_manipulation,
    'How consistently does the IRS apply material participation tests across audit population? What fraction of PAL benefits accrue to agents with superior audit defense resources?',
    'IRS audit rate analysis by income bracket and passive activity type; litigation win rates by type of taxpayer (individual vs organized group); disparity analysis in passive activity grouping election outcomes',
    'If enforcement is consistent: rule applies uniformly. If enforcement is gamed by sophisticated agents: suppression is lower than measured; wealthy taxpayers exit more easily; rule becomes regressive extraction from unsophisticated agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_consistency_and_manipulation, empirical, 'IRS enforcement consistency and audit-based manipulation of PAL rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usc_26_469_passive_loss, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pal_tr_t0, usc_26_469_passive_loss, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pal_tr_t20, usc_26_469_passive_loss, theater_ratio, 20, 0.48).
narrative_ontology:measurement(pal_tr_t40, usc_26_469_passive_loss, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(pal_be_t0, usc_26_469_passive_loss, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(pal_be_t20, usc_26_469_passive_loss, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(pal_be_t40, usc_26_469_passive_loss, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usc_26_469_passive_loss, enforcement_mechanism).
narrative_ontology:affects_constraint(usc_26_469_passive_loss, tax_shelter_detection).
narrative_ontology:affects_constraint(usc_26_469_passive_loss, real_estate_capital_formation).
narrative_ontology:affects_constraint(usc_26_469_passive_loss, individual_income_tax_progressivity).

% DUAL FORMULATION NOTE:
% PAL is downstream of the broader tax shelter prevention regime and upstream of specific real estate market constraints. The network reflects that PAL enforcement affects capital availability (real_estate_capital_formation) and interacts with tax shelter detection strategies and progressive tax structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usc_26_469_passive_loss, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
