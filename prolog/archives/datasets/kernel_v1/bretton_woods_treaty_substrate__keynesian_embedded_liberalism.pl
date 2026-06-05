% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods: Keynesian Embedded Liberalism (Capital Controls & Policy Space)
 *   domain: international_political_economy/monetary_institutions
 *
 * SUMMARY:
 *   The Bretton Woods treaty (1944) creates a constraint on international
 *   capital mobility justified by a specific theory of macroeconomic
 *   governance: Keynes and White argued that capital controls are necessary
 *   to permit national full-employment and welfare-state policy without
 *   external constraint. The constraint operates by fixing exchange rates to
 *   gold and the dollar, limiting currency speculation, and
 *   institutionalizing capital controls as legitimate tools for preserving
 *   policy autonomy. This is ONE READING of the contested Bretton Woods
 *   kernel—specifically, the Keynesian embedded liberalism reading that
 *   treats capital controls as the core commitment and trade coordination as
 *   the instrumental benefit. The sibling readings (neoliberal_convertibility
 *   and sovereignty_defense) read the same treaty text differently,
 *   emphasizing capital mobility and national defense respectively, and would
 *   produce different victim/beneficiary structures and classification
 *   outcomes. Under the Keynesian reading, national governments and labor
 *   movements are beneficiaries; international capital markets and peripheral
 *   economies are victims. The constraint exhibits tangled_rope structure:
 *   genuine coordination function (stable exchange rates enable trade),
 *   active enforcement (IMF policing of capital controls, gold pool
 *   management), asymmetric extraction (governments gain policy autonomy at
 *   the cost of capital restriction), and clear beneficiary/victim
 *   distinction.
 *
 * KEY AGENTS:
 *   - National Governments (Keynesian welfare states): Primary beneficiaries (institutional/constrained) — gain policy autonomy, full-employment capacity, welfare-state protection from external capital pressure
 *   - International Capital Markets: Primary victims (powerful/arbitrage) — face capital controls, portfolio restrictions, inability to arbitrage interest rate differentials across borders
 *   - Labor Movements & Welfare Constituencies: Secondary beneficiaries (organized/constrained) — benefit from full-employment policy, protected labor standards, public investment enabled by fiscal autonomy
 *   - Peripheral & Developing Economies: Secondary victims (powerless/trapped) — dependent on IMF credit for participation; constrained by dollar peg; experience capital scarcity without genuine policy autonomy
 *   - IMF/World Bank (Bretton Woods Institutions): Institutional managers (institutional/mobile) — enforce the framework through conditionality; see capital controls as temporary, with sunset toward capital liberalization
 *   - United States (Dollar Hegemon): Institutional authority (institutional/arbitrage) — maintains nominal gold backing; increasingly experiences function degradation as reserves deplete; piton perspective as hegemony becomes theatrical
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summit: naturalizing an institutional policy choice (capital controls + fixed rates) as natural law rather than designed system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.35).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.42).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.35).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods: Keynesian Embedded Liberalism (Capital Controls & Policy Space)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_institutions").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'bw-keynes-1740552000').
narrative_ontology:cs_kernel_codification('bw-keynes-1740552000', formalized).
narrative_ontology:cs_authority_grounding('bw-keynes-1740552000', lineage).
narrative_ontology:cs_interpretation_layer_present('bw-keynes-1740552000').
narrative_ontology:cs_reading_relation('bw-keynes-1740552000', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('bw-keynes-1740552000', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('bw-keynes-1740552000', foundational, capital_controls_are_legitimate_tools).
narrative_ontology:cs_axiom_status(capital_controls_are_legitimate_tools, holdable).
narrative_ontology:cs_axiom_grounding('bw-keynes-1740552000', capital_controls_are_legitimate_tools, instrumental).
narrative_ontology:cs_axiom('bw-keynes-1740552000', foundational, national_welfare_autonomy_overrides_capital_mobility).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_overrides_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('bw-keynes-1740552000', national_welfare_autonomy_overrides_capital_mobility, deontological).
narrative_ontology:cs_reference_frame('bw-keynes-1740552000', keynesian_full_employment_sovereignty).
narrative_ontology:cs_drift_state('bw-keynes-1740552000', contemporary_neoliberal_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('bw-keynes-1740552000', '2026-02-26T12:00:00Z').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, labor_movements).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_constituencies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_capital_markets).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATIONAL GOVERNMENT (ROPE) — Bretton Woods creates genuine coordination benefit: fixed exchange rates reduce currency volatility, enabling trade planning. Simultaneously benefits from capital control provisions that permit fiscal autonomy and full-employment policy without external constraint. Extraction runs TOWARD this agent — they are the intended beneficiary of the design. Constrained exit reflects institutional commitment to the treaty framework, not mobility restriction.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL CAPITAL HOLDER (TANGLED ROPE) — Bretton Woods enforces capital controls that restrict cross-border investment flows, yet simultaneously guarantees exchange-rate stability that reduces currency risk on international portfolios. Genuine coordination benefit (predictable rates for trade financing) paired with asymmetric extraction (capital mobility restrictions). Arbitrage exit reflects ability to route capital through permitted channels or into capital-scarce permitted jurisdictions.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PERIPHERAL ECONOMY (SNARE) — Smaller nations are trapped by dollar-peg commitment: they cannot pursue independent monetary policy, face severe capital scarcity, and depend on IMF/World Bank credit conditioned on accepting the framework's constraints. No exit capacity — withdrawal from Bretton Woods invokes severe economic isolation. Experiences the constraint as pure extraction masked by rhetorical coordination (development credit).
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: LABOR MOVEMENT & WELFARE COALITION (ROPE) — Primary constituency benefiting from Keynesian embedded liberalism. Capital controls enable full-employment policy without external deflationary pressure. Governments can pursue income redistribution, social insurance, and public investment without triggering capital flight. Exit is constrained by political-institutional commitments to the welfare state architecture, not by external barriers.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: BRETTON WOODS INSTITUTION (SCAFFOLD) — IMF and World Bank see the capital-control framework as a temporary coordination mechanism: necessary during post-war capital scarcity and reconstruction, but designed to sunset as capital markets mature and countries rebuild reserves. Institutions have mobile exit options (institutional pivots, mandate reinterpretation) and see a planned transition toward capital liberalization. Theater of 'development assistance' masks the underlying structural transition.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THE DOLLAR-HEGEMON (PITON) — The United States holds Bretton Woods authority nominally through IMF governance and gold-backing commitment, but this function degrades as: (a) US gold reserves deplete from persistent deficits, (b) the framework becomes theater ('gold guarantee' increasingly hollow as US runs trade surpluses that undermine the adjustment mechanism), and (c) institutional inertia sustains dollar primacy after the functional basis erodes. Theater ratio reflects that dollar authority persists through coordination-norm capture and institutional lock-in after the material constraint (actual gold reserves) weakens.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a civilizational perspective, the coupling of fixed exchange rates to trade flows is presented as a natural law: 'Floating rates cause destructive instability; fixed rates are necessary for commerce.' This perspective treats the institutional substrate (treaty, gold standard, IMF coordination) as a natural discovery rather than a policy choice. However, the structural data contradicts mountain classification — identifiable beneficiaries (governments, labor movements) and clear victims (capital markets, peripheral economies) suggest a false summit masking contingent institutional design choices.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, TR),
    TR >= 0.70.

:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate, reflecting the genuine mixed-benefit structure of embedded liberalism. The constraint is not pure coordination (χ would be < 0.35) because capital controls do restrict legitimate international investment flows and impose real costs on capital holders. But it is not high extraction (ε > 0.46) because the fixed-rate regime and trade coordination deliver genuine benefit even to capital holders—currency stability for international commerce is real. Measurement trajectory shows rising extractiveness (0.18 → 0.35) as initial post-war capital scarcity gives way to European recovery and dollar surpluses accumulate, increasing pressure on the constraint. Suppression (0.42): Moderate-to-high initially, declining over the measured interval. Post-war conditions (capital scarcity, reconstruction need, weak alternative institutions) make suppression easier to enforce—agents have few exit options and states have strong coordination interest. As capital accumulates and alternative markets develop (Eurodollar market post-1957), suppression requirement declines; agents develop evasion paths and psychological resistance grows. Theater ratio (0.38): Moderate, increasing over time. Early Bretton Woods theater is low (genuine capital scarcity, real coordination need, functional gold pool management). By the 1950s, theater rises as: (a) Eurodollar market develops as escape valve, (b) US gold reserves erode while dollar authority persists through institutional inertia, (c) capital control evasion becomes sophisticated (transfer pricing, leads/lags), creating performative enforcement. Claimed type: Tangled Rope. This reading sees embedded liberalism as a hybrid: genuine coordination (trade stability) + legitimate but asymmetric extraction (capital restrictions that benefit governments at capital's expense). This distinguishes the Keynesian reading from the neoliberal_convertibility reading (which would classify as Rope—capital flows are coordination, controls are violations) and sovereignty_defense reading (which would focus on the constraint's defense function rather than its extraction mechanism).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is fundamental and reflects the reading contest at the kernel level. The Keynesian reading assumes that governments are beneficiaries and capital markets are victims; this produces a tangled-rope classification at the national government level (genuine coordination + benefit) and snare classification at peripheral economies (trapped, no policy autonomy). The neoliberal_convertibility reading would reverse the beneficiary/victim assignment (capital liberalization is the 'true' benefit, controls are the violation), producing rope classification for capital markets and snare for governments constrained by external capital discipline. The sovereignty_defense reading would shift focus away from capital flows entirely, seeing the constraint's function as defense against external economic coercion (hegemonic pressure, imperial financial control), which would produce a different classification entirely based on whether defense succeeds. The Keynesian reading is not 'wrong'—it is a coherent structural reading of the treaty that emphasizes one layer of the institutional design (capital control provisions as legitimate policy tools) and treats that layer as central to understanding the constraint. The piton perspective (dollar hegemony as degraded institutional function) is internal to the Keynesian reading—it shows that even as the core Keynesian benefit (capital-control-protected policy space) degrades, the institutional structure persists through inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d values) derive from each agent's structural relationship to the capital-control-and-fixed-rate constraint. National governments benefit (d ≈ 0.25–0.35) from the constraint because capital controls enable fiscal autonomy and the fixed rate provides exchange-rate certainty for trade. Capital holders face restriction (d ≈ 0.65–0.75) because controls prevent profitable capital arbitrage across borders. Peripheral economies face severe extraction and minimal benefit (d ≈ 0.85–0.95) because they are trapped by the dollar peg, depend on IMF credit, and have no independent monetary policy. The engine derives these d values automatically from the beneficiary/victim declarations and exit options. The Keynesian reading's beneficiary set (governments, labor) produces low d; the victim set (capital markets, peripheral economies) produces high d. The empirical signal: if the neoliberal_convertibility reading were adopted (reversing beneficiary/victim), d values would reverse, and the constraint would reclassify as Rope (capital liberalization) rather than Tangled Rope (embedded liberalism). This perspectival flip is not a measurement error—it is the reading contest made visible through the classification mathematics.
 *
 * MANDATROPHY ANALYSIS:
 *   The Keynesian embedded liberalism reading avoids mandatrophy by maintaining internal coherence across the five classification gates: (1) Genuine coordination function: trade stability via fixed rates. (2) Asymmetric extraction: capital controls and IMF conditionality restrict some agents' options while benefiting others. (3) Active enforcement: IMF peer surveillance, gold pool management, Article XIV capital controls. (4) Beneficiary/victim structure: governments + labor = beneficiaries; capital markets + peripheral economies = victims. (5) Measurable extraction asymmetry: beneficiaries experience the constraint as coordination (enabling their preferred policies); victims experience it as restriction. The constraint classifies as Tangled Rope under this reading because all five gates align: it is neither pure coordination (Rope) nor pure extraction (Snare), but a hybrid regime where coordination and extraction are structurally entangled. The mandatrophy would arise only if the reading contest were NOT acknowledged—if we tried to force a single classification across all readings. By explicitly declaring this as a kernel reading with sibling alternatives, the framework routes the contest through omega variables and cs_structure rather than embedding it as unresolved contradiction in the metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embedded_liberalism_temporal_viability,
    'Is embedded liberalism a sustainable equilibrium or an inherently unstable compromise between capital accumulation and national policy autonomy?',
    'Historical trajectory analysis: post-1968 collapse of Bretton Woods; decomposition of embedded liberalism into neoliberal capital liberalization (1980s–2020s); empirical measurement of erosion in capital controls and welfare-state autonomy across OECD period',
    'If sustainable: Keynesian reading is a legitimate permanent constraint architecture. If unstable: Keynesian reading is a historical period artifact, and neoliberal liberalization is the terminal attractor toward which the system drifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embedded_liberalism_temporal_viability, empirical, 'Whether embedded liberalism is stable equilibrium or inherently unstable').

omega_variable(
    capital_control_effectiveness,
    'Do actual capital controls function as the treaty intends — preventing destabilizing capital outflows and preserving monetary autonomy — or do markets develop sufficient workarounds (leads and lags, transfer pricing, parallel markets) that controls become theatrical?',
    'Empirical audit of capital flow patterns under Bretton Woods controls vs stated treaty intent; identification of evasion mechanisms; measurement of effective policy space actually retained vs policy autonomy claimed by treaty rhetoric',
    'If effective: capital controls deliver real policy space to governments; Keynesian reading classifies as genuine tangled rope (real coordination + real asymmetric benefit). If largely evaded: controls are primarily theater maintaining the fiction of Keynesian autonomy; constraint reclassifies toward piton (degraded function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_control_effectiveness, empirical, 'Actual vs theoretical effectiveness of capital controls under Bretton Woods').

omega_variable(
    dollar_hegemony_material_basis,
    'What maintains US dollar authority under Bretton Woods as US gold reserves deplete and trade surpluses accumulate in other hands? Is authority sustained by genuine material (remaining gold, dollar liquidity preference) or by institutional inertia and path dependence?',
    'Decomposition of dollar demand into: (a) gold-backed demand (reserve adequacy analysis), (b) Eurodollar market growth (institutional escape valve), (c) petrodollar arrangements (post-1973), (d) network coordination effects (switching costs). Track which component dominates at each time period.',
    'If primarily material: dollar hegemony is economically grounded and persistent. If primarily institutional: hegemony is increasingly theatrical, and the 1971 collapse reflects material basis exhaustion, making piton classification appropriate for later period.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dollar_hegemony_material_basis, empirical, 'Material vs institutional basis for dollar authority under Bretton Woods').

omega_variable(
    reading_contest_location,
    'KERNEL READING UNCERTAINTY: This constraint instantiates the Keynesian embedded liberalism reading of the Bretton Woods kernel. Where exactly does the reading contest locate? Is it: (a) factual disagreement about what Bretton Woods actually permits (capital controls yes/no)? (b) normative disagreement about whether capital mobility restriction is legitimate? (c) systemic disagreement about whether embedded liberalism is stable? (d) definitional disagreement about what ''capital'' and ''control'' mean in treaty text?',
    'Comparative reading analysis: examine neoliberal_convertibility reading (marginalizes capital controls, emphasizes capital flow liberalization as treaty endpoint) and sovereignty_defense reading (treats capital controls as irrelevant; focuses on defense of national policy space through devaluation, tariffs, trade negotiation). Locate precise points where axioms and reference frames diverge.',
    'If primarily factual: reading contest is about treaty interpretation and can be resolved by textual and historical analysis. If primarily normative: readings coexist as different value commitments. If primarily systemic/stability: readings foreclose each other (one equilibrium stable, other unstable). If definitional: readings may be incommensurable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_location, conceptual, 'Location of reading contest: textual, normative, systemic, or definitional').

omega_variable(
    false_summit_natural_law_risk,
    'Is the mountain perspective''s claim that ''fixed exchange rates are necessary for commerce'' a genuine natural law of economics or a naturalization of mid-20th-century institutional choice?',
    'Counterfactual analysis: pre-Bretton Woods trade patterns under floating rates (1920s gold standard, 1930s devaluation blocs), post-Bretton Woods floating-rate trade patterns (1973–present). Measurement of trade stability under different exchange regimes. Examination of whether alternative coordination mechanisms (commodity standards, EMU-type currency unions, local-currency trade) achieve equivalent benefits.',
    'If natural law: mountain classification is correct; fixed rates are inherent to functioning commerce. If institutional choice: mountain is a false summit; the constraint is a designed system with identifiable beneficiaries and victims, classifying as tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, empirical, 'Whether fixed exchange rates are natural law or institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwkeynes_theater_t0_1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bwkeynes_theater_t7_1951, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 7, 0.32).
narrative_ontology:measurement(bwkeynes_theater_t14_1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 14, 0.38).

% Extraction over time
narrative_ontology:measurement(bwkeynes_extract_t0_1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(bwkeynes_extract_t7_1951, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 7, 0.32).
narrative_ontology:measurement(bwkeynes_extract_t14_1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 14, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(bwkeynes_suppress_t0_1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bwkeynes_suppress_t7_1951, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 7, 0.48).
narrative_ontology:measurement(bwkeynes_suppress_t14_1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 14, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, gold_standard_exchange_rate_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_conditionality_sovereignty).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, eurodollar_market_emergence).

% DUAL FORMULATION NOTE:
% The Bretton Woods kernel is contested across three readings with different ε values and beneficiary/victim structures. The Keynesian reading (this file) treats capital-control-protected policy space as the core commitment (ε=0.35, governments beneficiary, capital victim). The neoliberal_convertibility reading treats capital liberalization as the core commitment (higher ε, capital beneficiary, governments victim). The sovereignty_defense reading treats policy autonomy defense as core (mechanism-neutral). All three are readings of the same institutional text; they are NOT separate constraints. Constraint families must be linked: this file to its sibling readings via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
