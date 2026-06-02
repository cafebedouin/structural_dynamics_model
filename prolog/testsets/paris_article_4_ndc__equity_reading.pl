% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: NDC Equity Obligation (Common But Differentiated Responsibilities Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   The Paris Agreement Article 4 NDC constraint embodies a contested kernel:
 *   how should the transition to global net-zero emissions be governed when
 *   states have radically asymmetric historical responsibility for
 *   atmospheric carbon, current capacity to decarbonize, and vulnerability to
 *   climate impacts? This story instantiates the EQUITY READING of Article 4:
 *   NDCs must be interpreted through Common But Differentiated
 *   Responsibilities (CBDR), requiring structural distinctions between
 *   developed and developing states. In this reading, developed states —
 *   which industrialized under high-carbon regimes and bear responsibility
 *   for cumulative atmospheric carbon — face binding absolute emissions
 *   reductions and transfer obligations to enable developing states to pursue
 *   poverty reduction while contributing proportionally to global mitigation.
 *   Developing states retain policy space and receive climate finance. Equity
 *   coalitions (BASIC, LDC group, Small Island States) gain veto power over
 *   supranational enforcement mechanisms that would override national policy
 *   autonomy. This reading is one of three sibling readings of the same
 *   kernel text. The sovereigntist reading prioritizes each state's right to
 *   define its own NDC commitments with minimal external constraint. The
 *   supranational reading prioritizes the global emissions budget and would
 *   subordinate national policy space to global allocative targets. These
 *   readings coexist in the UNFCCC negotiation space but are not logically
 *   compatible within a single state's framework — a state cannot
 *   simultaneously assert (1) CBDR as a binding principle that constrains
 *   developed states' behavior, (2) sovereignty as the primary organizing
 *   principle (which makes CBDR merely advisory), and (3) supranational
 *   authority as binding (which makes CBDR irrelevant because global
 *   authority overrides differentiation). The equity reading's extractiveness
 *   (0.48) reflects moderate asymmetry: developed states extract benefit
 *   (policy space, comparative advantage in clean technology markets) but are
 *   genuinely constrained by binding reduction obligations and finance
 *   commitments. Developing states achieve coordination benefits
 *   (risk-pooling via differentiation) but face suppression (historical
 *   responsibility framing used to justify climate conditionality on
 *   development aid). The theater ratio (0.58) models the gap between Article
 *   4's promise of equity enforcement and actual institutional capacity — the
 *   Loss and Damage Fund exists but is underfunded; NDC tracking occurs but
 *   without binding verification; Article 4 texts invoke equity but
 *   enforcement relies on state self-reporting and coalitional pressure.
 *
 * KEY AGENTS:
 *   - Developed States (EU, UK, US, Japan, Canada): Institutional beneficiaries (arbitrage exit) — capture policy space to set ambitious-sounding but achievable targets; benefit from technology transfer premium; use differentiation to export manufacturing emissions while claiming decarbonization.
 *   - Developing State Coalitions (BASIC, LDC Group, AOSIS, African Group): Organized victims/beneficiaries (constrained exit) — benefit from CBDR framing that legitimates lower targets; constrained by climate finance conditionality, historical responsibility pressure, and lack of technology access.
 *   - Least Developed Countries: Powerless victims (trapped exit) — face existential climate risk, cannot afford independent decarbonization, trapped by debt-for-climate obligations and development constraints.
 *   - Climate Finance Institutions (World Bank, Green Climate Fund): Institutional actors (arbitrage exit) — benefit from CBDR framing that creates demand for technical assistance and project finance; maintain gatekeeping power over differentiated finance allocation.
 *   - Global Climate System: Powerless victim (trapped exit) — cumulative emissions trajectory determined by aggregate NDC stringency; trapped in path-dependent warming from historical emissions and inadequate current commitments.
 *   - Analytical Observer: Sees full structure — can identify which elements are coordination (genuine burden-sharing) versus extraction (cost-shifting via differentiation framing).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.48).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.62).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "NDC Equity Obligation (Common But Differentiated Responsibilities Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '023df3d9-144a-40a7-b966-371748764b3d').
narrative_ontology:cs_kernel_codification('023df3d9-144a-40a7-b966-371748764b3d', formalized).
narrative_ontology:cs_authority_grounding('023df3d9-144a-40a7-b966-371748764b3d', lineage).
narrative_ontology:cs_interpretation_layer_present('023df3d9-144a-40a7-b966-371748764b3d').
narrative_ontology:cs_reading_relation('023df3d9-144a-40a7-b966-371748764b3d', paris_article_4_ndc_sovereigntist_reading, influences).
narrative_ontology:cs_reading_relation('023df3d9-144a-40a7-b966-371748764b3d', paris_article_4_ndc__supranational_reading, forecloses).
narrative_ontology:cs_axiom('023df3d9-144a-40a7-b966-371748764b3d', foundational, historical_responsibility_legitimates_differentiation).
narrative_ontology:cs_axiom_status(historical_responsibility_legitimates_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('023df3d9-144a-40a7-b966-371748764b3d', historical_responsibility_legitimates_differentiation, deontological).
narrative_ontology:cs_axiom('023df3d9-144a-40a7-b966-371748764b3d', foundational, developed_states_owe_transfers_to_enable_participation).
narrative_ontology:cs_axiom_status(developed_states_owe_transfers_to_enable_participation, holdable).
narrative_ontology:cs_axiom_grounding('023df3d9-144a-40a7-b966-371748764b3d', developed_states_owe_transfers_to_enable_participation, deontological).
narrative_ontology:cs_reference_frame('023df3d9-144a-40a7-b966-371748764b3d', paris_agreement_cbdr_framework).
narrative_ontology:cs_drift_state('023df3d9-144a-40a7-b966-371748764b3d', contemporary_2025_cde, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('023df3d9-144a-40a7-b966-371748764b3d', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, least_developed_countries).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, global_climate_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE DEVELOPING STATE (SNARE) — Trapped by existential climate risk and historical non-responsibility for emissions. NDC commitments extract through transfer obligations and conditional financing. No exit: cannot unilaterally withdraw from climate risk or restructure debt-for-climate obligations. Experiences maximum suppression.
constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING STATE COALITION (TANGLED ROPE) — Organized agents (India, South Africa, China coalitions) experience genuine coordination function (burden-sharing via differentiation) alongside asymmetric extraction (developed states shift decarbonization costs to developing states through NDC architecture). High suppression (constrained by historical responsibility framing), but organized exit optionality (UNFCCC walkout threat, alternative financing from China). Mixed extraction chi.
constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPED STATE — CLIMATE LEADER NARRATIVE (ROPE) — The EU and Nordic states experience NDC equity reading as pure coordination: Paris Article 4 coordinates burden-sharing while developed states retain policy arbitrage (can overachieve on NDCs at lower cost, transfer excess to developing states, then claim climate leadership). Net beneficiary experience — the equity reading legitimates their position while they externalize costs. Low experienced extraction.
constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL CLIMATE SYSTEM (SNARE) — Trapped by cumulative emissions. NDC inequity (developed states underbidding, developing states overcommitting to secure financing) prevents sufficient global mitigation. System bears extraction in the form of delayed decarbonization and locked-in warming. No exit mechanism.
constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPED STATE — REALPOLITIK POSITION (TANGLED ROPE) — From a powerful/mobile/immediate context, the same developed state experiences NDC equity as a mixed constraint: genuine coordination function (preventing climate tragedy of the commons) coupled with extraction asymmetry (developed states can exit via carbon pricing markets, green bonds, and domestic decarbonization while shifting manufacturing emissions to developing states that produce NDC-compatible goods). Exit is mobile because decarbonization is achievable through market mechanisms; coordination is necessary because climate is global; extraction flows because differentiation allows cost-shifting.
constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: UNFCCC ARTICLE 4 IMPLEMENTATION (PITON) — The treaty apparatus that claims to operationalize equity via differentiation is substantially degraded/theatrical. The institutions (NDC tracking, Loss and Damage Fund) exist but lack enforcement power, verification capacity, and binding transfer obligations. Theater ratio (0.58) reflects the gap between the text's promise of equity enforcement and actual institutional capacity. The apparatus persists through protocol legitimacy (inherited from Rio/Kyoto) despite functional atrophy.
constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, some burden asymmetry appears inevitable: developed states have already decarbonized their energy systems (sunk cost); developing states cannot decarbonize while meeting poverty reduction obligations. This perspective sees the inequity as a structural feature of asymmetric development, not a contingent institutional choice. HOWEVER: The structural data contradicts mountain classification. Identifiable beneficiaries (developed states, certain coalitions) exist. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paris_article_4_ndc__equity_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, TR),
    TR >= 0.70.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48): Moderate-high. The equity reading generates significant asymmetry in who bears mitigation costs. Developed states face binding absolute reduction commitments and transfer obligations worth ~$100B/year but retain comparative advantage in clean technology and can achieve reductions through efficiency gains and sectoral shifts. Developing states achieve lower targets through differentiation (legitimate under CBDR) but face suppression through conditionality: climate finance is contingent on NDC ambition, technology transfer is contingent on IP protections favorable to developed states, and development pathways are constrained by climate requirements. The extractiveness reflects that the burden-sharing is partial — genuine coordination via differentiation exists alongside cost-shifting mechanisms. Suppression (0.62): Moderate-high. Multiple suppression layers: (1) Structural — developing states cannot exit climate governance (existential risk) or development constraints; (2) Institutional — financial conditionality (IMF, World Bank require climate compatibility for structural adjustment loans); (3) Epistemic — the 'common but differentiated' framing can be used to justify why developed states face lower absolute targets (they've already decarbonized sunk costs) while developing states face stringent targets relative to development (differentiation becomes a ceiling, not a floor). Theater ratio (0.58): Moderate-high. The UNFCCC apparatus that ostensibly enforces Article 4 equity is substantially performative: NDC tracking exists but relies on self-reporting; the Loss and Damage Fund exists but is underfunded by orders of magnitude; Article 4 Article 6 mechanisms (carbon markets) are supposed to enable efficient mitigation but primarily service developed state compliance and do not guarantee developing state benefit; COP decisions invoke equity but lack enforcement power. The theater has increased over the interval (Paris 2015 to present) as the gap between stated commitments (net-zero pledges) and actual implementation (slow emissions decline) has widened, requiring more performative language about 'equity' and 'just transition' to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same institutional text generates radically different classifications depending on the observer's structural position. The developed state with arbitrage mobility sees Rope (coordination that enables cost-efficient global mitigation). The organized developing state coalition sees Tangled Rope (genuine differentiation coordination combined with asymmetric finance extraction). The powerless developing state sees Snare (trapped by climate risk and development constraints, coerced into stringent commitments via finance conditionality). The global system sees Snare (cumulative emissions from inadequate state-level commitments trap it in high-warming scenario). The realpolitik developed state sees Tangled Rope (the same institution simultaneously enables decarbonization and permits manufacturing-emissions outsourcing). The UNFCCC institutional apparatus sees Piton (the equity enforcement machinery is degraded — it persists through legitimacy inheritance from Rio/Kyoto but lacks binding power). The civilizational analytical observer risks seeing Mountain (the asymmetry appears inherent to development stages) but the structural data contradicts this — identifiable beneficiaries exist, extraction flows toward them, suppression mechanisms are deployed to maintain the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from beneficiary/victim status plus exit options. Developed states are beneficiaries with arbitrage exit — they can invest in decarbonization at competitive rates and exit carbon-intensive sectors; d ≈ 0.15–0.25 (low to moderate beneficiary position). Developing state coalitions are both beneficiaries (CBDR reduces their targets) and victims (finance conditionality suppresses their options); d ≈ 0.50–0.55 (symmetric to victim-leaning). Least-developed countries are pure victims with trapped exit; d ≈ 0.95 (maximum target position). The climate system has no agency; d ≈ 1.0 (pure target). The UNFCCC apparatus maintains institutional arbitrage (can set terms for climate finance access); d ≈ 0.20 (low-moderate beneficiary). The analytical observer derives d from structural visibility; d ≈ 0.72 (canonical observer position). These d values feed the sigmoid f(d) and produce chi = ε × f(d) × σ(S), with global scope modifier σ(1.2) amplifying the experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The equity reading resolves mandatrophy by showing that Article 4 can coherently coordinate emissions reductions while structurally advantaging developed states, because the differentiation mechanism serves both functions simultaneously. The apparatus achieves (1) genuine coordination — the global climate problem requires all states to participate; differentiation enables developing states' participation by reducing their targets; (2) asymmetric extraction — differentiation allows developed states to externalize costs through technology transfer requirements, carbon market mechanisms, and manufacturing-emissions outsourcing; (3) suppression — finance conditionality and implied responsibility framing pressure developing states into higher commitments than CBDR alone would justify. All three are operative. The classification as Tangled Rope (not pure Snare or pure Rope) reflects that the coordination and extraction components are both real and both structural. The theater ratio (0.58) captures the UNFCCC institutional gap — the equity enforcement machinery is substantially performative. If the theater ratio were lower (~0.30–0.40), the constraint would be pure Snare (extraction without genuine coordination benefit). If the theater ratio were higher (0.70+), the constraint would be Piton (degraded to pure performance). At 0.58, the constraint retains mixed function: the equity principle legitimately reduces targets for developing states and enables their participation, but the enforcement apparatus cannot ensure developed states honor their transfer obligations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    differentiation_mechanism_empirical,
    'Do historical responsibility metrics (per-capita vs. cumulative vs. territorial emissions) actually guide NDC stringency, or do they rationalize pre-existing power asymmetries?',
    'Regression analysis: correlation between stated responsibility metric and actual NDC targets; comparison of self-reported vs. empirically derived responsibility; time-series of responsibility metric definitions in UNFCCC language',
    'If responsibility metrics guide targets: equity reading is partially validated (differentiation serves allocative function). If pre-existing power determines targets: equity reading is a false cover story for power asymmetry (reinterprets as Snare from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differentiation_mechanism_empirical, empirical, 'Whether historical responsibility metrics guide NDC stringency or rationalize power asymmetries').

omega_variable(
    financing_obligation_binding_status,
    'Are developed state climate finance obligations under Article 4.4-4.5 legally binding or merely aspirational?',
    'Treaty interpretation via ICJ advisory opinion; analysis of enforcement mechanisms and penalties for non-delivery; comparison of finance commitments vs. actual transfers over 2009-2030 period',
    'If binding: developed states face real extraction (Tangled Rope classification robust). If aspirational: developed states retain arbitrage (Rope from their perspective, Snare from developing states'' perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financing_obligation_binding_status, conceptual, 'Binding vs. aspirational status of climate finance obligations').

omega_variable(
    equity_axiom_operationalization_gap,
    'Which interpretation of equity — historical responsibility, current capacity, vulnerability, or equal per-capita rights — governs actual NDC allocation in practice?',
    'Text analysis of NDC documents; voting patterns in UNFCCC COP decisions; interviews with negotiators documenting framings used in bilateral/coalitional negotiations',
    'If single metric dominates: that metric''s foundational axiom is operative. If multiple metrics coexist: equity_reading coexists_with other readings (not foreclosed). If no metric dominates: equity framing is purely performative (theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_axiom_operationalization_gap, conceptual, 'Which equity metric operationalizes Article 4 differentiation in practice').

omega_variable(
    kernel_reading_contest,
    'Is the Paris Article 4 NDC text best read through equity (burden-sharing based on responsibility/capacity), sovereignty (each state''s right to define own commitments), or supranational (global emissions budget that overrides national interest)?',
    'Comparative textual analysis: which reading best explains the text''s actual language and negotiation history; experimental framing of Article 4 to different stakeholder groups to measure which reading resonates; tracking which reading dominates in successive COP decisions and UNFCCC Secretariat guidance',
    'If equity reading proves most explanatory: NDC structure is genuinely organized around differentiation (current classification robust). If sovereigntist reading proves more explanatory: NDCs are merely binding self-commitment (reinterprets to Rope from all perspectives). If supranational reading proves more explanatory: equity is a constraint on state sovereignty (reinterprets to Snare for all states).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: equity vs sovereigntist vs supranational interpretation of Article 4').

omega_variable(
    developing_state_compliance_incentives,
    'What portion of developing state NDC compliance flows from genuine commitment to equity and what portion from finance conditionality or coercion?',
    'Analysis of countries that increased NDC ambition after receiving climate finance vs. those that maintained NDCs despite finance delays; interviews with negotiators from least-developed countries documenting domestic political constraints; comparison of NDCs submitted with vs. without finance commitment letters from developed states',
    'If high genuine commitment: equity reading reflects operative values (Tangled Rope classification robust, suppression moderate). If high conditionality: developing states are coerced (suppression high, shifts toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_state_compliance_incentives, empirical, 'Developing state NDC compliance drivers: commitment vs. conditionality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndc_equity_theater_t0, paris_article_4_ndc__equity_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ndc_equity_theater_t7, paris_article_4_ndc__equity_reading, theater_ratio, 7, 0.58).
narrative_ontology:measurement(ndc_equity_theater_t15, paris_article_4_ndc__equity_reading, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(ndc_equity_extract_t0, paris_article_4_ndc__equity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ndc_equity_extract_t7, paris_article_4_ndc__equity_reading, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(ndc_equity_extract_t15, paris_article_4_ndc__equity_reading, base_extractiveness, 15, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(ndc_equity_suppression_t0, paris_article_4_ndc__equity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ndc_equity_suppression_t7, paris_article_4_ndc__equity_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(ndc_equity_suppression_t15, paris_article_4_ndc__equity_reading, suppression_requirement, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, resource_allocation).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc_sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc_supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, climate_finance_conditionality).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, manufacturing_emissions_outsourcing).

% DUAL FORMULATION NOTE:
% Paris Article 4 NDC is a contested kernel decomposed into three sibling readings: equity_reading (this file), sovereigntist_reading, supranational_reading. Each reading generates different epsilon values and different beneficiary/victim structures, reflecting the logical incompatibility of the readings within a single state's framework. The equity reading emphasizes differentiation and generates moderate extractiveness (0.48) with clear developed/developing asymmetry. The sovereigntist reading emphasizes state autonomy and generates lower extractiveness (estimated 0.30–0.40) with no differentiation pressure. The supranational reading emphasizes global emissions budget and generates higher extractiveness (estimated 0.55–0.65) for all states relative to the global constraint. Network links reflect downstream constraints that depend on which reading governs: climate finance conditionality flows from equity reading; manufacturing emissions outsourcing flows from sovereignty reading being operationalized alongside equity language.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
