% ============================================================================
% CONSTRAINT STORY: iran_mandatrophic_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_mandatrophic_collapse, []).

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
 *   constraint_id: iran_mandatrophic_collapse
 *   human_readable: Iranian Mandatrophy (The Water-Economic Choke)
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   Iranian mandatrophy describes the structural hollowing of ecological and
 *   economic resilience caused by the rigid prioritization of the
 *   'Revolutionary Mandate' — regional proxy military funding, nuclear
 *   program advancement, and ideological self-sufficiency — over the organic
 *   survival margins of the state (aquifer systems, agricultural
 *   productivity, urban employment, currency stability). Beginning after
 *   1979, the Islamic Republic committed to a development trajectory that
 *   treated water, agricultural investment, and consumer economy as
 *   subordinate to security apparatus and proxy networks. This was initially
 *   a choice embedded in post-revolutionary ideology. Over 45 years, this
 *   choice has become increasingly coercive: aquifer depletion, agricultural
 *   collapse, and economic collapse have left the rural population and urban
 *   working class structurally trapped — unable to exit, unable to organize,
 *   unable to negotiate reallocation. The constraint exhibits the full
 *   signature of a mandatrophy: the extraction mechanism (state capture of
 *   resources for revolutionary mandate) is maintained through active
 *   enforcement (surveillance, control of subsidies, monopoly on currency),
 *   the victims are plural and unorganized (farmers, workers, youth), the
 *   beneficiaries are narrow and highly concentrated (security apparatus,
 *   proxy networks, connected elites), and the constraint progressively
 *   worsens because extraction creates desperation, which justifies increased
 *   suppression, which enables deeper extraction. The mandatrophy can only
 *   resolve through either (a) a fundamental reallocation of resources from
 *   mandate to margins (politically hazardous), (b) external intervention or
 *   regime change (geopolitically uncertain), or (c) continued deterioration
 *   until ecological and economic collapse forces restructuring. Current
 *   trajectory indicates (c) is most likely on a 10-30 year horizon.
 *
 * KEY AGENTS:
 *   - Rural Agricultural Communities: Primary victims (powerless/trapped) — aquifer depletion, soil degradation, loss of livelihood; no exit options
 *   - Urban Working Class: Secondary victims (moderate/constrained) — unemployment, inflation, food insecurity, reduced public investment
 *   - Revolutionary State Apparatus: Primary beneficiary (institutional/arbitrage) — captures water allocation, budget prioritization, security funding; maintains through active enforcement
 *   - Proxy Military Networks: Secondary beneficiary (organized/arbitrage) — receives steady funding, equipment, operational support; coordinates regional operations
 *   - International Sanctions Regime: Tertiary actor (institutional/constrained) — ostensibly enforces political conditionality but operates with high theater and inconsistent implementation
 *   - Ecological Resilience: Systemic victim (powerless/trapped) — aquifer depletion, soil salinization, biodiversity loss; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_mandatrophic_collapse, 0.78).
domain_priors:suppression_score(iran_mandatrophic_collapse, 0.82).
domain_priors:theater_ratio(iran_mandatrophic_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, extractiveness, 0.78).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_mandatrophic_collapse, snare).
narrative_ontology:human_readable(iran_mandatrophic_collapse, "Iranian Mandatrophy (The Water-Economic Choke)").
narrative_ontology:topic_domain(iran_mandatrophic_collapse, "political/economic/technological").

domain_priors:requires_active_enforcement(iran_mandatrophic_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, revolutionary_state_apparatus).
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, proxy_military_networks).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, rural_agricultural_communities).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, urban_working_class).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, ecological_resilience).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL AGRICULTURAL COMMUNITY (SNARE) — Trapped by aquifer depletion caused by state prioritization of nuclear programs and proxy military funding over agricultural sustainability. No exit option: cannot migrate (urban unemployment), cannot innovate (no investment), cannot organize (state surveillance). Bears maximum extraction cost through water scarcity, soil degradation, and loss of livelihood. Effective extraction is maximal — structural powerlessness combined with complete dependency on constraint.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: URBAN WORKING CLASS (SNARE) — Constrained by inflation, currency collapse, and unemployment driven by sanctions and misallocation of resources toward revolutionary mandate. Limited exit options (internal migration saturated, external migration blocked). Bears extraction through reduced wages, food insecurity, and lack of public investment in healthcare and education. The constraint systematically channels wealth toward state security apparatus and away from social welfare.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REVOLUTIONARY STATE APPARATUS (TANGLED ROPE) — Primary beneficiary of the mandate prioritization. Experiences a hybrid structure: genuine coordination function (unifying security apparatus, ideological coherence, state capacity for asymmetric regional competition) combined with massive extraction (capturing water, budget allocation, natural resources for proxy networks). Has arbitrage options (can shift resources, negotiate sanctions relief, reorient investments) but chooses to maintain the extraction mechanism. Active enforcement through surveillance, resource monopoly, and control of agricultural subsidies.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROXY MILITARY NETWORKS (ROPE) — Direct beneficiary of mandate prioritization. Receives steady funding, equipment, and operational support from state apparatus. Experiences constraint primarily as coordination mechanism (resource allocation, secure supply chains, unified command structure). Can shift resources, seek alternative funding, or exit if desired, but the coordination benefit is high. No suppression felt because extraction flows toward this agent.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL SANCTIONS REGIME (PITON) — Ostensibly enforces political conditionality (nuclear non-proliferation, human rights compliance) but operates largely as theater masking geopolitical competition and domestic political cycles in sanctioning states. The stated coordination function (enforcing international law) is substantially decoupled from actual function (resource extraction channeled through regime allies, enforcement inconsistency across similar actors). Theater ratio high because effectiveness debates persist despite consistent pattern of entrenchment. Maintains form through institutional inertia despite degraded coordination function.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT) — Risks naturalizing the mandatrophy as an immutable consequence of geopolitical position or 'national character.' Frames Iran's water crisis and economic collapse as inescapable natural constraints rather than as contingent institutional choices. This perspective falsely claims the constraint is a mountain (natural law) when structural analysis reveals it as a manufactured snare maintained through active enforcement. The engine's false summit detector identifies this naturalization error.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_mandatrophic_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_mandatrophic_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_mandatrophic_collapse, TR),
    TR >= 0.70.

:- end_tests(iran_mandatrophic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high and increasing. The constraint extracts resources from the rural and urban poor and redirects them toward the security apparatus and proxy networks. The extraction mechanism is structural — water subsidies for irrigation favor state-controlled industrial agriculture over smallholder farms; budget allocation prioritizes defense over healthcare and education; currency control channels hard currency to regime-connected elites. The trajectory over 45 years shows increasing extraction as the original post-revolutionary choice has hardened into coercive mechanism. Initial extractiveness (0.42 in 1979) reflected voluntary acceptance of sacrifice for revolutionary goals. Current extractiveness (0.78) reflects that the victims have no voluntary option — they are trapped by ecological collapse and economic desperation. Suppression (0.82): Very high. The state maintains the constraint through surveillance of dissent, monopoly control over water allocation, blocking of emigration, and restriction of information about aquifer depletion. Farmers cannot organize collective action; workers cannot form independent unions; journalists reporting on water crisis face imprisonment. Theater ratio (0.68): Moderately high and increasing. The regime presents the mandate as national necessity and dignity (anti-imperialism, nuclear sovereignty) while obscuring its connection to ecological and economic collapse. Public discourse about aquifer depletion is suppressed; official statistics underestimate unemployment; sanctions are blamed for economic collapse rather than mandate prioritization. Theater increased from 0.35 (early revolutionary period) to 0.68 (current) as the objective contradictions between mandate and welfare became harder to hide.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap is the maximum possible under the framework. From the powerless victim's perspective, it is pure extraction with maximal coercion (snare). From the beneficiary's perspective, it is coordination with low or negative extraction (rope or tangled rope). From the international observer's perspective, it risks naturalizing as immutable geopolitical necessity (false summit). The gap reflects that exit options vary from zero (rural farmer) to maximum (international actor), power levels vary from powerless to institutional, and the extraction flows unidirectionally from one group to another. This is the signature gap of a mandatrophy under the framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from power level, exit options, and beneficiary/victim status. Rural farmers (powerless/trapped/victim) derive d ≈ 0.95, producing f(d) ≈ 1.42 and χ ≈ 0.78 × 1.42 × 1.0 ≈ 1.11 (capped at effective maximum). Urban workers (moderate/constrained/victim) derive d ≈ 0.70, producing f(d) ≈ 1.08 and χ ≈ 0.78 × 1.08 × 1.0 ≈ 0.84. The state apparatus (institutional/arbitrage/beneficiary) derives d ≈ 0.05, producing f(d) ≈ -0.12 and χ ≈ 0.78 × (-0.12) × 1.0 ≈ -0.09 (negative effective extraction — they benefit). Proxy networks (organized/arbitrage/beneficiary) derive d ≈ 0.30, producing f(d) ≈ 0.25 and χ ≈ 0.78 × 0.25 × 1.0 ≈ 0.19 (low extraction). The directionality distribution reveals the classic snare structure: concentrated benefits for a narrow group, distributed costs for a large group with no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_natural_law.
 *   MANDATROPHY RESOLUTION: The Iranian mandatrophy resolves through explicit declaration that extractiveness > 0.70, requiring mandatrophy_resolved: true. The resolution identifies three structural facts: (1) The constraint began as a choice (post-revolutionary mandate prioritization) but has become increasingly coercive as its consequences (aquifer depletion, economic collapse) narrowed escape routes. (2) The constraint exhibits genuine coordination function alongside extraction — the security apparatus does provide regional deterrence and the proxy networks do enable asymmetric power projection. But the coordination function does not require the magnitude of extraction currently occurring — the extractiveness level (0.78) exceeds what coordination alone would demand. (3) The constraint is unsustainable: current extractiveness trajectory (0.42 → 0.78 over 45 years) cannot continue indefinitely due to ecological limits (aquifer irreversibility ~10-30 years) and economic collapse. Resolution on the mandatrophy occurs when the constraint's internal contradictions force restructuring. The two likeliest paths are (a) controlled transition: regime pragmatically reallocates resources from mandate to margins before ecological/economic collapse forces it, or (b) uncontrolled collapse: aquifer irreversibility and currency crisis trigger state failure and restructuring under duress. Path (b) is currently more likely given the regime's rigidity on mandate prioritization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_flexibility_threshold,
    'What percentage reallocation from proxy military funding to water/agricultural investment would trigger regime instability or loss of regional deterrent capacity?',
    'Scenario modeling of regional balance-of-power; analysis of proxy network dependencies; historical comparison with post-revolutionary prioritization shifts',
    'If threshold < 5%: reallocation path exists with minimal strategic loss (snare is choice-dependent). If threshold > 30%: mandate appears structurally necessary for regime survival (shifts toward mountain). If threshold 5-15%: reveals hybrid extraction-coordination structure (confirms tangled rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_flexibility_threshold, empirical, 'Threshold for mandate reallocation before strategic destabilization').

omega_variable(
    rural_exit_velocity,
    'What rate of rural-to-urban migration does the urban employment system actually sustain before labor market saturation becomes catastrophic?',
    'Time-series analysis of rural population decline, urban unemployment, informal economy absorption capacity, and emigration flow rates',
    'If saturation < 40% rural outmigration: exit is genuinely trapped (snare confirmed). If saturation > 70%: limited exit routes exist (modifies to tangled rope for some agents). Current trajectory suggests saturation near 60% — confirming partial trap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rural_exit_velocity, empirical, 'Urban labor market saturation threshold under rural outmigration').

omega_variable(
    aquifer_irreversibility_point,
    'At what depletion level do Iranian aquifers cross into irreversible damage where even cessation of extraction cannot restore pre-crisis hydrological function?',
    'Hydrogeological modeling; comparison with irreversible depletion cases (Ogallala, Indus); analysis of compaction, mineral concentration, and microbial community collapse',
    'If Iran has already crossed point: constraint becomes literally a mountain (irreversible natural law). If not crossed but < 10 years remain: urgency is high but choice window exists. If > 30 years: delay permits continued mandate prioritization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aquifer_irreversibility_point, empirical, 'Aquifer irreversibility threshold and time to crossing').

omega_variable(
    proxy_network_substitutability,
    'How substitutable are Iran''s proxy networks with alternative state power projection mechanisms (conventional military, soft power, economic integration)?',
    'Comparative analysis of regional deterrent effect per unit cost; modeling of conflict scenarios under different force postures; analysis of proxy cost trends',
    'If highly substitutable: proxy funding can be reduced without strategic loss (suggests choice to maintain extraction). If non-substitutable: proxy funding becomes structurally necessary (reinforces mountain framing). Most evidence suggests moderate substitutability — supporting snare/tangled rope rather than mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_network_substitutability, conceptual, 'Substitutability of proxy networks for alternative power projection').

omega_variable(
    regime_legitimacy_dependency,
    'How dependent is the Islamic Republic''s internal legitimacy on the revolutionary mandate (anti-imperialism, regional hegemony, ideological leadership)? Would abandoning mandate prioritization trigger political collapse?',
    'Analysis of regime stability mechanisms; comparison with other post-revolutionary states; survey and historical data on popular support for mandate vs. welfare spending',
    'If highly dependent: mandate is functionally inseparable from regime structure (supports mountain or necessary tangled rope). If weakly dependent: mandate is extractive choice rather than structural necessity (supports snare). Evidence suggests moderate dependency — mandate is ideology, but pragmatic reallocation is theoretically possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_legitimacy_dependency, preference, 'Regime legitimacy dependency on revolutionary mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_mandatrophic_collapse, 1979, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_tr_t0, iran_mandatrophic_collapse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iran_tr_t15, iran_mandatrophic_collapse, theater_ratio, 15, 0.52).
narrative_ontology:measurement(iran_tr_t30, iran_mandatrophic_collapse, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(iran_be_t0, iran_mandatrophic_collapse, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(iran_be_t15, iran_mandatrophic_collapse, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(iran_be_t30, iran_mandatrophic_collapse, base_extractiveness, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_mandatrophic_collapse, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_mandatrophic_collapse, gulf_water_scarcity).
narrative_ontology:affects_constraint(iran_mandatrophic_collapse, middle_east_proxy_competition).
narrative_ontology:affects_constraint(iran_mandatrophic_collapse, sanctions_regime_effectiveness).

% DUAL FORMULATION NOTE:
% The Iranian mandatrophy can be decomposed into three structurally related constraints: (1) constraint_aquifer_depletion (ε ≈ 0.15, ecological mountain) — the inherent limits of groundwater extraction, (2) constraint_resource_allocation_conflict (ε ≈ 0.50, tangled rope) — the hybrid coordination-extraction structure of revolutionary mandate prioritization, (3) constraint_iran_mandatrophic_collapse (ε ≈ 0.78, snare) — the systemic feedback loop where extraction creates desperation which justifies suppression which enables deeper extraction. The three constraints are causally linked: aquifer depletion is enabled by mandate prioritization, mandate prioritization creates economic desperation, and desperation feeds back into suppression. The present story covers the emergent snare level; upstream constraints cover the ecological and institutional foundations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_mandatrophic_collapse, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
